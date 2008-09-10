// ==========================================================================
// Tetris.Piece
// ==========================================================================

/** @class

  (Document your class here)

  @author AuthorName
  @version 0.1
*/
Tetris.Piece = {
    
    create: function() {
	return this.createWith(Tetris.CoordSet.createWith([[0,3]]));
    },

    createWith: function(cells) {
	var that = this;
	var m_cells = cells;
	var translate = function(dr, dc) {
	    var cells = Tetris.CoordSet.create();
	    m_cells.each(function(row, col) {
		cells.add(row + dr, col + dc);
	    });
	    return that.createWith(cells);
	};
	return {
	    contains: function(r, c) {
		return m_cells.contains(r,c);
	    },
	    each: function(doWith) {
		m_cells.each(doWith);
	    },
	    left: function() {
		return translate(0, -1);
	    },
	    right: function() {
		return translate(0, 1);
	    },
	    down: function() {
		return translate(1, 0);
	    },
	    rotate: function() {
		return translate(0, 0);
	    },
	};
    },
};

Tetris.SquarePiece = {
    
    create: function() {
	return Tetris.Piece.createWith(Tetris.CoordSet.createWith([[0,3],[0,4],[1,3],[1,4]]));
    },
};

Tetris.createCenteredPieceFactory = function(settings) {

    return {
	create: function() {
	    return this.createWith(settings.initial.center.row, settings.initial.center.col, settings.initial.direction);
	},

	createWith: function(centerRow, centerCol, direction) {
	    var that = this;
	    var cells = settings.cells(centerRow, centerCol, direction);
	    var nextDirection = function() {
		for(var i = 0; i < settings.directions.length - 1; i++) {
		    if (settings.directions[i] === direction) {
			return settings.directions[i+1];
		    }
		}
		return settings.directions[0];
	    }();

	    return {
		contains: function(row, col) {
		    return cells.contains(row, col);
		},
		each: function(doWith) {
		    return cells.each(doWith);
		},
		left: function() {
		    return that.createWith( centerRow, centerCol - 1, direction);
		},
		right: function() {
		    return that.createWith( centerRow, centerCol + 1, direction);
		},
		down: function() {
		    return that.createWith( centerRow + 1, centerCol, direction);
		},
		rotate: function() {
		    return that.createWith(centerRow, centerCol, nextDirection);
		},
	    };
	},
    };
};

Tetris.IPiece = Tetris.createCenteredPieceFactory({

    initial: { center: { row: 2, col: 3}, direction: "vertical" },

    directions: ["vertical", "horizontal"],

    cells: function(centerRow, centerCol, direction) {
	if (direction === "vertical") {
	    return Tetris.CoordSet.createWithXY(Tetris.Arrays.range(centerRow - 2, centerRow + 1), [centerCol]);
	} else {
	    return Tetris.CoordSet.createWithXY([centerRow], Tetris.Arrays.range(centerCol - 2, centerCol + 1));
	}
    },
});
    
Tetris.TPiece = Tetris.createCenteredPieceFactory({

    initial: { center: { row: 0, col: 3}, direction: "down" },

    directions: ["down", "left", "up", "right"],

    cells: function(centerRow, centerCol, direction) {
	var result = Tetris.CoordSet.createWith([[centerRow, centerCol]]);

	if (direction !== "down") {
	    result.add(centerRow - 1, centerCol);
	}
	if (direction !== "left") {
	    result.add(centerRow, centerCol + 1);
	}
	if (direction !== "up") {
	    result.add(centerRow + 1, centerCol);
	}
	if (direction !== "right") {
	    result.add(centerRow, centerCol - 1);
	}

	return result;
    },
});

Tetris.LPiece = Tetris.createCenteredPieceFactory({

    initial: { center: { row: 0, col: 3}, direction: "down" },

    directions: ["down", "left", "up", "right"],

    cells: function(centerRow, centerCol, direction) {
	if (direction === "down") {
	    return Tetris.CoordSet.createWithX(centerRow, Tetris.Arrays.range(centerCol - 1, centerCol + 1)).add(centerRow + 1, centerCol - 1);
	}
	if (direction === "left") {
	    return Tetris.CoordSet.createWithXY(Tetris.Arrays.range(centerRow - 1, centerRow + 1), [centerCol]).add(centerRow - 1, centerCol - 1);
	}
	if (direction === "up") {
	    return Tetris.CoordSet.createWithX(centerRow, Tetris.Arrays.range(centerCol - 1, centerCol + 1)).add(centerRow - 1, centerCol + 1);
	}
	if (direction === "right") {
	    return Tetris.CoordSet.createWithXY(Tetris.Arrays.range(centerRow - 1, centerRow + 1), [centerCol]).add(centerRow + 1, centerCol + 1);
	}
    },
});

Tetris.InvertedLPiece = Tetris.createCenteredPieceFactory({

    initial: { center: { row: 0, col: 3}, direction: "down" },

    directions: ["down", "left", "up", "right"],

    cells: function(centerRow, centerCol, direction) {
	if (direction === "down") {
	    return Tetris.CoordSet.createWithX(centerRow, Tetris.Arrays.range(centerCol - 1, centerCol + 1)).add(centerRow + 1, centerCol + 1);
	}
	if (direction === "left") {
	    return Tetris.CoordSet.createWithXY(Tetris.Arrays.range(centerRow - 1, centerRow + 1), [centerCol]).add(centerRow + 1, centerCol - 1);
	}
	if (direction === "up") {
	    return Tetris.CoordSet.createWithX(centerRow, Tetris.Arrays.range(centerCol - 1, centerCol + 1)).add(centerRow - 1, centerCol - 1);
	}
	if (direction === "right") {
	    return Tetris.CoordSet.createWithXY(Tetris.Arrays.range(centerRow - 1, centerRow + 1), [centerCol]).add(centerRow - 1, centerCol + 1);
	}
    },
});

Tetris.StepPiece = Tetris.createCenteredPieceFactory({

    initial: { center: { row: 1, col: 3}, direction: "horizontal" },

    directions: ["horizontal", "vertical"],

    cells: function(centerRow, centerCol, direction) {
	if (direction === "horizontal") {
	    return Tetris.CoordSet.createWith([[centerRow, centerCol - 1], [centerRow, centerCol], [centerRow - 1, centerCol], [centerRow - 1, centerCol + 1]]);
	}
	if (direction === "vertical") {
	    return Tetris.CoordSet.createWith([[centerRow - 1, centerCol], [centerRow, centerCol], [centerRow, centerCol + 1], [centerRow + 1, centerCol + 1]]);
	}
    },
});

Tetris.InvertedStepPiece = Tetris.createCenteredPieceFactory({

    initial: { center: { row: 1, col: 3}, direction: "horizontal" },

    directions: ["horizontal", "vertical"],

    cells: function(centerRow, centerCol, direction) {
	if (direction === "horizontal") {
	    return Tetris.CoordSet.createWith([[centerRow - 1, centerCol - 1], [centerRow - 1, centerCol], [centerRow, centerCol], [centerRow, centerCol + 1]]);
	}
	if (direction === "vertical") {
	    return Tetris.CoordSet.createWith([[centerRow - 1, centerCol + 1], [centerRow, centerCol + 1], [centerRow, centerCol], [centerRow + 1, centerCol]]);
	}
    },
});

// possible refactoring : use a function for each available direction, with an associated function. no more ifs all over the place.
