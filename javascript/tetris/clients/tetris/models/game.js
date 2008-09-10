// ==========================================================================
// Tetris.Game
// ==========================================================================

require('core');

/** @class

  (Document your class here)

  @extends SC.Object
  @author AuthorName
  @version 0.1
*/
Tetris.Game = SC.Object.extend(
    /** @scope Tetris.Game.prototype */ {

	playing: false,
	currentPiece: null,
	filledCells: null,
	lineCount: 0,

	// score()
	// level()

	// random mock

	start: function() {
	    this.set('playing', true);
	    this.filledCells = Tetris.CoordSet.create();
	    this.set('lineCount', 0);
	    this.resetCurrentPiece();
	    this.updateBoard();
	    this.get('ticker').start(this);
	},
	stop: function() {
	    this.set('playing', false);
	    this.get('ticker').stop(this);
	    this.currentPiece = null;
	},
	left: function() {
	    this.move(
		function(piece) { return piece.left(); },
		function() {});
	},
	right: function() {
	    this.move(
		function(piece) { return piece.right(); },
		function() {});
	},
	down: function() {
	    this.move(
		function(piece) { return piece.down(); },
		function() {});
	},
	rotate: function() {
	    this.move(
		function(piece) { return piece.rotate(); },
		function() {});
	},
	tick: function() {
	    var that = this;
	    this.move(
		function(piece) { return piece.down(); }, 
		function() {
		    that.blockCurrentPiece();
		    that.set('lineCount', that.get('lineCount') + that.filledRowsCount(that.filledCells));
		    that.filledCells = that.cellsWithoutFilledRows(that.filledCells);
		    that.resetCurrentPiece();
		});
	},
	cellStatus: function(row, col) {
	    if ((null != this.filledCells) && this.filledCells.contains(row, col))
		return true;

	    return (null !== this.currentPiece) && this.currentPiece.contains(row, col);
	},

	// private methods
	isValidPiece: function(piece) {
	    var that = this;
	    var result = true;
	    piece.each(function(row, col) {
		result &= that.isValidCell(row, col);
	    });
	    return result;
	},
	isValidCell: function(row, col) {
	    return (0 <= row) && (row <= Tetris.Game.MaxRow) && (0 <= col) && (col <= Tetris.Game.MaxCol) && !this.filledCells.contains(row, col);
	},
	blockCurrentPiece: function() {
	    if (null === this.currentPiece)
		return;

	    var that = this;
	    this.currentPiece.each(function(row, col) {
		that.filledCells.add(row, col);
	    });
	},
	isRowFilled: function(cells, row) {
	    for(var col = 0; col < Tetris.Game.ColCount; col++) {
		if (!cells.contains(row, col))
		    return false;
	    }
	    return true;
	},
	cellsWithoutRow: function(cells, removedRow) {
	    var result = Tetris.CoordSet.create();
	    cells.each(function(row, col) {
		if (row < removedRow) {
		    result.add(row + 1, col);
		} else if (removedRow < row) {
		    result.add(row, col);
		}
	    });
	    return result;
	},
	filledRowsCount: function(cells) {
	    var result = 0;
	    for(var row = 0; row <= Tetris.Game.MaxRow; row++) {
		if (this.isRowFilled(cells, row)) {
		    result++;
		}
	    }
	    return result;
	},
	cellsWithoutFilledRows: function(cells) {
	    var result = cells;
	    var row = Tetris.Game.MaxRow;
	    while (0 <= row) {
		if (this.isRowFilled(result, row)) {
		    result = this.cellsWithoutRow(result, row);
		} else {
		    row--;
		}
	    }
	    return result;
	},
	resetCurrentPiece: function() {
	    var newPiece = this.pieceFactory.create();
	    if (!this.isValidPiece(newPiece)) {
		this.stop();
	    } else {
		this.currentPiece = newPiece;
	    }
	},
	updateBoard: function() {
	    // ne serait-il pas possible de transformer boardTime en propriété qui observe currentPiece et filledSet ???
	    this.set('boardTime', this.now());
	},
	now: function() {
	    return new Date();
	},
	move: function(transformation, doIfBlocked) {
	    if (null === this.currentPiece)
		return;

	    var newPiece = transformation(this.currentPiece);
	    if (this.isValidPiece(newPiece)) {
		this.currentPiece = newPiece;
	    } else {
		doIfBlocked();
	    }
	    this.updateBoard();
	},

	// testing or cheating only methods
	setFilledCells: function(filledCells) {
	    this.filledCells = filledCells;
	    this.updateBoard();
	},
	setPieceFactory: function(pieceFactory) {
	    this.pieceFactory = pieceFactory;
	},

	// essayer de partager la taille du board entre ruby et javascript ???
	// tracer la grille en dur dans my_tetris_board, et utiliser une fonction par case ??? 
    }) ;

Tetris.Game.RowCount = 16;
Tetris.Game.ColCount = 8;

Tetris.Game.MaxRow = Tetris.Game.RowCount - 1;
Tetris.Game.MaxCol = Tetris.Game.ColCount - 1;

