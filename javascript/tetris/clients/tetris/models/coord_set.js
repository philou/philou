// ==========================================================================
// Tetris.CoordSet
// ==========================================================================

/** @class

  (Document your class here)

  @author AuthorName
  @version 0.1
*/
Tetris.CoordSet = {
    create: function() {
	var content = {};
	var m_count = 0;
	var makeKey = function(x, y) {
	    return x + "x" + y;
	};
	return {
	    isEmpty: function() {
		return m_count === 0;
	    },
	    count: function() {
		return m_count;
	    },
	    contains: function(x, y) {
		return content.hasOwnProperty(makeKey(x,y));
	    },
	    each: function(doWith) {
		for(var key in content) {
		    var coords = content[key];
		    doWith(coords.x, coords.y);
		}
	    },
	    add: function(x, y) {
		content[makeKey(x,y)] = {x: x, y: y};
		m_count++;
		return this;
	    },
	    addMany: function(coords) {
		var that = this;
		coords.each(function(coord) {
		    that.add(coord[0], coord[1]);
		});
		return this;
	    },
	    addManyX: function(x, ys) {
		var that = this;
		ys.each(function(y) {
		    that.add(x, y);
		});
		return this;
	    },
	    addManyXY: function(xs, ys) {
		var that = this;
		xs.each(function(x) {
		    that.addManyX(x, ys);
		});
		return this;
	    },
	};
    },
    createWith: function(coords) {
	return this.create().addMany(coords);
    },
    createWithX: function(x, ys) {
	return this.create().addManyX(x, ys);
    },
    createWithXY: function(xs, ys) {
	return this.create().addManyXY(xs, ys);
    },
};
