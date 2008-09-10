// ==========================================================================
// Tetris.MyTableView
// ==========================================================================

require('core');

/** @class

  (Document Your View Here)

  @extends SC.View
  @author AuthorName
  @version 0.1
*/
Tetris.BoardView = SC.View.extend(function() {
    /** @scope Tetris.MyTableView.prototype */

    var renderHtml = function(elemValues) {
	var html = [];

	for (row = 0; row < Tetris.Game.RowCount; row++) {

	    html.push('<tr>');
	    
	    for (col = 0; col < Tetris.Game.ColCount; col++) {

		html.push('<td class="');
		if (elemValues(row, col)) {
		    html.push('filled');
		} else {
		    html.push('blank');
		}
		html.push('"/>');
	    }
	    html.push('</tr>');
	}

	return html.join('');
    };

    return {

	emptyElement: '<table>' + renderHtml(function(_row, _col) { return false; }) + '</table>',

	boardTime: null,
	game: null,
	acceptsFirstResponder: true,

	render: function() {

	    var that = this;
	    this.set('innerHTML', renderHtml( function(row, col) { return that.get('game').cellStatus(row, col); } ));

	}.observes('boardTime', 'game'),

	keyDown: function(evt) {
	    return this.interpretKeyEvents(evt) ;
	},
	moveRight: function(sender, evt) {
	    this.get('game').right();
	    return true;
	},
	moveLeft: function(sender, evt) {
	    this.get('game').left();
	    return true;
	},
	moveDown: function(sender, evt) {
	    this.get('game').down();
	    return true;
	},
	moveUp: function(sender, evt) {
	    this.get('game').rotate();
	    return true;
	},

    };
}()) ;
