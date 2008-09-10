// ==========================================================================
// Tetris.Ticker
// ==========================================================================

require('core');

/** @class

  (Document your class here)

  @extends SC.Record
  @author AuthorName
  @version 0.1
*/
Tetris.Ticker = SC.Object.extend(
/** @scope Tetris.Ticker.prototype */ {

    start: function(game) {
	this._game = game;
	this._timer = SC.Timer.schedule({
	    target: this._game, action: 'tick', repeats: YES, interval: 1000
	});
    },

    stop: function(game) {
	if (this._game === game) {
	    this._timer.invalidate();
	}
    },

}) ;
