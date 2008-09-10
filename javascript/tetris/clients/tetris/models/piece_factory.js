// ==========================================================================
// Tetris.PieceFactory
// ==========================================================================

require('core');

/** @class

  (Document your class here)

  @extends SC.Record
  @author AuthorName
  @version 0.1
*/
Tetris.PieceFactory = SC.Object.extend(
/** @scope Tetris.PieceFactory.prototype */ {

    factories: [Tetris.SquarePiece, Tetris.IPiece, Tetris.TPiece, Tetris.LPiece, Tetris.InvertedLPiece, Tetris.StepPiece, Tetris.InvertedStepPiece],

    create: function() {
	
	var rand = Math.random();
	var index = Math.round(rand * this.factories.length) % this.factories.length;

	return this.factories[index].create();
    },

}) ;
