// ==========================================================================
// Tetris.Helper
// ==========================================================================

/** @class

  (Document your class here)

  @author AuthorName
  @version 0.1
*/
Tetris.Helper = {

    times: function(count, f) {
	for(var i = 0; i < count; i++) {
	    f();
	}
    },

};
