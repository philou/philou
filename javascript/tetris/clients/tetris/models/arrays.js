// ==========================================================================
// Tetris.Arrays
// ==========================================================================


/** @class

  (Document your class here)

  @author AuthorName
  @version 0.1
*/
Tetris.Arrays = {

    constant: function(length, value) {
	var result = [];
	for(var i = 0; i < length; i++) {
	    result[i] = value;
	}
	return result;
    },
    range: function(min, max) {
	var result = [];
	for(var i = min; i <= max; i++) {
	    result[result.length] = i;
	}
	return result;
    },
    zip: function(array1, array2) {
	var result = [];
	for(var i = 0; (i < array1.length) && (i < array2.length); i++) {
	    result[i] = [array1[i], array2[i]];
	}
	return result;
    },

};
