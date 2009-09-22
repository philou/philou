#include <assert.h>

/*

Before the rotation, the values in the array were always
increasing. This should not change in the rotated array,
appart when going from what was the last index to what
was the first index.
So to find the amount of rotation, we just have to found 
the first index which has a value smaller than the
previous index.

This solution runs in linear time, O(n) and constant
space O(1).

*/
int FindFirstDecreasingIndex(int array[], unsigned length)
{
  if (0 == length)
    return 0;

  for (unsigned int i = 1; i < length; ++i)
    {
      if (array[i-1] > array[i])
	return i;
    }
  return 0;
}

int FindSortedArrayRotation( int array[], unsigned length )
{
  return FindFirstDecreasingIndex(int array[], unsigned length);
}

int main(int argc, char** args)
{
  int emptyArray[] = {};
  assert(0 == FindSortedArrayRotation(emptyArray, 0));

  int sortedArray[] = {0, 1, 2, 3, 4};
  assert(0 == FindSortedArrayRotation(sortedArray, sizeof(sortedArray)/sizeof(*sortedArray)));

  int rotatedByOneArray[] = {4, 0, 1, 2, 3};
  assert(1 == FindSortedArrayRotation(rotatedByOneArray, sizeof(rotatedByOneArray)/sizeof(*rotatedByOneArray)));

  int rotatedByFourArray[] = {1, 2, 3, 4, 0};
  assert(4 == FindSortedArrayRotation(rotatedByFourArray, sizeof(rotatedByFourArray)/sizeof(*rotatedByFourArray)));

  int constantArray[] = {1, 1, 1, 1};
  assert(0 == FindSortedArrayRotation(constantArray, sizeof(constantArray)/sizeof(*constantArray)));

  int twoMinimumsArray[] = {4, 0, 0, 2, 3};
  assert(1 == FindSortedArrayRotation(twoMinimumsArray, sizeof(twoMinimumsArray)/sizeof(*twoMinimumsArray)));

  int twoMinimumsAtExtremitiesArray[] = {0, 2, 3, 4, 0};
  assert(4 == FindSortedArrayRotation(twoMinimumsAtExtremitiesArray, sizeof(twoMinimumsAtExtremitiesArray)/sizeof(*twoMinimumsAtExtremitiesArray)));

  return 0;
}
