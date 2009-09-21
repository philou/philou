#include <assert.h>

// For our specific problem, we could optimize it as following: 
// we know that the array was sorted, so once we found a minimum,
// we know we have found the result.
// On the other hand, this function is more general and might be usable
// in other contexts.
int IndexOfFirstMinimum(int array[], unsigned length)
{
  if (0 == length)
    return 0;

  int minimum = array[0];
  int result = 0;
  for (int i = 1; i < length; ++i)
    {
      if (array[i] < minimum)
	{
	  minimum = array[i];
	  result = i;
	}
    }
  return result;
}

int FindSortedArrayRotation( int array[], unsigned length )
{
  return IndexOfFirstMinimum(array, length);
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

  return 0;
}
