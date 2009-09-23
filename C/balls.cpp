#include <assert.h>

class Ball
{
public:
  enum BallColor { RED, BLUE };

  Ball(BallColor color) : _color(color) {}

  BallColor Color() const { return _color; }

private:
  BallColor  _color;
  // Other data in class (unrelated to assignment)
};




// exchanges the balls at position i and j in aBalls
void Swap(Ball aBalls[], unsigned int i, unsigned int j)
{
  const Ball temp = aBalls[i];
  aBalls[i] = aBalls[j];
  aBalls[j] = temp;
}

// Starting from 'i', founds the next index at which the ball as a color different than 'color'
unsigned int NextIndexWithDifferentColor( Ball aBalls[], unsigned int cBalls, unsigned int i, Ball::BallColor color)
{
  assert(aBalls && 0 <= i && i < cBalls);

  while((color == aBalls[i].Color()) && (i < cBalls))
    ++i;

  return i;
}

// Starting from 'i', founds the next index at which the ball as a color different than 'color'
unsigned int PreviousIndexWithColor( Ball aBalls[], unsigned int cBalls, unsigned int i, Ball::BallColor color)
{
  assert(aBalls && 0 <= i && i < cBalls);

  while((color != aBalls[i].Color()) && (0 <= i))
    --i;

  return i;
}

/*
The idea in this algorithm is to start from both ends of the array.
The color of the first ball is selected as the color to pack at the begining.
Iterating from both ends, the first ball that is not of the begining color is swaped
with the last ball that is of the begining color.
When both cursors meet, we know the array was partitioned.

The algorithm runs in linear time O(n) and constant place O(1).

When applied as is to an array with more than 3 colors, the algorithm will simply
pack the first color at the begining, and leave the rest unsorted ... Recusively
applying the algorithm to the rest of the array before returning would allow to
solve the problem.
This modification would run in O(cn) time where c is the number of colors and in
O(c) in space, due to recursive calls.

Another way would be to define an order on Balls according to their colors, and
applying a classical sort algorithm. This could run in O(nlog(n)) in both space
and time, and might be or not a better choice depending on the context of execution,
and the number of colors in particular.

*/

unsigned Partition( Ball aBalls[], unsigned cBalls )
{
  assert(aBalls);

  if (0 == cBalls)
    return 0;

  const Ball::BallColor leftColor = aBalls[0].Color();
  unsigned int left = 0;
  unsigned int right = cBalls -1;
  while (true)
    {
      left = NextIndexWithDifferentColor(aBalls, cBalls, left, leftColor);
      if (cBalls <= left)
	return 0;

      right = PreviousIndexWithColor(aBalls, cBalls, right, leftColor);
      if (right <= left)
      {
        // Line to add to handle multiple colors:
        // Partition(aBalls+left, cBalls-left);
	return left;
      }

      Swap(aBalls, left, right);

      assert(leftColor == aBalls[left].Color());
      assert(leftColor != aBalls[right].Color());
    }
}

int main(int argc, char** args)
{
  Ball emptyArray[] = {};
  assert(0 == Partition(emptyArray, 0));

  Ball oneElementArray[] = { Ball(Ball::RED) };
  assert(0 == Partition(oneElementArray, sizeof(oneElementArray)/sizeof(*oneElementArray)));

  Ball oneColorArray[] = { Ball(Ball::RED), Ball(Ball::RED), Ball(Ball::RED) };
  assert(0 == Partition(oneColorArray, sizeof(oneColorArray)/sizeof(*oneColorArray)));

  Ball twoElementsArray[] = { Ball(Ball::RED), Ball(Ball::BLUE) };
  assert(1 == Partition(twoElementsArray, sizeof(twoElementsArray)/sizeof(*twoElementsArray)));

  Ball alternatingColorsArray[] = { Ball(Ball::RED), Ball(Ball::BLUE), Ball(Ball::RED) };
  assert(2 == Partition(alternatingColorsArray, sizeof(alternatingColorsArray)/sizeof(*alternatingColorsArray)));

  Ball largeAlternatingColorsArray[] = { Ball(Ball::RED), Ball(Ball::BLUE), Ball(Ball::RED), Ball(Ball::BLUE), Ball(Ball::RED), Ball(Ball::BLUE), Ball(Ball::RED), Ball(Ball::BLUE) };
  assert(4 == Partition(largeAlternatingColorsArray, sizeof(largeAlternatingColorsArray)/sizeof(*largeAlternatingColorsArray)));

  return 0;
}

