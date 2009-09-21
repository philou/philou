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
	return left;

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

/*

With three colors, I could modify my algorithm to first pack one of the colors at the begining of the array.
And then, recurse and perform a 2 colors partition on the remaining elements.

With 9 colors, I would use the stl or another library to quicksort the Balls in the array according to their colors.

int operator<(const Ball& a, const Ball& b)
{
  return a.Color() < b.Color();
}

*/
