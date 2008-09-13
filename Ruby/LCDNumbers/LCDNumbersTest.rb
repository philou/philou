require 'LCDNumbers'
require 'test/unit'

class LCDNumbersTest < Test::Unit::TestCase

  def setup
    @writer = LCDNumbers.new
  end

  def test_default_size
    assert_equal(2, @writer.size)
  end

  def test_0
    assert_equal(
      " -- \n"+
      "|  |\n"+
      "|  |\n"+
      "    \n"+
      "|  |\n"+
      "|  |\n"+
      " -- \n",
      @writer.format("0"))
  end

  def test_00
    assert_equal(
      " --   -- \n"+
      "|  | |  |\n"+
      "|  | |  |\n"+
      "         \n"+
      "|  | |  |\n"+
      "|  | |  |\n"+
      " --   -- \n",
      @writer.format("00"))
  end

  def test_1
    assert_equal(
      "    \n"+
      "   |\n"+
      "   |\n"+
      "    \n"+
      "   |\n"+
      "   |\n"+
      "    \n",
      @writer.format("1"))
  end

  def test_small_0
    @writer.size = 1
    assert_equal(
      " - \n"+
      "| |\n"+
      "   \n"+
      "| |\n"+
      " - \n",
      @writer.format("0"))
  end

  def test_digit_line_0_0
    assert_equal(" -- ", @writer.digit_line(?0, 0))
  end

  def test_unknown_chararcter
    assert_raise(InvalidCharException) do
      @writer.format("a")
    end
  end

end
