
class LCDNumbers

  def initialize()
    @size = 2
  end

  attr_accessor :size

  def format(input)
    result = "";
    0.upto(height) do |line_number|
      result << full_line(input, line_number)
    end
    result
  end

  def height
    LCDNumbers.height(size)
  end
  def self.height(size)
    3 + 2*size - 1
  end

  def width
    LCDNumbers.width(size)
  end
  def self.width(size)
    2 + size - 1
  end

  def mid_line
    LCDNumbers.mid_line(size)
  end
  def self.mid_line(size)
    (height(size))/2
  end

  def full_line(input, line_number)
    result = ""
    first_digit = true
    input.each_byte do |digit|
      if (first_digit)
        first_digit = false
      else
        result << " "
      end
      result << digit_line(digit, line_number)
    end
    result << "\n"
  end


  @@template_size = 1
  @@template_height = height(@@template_size)
  @@template_width = width(@@template_size)
  @@template_mid_line = mid_line(@@template_size)
  @@template_digits =
    { ?0 => [" - ",
             "| |",
             "   ",
             "| |",
             " - "],
      ?1 => ["   ",
             "  |",
             "   ",
             "  |",
             "   "],
      ?2 => [" - ",
             "  |",
             " - ",
             "|  ",
             " - "],
      ?3 => [" - ",
             "  |",
             " - ",
             "  |",
             " - "],
      ?4 => ["   ",
             "| |",
             " - ",
             "  |",
             "   "],
      ?5 => [" - ",
             "|  ",
             " - ",
             "  |",
             "-- "],
      ?6 => [" - ",
             "|  ",
             " - ",
             "| |",
             " - "],
      ?7 => [" - ",
             "  |",
             "   ",
             "  |",
             "   "],
      ?8 => [" - ",
             "| |",
             " - ",
             "| |",
             " - "],
      ?9 => [" - ",
             "| |",
             " - ",
             "  |",
             " - "]}

  def template_line(line_number)
    case line_number
      when 0: 0
      when mid_line: @@template_mid_line
      when height : @@template_height
      when 0..mid_line: 1
      else 3
    end
  end

  def digit_line(digit, line_number)
    unless (@@template_digits.has_key?(digit))
      raise InvalidCharException.new("Character '#{digit}' is not handled by the LCD formater")
    end

    result = @@template_digits[digit][template_line(line_number)].clone
    result[1..1] *= size
    result
  end

end

class InvalidCharException < RuntimeError
  def initialize(message)
    super(message)
  end
end
