require 'LCDNumbers'
require 'optparse'

lcd_formater = LCDNumbers.new

options = OptionParser.new
options.on("-s", "--size VAL", Integer) { |val| lcd_formater.size = val }

input = options.parse(ARGV)

begin
  puts lcd_formater.format(input.join)
rescue InvalidCharException => detail
  puts detail.message
end
