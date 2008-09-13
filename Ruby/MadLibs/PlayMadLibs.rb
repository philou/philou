require 'MadLibs'

print("Please enter a template sentence :\n")
template = gets
game = MadLibs.new(template) do | question|
  print question + "\n"
  gets
end
game.play
print "The result sentence is ...\n"
print game.result
    

