
class A

  def self.add_method(name, &content)
    define_method(name, content)
  end

  add_method(:wilma) { puts "Charge it !" }
end

toto = A.new

toto.wilma
