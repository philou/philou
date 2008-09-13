require 'AnimalQuiz'

class HumanPlayer

  def ask(question)
    puts question
    gets.chomp
  end

  def tell(sentence)
    puts sentence
  end

end

quiz = AnimalQuiz.new(Interprete.french(HumanPlayer.new))
quiz.play
