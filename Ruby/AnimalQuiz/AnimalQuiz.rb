
class Interprete

  ENGLISH_RESOURCES = 
  { :database => "english.animals.db",
    :think_of_an_animal => "Think of an animal ...",
    :an_elephant => "an elephant",
    :play_again => "Play again ? (y or n)",
    :y_or_n => " (y or n)",
    :i_win => "I win. Pretty smart aren\'t I ?",
    :you_win => "You win. Please help me learn from my mistake before you go ...",
    :thanks => "Thanks.",
    :what_animal_were_you_thinking_of => "What animal were you thinking of ?",
    :is_it => "Is it %s ? (y or n)",
    :give_me_a_question_to_distinguish => "Give me a question to distinguish %s from %s ?",
    :what_is_the_answer_to_that_question => "For %s, what is the answer to that question ?"}

  FRENCH_RESOURCES =
  { :database => "french.animals.db",
    :think_of_an_animal => "Pensez à un animal ...",
    :an_elephant => "un elephant",
    :play_again => "Voulez vous encore jouer ? (o ou n)",
    :y_or_n => " (o ou n)",
    :i_win => "J\'ai gagné. Malin non ?",
    :you_win => "Vous avez gagné. Aidez moi à apprendre de mes erreurs avant de partir ...",
    :thanks => "Merci.",
    :what_animal_were_you_thinking_of => "A quel animal pensiez vous ?",
    :is_it => "Est ce que c'est %s ? (o ou n)",
    :give_me_a_question_to_distinguish => "Donnez moi une question pour distinguer %s et %s ?",
    :what_is_the_answer_to_that_question => "Quelle est la réponse à cette question pour %s ?"}

  attr_reader :player
  attr_reader :resources

  def initialize(player, resources)
    @player = player
    @resources = resources
  end

  def self.french(player)
    new(player, FRENCH_RESOURCES)
  end

  def self.english(player)
    new(player, ENGLISH_RESOURCES)
  end

  def tell(resource)
    @player.tell(@resources[resource])
  end

  def ask(resource, *format_args)
    @player.ask(sprintf(@resources[resource], *format_args))
  end

  def y_or_n?(resource, *format_args)
    "n" != ask(resource, format_args)
  end

end

class Deduction

  def initialize(question, answer, if_answer, otherwise)
    @question = question
    @answer = answer
    @if_answer = if_answer
    @otherwise = otherwise
  end

  def guess(interprete)
    deduce(interprete)
  end

  private

  def deduce(interprete)
    player_answer = interprete.player.ask(@question + interprete.resources[:y_or_n])
    if (@answer == player_answer)
      @if_answer = @if_answer.guess(interprete)
    else
      @otherwise = @otherwise.guess(interprete)
    end
    self
  end

end

class Proposition

  def initialize(animal)
    @animal = animal
  end
  
  def guess(interprete)
    submit_animal(interprete)
  end

  private

  def submit_animal(interprete)
    if (ask_if_animal(interprete))
      tell_victory(interprete)
    else
      ask_defeat(interprete)
    end
  end

  def ask_if_animal(interprete)
    interprete.y_or_n?(:is_it, @animal)
  end

  def tell_victory(interprete)
    interprete.tell(:i_win)
    self
  end

  def ask_defeat(interprete)
    interprete.tell(:you_win)
    good_animal = interprete.ask(:what_animal_were_you_thinking_of)
    question = interprete.ask(:give_me_a_question_to_distinguish, good_animal, @animal)
    answer = interprete.ask(:what_is_the_answer_to_that_question, good_animal)

    result = Deduction.new(question, answer, Proposition.new(good_animal), self)

    interprete.tell(:thanks)

    result
  end

end

class AnimalQuiz

  def initialize(interprete)
    @interprete = interprete
    @deduction = Proposition.new(interprete.resources[:an_elephant])
    load_database
  end

  def play
    @interprete.tell(:think_of_an_animal)

    @deduction = @deduction.guess(@interprete)

    if (@interprete.y_or_n?(:play_again))
      play
    else
      save_database
    end
  end

  def load_database
    begin
      File.open(@interprete.resources[:database]) do |file|
        @deduction = Marshal.load(file)
      end
    rescue
    end
  end    

  def save_database
    File.open(@interprete.resources[:database], "w+") do |file|
      Marshal.dump(@deduction, file)
    end
  end

end
