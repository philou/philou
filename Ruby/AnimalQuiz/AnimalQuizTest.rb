require 'AnimalQuiz'
require 'test/unit'

class AnimalQuizTest < Test::Unit::TestCase

  def setup
    begin
      File.delete(Interprete::ENGLISH_RESOURCES[:database])
    rescue
    end
    short_setup
  end

  def short_setup
    @call_number = 0
    @quiz = AnimalQuiz.new(Interprete.english(self))
  end    

  # test suite toolkit
  private
  def self.game_test(test_name, expected_calls)
    define_method(("test_" + test_name.to_s).intern) do
      play_and_test(expected_calls)
    end
  end

  public
  # games tests
  game_test(:rabbit_undiscovered, 
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a rabbit"],
             [:ask, "Give me a question to distinguish a rabbit from an elephant ?", "Is it a small animal ?"],
             [:ask, "For a rabbit, what is the answer to that question ?", "y" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "n"]])

  game_test(:mouse_undiscovered, 
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a mouse"],
             [:ask, "Give me a question to distinguish a mouse from an elephant ?", "Is it a small animal ?"],
             [:ask, "For a mouse, what is the answer to that question ?", "y" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "n"]])
  
  game_test(:whale_undiscovered,
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a whale"],
             [:ask, "Give me a question to distinguish a whale from an elephant ?", "Does it live on the ground ?"],
             [:ask, "For a whale, what is the answer to that question ?", "n" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "n"]])

  game_test(:rabbit_discovered,
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "y"],
             [:tell, "I win. Pretty smart aren\'t I ?" ],
             [:ask, "Play again ? (y or n)", "n"]])

  game_test(:rabbit_and_elephant_discovered,
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a rabbit"],
             [:ask, "Give me a question to distinguish a rabbit from an elephant ?", "Is it a small animal ?"],
             [:ask, "For a rabbit, what is the answer to that question ?", "y" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "n"],
             [:ask, "Is it an elephant ? (y or n)", "y"],
             [:tell, "I win. Pretty smart aren\'t I ?"],
             [:ask, "Play again ? (y or n)", "n"]])

  game_test(:rabbit_and_whale_undiscovered,
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a rabbit"],
             [:ask, "Give me a question to distinguish a rabbit from an elephant ?", "Is it a small animal ?"],
             [:ask, "For a rabbit, what is the answer to that question ?", "y" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "n"],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a whale"],
             [:ask, "Give me a question to distinguish a whale from an elephant ?", "Does it live on the ground ?"],
             [:ask, "For a whale, what is the answer to that question ?", "n" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "n"]])

 game_test(:rabbit_and_mouse_undiscovered,
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a rabbit"],
             [:ask, "Give me a question to distinguish a rabbit from an elephant ?", "Is it a small animal ?"],
             [:ask, "For a rabbit, what is the answer to that question ?", "y" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "y"],
             [:ask, "Is it a rabbit ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a mouse"],
             [:ask, "Give me a question to distinguish a mouse from a rabbit ?", "Does it have big ears ?"],
             [:ask, "For a mouse, what is the answer to that question ?", "n" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "n"]])

 game_test(:rabbit_mouse_and_mouse_discovered,
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a rabbit"],
             [:ask, "Give me a question to distinguish a rabbit from an elephant ?", "Is it a small animal ?"],
             [:ask, "For a rabbit, what is the answer to that question ?", "y" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "y"],
             [:ask, "Is it a rabbit ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a mouse"],
             [:ask, "Give me a question to distinguish a mouse from a rabbit ?", "Does it have big ears ?"],
             [:ask, "For a mouse, what is the answer to that question ?", "n" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "y"],
             [:ask, "Does it have big ears ? (y or n)", "n"],
             [:ask, "Is it a mouse ? (y or n)", "y"],
             [:tell, "I win. Pretty smart aren\'t I ?"],
             [:ask, "Play again ? (y or n)", "n"]])

 game_test(:rabbit_mouse_dog_and_mouse_discovered,
            [[:tell, "Think of an animal ..."],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a rabbit"],
             [:ask, "Give me a question to distinguish a rabbit from an elephant ?", "Is it a small animal ?"],
             [:ask, "For a rabbit, what is the answer to that question ?", "y" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "y"],
             [:ask, "Is it a rabbit ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a mouse"],
             [:ask, "Give me a question to distinguish a mouse from a rabbit ?", "Does it have big ears ?"],
             [:ask, "For a mouse, what is the answer to that question ?", "n" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "n"],
             [:ask, "Is it an elephant ? (y or n)", "n"],
             [:tell, "You win. Please help me learn from my mistake before you go ..."],
             [:ask, "What animal were you thinking of ?", "a horse"],
             [:ask, "Give me a question to distinguish a horse from an elephant ?", "Does it have a trunk ?"],
             [:ask, "For a horse, what is the answer to that question ?", "n" ],
             [:tell, "Thanks."],
             [:ask, "Play again ? (y or n)", "y"],
             [:tell, "Think of an animal ..."],
             [:ask, "Is it a small animal ? (y or n)", "y"],
             [:ask, "Does it have big ears ? (y or n)", "n"],
             [:ask, "Is it a mouse ? (y or n)", "y"],
             [:tell, "I win. Pretty smart aren\'t I ?"],
             [:ask, "Play again ? (y or n)", "n"]])

  def test_animal_database
    play_and_test([[:tell, "Think of an animal ..."],
                   [:ask, "Is it an elephant ? (y or n)", "n"],
                   [:tell, "You win. Please help me learn from my mistake before you go ..."],
                   [:ask, "What animal were you thinking of ?", "a rabbit"],
                   [:ask, "Give me a question to distinguish a rabbit from an elephant ?", "Is it a small animal ?"],
                   [:ask, "For a rabbit, what is the answer to that question ?", "y" ],
                   [:tell, "Thanks."],
                   [:ask, "Play again ? (y or n)", "n"]])

    short_setup

    play_and_test([[:tell, "Think of an animal ..."],
                   [:ask, "Is it a small animal ? (y or n)", "n"],
                   [:ask, "Is it an elephant ? (y or n)", "y"],
                   [:tell, "I win. Pretty smart aren\'t I ?" ],
                   [:ask, "Play again ? (y or n)", "n"]])
  end


  # mock player

  def tell(sentence)
    assert( @call_number < @expected_calls.length)
    assert_equal(expected_call_content, sentence)
    assert_equal(:tell, expected_call_type)

    @call_number += 1
  end
  
  def ask(question)
    assert( @call_number < @expected_calls.length)
    assert_equal(expected_call_content, question)
    assert_equal(:ask, expected_call_type)
    result = expected_call_answer

    @call_number += 1
    result
  end

  private

  def play_and_test(expected_calls)
    @expected_calls = expected_calls

    @quiz.play
    assert_equal(@expected_calls.length,  @call_number)
  end

  def expected_call
    @expected_calls[@call_number]
  end

  def expected_call_type
    expected_call[0]
  end

  def expected_call_content
    expected_call[1]
  end

  def expected_call_answer
    expected_call[2]
  end

end
