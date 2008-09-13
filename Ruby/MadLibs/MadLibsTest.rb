require 'MadLibs'
require 'test/unit'

class MadLibsTest < Test::Unit::TestCase

  def test_template_without_placeholder
    template = "Simplest mad libs game !"
    game = MadLibs.new(template)
    game.play
    assert_equal(template, game.result)
  end

  def test_template_with_one_placeholder
    game = MadLibs.new("Thanks for your ((a present)).") { "flowers" }
    game.play
    assert_equal("Thanks for your flowers.", game.result)
  end

  def test_placeholder_alone
    game = MadLibs.new("((a precious stone))") { "ruby" }
    game.play
    assert_equal("ruby", game.result)
  end

  def test_placeholder_should_be_asked_politely
    asked_question = nil
    game = MadLibs.new("((a precious stone))") do | question |
      asked_question = question
    end
    game.play
    assert_equal("Please enter a precious stone.", asked_question)
  end

  def test_two_placeholders
    placeholders = ["apple", "Anne Claire"]
    current_placeholder = 0
    game = MadLibs.new("Give that ((thing)) to ((a person)).") do
      result = placeholders[current_placeholder]
      current_placeholder = current_placeholder + 1
      result
    end
    game.play
    assert_equal("Give that apple to Anne Claire.", game.result)
  end

  def test_repeated_placeholder
    placeholder_already_asked = false
    game = MadLibs.new("((thing:a thing)) is the best because ((thing)) is stincky.") do
      assert(! placeholder_already_asked)
      placeholder_already_asked = true
      "Tutu"
    end
    game.play
    assert_equal("Tutu is the best because Tutu is stincky.", game.result)
  end

end
