class MadLibs
  def initialize(template, &ask_placeholder)
    @template = template
    @ask_placeholder = ask_placeholder
  end

  def play
    @placeholders = Hash.new()
    @result = expand_template(@template)
  end

  def result
    @result
  end
  
  private

  def expand_template(template)
    if template =~ /\(\([^\)]*\)\)/
      $` + expand_placeholder($&) + expand_template($')
    else
      template
    end
  end

  def expand_placeholder(placeholder)
    placeholder = placeholder.slice(2..-3)
    key, name = placeholder.split(':')
    if @placeholders.has_key?(key)
      @placeholders[key]
    else
      if (nil != name)
        @placeholders[key] = ask_placeholder(name)
      else
        ask_placeholder(placeholder)
      end
    end
  end

  def ask_placeholder(placeholder_name)
    @ask_placeholder.call("Please enter " + placeholder_name + ".").strip
  end

end
