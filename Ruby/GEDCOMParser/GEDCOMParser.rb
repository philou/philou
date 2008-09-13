
class GEDCOMParser

  def self.to_xml(gedcom)
    GEDCOMParser.new(gedcom).to_xml
  end

  def initialize(gedcom)
    @gedcom = gedcom
  end

  def to_xml
    @result = "<gedcom>\n"

    @opened_tags = []

    match_each_line(@gedcom, 
      {
        /(\d+)\s+(@[^@]+@)\s+([A-Z0-9][A-Z0-9][A-Z0-9][A-Z0-9])/ => Proc.new do |match|
          open_tag(match[1].to_i, match[3], { "id" => match[2] })
        end,
        /(\d+)\s+([A-Z0-9][A-Z0-9]?[A-Z0-9]?[A-Z0-9]?)\s+(.*)/ => Proc.new do |match|
          open_tag(match[1].to_i, match[2], {})
          @result << tabs << match[3] << "\n"
        end,
        /.*/ => Proc.new { raise "Invalid GEDCOM" }
      })
        
    close_opened_tags
    @result << "</gedcom>\n"
  end

  def match_each_line(input, actions)
    input.each do |line|
      actions.each do |regexp, action|
        match = regexp.match(line)
        if (nil != match)
          action.call(match)
          break
        end
      end
    end
  end

  def close_opened_tags
    close_to_level(0)
  end

  def close_to_level(desired_level)
    while (desired_level <= level)
      close_tag
    end
  end

  def level
    @opened_tags.length - 1
  end

  def tabs
    "  " * (@opened_tags.length + 1)
  end

  def open_tag(level, tag, attributes)
    close_to_level(level)
    new_tag = tag.downcase
    @result << tabs << "<" << new_tag
    attributes.each do |key, value|
      @result << " " << key << "=\"" << value << "\""
    end
    @result << ">\n"
    push_opened_tags(new_tag)
  end

  def close_tag
    tag = pop_opened_tags
    @result << tabs << "</" << tag << ">\n"
  end

  def peak_opened_tags
    @opened_tags.last
  end

  def push_opened_tags(tag)
    @opened_tags.push(tag)
  end

  def pop_opened_tags
    @opened_tags.pop
  end

end
