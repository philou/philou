require 'test/unit'
require 'GEDCOMParser'

class GEDCOMParserTest < Test::Unit::TestCase

  def test_empty_gedcom
    assert_equal("<gedcom>\n"+
                 "</gedcom>\n",
                 GEDCOMParser::to_xml(""))
  end

  def test_id_gedcom
    assert_equal("<gedcom>\n"+
                 "  <indi id=\"@I1@\">\n"+
                 "  </indi>\n"+
                 "</gedcom>\n",
                 GEDCOMParser::to_xml("0 @I1@ INDI\n"))
  end

  def test_tag_gedcom
    assert_equal("<gedcom>\n"+
                 "  <name>\n"+
                 "    Jamis Gordon /Buck/\n"+
                 "  </name>\n"+
                 "</gedcom>\n",
                 GEDCOMParser::to_xml("0 NAME Jamis Gordon /Buck/\n"))
  end

  def test_many_level0_gedcom
    assert_equal("<gedcom>\n"+
                 "  <indi id=\"@I1@\">\n"+
                 "  </indi>\n"+
                 "  <name>\n"+
                 "    Jamis Gordon /Buck/\n"+
                 "  </name>\n"+
                 "</gedcom>\n",
                 GEDCOMParser::to_xml("0 @I1@ INDI\n"+
                                      "0 NAME Jamis Gordon /Buck/\n"))
  end

  def test_level1_gedcom
    assert_equal("<gedcom>\n"+
                 "  <indi id=\"@I1@\">\n"+
                 "    <name>\n"+
                 "      Jamis Gordon /Buck/\n"+
                 "    </name>\n"+
                 "  </indi>\n"+
                 "</gedcom>\n",
                 GEDCOMParser::to_xml("0 @I1@ INDI\n"+
                                      "1 NAME Jamis Gordon /Buck/\n"))
  end

  def test_complete_gedcom
    assert_equal("<gedcom>\n"+
                 "  <indi id=\"@I1@\">\n"+
                 "    <name>\n"+
                 "      Jamis Gordon /Buck/\n"+
                 "      <surn>\n"+
                 "        Buck\n"+
                 "      </surn>\n"+
                 "      <givn>\n"+
                 "        Jamis Gordon\n"+
                 "      </givn>\n"+
                 "    </name>\n"+
                 "    <sex>\n"+
                 "      M\n"+
                 "    </sex>\n"+
                 "  </indi>\n"+
                 "</gedcom>\n",
                 GEDCOMParser::to_xml("0 @I1@ INDI\n"+
                                      "1 NAME Jamis Gordon /Buck/\n"+
                                      "2 SURN Buck\n"+
                                      "2 GIVN Jamis Gordon\n"+
                                      "1 SEX M\n"))
  end

  def test_invalid_gedcom
    assert_raise(String, GEDCOMParser::to_xml("invalid gedcom ...\n"))
  end

  # lancer des exceptions si le fichier est invalide.

end
