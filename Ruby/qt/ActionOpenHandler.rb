require 'Qt'

class ActionOpenHandler < Qt::Object
  slots "on_activated()"

  def initialize(main = nil)
    super
    @main = main
  end

  def on_activated
    path = nil
    Qt::FileDialog.new do |fd|
      path = fd.get_open_file_name()
    end
    return unless path

    text = nil;
    File.open(path, 'r') {|f| text = f.read }
    @main.findChild(Qt::TextEdit, "textBrowser").text = text
  end
end

a = Qt::Application.new(ARGV)
QtSuperLoader.new.initialize_and_hook("./main.ui").show
a.exec
