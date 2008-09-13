require 'Qt'

app = Qt::Application.new(ARGV)

window = Qt::Widget.new()
window.resize(200,100)

quit = Qt::PushButton.new('Quit', window)
quit.font = Qt::Font.new('Times', 18, Qt::Font::Bold)
quit.setGeometry(10,40,180,40)
Qt::Object.connect(quit, SIGNAL('clicked()'), app, SLOT('quit()'))

window.show()
app.exec()
