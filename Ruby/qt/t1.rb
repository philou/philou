require 'Qt'

app = Qt::Application.new(ARGV)

hello = Qt::PushButton::new('Hello world!')
hello.resize(100,30)
hello.show()

app.exec()
