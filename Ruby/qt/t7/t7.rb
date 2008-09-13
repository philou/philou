require 'Qt'
require 'LCDRange'

class MyWidget < Qt::Widget
  def initialize(parent = nil)
    super()
    quit = Qt::PushButton.new(tr('Quit'))
    quit.setFont(Qt::Font.new('Times', 18, Qt::Font::Bold))
    connect(quit, SIGNAL('clicked()'), $qApp, SLOT('quit()'))

    grid = Qt::GridLayout.new()

    previousRange = nil
    for row in 0..2
      for column in 0..2
        lcdRange = LCDRange.new()
        grid.addWidget(lcdRange, row, column)
        unless previousRange.nil?
          connect(lcdRange, SIGNAL('valueChanged(int)'), previousRange, SLOT('setValue(int)'))
        end
        previousRange = lcdRange
      end
    end

    layout = Qt::VBoxLayout.new()
    layout.addWidget(quit)
    layout.addLayout(grid)
    setLayout(layout)
  end
end

app = Qt::Application.new(ARGV)
widget = MyWidget.new()
widget.show()
app.exec()
