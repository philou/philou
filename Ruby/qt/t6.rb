require 'Qt'

class LCDRange < Qt::Widget
  def initialize(parent = nil)
    super()
    lcd = Qt::LCDNumber.new(2)

    slider = Qt::Slider.new(Qt::Horizontal)
    slider.setRange(0,99)
    slider.setValue(0)

    connect(slider, SIGNAL('valueChanged(int)'), lcd, SLOT('display(int)'))

    layout = Qt::VBoxLayout.new()
    layout.addWidget(lcd)
    layout.addWidget(slider)
    setLayout(layout)
  end
end

class MyWidget < Qt::Widget
  def initialize(parent = nil)
    super()
    quit = Qt::PushButton.new(tr('Quit'))
    quit.setFont(Qt::Font.new('Times', 18, Qt::Font::Bold))
    connect(quit, SIGNAL('clicked()'), $qApp, SLOT('quit()'))

    grid = Qt::GridLayout.new()

    for row in 0..2
      for column in 0..2
        grid.addWidget(LCDRange.new(), row, column)
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
