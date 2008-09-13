require 'Qt'

class LCDRange < Qt::Widget
  def initialize(parent = nil)
    super()
    lcd = Qt::LCDNumber.new(2)
    lcd.setSegmentStyle(Qt::LCDNumber::Filled)

    @slider = Qt::Slider.new(Qt::Horizontal)
    @slider.setRange(0,99)
    @slider.setValue(0)

    connect(@slider, SIGNAL('valueChanged(int)'), lcd, SLOT('display(int)'))
    connect(@slider, SIGNAL('valueChanged(int)'), self, SIGNAL('valueChanged(int)'))

    layout = Qt::VBoxLayout.new()
    layout.addWidget(lcd)
    layout.addWidget(@slider)
    setLayout(layout)
  end

  signals 'valueChanged(int)'
  slots 'setValue(int)', 'setRange(int,int)'

  def value()
    @slider.value()
  end

  def setValue(value)
    @slider.setValue(value)
  end

  def setRange(minVal, maxVal)
    if minVal < 0 || maxVal > 99 || minVal > maxVal
      qWarning("LCDRange:setRange(#{minVal}, #{maxVal})\n" +
               "\tRange must be 0..99\n" +
               "\tand minVal must not be greater than maxVal\n")
      return
    end

    @slider.setRange(minVal, maxVal)
  end

end
