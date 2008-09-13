require 'Qt'

class LCDRange < Qt::Widget
  def initialize(title, parent = nil)
    super(parent)
    init()
    setText(title)
  end

  def init()
    lcd = Qt::LCDNumber.new(2)
    lcd.setSegmentStyle(Qt::LCDNumber::Filled)

    @slider = Qt::Slider.new(Qt::Horizontal)
    @slider.setRange(0,99)
    @slider.setValue(0)

    @label = Qt::Label.new()
    @label.setAlignment(Qt::AlignHCenter.to_i | Qt::AlignTop.to_i)

    connect(@slider, SIGNAL('valueChanged(int)'), lcd, SLOT('display(int)'))
    connect(@slider, SIGNAL('valueChanged(int)'), self, SIGNAL('valueChanged(int)'))

    layout = Qt::VBoxLayout.new()
    layout.addWidget(lcd)
    layout.addWidget(@slider)
    layout.addWidget(@label)
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

  def setText(title)
    @label.setText(title)
  end

end
