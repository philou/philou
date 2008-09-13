require 'Qt'

class CannonField < Qt::Widget

  signals 'angleChanged(int)'
  slots 'setAngle(int)'

  def initialize(parent = nil)
    super()
    @currentAngle = 45
    setPalette(Qt::Palette.new(Qt::Color.new(250,250,200)))
    setAutoFillBackground(true)
  end

  def setAngle(angle)
    if angle < 5
      angle = 5
    elsif angle > 70
      angle = 70
    end

    if @currentAngle == angle
      return
    end

    @currentAngle = angle
    update()
    emit angleChanged(@currentAngle)
  end

  def paintEvent(event)
    painter = Qt::Painter.new(self)

    painter.setPen(Qt::NoPen)
    painter.setBrush(Qt::Brush.new(Qt::blue))
    painter.translate(0, rect().height())
    painter.drawPie(Qt::Rect.new(-35,-35,70,70), 0, 90*16)
    painter.rotate(-@currentAngle)
    painter.drawRect(Qt::Rect.new(30,-5,20,10))

    painter.end()
  end

end
