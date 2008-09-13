require 'Qt'
include Math

class CannonField < Qt::Widget

  signals 'angleChanged(int)', 'forceChanged(int)'
  slots 'setAngle(int)', 'setForce(int)', 'shoot()', 'moveShot()'

  def initialize(parent = nil)
    super()

    @currentAngle = 45
    @currentForce = 0
    @timerCount = 0
    @autoShootTimer = Qt::Timer.new(self)
    @shootAngle = 0
    @shootForce = 0

    connect(@autoShootTimer, SIGNAL('timeout()'), self, SLOT('moveShot()'))
    setPalette(Qt::Palette.new(Qt::Color.new(250,250,200)))
    setAutoFillBackground(true)

    @barrelRect = Qt::Rect.new(30,-5,20,10)
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
    update(cannonRect())
    emit angleChanged(@currentAngle)
  end

  def setForce(force)
    if force < 0
      force = 0
    end
    if @currentForce == force
      return
    end

    @currentForce = force
    emit forceChanged(@currentForce)
  end

  def shoot()
    if @autoShootTimer.isActive()
      return
    end

    @timerCount = 0
    @shootAngle = @currentAngle
    @shootForce = @currentForce
    @autoShootTimer.start(5)
  end

  def moveShot()
    region = Qt::Region.new(shotRect())
    @timerCount += 1

    shotR = shotRect()

    if shotR.x() > width() || shotR.y() > height()
      @autoShootTimer.stop()
    else
      region = region.unite(Qt::Region.new(shotR))
    end
    update(region)
  end

  def paintEvent(event)
    painter = Qt::Painter.new(self)

    paintCannon(painter)
    if @autoShootTimer.isActive()
      paintShot(painter)
    end

    painter.end()
  end

  def paintCannon(painter)
    painter.save()

    painter.setPen(Qt::NoPen)
    painter.setBrush(Qt::Brush.new(Qt::blue))

    painter.translate(0, rect().height())
    painter.drawPie(Qt::Rect.new(-35,-35,70,70), 0, 90*16)
    painter.rotate(-@currentAngle)
    painter.drawRect(@barrelRect)

    painter.restore()
  end

  def paintShot(painter)
    painter.setPen(Qt::NoPen)
    painter.setBrush(Qt::Brush.new(Qt::black))
    painter.drawRect(shotRect())
  end

  def cannonRect()
    result = Qt::Rect.new(0,0,50,50)
    result.moveBottomLeft(rect().bottomLeft())
    return result
  end

  def shotRect()
    gravity = 4.0
    
    time = @timerCount / 20.0
    velocity = @shootForce
    radians = @shootAngle * 3.14159265 / 180.0

    velx = velocity * cos(radians)
    vely = velocity * sin(radians)
    x0 = (@barrelRect.right() + 5.0) * cos(radians)
    y0 = (@barrelRect.right() + 5.0) * sin(radians)
    x = x0 + velx * time
    y = y0 + vely * time - 0.5 * gravity * time * time

    result = Qt::Rect.new(0,0,6,6)
    result.moveCenter(Qt::Point.new(x.round, height() -1 -y.round))
    return result
  end

end
