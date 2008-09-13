require 'Qt'
require 'LCDRange'
require 'Cannon'

class MyWidget < Qt::Widget
  def initialize(parent = nil)
    super()
    quit = Qt::PushButton.new(tr('Quit'))
    quit.setFont(Qt::Font.new('Times', 18, Qt::Font::Bold))

    shoot = Qt::PushButton.new(tr('&Shoot'))
    shoot.setFont(Qt::Font.new('Times', 18, Qt::Font::Bold))

    angle = LCDRange.new('Angle')
    angle.setRange(5,70)

    force = LCDRange.new('Force')
    force.setRange(10,50)

    cannonField = CannonField.new()

    connect(quit, SIGNAL('clicked()'), $qApp, SLOT('quit()'))

    connect(shoot, SIGNAL('clicked()'), cannonField, SLOT('shoot()'))

    connect(angle, SIGNAL('valueChanged(int)'), cannonField, SLOT('setAngle(int)'))
    connect(cannonField, SIGNAL('angleChanged(int)'), angle, SLOT('setValue(int)'))

    connect(force, SIGNAL('valueChanged(int)'), cannonField, SLOT('setForce(int)'))
    connect(cannonField, SIGNAL('forceChanged(int)'), force, SLOT('setValue(int)'))

    leftLayout = Qt::VBoxLayout.new()
    leftLayout.addWidget(angle)
    leftLayout.addWidget(force)

    gridLayout = Qt::GridLayout.new()
    gridLayout.addWidget(quit, 0, 0)
    gridLayout.addWidget(shoot, 0, 1)
    gridLayout.addLayout(leftLayout, 1, 0)
    gridLayout.addWidget(cannonField, 1, 1, 2, 1)
    gridLayout.setColumnStretch(1, 10)

    setLayout(gridLayout)

    angle.setValue(60)
    force.setValue(25)

    angle.setFocus()
  end
end

app = Qt::Application.new(ARGV)
widget = MyWidget.new()
widget.resize(500,355)
widget.show()
app.exec()
