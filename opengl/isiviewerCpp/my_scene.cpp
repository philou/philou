#include "my_scene.h"

#include <iostream>
#include <math.h>
#include <GL/gl.h>  // OpenGL include file
#include <boost/shared_ptr.hpp>

using namespace std;

/** 
 * Constructor
 * 
 * @param radius 
 */
MyScene::MyScene(float radius)
 : _allObjects(NULL)
{
  std::cout << "constructing :  MyScene" << std::endl;

  _currentObject = MyScene::CUBE;
 
  _displayMode = MyScene::SMOOTHSHADED;

  _radius = radius;

  _radiusMin = 0.1;
  _radiusMax = 2.0;
  _radiusIncr = 0.1;

  _dayOfYear = 0;
}

/** 
 * Destructor
 * 
 */
MyScene::~MyScene()
{
  std::cout<<"~MyScene"<<std::endl;
  delete _allObjects;
}

/** 
 * Init the scene and OpenGL state
 * 
 */
void MyScene::init()
{
  _objects[CUBE].load("./data/cube.off");
  _objects[PYRAMID].load("./data/pyramid.off");
  _objects[DISK].load("./data/disk.off");
  _objects[DISKHOLE].load("./data/diskhole.off");
  _objects[CYLINDER].load("./data/cylinder.off");
  _objects[CONE].load("./data/cone.off");
  _objects[SPHERE].load("./data/sphere.off");
  _objects[AIRCRAFT].load("./data/aircraft.off");
  _objects[VENUS].load("./data/venus.off");
  _objects[NEFERTITI].load("./data/nefertiti.off");
	
  Mesh* cone = new Mesh();
  cone->load("./data/cone.off");
  cone->addTransformation(glTranslatef,0.0,0.0,_radius);
  cone->addTransformation(glScalef,0.1, 0.1, 0.5);

  Mesh* cylinder = new Mesh();
  cylinder->load("./data/cylinder.off");
  cylinder->addTransformation(glScalef,0.01, 0.01, 1.0);

  ObjectGroup* zArrow = new ObjectGroup();
  zArrow->add(cone);
  zArrow->add(cylinder);
  //zArrow->addTransformation(glScalef, _radius/2, _radius/2, _radius/2);
  zArrow->addTransformation(glColor3f, 0, 0, 0.5);

  ObjectGroup* xArrow = zArrow->clone();
  xArrow->addTransformation(glRotatef, 90, 0, 1, 0);
  xArrow->addTransformation(glColor3f, 0.5, 0, 0);

  ObjectGroup* yArrow = zArrow->clone();
  yArrow->addTransformation(glRotatef, -90, 1, 0, 0);
  yArrow->addTransformation(glColor3f, 0, 0.5, 0);
  
  Mesh* centerSphere = new Mesh();
  centerSphere->load("./data/sphere.off");
  centerSphere->addTransformation(glScalef,0.05, 0.05, 0.05);
  centerSphere->addTransformation(glColor3f, 0.5, 0.5, 0.5);

  _allObjects = new ObjectGroup();
  _allObjects->add(centerSphere);
  _allObjects->add(xArrow);
  _allObjects->add(yArrow);
  _allObjects->add(zArrow);
  
  Mesh* sun = new Mesh();
  sun->load("./data/sphere.off");
  sun->addTransformation(glScalef, 0.3, 0.3, 0.3);
  sun->addTransformation(glColor3f, 0.9, 0.2, 0.0);
  _allObjects->add(sun);
  
  ObjectGroup* earthAndMoon = new ObjectGroup();
  _earthAndMoonRotator = new ParametrizedRotator(0,0,1);
  earthAndMoon->addTransformation(_earthAndMoonRotator);
  earthAndMoon->addTransformation(glTranslatef, 2, 0.0, 0.0);
  
  Mesh* earth = new Mesh();
  earth->load("./data/sphere.off");
  earth->addTransformation(glScalef, 0.1, 0.1, 0.1);
  earth->addTransformation(glColor3f, 0.0, 0.2, 0.9);
  earthAndMoon->add(earth);
  
  Mesh* moon = new Mesh();
  moon->load("./data/sphere.off");
  _moonRotator = new ParametrizedRotator(0,0,1);
  moon->addTransformation(_moonRotator);
  moon->addTransformation(glTranslatef, 0.5, 0.0, 0.0);
  moon->addTransformation(glScalef, 0.04, 0.04, 0.04);
  moon->addTransformation(glColor3f, 0.3, 0.3, 0.3);
  earthAndMoon->add(moon);

  _allObjects->add(earthAndMoon);
  
  setDayOfYear(0);
 
  // set antialiased lines
  //glEnable(GL_LINE_SMOOTH);	// FIXME current MESA driver messes up with antialiasing ...
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glLineWidth(1.5);
}

/** 
 * Draw the scene
 * 
 */
void MyScene::draw()
{
	//glDisable(GL_LIGHT0);

	glScalef( _radius/2, _radius/2, _radius/2);
	_allObjects->draw(_displayMode == MyScene::SMOOTHSHADED);

 	GLfloat light_position[] = { 1.0, 1.0, 1.0, 0.0 };
	glLightfv(GL_LIGHT1, GL_POSITION, light_position);
	glEnable(GL_LIGHT1);
}

void MyScene::setDayOfYear(int dayOfYear)
{
	_dayOfYear = dayOfYear;
	if( NULL != _earthAndMoonRotator)
	{
		_earthAndMoonRotator->setAngle( (GLfloat)(_dayOfYear % 365) / 365.0 * 360.0 );
	}

	if( NULL != _moonRotator)
	{
		_moonRotator->setAngle( (GLfloat)(_dayOfYear % 21) / 21.0 * 360.0 );
	}
}

/** 
 * Slot set current object
 * 
 * @param currentObject
 */
void MyScene::slotSetCurrentObject(int currentObject)
{
  std::cout << "slotSetCurrentObject "<< currentObject << std::endl;
  _currentObject = currentObject % COUNT;
  //std::cout << _objects[_currentObject] << std::endl;
  emit sigCurrentObjectChanged(currentObject);
}

/** 
 * Slot set display mode
 * 
 * @param currentObject
 */
void MyScene::slotSetDisplayMode(int displayMode)
{
  std::cout << "slotSetDisplayMode "<< displayMode%3 << std::endl;
  _displayMode = displayMode;

  switch (displayMode%3) {
  case MyScene::WIREFRAME:
    glDisable(GL_LIGHTING);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    break;
  case MyScene::FLATSHADED: 
    glEnable(GL_LIGHTING);
    glShadeModel(GL_FLAT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    break;
  case MyScene::SMOOTHSHADED:
    glEnable(GL_LIGHTING);
    glShadeModel(GL_SMOOTH);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    break;
  }

  emit sigDisplayModeChanged(displayMode);
}

/** 
 * Slot set radius
 * 
 * @param radius 
 */
void MyScene::slotSetRadius(double radius)
{
  if (fabs(_radius - float(radius))>1e-6) {
    //std::cout << "slotSetRadius "<< radius << std::endl;
    _radius = radius;
    emit sigRadiusChanged(radius);
  }
}

/** 
 * Process keyboard events for QGLViewer widget
 * 
 * @param e a keyboard event from QGLViewer 
 * 
 * @return true if the event has been handled
 */
bool MyScene::keyPressEvent(QKeyEvent *e)
{
  const Qt::KeyboardModifiers modifiers = e->modifiers();

  // A simple switch on e->key() is not sufficient if we want to take
  // state key into account.  With a switch, it would have been
  // impossible to separate 'F' from 'CTRL+F'.  That's why we use
  // imbricated if...else and a "handled" boolean.

  bool handled = false;

  float epsilon = 1e-5;  // for float comparison
  
  // Increase radius with 'r'
  if ((e->key()==Qt::Key_R) && (modifiers==Qt::NoButton)) {
    if (_radius+_radiusIncr <= _radiusMax + epsilon)
      this->slotSetRadius(_radius+_radiusIncr);
    handled = true;
  } 
  // Decrease radius with 'R'
  else if ((e->key()==Qt::Key_R) && (modifiers==Qt::SHIFT)) {
    if (_radius-_radiusIncr >= _radiusMin - epsilon)
      this->slotSetRadius(_radius-_radiusIncr);
    handled = true;
  }
      
  // Increase current displayed object with 'o'
  if ((e->key()==Qt::Key_O) && (modifiers==Qt::NoButton)) { 
    this->slotSetCurrentObject(_currentObject+1);
    handled = true;
  } 
  // Decrease current displayed object with 'O'
  else if ((e->key()==Qt::Key_O) && (modifiers==Qt::SHIFT)) {
    this->slotSetCurrentObject(_currentObject-1);
    handled = true;
  }
      
  // change displau mode with 's'
  else if ((e->key()==Qt::Key_S) && (modifiers==Qt::NoButton)) {
    this->slotSetDisplayMode(_displayMode+1);
    handled = true;
  }

  // increase day with 'd'
  else if ((e->key()==Qt::Key_D) && (modifiers==Qt::NoButton)) {
    this->setDayOfYear(_dayOfYear+1);
    handled = true;
  }
  
  // decrease day with 'd'
  else if ((e->key()==Qt::Key_D) && (modifiers==Qt::SHIFT)) {
    this->setDayOfYear(_dayOfYear-1);
    handled = true;
  }
  
  
      // ... and so on with other else/if blocks.

  return handled;
}
