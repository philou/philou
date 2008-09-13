#include "my_main_window.h"
#include <math.h>
#include <QMessageBox>

/** 
 * Construct MyMainWindow
 * 
 * @param myScene 
 * @param parent 
 */
MyMainWindow::MyMainWindow(MyScene* myScene, QWidget *parent) 
  : QMainWindow(parent), _myScene(myScene)
{
  _ui.setupUi(this);
  _ui.viewer->setMyScene(_myScene);

  // Connect menu items
  connect(_ui.actionQuit, SIGNAL(activated()),
	  this, SLOT(close()));

  connect(_ui.actionAbout, SIGNAL(activated()),
	  this, SLOT(slotAbout()));

  // Connect GUI and myScene
  connect(_ui.radiusSpinBox, SIGNAL(valueChanged(double)), 
	  _myScene, SLOT(slotSetRadius(double)));

  connect(_myScene, SIGNAL(sigRadiusChanged(double)),
	  _ui.radiusSpinBox, SLOT(setValue(double)));

  connect(_myScene, SIGNAL(sigRadiusChanged(double)), 
	  _ui.viewer, SLOT(updateGL()));

  // Init GUI default values
  _ui.radiusSpinBox->setValue     (_myScene->radius());
  _ui.radiusSpinBox->setMinimum   (_myScene->radiusMin());
  _ui.radiusSpinBox->setMaximum   (_myScene->radiusMax());
  _ui.radiusSpinBox->setSingleStep(_myScene->radiusIncr());
}

/** 
 * Slot About
 * 
 */
void MyMainWindow::slotAbout()
{
  QMessageBox::about(this, "SIA Viewer", "Bruno Jobard - 2006\nreplace by your names...");
}
