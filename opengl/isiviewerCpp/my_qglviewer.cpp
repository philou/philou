#include "my_qglviewer.h"

MyQGLViewer::MyQGLViewer(QWidget *parent)
  : QGLViewer(parent), _myScene(NULL)
{

}

void MyQGLViewer::setMyScene(MyScene* myScene)
{
  _myScene = myScene;
}

void MyQGLViewer::init()
{
  // call the scene init method
  _myScene->init();

}


void MyQGLViewer::draw()
{
  // call the scene draw method
  _myScene->draw();

}

void MyQGLViewer::keyPressEvent(QKeyEvent *e)
{
  bool handled = false;

  handled = _myScene->keyPressEvent(e);
  
  if (!handled)
    QGLViewer::keyPressEvent(e);

  // update the window
  updateGL();
}
