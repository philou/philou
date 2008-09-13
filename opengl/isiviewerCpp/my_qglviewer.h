/**
 * @file   my_qglviewer.h
 * @author Bruno Jobard
 * @author Author1
 * @author Author2
 * @date   Oct 2006
 * 
 * @brief  Defines a custom viewer
 * 
 * 
 */
#ifndef _ISI_MY_QGLVIEWER_H_
#define _ISI_MY_QGLVIEWER_H_

#include <QGLViewer/qglviewer.h>
#include <QPointer>
#include "my_scene.h"

class MyQGLViewer : public QGLViewer
{
 public:
  MyQGLViewer(QWidget *parent);
  void setMyScene(MyScene* myScene);
  
 protected:
  virtual void init();
  virtual void draw();
  //virtual QString helpString() const; 
  virtual void keyPressEvent(QKeyEvent *e);

 protected:
  QPointer<MyScene> _myScene;
};

#endif
