/**
 * @file   my_main_window.h
 * @author Bruno Jobard
 * @author Author1
 * @author Author2
 * @date   Oct 2006
 * 
 * @brief  Defines the main window composed of a Graphical User
 * Interface and a 3D scene.
 * 
 * 
 */
#ifndef _ISI_MY_MAIN_WINDOW_H_
#define _ISI_MY_MAIN_WINDOW_H_

#include <QApplication>
#include <QPointer>
#include "ui_my_gui_form.h"

class MyMainWindow : public QMainWindow
{
  Q_OBJECT
  
public:
  MyMainWindow(MyScene* _myScene, QWidget *parent = 0);
  
public slots:
void slotAbout(); 
  
private:
  Ui::MainWindow _ui;
  QPointer<MyScene> _myScene;

};

#endif
