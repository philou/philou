/**
 * @file   main.cpp
 * @author Bruno Jobard
 * @author Author1
 * @author Author2
 * @date   Oct 2006
 * 
 * @brief  Declares an application, a main window and a 3D scene
 * 
 * 
 */
#include <QApplication>
#include "my_main_window.h"

int main(int argc, char *argv[])
{
  QApplication app(argc, argv);
  
  // initialize my custom 3D scene
  float objectRadius = 1.;
  QPointer<MyScene> myScene = new MyScene(objectRadius);

  // initialize my custom main window
  QPointer<MyMainWindow> myMainWindow = new MyMainWindow(myScene);
  
  // display the window
  myMainWindow->show();
  
  // enter in the main loop
  return app.exec();
}

