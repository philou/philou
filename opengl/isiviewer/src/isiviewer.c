/**
 * @file   isiviewer.c
 * @author Bruno Jobard <bruno.jobard@univ-pau.fr>
 * @author Binome1 <binome1@son.adresse.fr>
 * @author Binome2 <binome2@son.adresse.fr>
 * @date   Sep 2005
 * 
 * @brief  Main ISIViewer file
 * 
 * 
 */

#include "isiviewer.h"
#include "objects.h"
#include "tb.h"
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "trackball.h"


// Glut application global variables
static ISIViewerApp *iv;
static Mesh *cube;
static Mesh *aircraft;
static Mesh *venus;
static Mesh *nefertiti;
static Mesh *pyramid;
static Mesh *disk;
static Mesh *diskhole;
static Mesh *cylinder;
static Mesh *cone;
static Mesh *sphere;
static Mesh *test;



/// Initialise ISIViewerApp structure attributes
void initISIViewerApp(ISIViewerApp* iv) 
{
  // window attributes
  iv->window.xsize = 900;
  iv->window.ysize = 700;
  iv->window.title = strdup("Glut Base Application");

  // view attributes
  iv->view.scalefactor = 1.0;
  iv->view.zNear = 1.0;
  iv->view.zFar  = 100.0;

  // display attributes
  iv->display.mode = FLATSHADED;

  // lighting attributes
  GLfloat lpos[4]= { 0.0, 0.0, 1.0, 1.0}; 
  memcpy(iv->lighting.lightPosition, lpos, sizeof lpos);

  // mouse attributes 
  iv->mouse.scaling = false;

  // scene attributes
  iv->scene.currentObject = NEFERTITI;
}

/// Initialize OpenGL state
void initGL(ISIViewerApp* iv)
{
  glClearColor(1.0, 1.0, 1.0, 0.0);
  glColor3f(0.5, 0., 0.);
  glEnable(GL_DEPTH_TEST);

  // initialize trackball environment
  tbInit(GLUT_LEFT_BUTTON);
  tbAnimate(GL_TRUE);

  // enable ligthing
  glLightfv (GL_LIGHT0, GL_POSITION, iv->lighting.lightPosition);
  glEnable(GL_LIGHT0);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);
  
  // set antialiased lines
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glLineWidth(1.);

  setDisplayMode(iv->display.mode);
}

/** 
 * Display the current 3D scene depending on the current object
 * selected with the menu.
 * 
 */
void display(void)
{
  // save current MODELVIEW matrix on the stack
  glPushMatrix();
  
  // Trackball rotation and Zoom
  tbMatrix();
  glScalef(iv->view.scalefactor, iv->view.scalefactor, iv->view.scalefactor);

  // clear color and depth buffer
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  //--- Draw obects in the scene

  switch(iv->scene.currentObject) {
  case CUBE : drawMesh(cube, iv->display.mode == SMOOTHSHADED);  break;
  case VENUS : drawMesh(venus, iv->display.mode == SMOOTHSHADED);  break;
  case AIRCRAFT : drawMesh(aircraft, iv->display.mode == SMOOTHSHADED);  break;
  case NEFERTITI : drawMesh(nefertiti, iv->display.mode == SMOOTHSHADED);  break;
  case PYRAMID : drawMesh(pyramid, iv->display.mode == SMOOTHSHADED);  break;
  case DISK : drawMesh(disk, iv->display.mode == SMOOTHSHADED);  break;
  case DISKHOLE : drawMesh(diskhole, iv->display.mode == SMOOTHSHADED);  break;
  case CYLINDER : drawMesh(cylinder, iv->display.mode == SMOOTHSHADED);  break;
  case CONE : drawMesh(cone, iv->display.mode == SMOOTHSHADED);  break;
  case SPHERE : drawMesh(sphere, iv->display.mode == SMOOTHSHADED);  break;
  case TEST : drawMesh(test, iv->display.mode == SMOOTHSHADED);  break;
  }

  // swap back color buffer with the front color buffer
  glutSwapBuffers();

  // restore previous MODELVIEW matrix
  glPopMatrix();
}

/** 
 * Sets correct projection parameters each time the window is resized.
 * 
 * @param width  horizontal size in pixels of the window
 * @param height   vertical size in pixels of the window
 */
void reshape(int width, int height)
{
  // give the new window dimensions to the trackball environment
  tbReshape(width, height);
  
  // adjust the viewport dimensions
  glViewport(0,0,width,height);

  // Set up a perspective projection
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(45., (GLfloat) width/height, 1., 25.);
  
  // Set up the view point and direction, and up vector
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt( 0.0, 0.0, 5.0,  // eye is at (0,0,10)
  	     0.0, 0.0, 0.0,  // center is at (0,0,0)
  	     0.0, 1.0, 0.);  // up is in positive Y direction
  
  // ask the glutMainLoop to redisplay the 3D scene as soon as it can
  glutPostRedisplay();
}

/** 
 * Define the behaviour of the application when a mouse button is
 * pressed. Here, if the shift key is pressed at the same time, we
 * enter in the scaling mode.
 * 
 * @param button which button as been pressed
 * @param state what is the state of the button
 * @param x at which x pixel coordinate
 * @param y at which y pixel coordinate
 */
void mouse(int button, int state, int x, int y)
{
  // give the current state to the trackball
  tbMouse(button, state, x, y);

  // if the left button is pressed...
  if (button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
    iv->mouse.downX = x;
    iv->mouse.downY = y;
    
    // and if the shift key is pressed at the same time
    if (glutGetModifiers() & GLUT_ACTIVE_SHIFT) {
      // we are scaling the objects
      iv->mouse.scaling = true;
    } else {
      iv->mouse.scaling = false;
    }
  }
  
  // ask the glutMainLoop to redisplay the 3D scene as soon as it can
  glutPostRedisplay();
}

/** 
 * Defines what happens when the mouse is moved while a button is
 * pressed. In our case, either we record a rotation with the
 * trackball, either change the scaling factor for the objects
 * 
 * @param x x location of the mouse cursor
 * @param y y location of the mouse cursor
 */
void motion(int x, int y)
{
  // if we are scaling the objects
  if (iv->mouse.scaling) {
    // compute the new scaling factor depending on the mouse displacement
    iv->view.scalefactor *=(1.0+(iv->mouse.downY-y)/(float)iv->window.ysize);
    iv->mouse.downX = x;
    iv->mouse.downY = y;
    
    // Ask OpenGL to normalize normals if necessary
    if (iv->view.scalefactor == 1.0) glDisable(GL_NORMALIZE);
    else                             glEnable (GL_NORMALIZE);

  } else { // if we're not scaling the objects, rotate them!
    tbMotion(x, y);
  }

  glutPostRedisplay();
}

/** 
 * Gives application modification when a key is pressed. 
 * 
 * @param key code of the key that is pressed
 * @param x x location of the mouse cursor
 * @param y y location of the mouse cursor
 */
void keyboard(unsigned char key, int x, int y)
{
  switch (key) 
    {
    case 27 : // quit the program when the user presses on "echap" key 
      quit(); 
      break;  
    case 'c': iv->scene.currentObject = CUBE; break;
    case 'v': iv->scene.currentObject = VENUS; break;
    case 'n': iv->scene.currentObject = NEFERTITI; break;
    case 'a': iv->scene.currentObject = AIRCRAFT; break;
    case 'p': iv->scene.currentObject = PYRAMID; break;
    case 'd': iv->scene.currentObject = DISK; break;
    case 'h': iv->scene.currentObject = DISKHOLE; break;
    case 'y': iv->scene.currentObject = CYLINDER; break;
    case 'o': iv->scene.currentObject = CONE; break;
    case 's': iv->scene.currentObject = SPHERE; break;
    case 't': iv->scene.currentObject = TEST; break;
    case 'W': setDisplayMode(WIREFRAME); break;
    case 'F': setDisplayMode(FLATSHADED); break;
    case 'S': setDisplayMode(SMOOTHSHADED); break;
    }
  
  glutPostRedisplay();
}

/** 
 * Select the display mode between : wireframe, flat shaded and smooth shaded
 * 
 * @param displayModeItem one of the DisplayModes Enumerator items
 */
void setDisplayMode(int displayModeItem)
{
  iv->display.mode = displayModeItem;
  switch(displayModeItem) 
    {
    case WIREFRAME   : 
      glDisable(GL_LIGHTING);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      break;
    case FLATSHADED  : 
      glEnable(GL_LIGHTING);
      glShadeModel(GL_FLAT);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      break;
    case SMOOTHSHADED :
      glEnable(GL_LIGHTING);
      glShadeModel(GL_SMOOTH); 
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      break;
    }
  glutPostRedisplay();
}

/** 
 * Define the objet that will be displayed the next time
 * 
 * @param objectItem one of the SceneObjects enumerator items 
 */
void setCurrentObject(int objectItem)
{
  iv->scene.currentObject = objectItem;

  glutPostRedisplay();
}

/** 
 * Reacts to the main menu item selection
 * 
 * @param menuItem one of the MenuIem items 
 */
void setMain(int menuItem)
{
  switch(menuItem) 
    {
    case QUIT: 
    case 'q':
      quit(); 
      break;
    }
}

/**
 * Cleans everything before leaving
 * 
 */
void quit(void)
{
  free(iv->window.title);

  freeMesh(cube);
  freeMesh(venus);
  freeMesh(nefertiti);
  freeMesh(aircraft);
  freeMesh(pyramid);
  freeMesh(disk);
  freeMesh(diskhole);
  freeMesh(cylinder);
  freeMesh(cone);
  freeMesh(sphere);
  freeMesh(test);

  exit(0);
}

/**
 * 
 * 
 */
int main(int argc, char **argv)
{
  // Initialize the ISI Viewer Application
  ISIViewerApp isiViewerApp;
  iv = &isiViewerApp;
  initISIViewerApp(iv);

  // Objects in the scene
  Mesh m1;
  cube = &m1;
  initFileMesh(cube, "/home/poulet/opengl/isiviewer/data/cube.off");
  
  Mesh m2;
  aircraft = &m2;
  initFileMesh(aircraft, "/home/poulet/opengl/isiviewer/data/aircraft.off");

  Mesh m3;
  venus = &m3;
  initFileMesh(venus, "/home/poulet/opengl/isiviewer/data/venus.off");

  Mesh m4;
  nefertiti = &m4;
  initFileMesh(nefertiti, "/home/poulet/opengl/isiviewer/data/nefertiti-entire.off");
  
  Mesh m5;
  pyramid = &m5;
  initFileMesh(pyramid, "/home/poulet/opengl/isiviewer/data/pyramid.off");
  
  Mesh m6;
  disk = &m6;
  initFileMesh(disk, "/home/poulet/opengl/isiviewer/data/disk.off");
  
  Mesh m7;
  diskhole = &m7;
  initFileMesh(diskhole, "/home/poulet/opengl/isiviewer/data/diskhole.off");
  
  Mesh m8;
  cylinder = &m8;
  initFileMesh(cylinder, "/home/poulet/opengl/isiviewer/data/cylinder.off");
  
  Mesh m9;
  cone = &m9;
  initFileMesh(cone, "/home/poulet/opengl/isiviewer/data/cone.off");
  
  Mesh m10;
  sphere = &m10;
  initFileMesh(sphere, "/home/poulet/opengl/isiviewer/data/sphere.off");
    
  Mesh m11;
  test = &m11;
  initFileMesh(test, "/home/poulet/opengl/isiviewer/data/cube.off");

  // Glut and OpenGL initialization
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(iv->window.xsize, iv->window.ysize);
  glutCreateWindow(iv->window.title);

  // Initialize OpenGL state
  initGL(iv);

  // Callback functions
  glutReshapeFunc(reshape);
  glutDisplayFunc(display);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse);
  glutMotionFunc(motion);

  // Menus
  iv->menu.displayMenu = glutCreateMenu(setDisplayMode);
  glutAddMenuEntry("Wireframe"    , WIREFRAME );
  glutAddMenuEntry("Flat Shaded"  , FLATSHADED);
  glutAddMenuEntry("Smooth Shaded", SMOOTHSHADED);

  iv->menu.objectsMenu = glutCreateMenu(setCurrentObject);
  glutAddMenuEntry("Cube"    , CUBE);
  glutAddMenuEntry("Aircraft" , AIRCRAFT);
  glutAddMenuEntry("Nefertiti" , NEFERTITI);
  glutAddMenuEntry("Venus" , VENUS);
  glutAddMenuEntry("Pyramid" , PYRAMID);
  glutAddMenuEntry("Disk" , DISK);
  glutAddMenuEntry("Disk hole" , DISKHOLE);
  glutAddMenuEntry("Cylinder" , CYLINDER);
  glutAddMenuEntry("Cone" , CONE);
  glutAddMenuEntry("Sphere" , SPHERE);
  glutAddMenuEntry("Test" , TEST);
  
  iv->menu.mainMenu = glutCreateMenu(setMain);
  glutAddSubMenu("Display", iv->menu.displayMenu);
  glutAddSubMenu("Objects", iv->menu.objectsMenu);
  glutAddMenuEntry("Exit", QUIT);
  glutAttachMenu(GLUT_RIGHT_BUTTON);

  // Main Loop
  glutMainLoop();

  return 0;
}
