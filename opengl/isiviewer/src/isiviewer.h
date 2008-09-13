/**
 * @file   isiviewer.h
 * @author Bruno Jobard <bruno.jobard@univ-pau.fr>
 * @author Binome1 <binome1@son.adresse.fr>
 * @author Binome2 <binome2@son.adresse.fr>
 * @date   Sep 2005
 * 
 * @brief  Ze ISI Viewer !
 * 
 * 
 */

#ifndef _ISIVIEWER_
#define _ISIVIEWER_

#include <GL/glut.h>
#include <stdbool.h>

/// Some <math.h> files do not define M_PI...
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/// Structure that gathers all the variables the glut application depends on

/**
 * This structure is composed of several sub-structures that split the
 * global variables of the application in distinct sections
 *
 */
struct StructISIViewerApp {

  /// graphical window variables
  struct Window {
    int xsize, ysize;		/**< window resolution in x and y */
    char* title;		/**< title of the window */
  } window;

  /// View and Frustrum variables 
  struct View {
    float scalefactor;		/**< scale factor to display the objects */
    float zNear, zFar;
  } view;

  /// Defines how are displayed the objects
  struct Display {
    int mode;
  } display;

  /// Light information
  struct Ligthing {
    GLfloat lightPosition[4];
  } lighting;

  /// Mouse variables
  struct Mouse {
    int downX, downY;
    bool scaling;		/**< yes if we are scaling the objects */
  } mouse;

  /// Defines the current object that is displayed
  struct Scene {
    int currentObject;
  } scene;

  /// Menu identificators
  struct Menu {
    int mainMenu, displayMenu, objectsMenu; 
  } menu;

};
typedef struct StructISIViewerApp ISIViewerApp;

/// Display modes
enum DisplayModes {WIREFRAME, FLATSHADED, SMOOTHSHADED};
/// Menu items
enum MenuItem {QUIT};
/// Objects in the scene
enum SceneObjects {CUBE, PYRAMID, DISK, DISKHOLE, CYLINDER, CONE, SPHERE, VENUS, NEFERTITI, AIRCRAFT, TEST};


/// Called when the 3D scene needs to be redisplayed
void display(void);

/// Called when the window is resized
void reshape(int width, int height);

/// Called when a mouse button has been pressed
void mouse(int button, int state, int x, int y);

/// Called when the mouse move while a button is pressed
void motion(int x, int y);

/// Called when a key has been pressed 
void keyboard(unsigned char ch, int x, int y);


/// Main menu function
void setMain(int menuItem);

/// Display mode menu function
void setDisplayMode(int displayModeItem);

/// Object menu function
void setCurrentObject(int objectItem);

void quit();

#endif
