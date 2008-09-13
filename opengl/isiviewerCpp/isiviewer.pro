TEMPLATE = app
TARGET   = 
CONFIG  += qt opengl warn_on debug thread
QT      += xml opengl

FORMS       = my_gui_form.ui
HEADERS     = my_qglviewer.h my_main_window.h my_scene.h Mesh.h Object.h GLFunctor.h ObjectGroup.h
SOURCES     = my_qglviewer.cpp my_main_window.cpp my_scene.cpp Mesh.cpp main.cpp operators.cpp Triangle.cpp Object.cpp GLFunctor.cpp ObjectGroup.cpp

INCLUDEPATH += ./glm-0.4.1

unix:LIBS *= -lQGLViewer

# Intermediate files are created in a separate folder
MOC_DIR = .moc
OBJECTS_DIR = .obj
