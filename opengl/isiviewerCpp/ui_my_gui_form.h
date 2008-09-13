/********************************************************************************
** Form generated from reading ui file 'my_gui_form.ui'
**
** Created: Sun Feb 25 08:18:04 2007
**      by: Qt User Interface Compiler version 4.2.0
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_MY_GUI_FORM_H
#define UI_MY_GUI_FORM_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QDoubleSpinBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QLabel>
#include <QtGui/QMainWindow>
#include <QtGui/QMenu>
#include <QtGui/QMenuBar>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QStatusBar>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "my_qglviewer.h"

class Ui_MainWindow
{
public:
    QAction *actionQuit;
    QAction *actionOpen;
    QAction *actionAbout;
    QWidget *centralwidget;
    QVBoxLayout *vboxLayout;
    MyQGLViewer *viewer;
    QHBoxLayout *hboxLayout;
    QCheckBox *checkBox;
    QCheckBox *checkBox_2;
    QSpacerItem *spacerItem;
    QLabel *label;
    QDoubleSpinBox *radiusSpinBox;
    QSpacerItem *spacerItem1;
    QPushButton *pushButton;
    QMenuBar *menubar;
    QMenu *menuHelp;
    QMenu *menuFile;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *MainWindow)
    {
    MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
    actionQuit = new QAction(MainWindow);
    actionQuit->setObjectName(QString::fromUtf8("actionQuit"));
    actionOpen = new QAction(MainWindow);
    actionOpen->setObjectName(QString::fromUtf8("actionOpen"));
    actionAbout = new QAction(MainWindow);
    actionAbout->setObjectName(QString::fromUtf8("actionAbout"));
    centralwidget = new QWidget(MainWindow);
    centralwidget->setObjectName(QString::fromUtf8("centralwidget"));
    vboxLayout = new QVBoxLayout(centralwidget);
    vboxLayout->setSpacing(6);
    vboxLayout->setMargin(9);
    vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
    viewer = new MyQGLViewer(centralwidget);
    viewer->setObjectName(QString::fromUtf8("viewer"));

    vboxLayout->addWidget(viewer);

    hboxLayout = new QHBoxLayout();
    hboxLayout->setSpacing(6);
    hboxLayout->setMargin(0);
    hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
    checkBox = new QCheckBox(centralwidget);
    checkBox->setObjectName(QString::fromUtf8("checkBox"));

    hboxLayout->addWidget(checkBox);

    checkBox_2 = new QCheckBox(centralwidget);
    checkBox_2->setObjectName(QString::fromUtf8("checkBox_2"));

    hboxLayout->addWidget(checkBox_2);

    spacerItem = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

    hboxLayout->addItem(spacerItem);

    label = new QLabel(centralwidget);
    label->setObjectName(QString::fromUtf8("label"));

    hboxLayout->addWidget(label);

    radiusSpinBox = new QDoubleSpinBox(centralwidget);
    radiusSpinBox->setObjectName(QString::fromUtf8("radiusSpinBox"));
    radiusSpinBox->setMaximum(5);
    radiusSpinBox->setMinimum(0.1);
    radiusSpinBox->setSingleStep(0.5);

    hboxLayout->addWidget(radiusSpinBox);

    spacerItem1 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

    hboxLayout->addItem(spacerItem1);

    pushButton = new QPushButton(centralwidget);
    pushButton->setObjectName(QString::fromUtf8("pushButton"));

    hboxLayout->addWidget(pushButton);


    vboxLayout->addLayout(hboxLayout);

    MainWindow->setCentralWidget(centralwidget);
    menubar = new QMenuBar(MainWindow);
    menubar->setObjectName(QString::fromUtf8("menubar"));
    menubar->setGeometry(QRect(0, 0, 594, 29));
    menuHelp = new QMenu(menubar);
    menuHelp->setObjectName(QString::fromUtf8("menuHelp"));
    menuFile = new QMenu(menubar);
    menuFile->setObjectName(QString::fromUtf8("menuFile"));
    MainWindow->setMenuBar(menubar);
    statusbar = new QStatusBar(MainWindow);
    statusbar->setObjectName(QString::fromUtf8("statusbar"));
    statusbar->setGeometry(QRect(0, 406, 594, 22));
    MainWindow->setStatusBar(statusbar);

    menubar->addAction(menuFile->menuAction());
    menubar->addAction(menuHelp->menuAction());
    menuHelp->addAction(actionAbout);
    menuFile->addAction(actionOpen);
    menuFile->addAction(actionQuit);

    retranslateUi(MainWindow);

    QSize size(594, 428);
    size = size.expandedTo(MainWindow->minimumSizeHint());
    MainWindow->resize(size);

    QObject::connect(pushButton, SIGNAL(clicked()), MainWindow, SLOT(close()));
    QObject::connect(checkBox, SIGNAL(clicked(bool)), viewer, SLOT(setFPSIsDisplayed(bool)));
    QObject::connect(viewer, SIGNAL(FPSIsDisplayedChanged(bool)), checkBox, SLOT(setChecked(bool)));
    QObject::connect(checkBox_2, SIGNAL(clicked(bool)), viewer, SLOT(setGridIsDrawn(bool)));
    QObject::connect(viewer, SIGNAL(gridIsDrawnChanged(bool)), checkBox_2, SLOT(setChecked(bool)));

    QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
    MainWindow->setWindowTitle(QApplication::translate("MainWindow", "QGLViewer + Qt4", 0, QApplication::UnicodeUTF8));
    actionQuit->setText(QApplication::translate("MainWindow", "Quit", 0, QApplication::UnicodeUTF8));
    actionOpen->setText(QApplication::translate("MainWindow", "Open", 0, QApplication::UnicodeUTF8));
    actionAbout->setText(QApplication::translate("MainWindow", "About", 0, QApplication::UnicodeUTF8));
    checkBox->setText(QApplication::translate("MainWindow", "FPS", 0, QApplication::UnicodeUTF8));
    checkBox_2->setText(QApplication::translate("MainWindow", "Grid", 0, QApplication::UnicodeUTF8));
    label->setText(QApplication::translate("MainWindow", "Radius :", 0, QApplication::UnicodeUTF8));
    radiusSpinBox->setPrefix(QString());
    pushButton->setText(QApplication::translate("MainWindow", "Quit", 0, QApplication::UnicodeUTF8));
    menuHelp->setTitle(QApplication::translate("MainWindow", "Help", 0, QApplication::UnicodeUTF8));
    menuFile->setTitle(QApplication::translate("MainWindow", "File", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

#endif // UI_MY_GUI_FORM_H
