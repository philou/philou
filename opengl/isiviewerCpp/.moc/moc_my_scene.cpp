/****************************************************************************
** Meta object code from reading C++ file 'my_scene.h'
**
** Created: Sun Feb 25 08:18:20 2007
**      by: The Qt Meta Object Compiler version 59 (Qt 4.2.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../my_scene.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'my_scene.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.2.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

static const uint qt_meta_data_MyScene[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      12,    9,    8,    8, 0x05,
      44,   41,    8,    8, 0x05,
      78,   71,    8,    8, 0x05,

 // slots: signature, parameters, type, tag, flags
     103,    9,    8,    8, 0x0a,
     129,   41,    8,    8, 0x0a,
     153,   71,    8,    8, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MyScene[] = {
    "MyScene\0\0co\0sigCurrentObjectChanged(int)\0dm\0"
    "sigDisplayModeChanged(int)\0radius\0sigRadiusChanged(double)\0"
    "slotSetCurrentObject(int)\0slotSetDisplayMode(int)\0"
    "slotSetRadius(double)\0"
};

const QMetaObject MyScene::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MyScene,
      qt_meta_data_MyScene, 0 }
};

const QMetaObject *MyScene::metaObject() const
{
    return &staticMetaObject;
}

void *MyScene::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MyScene))
	return static_cast<void*>(const_cast<MyScene*>(this));
    return QObject::qt_metacast(_clname);
}

int MyScene::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: sigCurrentObjectChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: sigDisplayModeChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: sigRadiusChanged((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 3: slotSetCurrentObject((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: slotSetDisplayMode((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: slotSetRadius((*reinterpret_cast< double(*)>(_a[1]))); break;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MyScene::sigCurrentObjectChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MyScene::sigDisplayModeChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MyScene::sigRadiusChanged(double _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
