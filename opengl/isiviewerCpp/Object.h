#ifndef OBJECT_H_
#define OBJECT_H_

#include <ostream>
#include <vector>
#include "GLFunctor.h"

class Object
{
	std::vector<GLFunctor*> _transformations;
	
public:
	Object();
	Object(const Object& other);
	virtual ~Object();
	
public:
	virtual void dump(std::ostream& output) const = 0;
	
	virtual Object* clone() const = 0;
	
	void draw(bool smooth) const;

	void addTransformation(GLFunctor* glFunctor);
	void addTransformation(glFunction3f function, GLfloat a, GLfloat b, GLfloat c);
	void addTransformation(glFunction4f function, GLfloat a, GLfloat b, GLfloat c, GLfloat d);
	
protected:
	virtual void drawShape(bool smooth) const = 0;
};

std::ostream& operator << (std::ostream& output, const Object& object);

#endif /*OBJECT_H_*/
