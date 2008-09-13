#include "Object.h"
#include <functional>

Object::Object()
{
}

Object::Object(const Object& other)
	: _transformations(other._transformations.size())
{
	std::transform( other._transformations.begin(), other._transformations.end(), _transformations.begin(), std::mem_fun(&GLFunctor::clone));
}

Object::~Object()
{
	for (std::vector<GLFunctor*>::iterator it = _transformations.begin(); it != _transformations.end(); ++it)
	{
		delete (*it);
	}
}

void Object::draw(bool smooth) const
{
	glPushMatrix();
	
	for (std::vector<GLFunctor*>::const_iterator it = _transformations.begin(); it != _transformations.end(); ++it)
	{
		(*it)->call();
	}

	drawShape(smooth);
	
	glPopMatrix();
}

void Object::addTransformation(GLFunctor* glFunctor)
{
	_transformations.push_back(glFunctor);
}

void Object::addTransformation(glFunction3f function, GLfloat a, GLfloat b, GLfloat c)
{
	addTransformation(new GLFunctor3f(function,a,b,c));
}

void Object::addTransformation(glFunction4f function, GLfloat a, GLfloat b, GLfloat c, GLfloat d)
{
	addTransformation(new GLFunctor4f(function,a,b,c,d));
}



std::ostream& operator << (std::ostream& output, const Object& object)
{
	object.dump(output);
	return output;
}
