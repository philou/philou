#include "GLFunctor.h"
#include <GL/gl.h>  // OpenGL include file

GLFunctor::GLFunctor()
{
}

GLFunctor::GLFunctor( const GLFunctor&)
{
}

GLFunctor::~GLFunctor()
{
}

GLFunctor3f::GLFunctor3f(glFunction3f function, GLfloat a, GLfloat b, GLfloat c)
	:_function(function), _a(a), _b(b), _c(c)
{
}

GLFunctor3f::GLFunctor3f(const GLFunctor3f& other)
	: GLFunctor(other)
	, _function(other._function), _a(other._a), _b(other._b), _c(other._c)
{
}

GLFunctor3f::~GLFunctor3f()
{
}
void GLFunctor3f::call() const
{
	_function(_a,_b,_c);
}

GLFunctor3f* GLFunctor3f::clone() const
{
	return new GLFunctor3f(*this);
}


GLFunctor4f::GLFunctor4f(glFunction4f function, GLfloat a, GLfloat b, GLfloat c, GLfloat d)
	:_function(function), _a(a), _b(b), _c(c), _d(d)
{
}

GLFunctor4f::GLFunctor4f(const GLFunctor4f& other)
	: GLFunctor(other)
	, _function(other._function), _a(other._a), _b(other._b), _c(other._c), _d(other._d)
{
}

GLFunctor4f::~GLFunctor4f()
{
}
void GLFunctor4f::call() const
{
	_function( _a, _b, _c, _d);
}

GLFunctor4f* GLFunctor4f::clone() const
{
	return new GLFunctor4f(*this);
}

ParametrizedRotator::ParametrizedRotator(GLfloat centerX, GLfloat centerY, GLfloat centerZ)
	: _angle(0)
	, _centerX(centerX)
	, _centerY(centerY)
	, _centerZ(centerZ)
{
}
ParametrizedRotator::ParametrizedRotator(const ParametrizedRotator& other)
	: GLFunctor(other)
	, _angle(other._angle)
	, _centerX(other._centerX)
	, _centerY(other._centerY)
	, _centerZ(other._centerZ)
{
}
ParametrizedRotator::~ParametrizedRotator()
{
}

void ParametrizedRotator::call() const
{
	glRotatef( _angle, _centerX, _centerY, _centerZ);
}
ParametrizedRotator* ParametrizedRotator::clone() const
{
	return new ParametrizedRotator(*this);
}
