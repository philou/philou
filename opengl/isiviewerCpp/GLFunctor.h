#ifndef GLFUNCTOR_H_
#define GLFUNCTOR_H_

#include <GL/gl.h>  // OpenGL include file

class GLFunctor
{
public:
	GLFunctor();
	GLFunctor( const GLFunctor& other);
	virtual ~GLFunctor();
	
	virtual void call() const = 0;
	virtual GLFunctor* clone() const = 0;
};

typedef void (*glFunction3f)(GLfloat,GLfloat,GLfloat);
class GLFunctor3f : public GLFunctor
{
	glFunction3f _function;
	GLfloat _a;
	GLfloat _b;
	GLfloat _c;

public:
	GLFunctor3f(glFunction3f function, GLfloat a, GLfloat b, GLfloat c);
	GLFunctor3f(const GLFunctor3f& other);
	virtual ~GLFunctor3f();
	virtual void call() const;
	virtual GLFunctor3f* clone() const;
};

typedef void (*glFunction4f)(GLfloat,GLfloat,GLfloat,GLfloat);
class GLFunctor4f : public GLFunctor
{
	glFunction4f _function;
	GLfloat _a;
	GLfloat _b;
	GLfloat _c;
	GLfloat _d;

public:
	GLFunctor4f(glFunction4f function, GLfloat a, GLfloat b, GLfloat c, GLfloat d);
	GLFunctor4f(const GLFunctor4f& other);
	virtual ~GLFunctor4f();
	virtual void call() const;
	virtual GLFunctor4f* clone() const;
};

class ParametrizedRotator : public GLFunctor
{
	GLfloat _angle;
	GLfloat _centerX;
	GLfloat _centerY;
	GLfloat _centerZ;

public:
	ParametrizedRotator(GLfloat centerX, GLfloat centerY, GLfloat centerZ);
	ParametrizedRotator(const ParametrizedRotator& other);
	virtual ~ParametrizedRotator();
	virtual void call() const;
	virtual ParametrizedRotator* clone() const;
	
	GLfloat angle() const { return _angle; }
	void setAngle(GLfloat angle) { _angle = angle; }
};


#endif /*GLFUNCTOR_H_*/
