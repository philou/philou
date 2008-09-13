/**
 * @file   my_objects.h
 * @author Bruno Jobard
 * @author Author1
 * @author Author2
 * @date   Oct 2006
 * 
 * @brief  Defines 3D objects
 * 
 * 
 */
#ifndef _ISI_MY_OBJECTS_H_
#define _ISI_MY_OBJECTS_H_

#include <glm/glm.h>  // for vec3 (and other vector) type and related
		      // geometric operations. Look at documentation
		      // file: glm-0.4.1/info/fr/news.html
#include "Triangle.h"
#include "Object.h"

class Mesh : public Object {
	std::vector<Triangle> _triangles;
public:
	Mesh();
	Mesh(const Mesh& other);
	virtual ~Mesh();

	virtual void dump(std::ostream& output) const;

	virtual Mesh* clone() const;

	void load(const char* file);
	
	virtual void drawShape(bool smooth) const;
};

#endif
