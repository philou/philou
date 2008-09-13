#ifndef TRIANGLE_H_
#define TRIANGLE_H_

#include <glm/glm.h>
#include <ostream> 

class Triangle
{
public:
	glm::vec3 _vertices[3];
	glm::vec3 _normal;
 	glm::vec3 _normals[3];
	
	Triangle();
	Triangle(const Triangle& other);
	Triangle& operator=(const Triangle& other);
	virtual ~Triangle();

	void init(const glm::vec3& a, const glm::vec3& b, const glm::vec3& c);
};

std::ostream& operator << (std::ostream& output, const Triangle& triangle);

#endif /*TRIANGLE_H_*/
