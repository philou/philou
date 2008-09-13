#include "Triangle.h"
#include "operators.h"

Triangle::Triangle()
{
}

Triangle::Triangle(const Triangle& other)
{
	operator=(other);
}
Triangle& Triangle::operator=(const Triangle& other)
{
	for (size_t i = 0; i < sizeof(_vertices)/sizeof(glm::vec3); ++i)
		_vertices[i] = other._vertices[i];

	_normal = other._normal;

	for (size_t i = 0; i < sizeof(_normals)/sizeof(glm::vec3); ++i)
		_normals[i] = other._normals[i];
		
	return *this;
}

Triangle::~Triangle()
{
}

void Triangle::init(const glm::vec3& a, const glm::vec3& b, const glm::vec3& c)
{
	_vertices[0] = a;
	_vertices[1] = b;
	_vertices[2] = c;
	
	//_normal = normalize(produitVectoriel(a-b, a-c));
	_normal = produitVectoriel(a-b, a-c);
}

std::ostream& operator << (std::ostream& output, const Triangle& triangle)
{
	for (size_t i = 0; i < 3; i++)
	{
  		output << "vertex" << triangle._vertices[i] << std::endl;
  		output << "normal" << triangle._normals[i] << std::endl;
	}
  	output << "mean normal" << triangle._normal << std::endl;
  	return output;
}
