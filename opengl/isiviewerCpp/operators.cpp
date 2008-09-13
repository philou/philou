#include "operators.h"
#include <math.h>

glm::vec3 produitVectoriel(const glm::vec3& u, const glm::vec3& v)
{
	return glm::vec3( u.y*v.z - u.z*v.y
	                , u.x*v.z - u.z*v.x
                    , u.x*v.y - u.y*v.x);
}

float length(const glm::vec3& v)
{
	if ( v.x == 0 && v.y ==0 && v.z == 0)
		return 0;
	
	return sqrt( v.x*v.x + v.y*v.y + v.z*v.z);
}

glm::vec3 normalized(const glm::vec3& v)
{
	if (length(v) == 0)
		return v;
	
	return v / length(v);
}

std::ostream& operator << (std::ostream& output, const glm::vec3& v)
{
	output << "(x=" << v.x << ",y=" << v.y << ",z=" << v.z << ")";
	return output;
}

