#ifndef OPERATORS_H_
#define OPERATORS_H_

#include <glm/glm.h>
#include <ostream>
#include <vector>

// glm::vec3 helper operators

glm::vec3 produitVectoriel(const glm::vec3& u, const glm::vec3& v);

float length(const glm::vec3& v);

glm::vec3 normalized(const glm::vec3& v);

std::ostream& operator << (std::ostream& output, const glm::vec3& v);

/*template<typename VectorType> VectorType cloneVector(const VectorType& original)
{
	VectorType result;
	for (VectorType::const_iterator it = original.begin(); it != original.end(); ++it)
	{
		result.push_back((*it)->clone());
	}
	return result;
}*/

#endif /*OPERATORS_H_*/
