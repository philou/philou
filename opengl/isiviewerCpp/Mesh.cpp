#include "Mesh.h"

#include <iostream>
#include <algorithm>
#include <iterator>
#include <GL/gl.h>  // OpenGL include file
#include <iostream>
#include "operators.h"

Mesh::Mesh()
{
}

Mesh::Mesh(const Mesh& other)
	: Object(other)
	, _triangles(other._triangles)
{
}

Mesh::~Mesh()
{
}

struct IndexTriangle
{
	size_t indexes[3];
};

void Mesh::load(const char* file)
{
  FILE* stream = fopen(file, "r");
  if (NULL == stream)
  {
  	std::cout << "file " << file << " could not be read ..." << std::endl;
    return;
  }
  fscanf(stream, "OFF\n");

  // lecture de la taille du mesh
  size_t verticesCount = 0;
  size_t trianglesCount = 0;
  int placeholder = 0;
  fscanf(stream, "%d %d %d\n", &verticesCount, &trianglesCount, &placeholder);
  fscanf(stream, "\n");

  // lecture des vertexes
  std::vector<glm::vec3> vertices(verticesCount);
  for (size_t vertex_idx=0; vertex_idx < verticesCount; vertex_idx++ )
    fscanf(stream, "%f %f %f\n", &vertices[vertex_idx].x
                               , &vertices[vertex_idx].y
                               , &vertices[vertex_idx].z);

  // lecture des triangles
  std::vector<IndexTriangle> trianglesVerticesIndexes(trianglesCount);
  for (size_t triangle_idx=0; triangle_idx < trianglesCount; triangle_idx++)
    fscanf(stream, "3 %d %d %d\n", &trianglesVerticesIndexes[triangle_idx].indexes[0]
                                 , &trianglesVerticesIndexes[triangle_idx].indexes[1]
                                 , &trianglesVerticesIndexes[triangle_idx].indexes[2]);

  // construction des triangles du mesh  
  _triangles.resize(trianglesCount);
  for (size_t triangle_idx=0; triangle_idx < trianglesCount; triangle_idx++)
  	_triangles[triangle_idx].init
  		( vertices[trianglesVerticesIndexes[triangle_idx].indexes[0]]
	    , vertices[trianglesVerticesIndexes[triangle_idx].indexes[1]]
        , vertices[trianglesVerticesIndexes[triangle_idx].indexes[2]]
        );

  // somme des normales en chaque vertex
  std::vector<glm::vec3> normals(verticesCount);
  for (size_t triangle_idx=0; triangle_idx < trianglesCount; triangle_idx++)
  	for (size_t vertice_idx = 0; vertice_idx < 3; vertice_idx++)
	  	normals[trianglesVerticesIndexes[triangle_idx].indexes[vertice_idx]] += _triangles[triangle_idx]._normal;

  // normalisation
  for (size_t j=0; j < verticesCount; j++)
    normals[j] = normalize(normals[j]);

  // copie des normales dans les triangles du mesh
  for (size_t triangle_idx=0; triangle_idx < trianglesCount; triangle_idx++)
  	for (size_t vertice_idx = 0; vertice_idx < 3; vertice_idx++)
  	  _triangles[triangle_idx]._normals[vertice_idx] = normals[trianglesVerticesIndexes[triangle_idx].indexes[vertice_idx]];
  	  
  //std::cout << *this << std::endl;
};

void Mesh::drawShape(bool smooth) const
{
	for ( std::vector<Triangle>::const_iterator t = _triangles.begin(); t != _triangles.end(); t++)
	{
		glBegin(GL_TRIANGLES);
		for (size_t i = 0; i < 3; i++)
		{
			if (smooth)
				glNormal3fv((*t)._normals[i]);
			else
				glNormal3fv((*t)._normal);
			
			glVertex3fv((*t)._vertices[i]);
		}
		glEnd();
	}
}

Mesh* Mesh::clone() const
{
	return new Mesh(*this);
}

void Mesh::dump(std::ostream& output) const
{
	output << "==== Starting to dump mesh ====" << std::endl;
	
	output << " mesh of " << _triangles.size() << " triangles" << std::endl;
	
	size_t i = 0;
	for ( std::vector<Triangle>::const_iterator t = _triangles.begin(); t != _triangles.end(); t++)
  	{
    	output << "triangle " << i << std::endl;
    	output << *t;

    	++i;
  	}

	output << "==== Finished to dump mesh ====" << std::endl;
}

