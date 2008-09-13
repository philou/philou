/**
 * @file   objects.h
 * @author Bruno Jobard <bruno.jobard@univ-pau.fr>
 * @author Binome1 <binome1@son.adresse.fr>
 * @author Binome2 <binome2@son.adresse.fr>
 * @date   Sep 2005
 * 
 * @brief  Definitions of the 3D objects that will be displayed in the scene
 * 
 * 
 */

#ifndef _OBJECTS_H_
#define _OBJECTS_H_

#include <GL/glut.h>

/*--- Vertex ---*/
struct StructVertex {
  GLfloat coords[3];
};
typedef struct StructVertex Vertex;

void dumpVertex( const char* message, const Vertex* v);

void nullVertex(Vertex* vertex);
void initVertex(Vertex* vertex, GLfloat x, GLfloat y, GLfloat z);
void copyVertex(Vertex* vertex, const Vertex* v);
void freeVertex(Vertex* vertex);

void add(Vertex* result, const Vertex* a, const Vertex* b);
void substract(Vertex* result, const Vertex* a, const Vertex* b);
void produitVectoriel(Vertex* result, const Vertex* a, const Vertex* b);
GLfloat length(const Vertex* v);
void scale(Vertex* result, const Vertex* v, GLfloat factor);
void normalize(Vertex* result, const Vertex* v);

struct StructTriangle {
  Vertex vertices[3];
  Vertex normal;
  Vertex normals[3];
};
typedef struct StructTriangle Triangle;

void dumpTriangle(const Triangle* triangle);

void initTriangle(Triangle* triangle, const Vertex* a, const Vertex* b, const Vertex* c);
void freeTriangle(Triangle* triangle);

void triangleNormal( Vertex* result, const Triangle* triangle);

/*--- Mesh ---*/

/// Mesh structure

struct StructMesh {
  size_t triangleCount;
  Triangle* triangles;
};
typedef struct StructMesh Mesh;

void dumpMesh(const Mesh* mesh);

void initFileMesh(Mesh* mesh, const char* file);
void freeMesh(Mesh* mesh);
void drawMesh(const Mesh* mesh, int smooth);

#endif

