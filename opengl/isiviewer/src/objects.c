/**
 * @file   objects.c
 * @author Bruno Jobard <bruno.jobard@univ-pau.fr>
 * @author Binome1 <binome1@son.adresse.fr>
 * @author Binome2 <binome2@son.adresse.fr>
 * @date   Sep 2005
 * 
 * @brief  Implementation of the 3D objects
 * 
 * 
 */

#include "objects.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void nullVertex(Vertex* vertex)
{
  vertex->coords[0] = 0;
  vertex->coords[1] = 0;
  vertex->coords[2] = 0;
}

void initVertex(Vertex* vertex, GLfloat x, GLfloat y, GLfloat z)
{
  vertex->coords[0] = x;
  vertex->coords[1] = y;
  vertex->coords[2] = z;
}

void copyVertex(Vertex* vertex, const Vertex* v)
{
  initVertex(vertex, v->coords[0], v->coords[1], v->coords[2]);
}

void freeVertex(Vertex* vertex)
{
  // nop
}

void add(Vertex* result, const Vertex* a, const Vertex* b)
{
  initVertex( result, a->coords[0] + b->coords[0]
	            , a->coords[1] + b->coords[1]
                    , a->coords[2] + b->coords[2]);
}

void substract(Vertex* result, const Vertex* a, const Vertex* b)
{
  initVertex( result, a->coords[0] - b->coords[0]
	            , a->coords[1] - b->coords[1]
                    , a->coords[2] - b->coords[2]);
}

void produitVectoriel(Vertex* result, const Vertex* a, const Vertex* b)
{
  initVertex( result, a->coords[1]*b->coords[2] - a->coords[2]*b->coords[1]
	            , a->coords[0]*b->coords[2] - a->coords[2]*b->coords[0]
                    , a->coords[0]*b->coords[1] - a->coords[1]*b->coords[0]);
}

GLfloat length(const Vertex* v)
{
  return sqrt( v->coords[0]*v->coords[0] + v->coords[1]*v->coords[1] + v->coords[2]*v->coords[2]);
}

void scale(Vertex* result, const Vertex* v, GLfloat factor)
{
  initVertex( result, v->coords[0]*factor
	            , v->coords[1]*factor
                    , v->coords[2]*factor);
}

void normalize(Vertex* result, const Vertex* v)
{
  GLfloat lgth = length(v);
  if ( lgth == 0 )
  {
    nullVertex(result);
  }
  else
  {
    scale(result, v, 1/lgth);
  }
}

void dumpTriangle(const Triangle* triangle)
{
  // for each vertex index for the current triangle
  int i = 0;
  for(i=0; i<3; ++i) {
    
    dumpVertex("vertex", &triangle->vertices[i]);

    dumpVertex("normal", &triangle->normals[i]);
  } // end for each vertex

  dumpVertex("mean normal", &triangle->normal);
}


void initTriangle(Triangle* triangle, const Vertex* a, const Vertex* b, const Vertex* c)
{
  triangle->vertices[0] = *a;
  triangle->vertices[1] = *b;
  triangle->vertices[2] = *c;

  triangleNormal( &triangle->normal, triangle);

  copyVertex(&triangle->normals[0], &triangle->normal);
  copyVertex(&triangle->normals[1], &triangle->normal);
  copyVertex(&triangle->normals[2], &triangle->normal);
}
void freeTriangle(Triangle* triangle)
{
  //nop
}

void triangleNormal( Vertex* result, const Triangle* triangle)
{
  Vertex a, b, c;
  substract(&a, &triangle->vertices[0], &triangle->vertices[1]);
  substract(&b, &triangle->vertices[0], &triangle->vertices[2]);

  produitVectoriel(&c, &a, &b);

  normalize(result, &c);
}

/*--- Mesh ---*/

void dumpVertex( const char* message, const Vertex* v)
{
  printf("dumping vertex %s : %f; %f; %f \n", message, v->coords[0], v->coords[1], v->coords[2]);
}

void initFileMesh(Mesh* mesh, const char* file)
{
  FILE* stream = fopen(file, "r");
  if (NULL == stream)
  {
    mesh->triangleCount = 0;
  }
  fscanf(stream, "OFF\n");

  // lecture de la taille du mesh
  int verticesCount = 0;
  int placeholder = 0;
  fscanf(stream, "%d %d %d\n", &verticesCount, &mesh->triangleCount, &placeholder);
  fscanf(stream, "\n");
  
  // lecture des vertexes
  Vertex* vertices = (Vertex*)malloc(sizeof(Vertex) * verticesCount);
  int vertex_idx = 0;
  for (vertex_idx=0; vertex_idx < verticesCount; vertex_idx++ )
  {
    float x, y, z;
    fscanf(stream, "%f %f %f\n", &x, &y, &z);
    initVertex(&vertices[vertex_idx], x, y, z);
  }

  // allocation de la table des triangles
  int* trianglesVerticesData = (int*)malloc(sizeof(int) * 3 * mesh->triangleCount);
  int** trianglesVertices = (int**)malloc(sizeof(int) * mesh->triangleCount);
  int i = 0;
  for (i=0; i < mesh->triangleCount; i++)
  {
    trianglesVertices[i] = trianglesVerticesData + 3*i;
  }
  
  // lectures des triangles
  int triangle_idx = 0;
  for (triangle_idx=0; triangle_idx < mesh->triangleCount; triangle_idx++)
  {
    int v1, v2, v3;
    fscanf(stream, "3 %d %d %d\n", &v1, &v2, &v3);

    trianglesVertices[triangle_idx][0] = v1;
    trianglesVertices[triangle_idx][1] = v2;
    trianglesVertices[triangle_idx][2] = v3;
  }
  
  // allocation et construction des triangles du mesh
  mesh->triangles = (Triangle*)malloc(sizeof(Triangle)*mesh->triangleCount);
  for (triangle_idx=0; triangle_idx < mesh->triangleCount; triangle_idx++)
  {
    initTriangle(&mesh->triangles[triangle_idx], &vertices[trianglesVertices[triangle_idx][0]]
	                                       , &vertices[trianglesVertices[triangle_idx][1]]
                                               , &vertices[trianglesVertices[triangle_idx][2]]);
  }
  
  // allocations des tables des normales moyennes
  Vertex* normals = (Vertex*)malloc(sizeof(Vertex) * verticesCount);

  int j = 0;
  for (j=0; j < verticesCount; j++)
  {
    nullVertex(&normals[j]);
  }  
  
  // somme des normales en chaque vertex
  for (triangle_idx=0; triangle_idx < mesh->triangleCount; triangle_idx++)
  {
    int corner = 0;
    for (corner=0; corner < 3; corner ++ )
    {
      add(&normals[trianglesVertices[triangle_idx][corner]]
	  ,&normals[trianglesVertices[triangle_idx][corner]]
	  ,&mesh->triangles[triangle_idx].normal);
    }
  }

  // normalisation
  for (j=0; j < verticesCount; j++)
  {
    normalize(&normals[j], &normals[j]);
  }  

  // copie des normales dans les triangles du mesh
  for (triangle_idx=0; triangle_idx < mesh->triangleCount; triangle_idx++)
  {
    int corner = 0;
    for (corner=0; corner < 3; corner ++ )
    {
      copyVertex(&mesh->triangles[triangle_idx].normals[corner], &normals[trianglesVertices[triangle_idx][corner]]);
    }
  }

  free(normals);
  free(trianglesVertices);
  free(trianglesVerticesData);
  free(vertices);
  fclose(stream);

  //dumpMesh(mesh);
}


void freeMesh(Mesh* mesh)
{
  free(mesh->triangles);
}

void dumpMesh(const Mesh* mesh)
{
  int t = 0;
  for(t=0; t<mesh->triangleCount; ++t)
  {

    printf("triangle %d\n", t);
    dumpTriangle(&mesh->triangles[t]);
  }
}


void drawMesh(const Mesh* mesh, int smooth)
{
  //static int value = 0;

  // for each triangle
  int t;
  for(t=0; t<mesh->triangleCount; ++t) {
    
    // start drawing triangles
    glBegin(GL_TRIANGLES);

    //int count = mesh->triangleCount;
    //glColor3f( (value%count)/count, ((value+count/3))%count/(float)count, ((value+count/3))%count/(float)count);
    //value++;

    // for each vertex index for the current triangle
    int i;
    for(i=0; i<3; ++i)
    {
      // send normals to OpenGL
      if ( smooth )
      {
	glNormal3fv(mesh->triangles[t].normals[i].coords);
      }
      else
      {
	glNormal3fv(mesh->triangles[t].normal.coords);
      }
    
      // send vertices components to OpenGL
      glVertex3fv(mesh->triangles[t].vertices[i].coords);
    } // end for each vertex
    
    // stop drawing triangles
    glEnd();
  
  } // end for each triangle
}

