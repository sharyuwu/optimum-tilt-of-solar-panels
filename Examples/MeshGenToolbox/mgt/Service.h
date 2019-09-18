/*****************************************************************************/
/*  PMGT                                                                     */
/*                                                                           */
/*  January, 2007                                                            */
/*                                                                           */
/*  Copyright 2007                                                           */
/*  Wen Yu                                                                   */
/*  Department of Computing and Software                                     */
/*  McMaster University                                                      */
/*  yuw4@mcmaster.ca                                                         */
/*                                                                           */
/*  This program may be freely redistributed under the condition that the    */
/*    copyright notices are not removed. You may distribute modified versions*/
/*    of this code UNDER THE CONDITION THAT THIS CODE AND ANY MODIFICATIONS  */
/*    MADE TO IT IN THE SAME FILE REMAIN UNDER COPYRIGHT OF THE ORIGINAL     */
/*    AUTHOR, BOTH SOURCE AND OBJECT CODE ARE MADE FREELY AVAILABLE WITHOUT  */
/*    CHARGE, AND CLEAR NOTICE IS GIVEN OF THE MODIFICATIONS.                */
/*                                                                           */
/*  This program is used for correctness test of PMGT.                       */
/*  This file includes classes for the service module.                       */
/*****************************************************************************/

#ifndef SERVICE
#define SERVICE
#endif

#ifndef VERTEX
#include "Vertex.h"
#endif

#ifndef EDGE
#include "Edge.h"
#endif

#ifndef CELL
#include "Cell.h"
#endif

#ifndef MESH
#include "Mesh.h"
#endif

namespace PMGT{
class Service{
public:

	// auxiliary functions
	vector<int> boundary_list(Mesh& mesh);
	bool in_boundary(Mesh& mesh1, int vId1, Mesh& mesh2, vector<int> b_list2);
	double area(Mesh& mesh, int vId1, int vId2, int vId3);
	bool inside(Mesh& mesh, int vId, int cId);
	bool across(Mesh& mesh, int eId1, int eId2);


	// functions test correctness
	inline bool euler(Mesh& mesh){
		return (mesh.n_of_v() +
				mesh.n_of_c() -
				mesh.n_of_e() == 1);
	}
	bool length_gt0(Mesh& mesh);
	bool areas_gt0(Mesh& mesh);
	bool no_interior_intersection(Mesh& mesh);
	bool conformal(Mesh& mesh);
	bool bounded(Mesh& mesh);
	bool counterclockwise(Mesh& mesh);

	// functions in the service module
	inline bool is_valid_mesh(Mesh& mesh){
		return areas_gt0(mesh) && bounded(mesh) &&
		conformal(mesh) && no_interior_intersection(mesh) &&
		length_gt0(mesh) && counterclockwise(mesh) &&
		euler(mesh);

	}
	bool covering_up(Mesh& mesh1, Mesh& mesh2);


};
}

