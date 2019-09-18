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
/*  This program used for correctness test of PMGT.                          */
/*****************************************************************************/

#include "Service.h"

namespace PMGT{

// auxiliary functions
vector<int> Service::boundary_list(Mesh& mesh){
	vector<int> list;
	vector<int> b_edges;
	vector<int>::iterator tempIterator;

	for(int i=0; i<mesh.n_of_e(); i++){
		if (mesh.is_b_e(i)){
			b_edges.push_back(i);
		}
	}
	vector<int> vIds;
	vIds = mesh.vIds_from_eId(b_edges[0]);
	list.push_back(vIds[0]);
	//list.push_back(vIds[1]);
	int first_vId = vIds[0];
	int current = vIds[1];
	b_edges[0] = b_edges[b_edges.size()-1];
	b_edges.pop_back();
	bool error = false, found;

	int next_vertex, i;
	while(b_edges.size()!=0){
		found = false;
		for( i=0; i<b_edges.size(); i++){
			vIds = mesh.vIds_from_eId(b_edges[i]);
			for(int j=0; j<2; j++){
				if(vIds[j] == current){ // find next boundary edge
					list.push_back(vIds[j]);
					b_edges[i] = b_edges[b_edges.size()-1];
					b_edges.pop_back();
					current = vIds[(j+1)%2];
					found = true;
					break;
				}
			}
			if(found) break;
		}
		if (!found){ // the boundary is not close
			error = true;
			break;
		}
		else if(first_vId==current){// find a close boundary
			if(b_edges.size()!=0){ // there are boundary edges left
				error = true;
			}
			break; //find an unique close boundary
		}
	}

	if (error){
		list.clear();
	}
	return list;
}


bool Service::in_boundary(Mesh& mesh1, int vId1, Mesh& mesh2, vector<int> b_list2){
	double x = mesh1.vertex(vId1).x();
	double y = mesh1.vertex(vId1).x();
	for (int i=0; i<b_list2.size()-2; i++){
		double xa = mesh2.vertex(b_list2[i]).x();
		double ya = mesh2.vertex(b_list2[i]).y();
		double xb = mesh2.vertex(b_list2[i+1]).x();
		double yb = mesh2.vertex(b_list2[i+1]).y();
		double xc = mesh2.vertex(b_list2[i+2]).x();
		double yc = mesh2.vertex(b_list2[i+2]).y();

		double detAB = (y-ya)*(xb-xa) - (x-xa)*(yb-ya);
		double detBC = (y-yb)*(xc-xb) - (x-xb)*(yc-yb);

		if (detAB*detBC < 0) return false;
	}
	return true;
}


double Service::area(Mesh& mesh, int vId1, int vId2, int vId3){
	double x1 = mesh.vertex(vId1).x();
	double y1 = mesh.vertex(vId1).y();
	double x2 = mesh.vertex(vId2).x();
	double y2 = mesh.vertex(vId2).y();
	double x3 = mesh.vertex(vId3).x();
	double y3 = mesh.vertex(vId3).y();

	return abs((x1*y2-x2*y1+x1*y3-x3*y1+x2*y3-x3*y2)/2.0);
}


bool Service::inside(Mesh& mesh, int vId, int cId){
	double x = mesh.vertex(vId).x();
	double y = mesh.vertex(vId).y();

	vector<int> vIds = mesh.vIds_from_cId(cId);
	double x1 = mesh.vertex(vIds[0]).x();
	double y1 = mesh.vertex(vIds[0]).y();
	double x2 = mesh.vertex(vIds[1]).x();
	double y2 = mesh.vertex(vIds[1]).y();
	double x3 = mesh.vertex(vIds[2]).x();
	double y3 = mesh.vertex(vIds[2]).y();

	double det12 = (y-y1)*(x2-x1) - (x-x1)*(y2-y1);
	double det23 = (y-y2)*(x3-x2) - (x-x2)*(y3-y2);
	double det31 = (y-y3)*(x1-x3) - (x-x3)*(y1-y3);
	//(x,y) in the same side of 12, 23, 31
	return (det12*det23>0)&&(det23*det31>0);

}

// http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d
bool Service::across(Mesh& mesh, int eId1, int eId2){

	vector<int> vIds1 = mesh.vIds_from_eId(eId1);
	double x1,y1,x2,y2,x3,y3,x4,y4;
	x1 = mesh.vertex(vIds1[0]).x();
	y1 = mesh.vertex(vIds1[0]).y();
	x2 = mesh.vertex(vIds1[1]).x();
	y2 = mesh.vertex(vIds1[1]).y();

	vector<int> vIds2 = mesh.vIds_from_eId(eId2);
	x3 = mesh.vertex(vIds2[0]).x();
	y3 = mesh.vertex(vIds2[0]).y();
	x4 = mesh.vertex(vIds2[1]).x();
	y4 = mesh.vertex(vIds2[1]).y();

	double det312 = (y3-y1)*(x2-x1) - (x3-x1)*(y2-y1);
	double det412 = (y4-y1)*(x2-x1) - (x4-x1)*(y2-y1);

	double det134 = (y1-y3)*(x4-x3) - (x1-x3)*(y4-y3);
	double det234 = (y2-y3)*(x4-x3) - (x2-x3)*(y4-y3);


	// 3 and 4 in the different side of 12 and
	// 1 and 2 in the different side of 34
	return (det312*det412<0 && det134*det234<0);
}


// functions test correctness
bool Service::length_gt0(Mesh& mesh){
	for (int i=0; i<mesh.n_of_e(); i++){
		if (mesh.dist(mesh.hId_from_eId(i,0)) < 0){
			cout << "length_gt0 is false." << endl;
			return false;
		}
	}
	return true;
}

bool Service::areas_gt0(Mesh& mesh){
	for (int i=0; i<mesh.n_of_c(); i++){
		vector<int> vIds = mesh.vIds_from_cId(i);
		if (area(mesh, vIds[0], vIds[1], vIds[2]) <= 0){
			cout << "areas_gt0 is false." << endl;
			return false;
		}
	}
	return true;
}

bool Service::bounded(Mesh& mesh){
	return (boundary_list(mesh).size()!=0);
}


bool Service::no_interior_intersection(Mesh& mesh){
	for (int i=0; i<mesh.n_of_v(); i++){
		for (int j=0; j<mesh.n_of_c(); j++){
			if (inside(mesh, i, j)){
				cout << "no_interior_intersection is false." << endl;
				return false;
			}
		}
	}

	for (int i=0; i<mesh.n_of_e(); i++){
		for (int j=0; j<mesh.n_of_e(); j++){
			if (across(mesh, i, j)){
				cout << "no_interior_intersection is false." << endl;
				return false;
			}
		}
	}
	return true;
}


bool Service::conformal(Mesh& mesh){
	for (int i=0; i<mesh.n_of_v(); i++){
		for (int j=0; j<mesh.n_of_e(); j++){
			vector<int> vIds = mesh.vIds_from_eId(j);
			double x = mesh.vertex(i).x();
			double y = mesh.vertex(i).y();
			double x1 = mesh.vertex(vIds[0]).x();
			double y1 = mesh.vertex(vIds[0]).y();
			double x2 = mesh.vertex(vIds[1]).x();
			double y2 = mesh.vertex(vIds[1]).y();
			if((x<x1&&y<y1 && x>x2&&y>y2) || (x>x1&&y>y1 && x<x2&&y<y2)){
				if (area(mesh,i,vIds[0],vIds[1])<10E-10){
					cout << "conformal is false." << endl;
					return false;
				}
			}
		}
	}
	return true;

}


bool Service::counterclockwise(Mesh& mesh){
	for (int i=0; i<mesh.n_of_c(); i++){
		vector<int> vIds = mesh.vIds_from_cId(i);
		Vertex v1 = mesh.vertex(vIds[0]);
		Vertex v2 = mesh.vertex(vIds[1]);
		Vertex v3 = mesh.vertex(vIds[2]);
		double detleft = (v1.x()-v3.x())*(v2.y()-v3.y());
		double detright = (v1.y()-v3.y())*(v2.x()-v3.x());
		//return detleft - detright > DBL_EPSILON*100;
		if ( detleft < detright + DBL_EPSILON*100){
			cout << "counterclockwise is false."<< endl;
			return false;
		}
	}
	return true;
}

bool Service::covering_up(Mesh& mesh1, Mesh& mesh2){
	vector<int> b_list = boundary_list(mesh1);
	for (int i=0; i<mesh2.n_of_v(); i++){
		if (!in_boundary(mesh2, i, mesh1, b_list)){
			return false;
		}
	}
	b_list = boundary_list(mesh2);
	for (int i=0; i<mesh1.n_of_v(); i++){
		if (!in_boundary(mesh1, i, mesh2, b_list)){
			return false;
		}
	}

	return true;
}


}

