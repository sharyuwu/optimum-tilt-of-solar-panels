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
/*  This is the test case TC5.                                               */
/*  This test case is design for test correctness of PMGT.                   */
/*****************************************************************************/
#include <iostream>

#ifndef SERVICE
#include "Service.h"
#endif

#ifndef COARSENING
#include "Coarsening.h"
#endif

#ifndef REFINING
#include "Refining.h"
#endif

#ifndef OUTPUT
#include "Output.h"
#endif

#ifndef INPUT
#include "Input.h"
#endif

#ifndef MESH
#include "Mesh.h"
#endif

using namespace PMGT;
int main(){

	Mesh mesh;
	Input input;
	Refining ref;
	Output output;
	
	input.read_file(mesh, "oldVertices.dat", "oldCells.dat");


	double x = 0.4;
	int count = 0;
	char vfile[32];
	char* vp = vfile;
	char cfile[32];
	char* cp = cfile;
	char number[8];
	while(1){

		// refine
		count++;
		sprintf(number, "%i", count);

		vp = strcpy(vp, "newVertices");
		vp = strcat(vp, number);
		vp = strcat(vp,".dat");

		cp = strcpy(cp, "newCells");
		cp = strcat(cp, number);
		cp = strcat(cp,".dat");

		int n = mesh.n_of_c();
		vector<int> vIds;
		Point zero; 
		Vertex vtx[3];
		
		zero.clear();
		zero.push_back(0);
		zero.push_back(0);

		for (int i=0; i<n; i++){
			vIds.clear();
			vIds = mesh.vIds_from_cId(i);
			for (int j=0; j<3; j++){
				vtx[j] = mesh.vertex(vIds[j]);
			}

			double first = vtx[0].distance(zero)-0.7;
			double second = vtx[1].distance(zero)-0.7;
			if (first*second<DBL_EPSILON){
				mesh.cell(i).setMflag(REFINE);
			}
			else{
				double third = vtx[2].distance(zero)-0.7;
				if (first*third < DBL_EPSILON){
					mesh.cell(i).setMflag(REFINE);
				}
			}
		}

		ref.longest_edge(mesh);
		output.write_file(mesh,vp, cp);

		if (count==15){
			break;
		}
	}

	cout << "There are " << count << " meshes generated." << endl;
	
	return 0;
}
