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
/*  This is the serial version of PMGT                                       */
/*****************************************************************************/
#ifndef REFINING
#include "Refining.h"
#endif

namespace PMGT{
	// split cells at their centroid points
	void Refining::split(Mesh& mesh){
		vector<int> vIds;
		vector<int> newIds;
		int i,j, newvId;
		Point p;

		for (i=0; i<mesh.n_of_c(); i++){
			if (mesh.cell(i).mFlag() == REFINE){
				// get three vertices
				vIds.clear();
				vIds.push_back(mesh.cv_first(i));
				vIds.push_back(mesh.cv_next(i, vIds[0]));
				vIds.push_back(mesh.cv_next(i, vIds[1]));
				// add the centroid point
				Point np = mesh.centroid(i);
				mesh.add_vertex(np);
				newvId = mesh.find_vertex(np);
				assert(newvId !=-1);
				// add three new edges
				for (j=0; j<3; j++){
					mesh.add_edge(newvId, vIds[j]);
				}
				// add/update three cells
				for (j=0; j<3; j++){
					newIds.clear();
					newIds.push_back(newvId);
					newIds.push_back(vIds[j]);
					newIds.push_back(vIds[(j+1)%3]);
					if (j==0){
						mesh.update_cell(newIds, i);
					}
					else
						mesh.add_cell(newIds);
				}
			}// if
		}

	}



	void Refining::longest_edge(Mesh& mesh){

		for(int i=0; i<mesh.n_of_c(); i++){
			if (mesh.cell(i).mFlag() == REFINE){
				int LhId = mesh.cell(i).LhId();
				int ops_LhId = mesh.ops_hId(LhId);
				int next_cell = mesh.IcId_from_hId(ops_LhId);

				vector<int> lepp;
				lepp.clear();

				while(1){
					lepp.push_back(LhId);
					lepp.push_back(ops_LhId);
					if (next_cell == -1){
						break;
					}
					if (mesh.LhId_from_cId(next_cell) == ops_LhId){
						break;
					}

					LhId = mesh.cell(next_cell).LhId();;
					ops_LhId = mesh.ops_hId(LhId);
					next_cell = mesh.IcId_from_hId(ops_LhId);
				} // while(1)

				assert(lepp.size()%2 == 0);

				// refine LEPP
				while (lepp.size() > 0){
					int hId = lepp[lepp.size()-1];
					lepp.pop_back();
					assert(lepp.size() > 0);
					int ops_hId = lepp[lepp.size()-1];
					lepp.pop_back();

					assert(ops_hId == mesh.ops_hId(hId));
					int next_hId = -1;
					if (lepp.size() > 0){
						next_hId = lepp[lepp.size()-1];
					}

					mesh.longest_ref2(hId, ops_hId, next_hId);
				}// while

			} // if REFINE
		}// for i

	}



}
