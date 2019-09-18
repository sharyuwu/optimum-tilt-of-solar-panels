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
#ifndef COARSENING
#include "Coarsening.h"
#endif

namespace PMGT{

	// coarsen the mesh by edge collapse
	void Coarsening::edge_collapse(Mesh& mesh){
		int i, j, k,
			cId, // temp cell Id
			eId, // temp edge Id
			hId, // temp halfedge Id
			vId, // temp vertex Id

		 	acId, // adjacent cell
		 	first_acId, // the first adjacent cell
		 	rvId, // the vertex to be remained
		 	dvId;  // the vertex to be deleted

		 vector<int>	del_cells,	// cells to be deleted
		 				mutual_halfedges, // halfedges of the mutual edge
		 				del_halfedges, // halfedges to be deleted/merged
		 			 	remain_halfedges, // halfedges to be remained
		 			 	update_halfedges, 	// halfedges whose target vertices
		 			 						// to be updated
						update_cells; // cells whose edge length are changed

		//int nc = mesh.n_of_c();
		for (i=0; i<mesh.n_of_c(); i++){
			bool legal = false;
			if (mesh.cell(i).mFlag() == COARSEN && !mesh.cell(i).del()){
				// check if it is legal to delete i

				// check if one of the adjacent cells is to be coarsened
				acId = mesh.cc_first(i);

				for (int k=0; k<3; k++){ // try 3 neighbour of cell i
//cout << "coarsen i=" << i << " acId=" << acId << " k=" << k <<endl;
					if (acId == -1){
						break;
					}
					// check if the i-th neighbour is to be coarsened
					if (mesh.cell(acId).mFlag() != COARSEN){
						acId = mesh.cc_next(i, acId);
						continue;
					}
					/* check if the mutual edge is legal to be removed */

					// find one halfedge of the mutual edge
					hId = mesh.ch_first(i);

					if (mesh.IcId_from_hId(mesh.ops_hId(hId)) != acId){
						if (mesh.IcId_from_hId(mesh.ops_hId
						(mesh.prev_hId(hId))) == acId){
							hId = mesh.prev_hId(hId);
						}
						else if(mesh.IcId_from_hId(mesh.ops_hId
						(mesh.next_hId(hId))) == acId){
							hId = mesh.next_hId(hId);
						}
						else assert(false);
					}

					// find the vertex to be delated
					int illegal_vId = -1; // the vertex that can not be deleted
					bool both_illegal = false; // both vertices are illegal to be deleted
					dvId = mesh.tvId_from_hId(hId);
					rvId = mesh.svId_from_hId(hId);
					int old_hId = hId;
					for (int x=0; x<2; x++){ // try two vertices of hId
						if (mesh.is_b_v(dvId)){
							if (illegal_vId == -1){
								illegal_vId = dvId;
								dvId = mesh.svId_from_hId(old_hId);
								rvId = mesh.tvId_from_hId(old_hId);
								hId = mesh.ops_hId(old_hId);
								continue;
							}
							else{
								both_illegal = true;
								break;
							}
						}
//cout << "mutual_h:"<<hId<<",";
						// reserve the entities that will be modified
						del_cells.clear();
						mutual_halfedges.clear();
						del_halfedges.clear();
						remain_halfedges.clear();
						update_halfedges.clear();
						update_cells.clear();
						// set variables
						mutual_halfedges.push_back(hId);
						del_halfedges.push_back(mesh.next_hId(hId));
						remain_halfedges.push_back(mesh.prev_hId(hId));
						del_cells.push_back(mesh.IcId_from_hId(hId));

						hId = mesh.ops_hId(hId);
//cout << hId<< endl;

						mutual_halfedges.push_back(hId);
						del_halfedges.push_back(mesh.prev_hId(hId));
						remain_halfedges.push_back(mesh.next_hId(hId));
						del_cells.push_back(mesh.IcId_from_hId(hId));
//cout << "del_cells "<< del_cells[0]<<","<<del_cells[1]<<endl;
						assert(del_cells[0]==i || del_cells[0]==acId);
						assert(del_cells[1]==i || del_cells[1]==acId);
						assert(del_cells[0] != del_cells[1]);
						assert(!mesh.is_b_v(dvId));

						// find the halfedges whose target vertices are dvId
						hId = mesh.vih_first(dvId);
						do{
							if (hId == -1){
								break;
							}
							if (hId != mutual_halfedges[0] && hId != del_halfedges[1]
									&& hId != mesh.ops_hId(del_halfedges[0])){
								update_halfedges.push_back(hId);
								//update_cells.push_back(mesh.IcId_from_hId(hId));
							}
//cout << "dvId="<<dvId<<"vih id=" << hId << endl;

							hId = mesh.vih_next(dvId, hId);
						}while (hId != mesh.vih_first(dvId));

						// find the cells whose whose one vertices are dvId
						cId = mesh.vc_first(dvId);
						do{
							if (cId ==-1){
								break;
							}
							if (cId != del_cells[0] && cId != del_cells[1]){
								update_cells.push_back(cId);
								//update_cells.push_back(mesh.IcId_from_hId(hId));
							}

							cId = mesh.vc_next(dvId, cId);

						}while (cId != mesh.vc_first(dvId));


						for (j=0; j<update_cells.size(); j++){
							vector<int> vIds;
							vIds.clear();
							vIds = mesh.vIds_from_cId(update_cells[j]);
							for (int jj=0; jj<3; jj++){
								if (vIds[jj] == dvId){
									vIds[jj] = rvId;
								}
							}
							if ( !mesh.is_ccw(vIds)){
								break;// for j
							}

						}
						if (j==update_cells.size()){
							break; // from (for x)
						}
						// dvId is not legal to be deleted
						if (illegal_vId == -1){
							illegal_vId = dvId;
							dvId = mesh.svId_from_hId(old_hId);
							rvId = mesh.tvId_from_hId(old_hId);
							hId = mesh.ops_hId(old_hId);
							continue;
						}
						else {// both vertices are illegal to be deleted
							both_illegal = true;
							break; // from (for x)
						}
					}// for x

					if (both_illegal){
						acId = mesh.cc_next(i, acId);
						if (acId == -1){
							break;
						}
						continue;
					}
					else {
						legal = true;
						break; // from (for k) since dvId is found
					}
				} // for k
				if (legal){
//cout << "legal coarsen i=" << i << " acId=" << acId << endl;

					// reset Mflag
					for (j=0; j<update_halfedges.size(); j++){
						mesh.halfedge(update_halfedges[j]).setTvId(rvId);

					}
					for (j=0; j<2; j++){
						hId = mesh.ops_hId(del_halfedges[j]);
						cId = mesh.IcId_from_hId(hId);
						if (mesh.LhId_from_cId(cId) == hId){
							int LhId = remain_halfedges[j];
							int next_hId = mesh.next_hId(hId);
							int prev_hId = mesh.prev_hId(hId);
							if (mesh.dist(LhId) < mesh.dist(next_hId)){
								LhId = next_hId;
							}
							if (mesh.dist(LhId) < mesh.dist(prev_hId)){
								LhId = prev_hId;
							}
							mesh.cell(cId).setLhId(LhId);
//cout << " ____cId = " << cId << " LhId = " << LhId <<endl;
						}
					}


					for (j=0; j<update_cells.size(); j++){
						mesh.cell(update_cells[j]).setMflag(UNCHANGE);
					}
//cout << "---update_cells:";
//for (int ii=0; ii<update_cells.size(); ii++){
//	cout <<" " << update_cells[ii]<< "("<<mesh.cell(update_cells[ii]).mFlag()<<")";
//}

//cout << endl;

					// reset OhId
					int svId = mesh.svId_from_hId(del_halfedges[1]);
					if (mesh.OhId_from_vId(svId) == del_halfedges[1]){
						//mesh.vertex(svId).setOhId(mesh.ccw_hId(del_halfedges[1]));
						//assert(mesh.svId_from_hId(del_halfedges[1])==
						//mesh.svId_from_hId(remain_halfedges[1]));
						mesh.vertex(svId).setOhId(mesh.ops_hId(remain_halfedges[1]));
//cout << "reset OhId of vertex " << svId << " to " << mesh.vertex(svId).OhId()  << endl;
					}

					svId = mesh.svId_from_hId(mesh.ops_hId(del_halfedges[0]));
					if (mesh.OhId_from_vId(svId) == mesh.ops_hId(del_halfedges[0])){
						//mesh.vertex(svId).setOhId(mesh.ccw_hId(mutual_halfedges[0]));
						//assert(mesh.svId_from_hId(del_halfedges[0])==
						//mesh.svId_from_hId(remain_halfedges[0]));
						mesh.vertex(svId).setOhId(remain_halfedges[0]);

//cout << "reset OhId of vertex " << svId << " to " << mesh.vertex(svId).OhId() <<  endl;
					}
					svId = mesh.svId_from_hId(mutual_halfedges[0]);
					if (mesh.OhId_from_vId(svId) == mutual_halfedges[0]){
						//mesh.vertex(svId).setOhId(mesh.ccw_hId(mutual_halfedges[0]));
						//assert(mesh.svId_from_hId(mutual_halfedges[0])==
						//mesh.svId_from_hId(remain_halfedges[0]));

						mesh.vertex(svId).setOhId(remain_halfedges[1]);
//cout << "reset OhId of vertex " << svId << " to " << mesh.vertex(svId).OhId()  << endl;
					}


					for (j=0; j<2; j++){
						hId = mesh.ops_hId(del_halfedges[j]);


						int phId = mesh.prev_hId(hId);
						int nhId = mesh.next_hId(hId);
						cId = mesh.IcId_from_hId(hId);

						mesh.halfedge(remain_halfedges[j]).setIcId(cId);
						mesh.halfedge(remain_halfedges[j]).setPhId(phId);
						mesh.halfedge(remain_halfedges[j]).setNhId(nhId);
						mesh.halfedge(nhId).setPhId(remain_halfedges[j]);
						mesh.halfedge(phId).setNhId(remain_halfedges[j]);


						mesh.halfedge(del_halfedges[j]).setIcId(-1);

					}


					// mark edges to be deleted
					mesh.edge(mesh.eId_from_hId(mutual_halfedges[0])).setDel(true);
					mesh.edge(mesh.eId_from_hId(del_halfedges[0])).setDel(true);
					mesh.edge(mesh.eId_from_hId(del_halfedges[1])).setDel(true);

//cout << "deleted edges:"<< mesh.eId_from_hId(mutual_halfedges[0])
//<< " "<< mesh.eId_from_hId(del_halfedges[0])
//<< " "<< mesh.eId_from_hId(del_halfedges[1])<< endl;

					// mark cells to be deleted
					mesh.cell(del_cells[0]).setDel(true);
					mesh.cell(del_cells[1]).setDel(true);


					// delete dvId
					mesh.vertex(dvId).set_isolated_v();
					mesh.vertex(dvId).setDel(true);
				}// if (legal)
			}// if COARSEN
		} // for i

		// delete entities
		mesh.delete_cells();
		mesh.delete_edges();//mesh.display_info();

		mesh.delete_vertices();

		// reset mFlags
	}// edge_collapse

}
