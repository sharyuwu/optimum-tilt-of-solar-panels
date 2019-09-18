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

#include "Mesh.h"

namespace PMGT{
//namespace Mesh{

	// Returns the id of the vertex that has given coordinate.
	// Returns -1 if the vertex does not exist.
	int Mesh::find_vertex(Point point){
		for (int i=0; i<vertices_.size(); i++){
			if (vertices_[i].point() == point)
				return i;
		}
		return -1;
	}

	// Returns the id of the edge that has given points.
	// Returns -1 if the edge does not exist.
	int  Mesh::find_edge(int svId, int tvId){
		assert(is_valid_vId(svId));
		assert(is_valid_vId(tvId));
		for (int i=0; i<edges_.size(); i++){
			if (edges_[i].pair()[0].TvId() == tvId){
				if (edges_[i].pair()[1].TvId() == svId)
					return i;
			}
			if (edges_[i].pair()[0].TvId() == svId){
				if (edges_[i].pair()[1].TvId() == tvId)
					return i;
			}
		}
		return -1;
	}

	// Returns the id of the halfedge that has given start and targate points.
	// Returns -1 if the halfedge does not exist.
	int  Mesh::find_halfedge(int svId, int tvId){
		assert(is_valid_vId(svId));
		assert(is_valid_vId(tvId));
		for (int i=0; i<edges_.size(); i++){
			if (edges_[i].pair()[0].TvId() == tvId){
				if (edges_[i].pair()[1].TvId() == svId)
					return (i<<1);
			}
			if (edges_[i].pair()[0].TvId() == svId){
				if (edges_[i].pair()[1].TvId() == tvId)
					return (i<<1)+1;
			}
		}
		return -1;
	}

	// Returns the id of the cell that has given vertices.
	// Returns -1 if the cell does not exist.
	// Make sure that vIds and corresponding halfedges exist.
	int Mesh::find_cell(vector<int> vIds){
		assert(vIds.size() == 3);
		for (int i=0; i<3; i++){
			assert(is_valid_vId(vIds[i]));
		}
		vector<int> hId(3);
		for (int i=0; i<3; i++){
			hId[i] = find_halfedge(vIds[i], vIds[(i+1)%3]);
			if(hId[i] == -1) return -1;
		}

		for (int i=0; i<cells_.size(); i++){
			for (int j=0; j<3; j++){
				if (cells_[i].LhId()==hId[j]){
					int k;
					for (k=0; k<3; k++){
						if (vIds[k]=tvId_from_hId(hId[j])){

							break;
						}
					}
					if (k<3 && vIds[(k+1)%3]==tvId_from_hId(hId[(j+1)%3])
					&& vIds[(k+2)%3]==tvId_from_hId(hId[(j+2)%3])){
						// three vertices is target vertices of three halfedges
						return i;
					}
				}
			}
		}
		return -1;
	}


	// Create a vertex and add it to the end of the vertex list.
	// Returns false if the vertex already exists.
	bool Mesh::add_vertex(Point point){
		int vId = find_vertex(point);
		if (vId != -1) return false;
		Vertex v(point, vertices_.size());
		vertices_.push_back(v);
		return true;
	}


	// Create an edge and add it to the end of the edge list.
	// Returns false if the edge already exists.
	bool Mesh::add_edge(int svId, int tvId){
		assert(is_valid_vId(svId));
		assert(is_valid_vId(tvId));
		int eId = find_edge(svId, tvId);
		if (eId != -1){
			cout << "add edge " << svId << " " << tvId << " failed" << endl;
			return false;
		}
		Edge e(svId, tvId, edges_.size());
		e.pair()[0].setIcId(-1);
		e.pair()[1].setIcId(-1);
		edges_.push_back(e);
		edge(edges_.size()-1).pair()[0].setIcId(-1);
		edge(edges_.size()-1).pair()[1].setIcId(-1);
		return true;
	}


	// Create a cell and add it to the end of the vertex list.
	// Returns false if the cell already exists.
	bool Mesh::add_cell(vector<int> vIds){
		assert(vIds.size() == 3);
		for (int i=0; i<3; i++){
			assert(is_valid_vId(vIds[i]));
		}
		int cId = find_cell(vIds);
		if (cId != -1) return false;
	assert(cId == -1);
		vector<int> hId(3);
		for (int i=0; i<3; i++){

			hId[i] = find_halfedge(vIds[i], vIds[(i+1)%3]);
			assert(hId[i] != -1);
		}
		int LhId = hId[0];
		if (dist(hId[1]) > dist(LhId)) LhId = hId[1];
		if (dist(hId[2]) > dist(LhId)) LhId = hId[2];

		//Cell c(cells_.size(), LhId);
		Cell c;
		cells_.push_back(c);
		cId = cells_.size()-1;
		cell(cId).setCId(cId);
		cell(cId).setLhId(LhId);
		// resolve connectivity of edges and vertices
		for (int i=0; i<3; i++){
			halfedge(hId[i]).setIcId(cId);
			halfedge(hId[i]).setNhId(hId[(i+1)%3]);
			halfedge(hId[i]).setPhId(hId[(i+2)%3]);

		}
		for (int i=0; i<3; i++){
			int OhId = vertex(vIds[i]).OhId();
			if (OhId ==-1 || (!is_b_h(ops_hId(OhId)))){
				vertex(vIds[i]).setOhId(hId[i]);
			}
			else if(svId_from_hId(OhId) != vIds[i]){
				vertex(vIds[i]).setOhId(hId[i]);
			}
		}
		return true;

	}

	// update the cell using given vertex.
	// Returns false if the cell does not exist
	// or the new cell is already exists.
	bool Mesh::update_cell(vector<int> vIds, int cId){
		if (!is_valid_cId(cId)) {
			cout << "update cell " << cId << " failed. " <<
			cId << " is not valid." << endl;
			return false;
		}
		int newId = find_cell(vIds);
		if (newId != -1){
			cout << "update cell " << cId << " failed. The cell (" <<
			 vIds[0]<<","<<vIds[1]<<","<<vIds[2]<<") exists."<<endl;
			return false;
		}

		vector<int> hId(3);
		for (int i=0; i<3; i++){
			hId[i] = find_halfedge(vIds[i], vIds[(i+1)%3]);
			assert(hId[i] != -1);
		}
		int LhId = hId[0];
		if (dist(hId[1]) > dist(LhId)) LhId = hId[1];
		if (dist(hId[2]) > dist(LhId)) LhId = hId[2];
		Cell& c = cell(cId);
		c.setLhId(LhId);
		c.setMflag(UNCHANGE);

		// resolve connectivity of edges and vertices
		for (int i=0; i<3; i++){
			halfedge(hId[i]).setIcId(c.cId());
			halfedge(hId[i]).setNhId(hId[(i+1)%3]);
			halfedge(hId[i]).setPhId(hId[(i+5)%3]);

			int OhId = vertex(vIds[i]).OhId();
			if (OhId ==-1 || (!is_b_h(ops_hId(OhId)))){
				vertex(vIds[i]).setOhId(hId[i]);
			}
			else if(svId_from_hId(OhId) != vIds[i]){
				vertex(vIds[i]).setOhId(hId[i]);
			}
		}
		return true;

	}


	// refine two cells whose longest edges are oppsite to each other
	bool Mesh::longest_ref2(int hId, int o_hId, int n_hId){

		assert(o_hId == ops_hId(hId));
		int cId	= IcId_from_hId(hId);
		int ops_cId = IcId_from_hId(o_hId);
		vector<int> vIds;
		// reserve vertices
		if (cId != -1){
			//vIds = vIds_from_cId(cId);
			vIds.push_back(tvId_from_hId(hId));
			vIds.push_back(tvId_from_hId(next_hId(hId)));
			vIds.push_back(svId_from_hId(hId));
		}
		vector<int> ops_vIds;// = vIds_from_cId(ops_cId);
		ops_vIds.push_back(tvId_from_hId(o_hId));
		ops_vIds.push_back(tvId_from_hId(next_hId(o_hId)));
		ops_vIds.push_back(svId_from_hId(o_hId));

		if (cId !=-1){
			assert(ops_vIds[0] == vIds[2]);
			assert(ops_vIds[2] == vIds[0]);
		}

		// reset mFlag
		cell(ops_cId).setMflag(UNCHANGE);
		if (cId != -1){
			cell(cId).setMflag(UNCHANGE);
		}

		// add new vertex
		Point p = mid_point(hId);
		assert(add_vertex(p));
		int new_vId = n_of_v()-1;

		// add/update edges
		assert(add_edge(ops_vIds[1], new_vId));
		if (cId != -1){
			assert(add_edge(vIds[1], new_vId));
		}

		halfedge(hId).setTvId(new_vId);
		assert(add_edge(ops_vIds[2], new_vId));

		// add/update cells
		int next_svId=0;
		if (n_hId != -1){
			for(int k=0; k<2; k++){
				if(ops_vIds[k]==svId_from_hId(n_hId)){
					next_svId = k;
					break;
				}
			}
		}
		vector<int> temp_vIds;
		temp_vIds.clear();
		temp_vIds.push_back(new_vId);
		temp_vIds.push_back(ops_vIds[next_svId]);
		temp_vIds.push_back(ops_vIds[next_svId+1]);
		assert(update_cell(temp_vIds, ops_cId));

		if (next_svId == 0){
			temp_vIds[1] = ops_vIds[1];
			temp_vIds[2] = ops_vIds[2];
		}
		else {
			temp_vIds[1] = ops_vIds[0];
			temp_vIds[2] = ops_vIds[1];
		}

		assert(add_cell(temp_vIds));
		if (cId != -1){
			temp_vIds.clear();
			temp_vIds.push_back(new_vId);
			temp_vIds.push_back(vIds[0]);
			temp_vIds.push_back(vIds[1]);
			assert(update_cell(temp_vIds, cId));

			temp_vIds[1] = vIds[1];
			temp_vIds[2] = vIds[2];
			assert(add_cell(temp_vIds));
		}
	}



	// ****** delete an entity *****

	// Delete vertices that marked to be deleted.
	bool Mesh::delete_vertices(){
		for (int i=vertices_.size()-1; i>=0; i--){
			if (vertices_[i].del() == true){

				// the vertex to be deleted is not the last one
				if (vertices_.size()-1 != i){
					int vnId = vertices_.size()-1;
					int hId = vih_first(vnId);

					do{
						int old_hId = hId;
						hId = vih_next(vnId, hId);
						halfedge(old_hId).setTvId(i);
						if (hId == -1){
							break;
						}
					}while (hId != vih_first(vnId));

					vertices_[i].vcopy(vertices_[vnId]);
					vertices_[i].setVId(i);

				}
				vertices_.pop_back();
			}
		}
		return true;
	}



// Delete edges that marked to be deleted from the list.
	bool Mesh::delete_edges(){
		vector<int> vIds(2);
		int j;
		for (int i=edges_.size()-1; i>=0; i--){
			if (edges_[i].del() == true){
				//assert(IcId_from_hId(hId_from_eId(i,0)) == -1);
				//assert(IcId_from_hId(hId_from_eId(i,1)) == -1);
				if (i!=edges_.size()-1){


					int enId = edges_.size()-1;
					// relink last edge in the edge list
					// this edge will be moved to to position i
					vIds = vIds_from_eId(enId);
					for (j=0; j<2; j++){
						int hnj = hId_from_eId(enId,j);
						int hij = hId_from_eId(i,j);
						// resolve OhIds of endpoints
						if (OhId_from_vId(vIds[j]) == hnj){
							vertex(vIds[j]).setOhId(hij);
						}


						// resolve PhIds and NhIds
						if (!is_b_h(hnj)){
							halfedge(prev_hId(hnj)).setNhId(hij);
							halfedge(next_hId(hnj)).setPhId(hij);
						}

						// resolve LhId of the incident cell
						if (IcId_from_hId(hnj) != -1 &&
						LhId_from_cId(IcId_from_hId(hnj))==hnj){
							cell(IcId_from_hId(hnj)).setLhId(hij);
						}
					}// for j
			// copy last edge
					edges_[i].ecopy(edges_[enId]);
					edges_[i].setEId(i);
					edges_[i].pair()[0].setHId(i<<1);
					edges_[i].pair()[1].setHId((i<<1)+1);
				}// if i
				// delete last edge
				edges_.pop_back();


			}
		} // for i
			return true;
	}


	// Delete cells that marked to be deleted from the list.
	bool Mesh::delete_cells(){

		//vector<int> hIds(3);
		for (int i=cells_.size()-1; i>=0; i--){
			if (cells_[i].del() == true){
				// the cell to be deleted is not the last one
				if (i!=cells_.size()-1){

					int cnId = cells_.size()-1;
					// relink last cell in the cell list
					// this cell will be moved to cId
					vector<int> hIds;
					hIds.push_back(LhId_from_cId(cnId));
					hIds.push_back(next_hId(LhId_from_cId(cnId)));
					hIds.push_back(prev_hId(LhId_from_cId(cnId)));

					for(int j=0; j<3; j++){
						halfedge(hIds[j]).setIcId(i);
					}
					// copy last cell
					cells_[i].ccopy(cells_[cnId]);
					cells_[i].setCId(i);

				}
				// delete last cell
				cells_.pop_back();
			}
		}
			return true;
	}


	// create a mesh using two input variables.
	// One is the coordinates of points
	// The other is the vertex list of cells
	bool Mesh::create(vector<Point> points, vector<Ids> cells){
		int i,j;
		int nv = points.size();
		int nc = cells.size();
		int eId[3];

		vector<int> vIds;

		for (i=0; i<nv; i++){
			// point duplication check
			if (!add_vertex(points[i])){
				cout << "Duplicate point is detected " <<
					"during mesh creatation."<< endl;
				return false;
			}
		}

		for (i=0; i<nc; i++){
			vIds.clear();

			for (j=0; j<3; j++){
				// validation check
				if (!is_valid_vId(cells[i][j])){

					cout << "Invalid id is detected " <<
					"during mesh creatation."<< endl;
					return false;
				}

				// find vertices
				vIds.push_back(cells[i][j]);

				// boundary check
				if (!is_b_v(vIds[j])){
					cout << "Complex vertex is detected " <<
					"during mesh creatation."<< endl;
					return false;
				}
			}
			// cell duplication check
			if (find_cell(vIds) != -1){
				cout << "Duplicate cell is detected " <<
					"during mesh creatation."<< endl;
				return false;
			}

			// add edges if necessary
			for (j=0; j<3; j++){
				eId[j] = find_edge(vIds[j], vIds[(j+1)%3]);
				if (eId[j] == -1){
					add_edge(vIds[j], vIds[(j+1)%3]);
				}

			}

			// add the cell
			add_cell(vIds);
		}

	}


	// display the internal information of a mesh on the screen
	void Mesh::display_info(){
		vector<int> ids;

		cout << endl <<"vertices:" << endl;
		for (int i=0; i< vertices_.size(); i++){
			cout << "id=" << vertex(i).vId() << " x=" << vertex(i).x() << " y=" << vertex(i).y() <<
			" OhId=" << vertex(i).OhId() << endl;
		}

		cout <<endl << "edges:" << endl;
		for (int i=0; i<edges_.size(); i++){
			cout << "id=" << edge(i).eId() << " svId=" << vIds_from_eId(i)[0] <<
			" tvId=" << vIds_from_eId(i)[1] << endl;
			cout << "hid=" << edge(i).pair()[0].hId() << " prev=" <<  edge(i).pair()[0].PhId() <<
			" next=" << edge(i).pair()[0].NhId() << " IcId=" << edge(i).pair()[0].IcId() << endl;
			cout << "hid=" << edge(i).pair()[1].hId() << " prev=" <<  edge(i).pair()[1].PhId() <<
			" next=" << edge(i).pair()[1].NhId() << " IcId=" << edge(i).pair()[1].IcId() << endl;
		}

		cout <<endl << "cells:" << endl;
		for (int i=0; i< cells_.size(); i++){
				cout << "id=" << cell(i).cId() << " LhId=" << cell(i).LhId() <<
				" vIds=" << vIds_from_cId(i)[0] << ","
						<< vIds_from_cId(i)[1] << ","
						<< vIds_from_cId(i)[2] <<
				" mFlag=" << cell(i).mFlag() << endl;
		}
	}


//}
}
