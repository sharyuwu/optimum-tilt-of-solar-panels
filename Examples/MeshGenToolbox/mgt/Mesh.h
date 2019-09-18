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
/*  This file includes classes for the mesh module.                          */
/*****************************************************************************/

#ifndef MESH
#define MESH
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

namespace PMGT{
//namespace Mesh{

//using namespace Vertex;
//using namespace Edge;
//using namespace Cell;


typedef vector<int> Ids;
class Mesh{
public:
	//constructor/destructor
	Mesh(){}

	// ***** size *****
	inline int n_of_v(){ return vertices_.size(); }
	inline int n_of_e(){ return edges_.size();}
	inline int n_of_c(){ return cells_.size(); }

	// ***** Id validation check *****
	inline bool is_valid_vId(int vId){
		return (vId < vertices_.size()) && (vId >= 0);
	}

	inline bool is_valid_hId(int hId){
		return (hId < (edges_.size()<<1)) && (hId >= 0);
	}

	inline bool is_valid_eId(int eId){
		return eId < edges_.size() && eId >= 0;
	}

	inline bool is_valid_cId(int cId){
		return cId < cells_.size() && cId >= 0;
	}


	// ***** get entity from id *****
	inline Vertex& vertex(int vId){
		assert(is_valid_vId(vId));
		return vertices_[vId];
	}

	inline Halfedge& halfedge(int hId){
		assert(is_valid_hId(hId));
		return edges_[hId>>1].pair()[hId & 1];
	}

	inline Edge& edge(int eId){
		assert(is_valid_eId(eId));
		return edges_[eId];
	}

	inline Cell& cell(int cId){
		assert(is_valid_cId(cId));
		return cells_[cId];
	}

	// ***** boundary check *****
	inline bool is_b_h(int hId){
		assert(is_valid_hId(hId));
		return halfedge(hId).IcId() == -1;
	}

	inline bool is_b_v(int vId){
		assert(is_valid_vId(vId));
		if(vertex(vId).OhId() == -1) { return true;}
		return is_b_h(ops_hId(vertex(vId).OhId()));
	}

	inline bool is_b_e(int eId){
		assert(is_valid_eId(eId));
		return is_b_h(eId << 1) || is_b_h((eId << 1) + 1);
	}
	inline bool is_b_c(int cId){
		assert(is_valid_cId(cId));
		int hId = cell(cId).LhId();
		return is_b_h(ops_hId(hId)) ||
			is_b_h(ops_hId(next_hId(hId))) ||
			is_b_h(ops_hId(prev_hId(hId)));
	}

	// ***** connectivity *****

	// ----- eId <==> hId -----

	inline int eId_from_hId(int hId){
		assert(is_valid_hId(hId));
		return (hId >> 1);
	}
	inline int hId_from_eId(int eId, int index){
		assert(is_valid_eId(eId));
		assert( index < 2 && index >= 0 );
		return (eId << 1) + index;
	}

	// ----- halfedge-halfedge -----

	inline int ops_hId(int hId){
		assert(is_valid_hId(hId));
		 return ((hId & 1) ? hId - 1: hId + 1);
	}
	inline int next_hId(int hId){
		assert(is_valid_hId(hId));
		return halfedge(hId).NhId();
	}
	inline int prev_hId(int hId){
		assert(is_valid_hId(hId));
		return halfedge(hId).PhId();
	}
	// ccw rotated halfedge
	inline int ccw_hId(int hId){
		assert(is_valid_hId(hId));
		if (is_b_h(hId)) return -1;
		return ops_hId(prev_hId(hId));
	}
	// cw rotated halfedge
	inline int cw_hId(int hId){
		assert(is_valid_hId(hId));
		if (is_b_h(ops_hId(hId))) return -1;
		return next_hId(ops_hId(hId));
	}

	// ----- vertex -----

	// first outgoing halfedge
	inline int OhId_from_vId(int vId){
		assert(is_valid_vId(vId));
		return vertex(vId).OhId();
	}

	// ----- halfedge -----

	// targate vertex
	inline int tvId_from_hId(int hId){
		assert(is_valid_hId(hId));
		return halfedge(hId).TvId();
	}

	// start vertex
	inline int svId_from_hId(int hId){
		assert(is_valid_hId(hId));
		return tvId_from_hId(ops_hId(hId));
	}

	// its cell
	inline int IcId_from_hId(int hId){
		assert(is_valid_hId(hId));
		return halfedge(hId).IcId();
	}

	// ----- edge -----
	// vertices
	inline vector<int> vIds_from_eId(int eId){
		assert(is_valid_eId(eId));
		vector<int> vIds;
		vIds.push_back(svId_from_hId(hId_from_eId(eId, 0)));
		vIds.push_back(tvId_from_hId(hId_from_eId(eId, 0)));
		return vIds;
	}

	// cells
	inline vector<int> cIds_from_eId(int eId){
		assert(is_valid_eId(eId));
		vector<int> cIds;
		cIds.push_back(IcId_from_hId(hId_from_eId(eId, 0)));
		cIds.push_back(IcId_from_hId(hId_from_eId(eId, 1)));
		return cIds;
	}

	// ----- cell -----

	// its longest halfedge
	inline int LhId_from_cId(int cId){
		assert(is_valid_cId(cId));
		return cell(cId).LhId();
	}

	// three vertices
	inline vector<int> vIds_from_cId(int cId){
		assert(is_valid_cId(cId));
		vector<int> vIds;
		vIds.push_back(tvId_from_hId(cell(cId).LhId()));
		vIds.push_back(tvId_from_hId(next_hId(cell(cId).LhId())));
		vIds.push_back(svId_from_hId(cell(cId).LhId()));
		return vIds;
	}

	// ***** isolation test *****

	// is the vertex is isolated
	inline bool is_isolated_v(int vId){
		assert(is_valid_vId(vId));
		return !is_valid_hId(OhId_from_vId(vId));
	}

	// ***** circulators *****

	// ----- vertex - vertex (ccw) one ring circulator -----

	inline int vv_first(int vId){
		assert(is_valid_vId(vId));
		return tvId_from_hId(OhId_from_vId(vId));
	}

	// Returns -1 if there is no next
	inline int vv_next(int svId, int tvId){
		assert(is_valid_vId(svId));
		assert(is_valid_vId(tvId));
		int hId = voh_first(svId);
		while (tvId_from_hId(hId) != tvId && hId != -1){
			hId = voh_next(svId, hId);
		}
		if (hId != -1){
			if (is_b_h(hId)) return -1;
			return tvId_from_hId(next_hId(hId));
		}
		assert(false);
	}

	// ----- vertex - outgoing-halfedge (ccw) one ring circulator -----
	inline int voh_first(int vId){
		assert(is_valid_vId(vId));
		return OhId_from_vId(vId);
	}

	// Returns -1 if there is no next
	inline int voh_next(int vId, int hId){
		assert(is_valid_vId(vId));
		assert(is_valid_hId(hId));
		assert(vId == svId_from_hId(hId));
		return ccw_hId(hId);
	}

	// ----- vertex - incoming-halfedge (ccw) one ring circulator -----
	inline int vih_first(int vId){
		assert(is_valid_vId(vId));
		if ( is_isolated_v(vId)) return -1;
		return ops_hId(voh_first(vId));
	}

	// Returns -1 if there is no next
	inline int vih_next(int vId, int hId){
		assert(is_valid_vId(vId));
		assert(is_valid_hId(hId));
		assert(vId == tvId_from_hId(hId));
		int ops = ops_hId(hId);
		if (is_b_h(ops)) return -1;
		//return prev_hId(ops);
		return  ops_hId(voh_next(vId, ops));
	}

	// ----- vertex - edge (ccw) one ring circulator -----
	inline int ve_first(int vId){
		assert(is_valid_vId(vId));
		if (is_isolated_v(vId)) return -1;
		return eId_from_hId(voh_first(vId));
	}

	// Return -1 if there is no next
	inline int ve_next(int vId, int eId){
		assert(is_valid_vId(vId));
		assert(is_valid_eId(eId));
		int hId = hId_from_eId(eId,0);
		if (vId == svId_from_hId(hId)){ // hId is an outgoing halfedge
			int voNext = voh_next(vId, hId);
			if (voNext == -1) return -1;
			return eId_from_hId(voNext);
		}
		//hId is an incoming halfedge
		assert(vId == tvId_from_hId(hId));
		int viNext = vih_next(vId, hId);
		if (viNext == -1) return -1;
		return eId_from_hId(viNext);
	}

	// ----- vertex - cell (cww) one ring circulator -----

	inline int vc_first(int vId){
		assert(is_valid_vId(vId));
		if (is_isolated_v(vId)) return -1;
		return IcId_from_hId(voh_first(vId));
	}

	// Returns -1 if there is no next
	inline int vc_next(int vId, int cId){
		assert(is_valid_vId(vId));
		assert(is_valid_cId(cId));
		// find the halfedge whose targate vertex is vId
		int hId = LhId_from_cId(cId);
		if (tvId_from_hId(hId) != vId){
			if (tvId_from_hId(prev_hId(hId)) == vId)
				hId = prev_hId(hId);
			else if (tvId_from_hId(next_hId(hId)) == vId)
				hId = next_hId(hId);
			else assert(false);
		}
		if (is_b_h(ops_hId(hId))) return -1;
		return IcId_from_hId(ops_hId(hId));
	}

	// ----- cell - vertex (cww) one ring circulator -----
	// The first vertex is the tartage vertex of the longest halfedge
	inline int cv_first(int cId){
		assert(is_valid_cId(cId));
		return tvId_from_hId(LhId_from_cId(cId));
	}

	inline int cv_next(int cId, int vId){
		assert(is_valid_cId(cId));
		assert(is_valid_vId(vId));
		// find the halfedge whose start vertex is vId
		int hId = LhId_from_cId(cId);
		if (svId_from_hId(hId) != vId){
			if (svId_from_hId(prev_hId(hId)) == vId)
				hId = prev_hId(hId);
			else if (svId_from_hId(next_hId(hId)) == vId)
				hId = next_hId(hId);
			else assert(false);
		}
		return tvId_from_hId(hId);
	}

	// ----- cell - halfedge (cww) one ring circulator -----
	// The first halfedge is the longest halfedge
	inline int ch_first(int cId){
		assert(is_valid_cId(cId));
		return LhId_from_cId(cId);
	}
	inline int ch_next(int cId, int hId){
		assert(is_valid_cId(cId));
		assert(is_valid_hId(hId));
		assert(IcId_from_hId(hId) == cId);
		return next_hId(hId);
	}

	// ----- cell - edge (cww) one ring circulator -----
	// The first edge is the longest edge
	inline int ce_first(int cId){
		assert(is_valid_cId(cId));
		return eId_from_hId(ch_first(cId));
	}
	// next should change
	inline int ce_next(int cId, int eId){
		assert(is_valid_cId(cId));
		assert(is_valid_eId(eId));
		int hId = hId_from_eId(eId, 0);
		if (IcId_from_hId(hId) == cId)
			return eId_from_hId(ch_next(cId,hId));
		else if (IcId_from_hId(ops_hId(hId)) == cId)
			return eId_from_hId(ch_next(cId, ops_hId(hId)));
		else assert(false);
	}

	// ----- cell - cell (cww) one ring circulator -----

	// The first cell is the cell adjacent the longest edge
	// if the given cell is not a boundary cell.
	// If one edge is boundary edge, returns the cell adjacent to the next edge (ccw).
	// If two edges are boundary edges, refturns the only adjacent cell.
	// Returns -1 if there is no adjacent cell.
	inline int cc_first(int cId){
		assert(is_valid_cId(cId));
		int hId = LhId_from_cId(cId);
		if (!is_b_c(cId)) return IcId_from_hId(ops_hId(hId));

		if (!is_b_h(ops_hId(hId))){
			if ( is_b_h(ops_hId(next_hId(hId))) && !is_b_h(ops_hId(prev_hId(hId))) )
				return IcId_from_hId(ops_hId(prev_hId(hId)));
			else return IcId_from_hId(ops_hId(hId));
		}
		else {
			if ( !is_b_h(ops_hId(next_hId(hId))) )
				return IcId_from_hId(ops_hId(next_hId(hId)));
			else return IcId_from_hId(ops_hId(prev_hId(hId)));
		}
	}

	// Returns -1 if there is no next
	inline int cc_next(int scId, int tcId){
		assert(is_valid_cId(tcId));
		assert(is_valid_cId(scId));
		int hId = LhId_from_cId(scId);

		if (IcId_from_hId(ops_hId(hId)) != tcId){
			if (IcId_from_hId(ops_hId(next_hId(hId))) == tcId)
				hId = next_hId(hId);
			else if (IcId_from_hId(ops_hId(prev_hId(hId))) == tcId)
				hId = prev_hId(hId);
			else assert(false);
		}
		return IcId_from_hId(ops_hId(next_hId(hId)));
	}



	// ***** find an entity *****

	// Returns the id of the vertex that has given coordinate.
	// Returns -1 if the vertex does not exist.
	int find_vertex(Point point);

	// Returns the id of the edge that has given points.
	// Returns -1 if the edge does not exist.
	int find_edge(int svId, int tvId);

	// Returns the id of the halfedge that has given start and targate points.
	// Returns -1 if the halfedge does not exist.
	int find_halfedge(int svId, int tvId);

	// Returns the id of the cell that has given vertices.
	// Returns -1 if the cell does not exist.
	int find_cell(vector<int> vIds);


	// ***** Geometric properties *****

	// Returns the distance of two vertex
	inline double dist(int svId, int tvId){
		assert(is_valid_vId(svId) && is_valid_vId(tvId));
		Point 	ps = vertex(svId).point(),
			pt = vertex(tvId).point();
		return sqrt((ps[0]-pt[0])*(ps[0]-pt[0]) +
				(ps[1]-pt[1])*(ps[1]-pt[1]));
	}

	inline double dist(int hId){
		assert(is_valid_hId(hId));
		return dist(svId_from_hId(hId), tvId_from_hId(hId));
	}

	// ----- mid point of a halfedge
	inline Point mid_point(int hId){
		assert(is_valid_hId(hId));
		double sx = vertex(svId_from_hId(hId)).x();
		double sy = vertex(svId_from_hId(hId)).y();
		double tx = vertex(tvId_from_hId(hId)).x();
		double ty = vertex(tvId_from_hId(hId)).y();
		Point mid;
		mid.push_back((sx+tx)/2);
		mid.push_back((sy+ty)/2);
		return mid;
	}

	// ----- mid point of two points
	inline Point mid_point(int svId, int tvId){
		double sx = vertex(svId).x();
		double sy = vertex(svId).y();
		double tx = vertex(tvId).x();
		double ty = vertex(tvId).y();
		Point mid;
		mid.push_back((sx+tx)/2);
		mid.push_back((sy+ty)/2);
		return mid;
	}

	// ----- mid point of the longest halfedge of a cell
	inline Point longest_mid(int cId){
		assert(is_valid_cId(cId));
		return mid_point(LhId_from_cId(cId));
	}

	// ----- centroid point of a cell -----
	inline Point centroid(int cId){
		assert(is_valid_cId(cId));
		Point point = longest_mid(cId);
		// find the opposite point to the longest halfedge
		Vertex& v = vertex(tvId_from_hId(next_hId(LhId_from_cId(cId))));
		point[0] = point[0]*2/3 + v.x()/3;
		point[1] = point[1]*2/3 + v.y()/3;

		return point;
	}


	// ----- size of a cell ----- //
	// The size of a cell if the length of longest halfedge of the cell
	inline double size(int cId){
		assert(is_valid_cId(cId));
		return dist(LhId_from_cId(cId));
	}

	// ----- Test for counter clockwise ----- //
	inline bool is_ccw(Point pa, Point pb, Point pc){
		double detleft = (pa[0] - pc[0]) * (pb[1] - pc[1]);
		double detright = (pa[1] - pc[1]) * (pb[0] - pc[0]);
  		return detleft - detright > DBL_EPSILON*100;
	}

	inline bool is_ccw(vector<int> vIds){
		assert(vIds.size() == 3);
		assert(is_valid_vId(vIds[0]));
		assert(is_valid_vId(vIds[1]));
		assert(is_valid_vId(vIds[2]));
		return is_ccw(vertex(vIds[0]).point(),
						vertex(vIds[1]).point(),
						vertex(vIds[2]).point());
	}

/*	// ----- Test for collinear ------ //
	inline bool is_collinear(Point pa, Point pb, Point pc){
		double detleft = (pa[0] - pc[0]) * (pb[1] - pc[1]);
		double detright = (pa[1] - pc[1]) * (pb[0] - pc[0]);
  		return detleft - detright == 0;
	}
*/
	// ----- Test for the same side ----- //

	inline bool is_same_side(int hId, Point pa, Point pb){
		Point ps = vertex(svId_from_hId(hId)).point();
		Point pt = vertex(tvId_from_hId(hId)).point();
		double deta = (pa[1]-ps[1])*(pt[0]-ps[0]) -
					(pa[0]-ps[0])*(pt[1]-ps[1]);
		double detb = (pb[1]-ps[1])*(pt[0]-ps[0]) -
					(pb[0]-ps[0])*(pt[1]-ps[1]);
		return deta*detb > 0;
	}

/*	inline bool is_in_halfedge(int hId, Point p){
		Point ps = vertex(svId_from_hId(hId)).point();
		Point pt = vertex(tvId_from_hId(hId)).point();
		return is_collinear(ps, pt, p);
	}
*/
	/** \ Add an entity
	*/

	// Create a vertex and add it to the end of the vertex list.
	// Returns false if the vertex already exists.
	bool add_vertex(Point point);


	// Create an edge and add it to the end of the edge list.
	// Returns false if the edge already exists.
	bool add_edge(int svId, int tvId);


	// Create a cell and add it to the end of the cell list.
	// Returns false if the cell already exists.
	bool add_cell(vector<int> vIds);

	/** \ Update an entity
	*/

	// update a cell using three vertices.
	// Returns false if the new cell already exists.
	bool update_cell(vector<int> vIds, int cId);


	// split 2 cells by split their common longest edge

	bool longest_ref2(int hId, int o_hId, int next_hId);

	// ****** delete an entity *****

	// Delete vertices that marked to be deleted from the list.
	bool delete_vertices();

	// Delete edges that marked to be deleted from the list.
	bool delete_edges();

	// Delete cells that marked to be deleted from the list.
	bool delete_cells();

	// ***** create a mesh ******
	// create a mesh using two input variables.
	// One is the coordinates of points
	// The other is the vertex list of cells
	bool create(vector<Point>, vector<Ids>);

	// ***** write a mesh *****
	// write the mesh to two variables
	// One is the coordinates of points
	// The other is the vertex list of cells
	void write_var(vector<Point>&, vector<Ids>&);



	// display the internal information of a mesh on the screen
	void display_info();


private:
	vector<Vertex> 		vertices_;
	vector<Edge>		edges_;
	vector<Cell>		cells_;


};


//}

}
