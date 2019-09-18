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
/*  This file includes classes for the edge module.							 */
/*****************************************************************************/

#ifndef EDGE
#define EDGE
#endif

#ifndef VERTEX
#include "Vertex.h"
#endif

namespace PMGT{
//namespace Edge{

//using namespace Vertex;


class Halfedge{
public:
	Halfedge(int hId=-1, int TvId=-1, int IcId=-1, int PhId=-1, int NhId=-1){
	hId_ = hId;
	TvId_ = TvId;
	IcId_ = IcId;
	PhId_ = PhId;
	NhId_ = NhId;
	}

	inline int hId(){ return hId_; }
	inline int IcId(){ return IcId_; }
	inline int TvId(){ return TvId_; }
	inline int NhId(){ return NhId_; }
	inline int PhId(){ return PhId_; }

	inline void setHId(int hId){ hId_ = hId; }
	inline void setTvId(int TvId){ TvId_ = TvId; }
	inline void setIcId(int IcId){ IcId_ = IcId; }
	inline void setNhId(int NhId){ NhId_ = NhId; }
	inline void setPhId(int PhId){ PhId_ = PhId; }

	inline bool is_boundary(){ return IcId_ == -1; }
	//opposite halfedge
	inline int ops_hId(){ return ((hId_ & 1) ? hId_-1: hId_+1); }
	inline void hcopy(Halfedge& other){
		hId_ = other.hId_;
		TvId_ = other.TvId_;
		IcId_ = other.IcId_;
		PhId_ = other.PhId_;
		NhId_ = other.NhId_;
	}

private:
	int	hId_;	//id
	int TvId_; 	//target vertex
	int	IcId_;	//its cel
	int	PhId_;	//previous halfedge
	int NhId_;	//next halfedge

}; // end of class Halfedge


class Edge{
public:
	Edge(int svId=-2, int tvId=-1, int eId=-1, bool del=false){
		assert(svId != tvId);
		eId_ = eId;
		pair_[0].setHId(eId << 1);
		pair_[0].setTvId(tvId);
		pair_[1].setHId((eId << 1)+1);
		pair_[1].setTvId(svId);
		del_ = del;
	}


	inline int eId(){ return eId_;}
	inline Halfedge* pair(){ return pair_; }
	inline bool del(){ return del_; }

	inline void setEId(int eId){ eId_ = eId; }
	inline void setPair(Halfedge& h1, Halfedge& h2){
		pair_[0].hcopy(h1);
		pair_[1].hcopy(h2);
	}
	inline void setDel(bool del) { del_ = del; }


	inline bool is_isolated_e(){
		return pair_[0].IcId() == -1
			&& pair_[1].IcId() == -1;
	}
	inline void set_isolated_e(){
		pair_[0].setIcId(-1);
		pair_[1].setIcId(-1);
	}

	inline bool same (int svId, int tvId){
		int v1Id = pair_[0].TvId();
		int v2Id = pair_[1].TvId();
		return ((v1Id==svId && v2Id==tvId) || (v1Id==tvId && v2Id==svId));
	}

	inline void ecopy(Edge& other){
		eId_ = other.eId_;
		pair_[0].hcopy(other.pair_[0]);
		pair_[1].hcopy(other.pair_[1]);
		del_ = other.del_;
	}

private:
	int			eId_;		// id
	Halfedge 	pair_[2]; 	//a pair of halfedges
	bool 		del_;	// a flag to indicate if the edge
						// is marked to be deleted
}; // end of class Edge


//} // end of namespace Edge


} // end of namespace PMGT
