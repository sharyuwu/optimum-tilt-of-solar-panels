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
/*  This file includes classes for the cell module.			     */
/*****************************************************************************/

#ifndef CELL
#define CELL
#endif

#ifndef VERTEX
#include "Vertex.h"
#endif

#ifndef EDGE
#include "Edge.h"
#endif

namespace PMGT{
//namespace Cell{

//using namespace Vertex;
//using namespace Edge;

class Cell{
public:
	Cell(int cId=-1, int LhId = -1, MFLAG mFlag=UNCHANGE, bool del=false){
		cId_ = cId;
		LhId_ = LhId;
		mFlag_ = mFlag;
		del_ = del;
	}

	inline int cId(){ return cId_;}
	inline MFLAG mFlag(){ return mFlag_; }
	inline int LhId(){ return LhId_; }
	inline bool del(){ return del_; }


	inline void setCId(int cId){ cId_ = cId; }
	inline void setMflag(MFLAG flag){ mFlag_ = flag; }
	inline void setLhId(int LhId){ LhId_ = LhId; }
	inline void setDel(bool del) { del_ = del; }

	inline void ccopy(Cell& other){
		cId_ = other.cId_;
		LhId_ = other.LhId_;
		mFlag_ = other.mFlag_;
		del_ = other.del_;
	}

private:
	int		cId_;	//id
	int		LhId_;	//longest halfedge
	MFLAG	mFlag_; //flag for modification
	bool 	del_;	// a flag to indicate if the cell
					// is marked to be deleted
}; // end of class Cell

//} // end of namespace Cell


} // end of namespace PMGT
