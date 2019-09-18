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
/*  This file includes classes for the vertex module.                        */
/*****************************************************************************/

#ifndef VERTEX
#define VERTEX
#endif

#include <vector>
#include <cfloat>
#include <cmath>
#include <iostream>
#include <cstdlib>
using namespace std;

namespace PMGT{
//namespace Vertex{
// type definition

// Point defines a coordinate of a point.
// The first is x coordinate,
// and the second is y coordinate.
typedef vector<double>	Point;

// MFLAG is a flag that indicates what should be done to a cell.
enum MFLAG {COARSEN = -1, UNCHANGE = 0, REFINE = 1};

class Vertex{
public:

	//constractor
	Vertex(Point point, int vId=-1, int OhId=-1, bool del=false){
	point_ = point;
	vId_= vId;
	OhId_= OhId;
	del_= del;
	}

	Vertex(double x=0, double y=0, int vId=-1, int OhId=-1, int del=false){
		point_.push_back(x);
		point_.push_back(y);
		vId_ = vId;
		OhId_ = OhId;
		del_ = del;
	}

	inline double x(){ return point_[0]; }
	inline double y(){ return point_[1]; }
	inline Point& point(){ return point_;}
	inline int vId(){ return vId_;}
	inline int OhId(){ return OhId_; }
	inline bool del(){ return del_; }


	inline void setPoint(double x, double y){
		point_[0] = x;
		point_[1] = y;
	}
	inline void setPoint(Point point){
		point_ = point;
	}
	inline void setVId(int vId){ vId_ = vId; }
	inline void setOhId(int OhId){ OhId_ = OhId; }
	inline void setDel(bool del) { del_ = del; }

	inline bool is_isolated_v(){ return OhId_ == -1; }
	inline void set_isolated_v(){ OhId_ = -1; }

	inline bool same(double x, double y){
		return (x==point_[0] && y==point_[1]);
	}
	inline bool same(Point point){
		return point_ == point;
	}
	inline void vcopy(Vertex& other){
		point_[0] = other.point_[0];
		point_[1] = other.point_[1];
		vId_ = other.vId_;
		OhId_ = other.OhId_;
		del_ = other.del_;
	}
	
	inline double distance(Point& p){
		return sqrt((p[0]-point_[0])*(p[0]-point_[0])+(p[1]-point_[1])*(p[1]-point_[1]));
	}

private:
	Point	point_;	// coordinate
	int		vId_;	// id
	int		OhId_;	// the first (ccw) outgoing halfedge.
	bool 	del_;	// a flag to indicate if the vertex
					// is marked to be deleted
}; // end of class Vertex

//} // end of namespace Vertex
} // end of namespace PMGT
