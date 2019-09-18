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
/*  This file contains the class for refine module.                          */
/*  There are two algorithms:                                                */
/*    split -- the point insertion algorithm for refining                    */
/*    longest_edge -- the longest side bisection algorithm for refining      */
/*****************************************************************************/

#ifndef REFINING
#define REFINING
#endif

#ifndef MESH
#include "Mesh.h"
#endif

namespace PMGT{
class Refining{
public:
	// split cells at their centroid points
	void split(Mesh& mesh);

	// refine cells at their longest edges
	void longest_edge(Mesh& mesh);

};
}
