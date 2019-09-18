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

#include "Output.h"

namespace PMGT{
	// write the mesh to two files
	// One is the coordinates of points
	// The other is the vertex list of cells
	bool Output::write_file(Mesh& mesh, char* vertexFile, char* cellFile){
		FILE *v;
		FILE *c;
		int i;
		vector<int> vIds(3);

  		/* Open files.  If NULL is returned there was an error */
  		if((v = fopen(vertexFile, "w")) == NULL) {
    			printf("Error Opening Vertex File.\n");
    			exit(1);
  		}
 		if((c = fopen(cellFile, "w")) == NULL) {
    			printf("Error Opening Cell File.\n");
    			exit(1);
  		}

		// write vertex file
		for (i=0; i<mesh.n_of_v(); i++){
			fprintf(v, "%.6g %.6g\n",
				mesh.vertex(i).x(), mesh.vertex(i).y());
		}
		// write cell file
		for (i=0; i<mesh.n_of_c(); i++){
			vIds[0] = mesh.tvId_from_hId(mesh.LhId_from_cId(i));
			vIds[1] = mesh.tvId_from_hId(mesh.next_hId(mesh.LhId_from_cId(i)));
			vIds[2] = mesh.tvId_from_hId(mesh.prev_hId(mesh.LhId_from_cId(i)));
			fprintf(c, "%d %d %d\n", vIds[0], vIds[1], vIds[2]);
		}
		fclose(v);
		fclose(c);
	}

}
