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

#include "Input.h"

namespace PMGT{


	// Read the mesh from two files
	// One is the coordinates of points
	// The other is the vertex list of cells
	void Input::read_file(Mesh& mesh, char* vertexFile, char* cellFile){
/**/
		FILE *v;
		FILE *c;
		Point p;
		Ids ids;
		vector<Point> points;
		vector<Ids> cells;

		char *s, *t;
		char line[100];
		bool error = false;

		// Open the file.  If NULL is returned there was an error
		if((v = fopen(vertexFile, "r")) == NULL) {
			cout << "Error opening file " << vertexFile << "." << endl;
			exit(1);
		}

		if((c = fopen(cellFile, "r")) == NULL) {
			cout << "Error opening file " << cellFile << "." << endl;
			exit(1);
		}

		//assert(vertices_.size() == 0);
		//assert(edges_.size() == 0);
		//assert(cells_.size() == 0);

		// get vertices
		points.clear();
		int count = 0;
		while( fgets(line, sizeof(line), v) != NULL ) {
				// Get each line from the infile
			s = line;
			p.clear();

			p.push_back(strtod(s, &t));
			while  ( *s != ' ' && *s !='\n'){
				s++;
			}
			if (*s == '\n'){
				error = true;
				break;
			}
			p.push_back(strtod(s, &t));
			points.push_back(p);
			count++;
		}
		fclose(v);  // Close the file
		if (error){ // error message for input file
			cout <<"Something wrong in your input file " << vertexFile <<
			" line " << count+1 << endl;
			exit(1);
		}

		// get cells
		int n=points.size();
		count = 0;
		cells.clear();
		while( fgets(line, sizeof(line), c) != NULL ) {
				// Get each line from the infile
			s = line;
			ids.clear();

			ids.push_back(strtol(s, &t, 10));

			if (ids[ids.size()-1] <0 || ids[ids.size()-1]>=n){
				error = true;
				break;
			}
			while  ( *s != ' ' && *s !='\n'){
				s++;
			}
			while(*s == ' ') s++;
			if (*s == '\n'){
				error = true;
				break;
			}

			ids.push_back(strtol(s, &t, 10));
			while(*s == ' ') s++;
			if (ids[ids.size()-1] <0 || ids[ids.size()-1]>=n){
				error = true;
				break;
			}

			while  ( *s != ' ' && *s !='\n'){
				s++;
			}
			while(*s == ' ') s++;
			if (*s == '\n'){
				error = true;
				break;
			}

			ids.push_back(strtol(s, &t, 10));
			if (ids[ids.size()-1] <0 || ids[ids.size()-1]>=n){
				error = true;
				break;
			}
			cells.push_back(ids);

			count++;
		}
		fclose(c);
		if (error){ // error message for input file
			cout <<"Something wrong in your input file " << cellFile <<
			" line " << count+1 <<endl;
			exit(1);
		}
		mesh.create(points, cells);
	}
}

