// testing code

#include <iostream>


#ifndef MESH
#include "Mesh.h"
#endif

#ifndef INPUT
#include "Input.h"
#endif

#ifndef OUTPUT
#include "Output.h"
#endif


#ifndef REFINING
#include "Refining.h"
#endif

#ifndef COARSENING
#include "Coarsening.h"
#endif

#ifndef SERVICE
#include "Service.h"
#endif


//using namespace PMGT::Vertex;
//using namespace PMGT::Edge;
//using namespace PMGT::Cell;
//using namespace PMGT::Mesh;

using namespace PMGT;

int main(){

	char* vertexFile = "oldVertices.dat";
	char* cellFile = "oldCells.dat";

	int i,nc;

	Mesh mesh;
	Input input;
	Output output;
	Refining ref;
	Coarsening cos;
	Service sev;

	input.read_file(mesh, vertexFile, cellFile);
	if (sev.is_valid_mesh(mesh)){
		cout << "Mesh creatation is ok. The # of cells is " << mesh.n_of_c() << endl;
	}
	else{
		cout << "Mesh creatation is wrong." << endl;
	}

	nc=mesh.n_of_c();
	for (i=0; i<nc; i++){
		mesh.cell(i).setMflag(REFINE);
	}
	//ref.split(mesh);
	ref.longest_edge(mesh);
	if (sev.is_valid_mesh(mesh)){
		cout << "Mesh refinement is ok. The # of cells is " << mesh.n_of_c()  << endl;
	}
	else{
		cout << "Mesh refinement is wrong." << endl;
	}


	nc=mesh.n_of_c();
	for (i=0; i<nc; i++){
			mesh.cell(i).setMflag(COARSEN);
	}
	cos.edge_collapse(mesh);
	if (sev.is_valid_mesh(mesh)){
		cout << "Mesh coarsening is ok. The # of cells is " << mesh.n_of_c()  << endl;
	}
	else{
		cout << "Mesh coarsening is wrong." << endl;
	}

	output.write_file(mesh, "newVertices.dat", "newCells.dat");
	return 0;
}
