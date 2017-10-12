using namespace std;

#include <iostream>

#include <stdio.h>
#include <string.h>

#include "moduleMesh.h"

int main (int argc, const char * argv[]) {
  
  meshC myMesh;
  
  bool ascii=0;
  
  cout << "\n\nDébut programme échange de pointeurs entre C++ et Fortran" << endl;
  
  myMesh.nlog=6;
  cout << "\tmyMesh.nlog="<< myMesh.nlog << endl;
  
  myMesh.nVert=0;
  myMesh.nSol =0;
  myMesh.nFld =0;
  
  
  char buffer[256];
  cout << "\tFichier (Tetra): ";
  //fgets(&buffer[0],256,stdin) equivalent à fgets(myMesh.mesh_name,256,stdin);
  fgets(buffer,256,stdin);
  int lenght=strlen(buffer);
  cout << "\tlenght=" << lenght << endl;
  
  myMesh.mesh_name =(char*)malloc(lenght);
  for(int i=0 ; i<lenght ; i++)
    myMesh.mesh_name[i]=buffer[i];
  
  // Pour le fun
  if( strcmp(myMesh.mesh_name,"Tetra")==0  ){
    cout << "\tBon Choix !"<< endl;
  }else{
    cout << "\tMauvais Choix mais c'est pas grave puisque on ne lit rien ;) ! "<< strcpy(myMesh.mesh_name,"Tetra") << endl;
    
    strcpy(myMesh.mesh_name,"Tetra");
    cout << "\tJe modifie le nom pour le Fun avec la commande strcpy... " <<  myMesh.mesh_name << endl;
  }
  
  myMesh.solu =NULL;
  myMesh.hexas=NULL;
  myMesh.quadr=NULL;
  myMesh.trian=NULL;
  myMesh.edges=NULL;
  myMesh.solu =NULL;
  myMesh.norms=NULL;
  
  //printf("myMesh.mesh_name=%s\n",myMesh.mesh_name);
  //cout << "myMesh.mesh_name=" << myMesh.mesh_name << endl;
  //cout << "calling procedure read_mesh" << endl;
  
  printf("\nAppel du Fortran\n");
  
  read_mesh(&myMesh);
  
  printf("\nRetour au C++\n");
  
  if( myMesh.mesh_name!=NULL )cout << "\tmyMesh.mesh_name=" << myMesh.mesh_name << endl;
  if( myMesh.solu_name!=NULL )cout << "\tmyMesh.solu_name=" << myMesh.solu_name << endl;
  cout << "\tmyMesh.geo=" << myMesh.geo << endl;
  
  int ad=0;
  cout << "\n\tmyMesh.nVert=" << myMesh.nVert << endl;
  for( int iVert=0; iVert<myMesh.nVert; ++iVert ) {
    cout << "\tmyMesh.vertx" << iVert << ": ";
    for(int iGeo=0; iGeo<3; ++iGeo){
      cout << myMesh.vertx[ad] << " " ;
      ad++;
    }
    cout << "\tmark=" << myMesh.mark[iVert] << endl;
  }
  
  ad=0;
  cout << "\n\tmyMesh.nT4=" << myMesh.nT4 << endl;
  for( int iCell=0; iCell<myMesh.nT4; ++iCell ) {
    cout << "\tmyMesh.tetra" << iCell << ": ";
    for(int ker=0; ker<5; ++ker){
      cout << myMesh.tetra[ad] << " ";
      ad++;
    }
    cout << endl;
  }
  
  ad=0;
  cout << "\n\tmyMesh.nT3=" << myMesh.nT3 << endl;
  for( int iCell=0; iCell<myMesh.nT3; ++iCell ) {
    cout << "\tmyMesh.trian" << iCell << ": ";
    for(int ker=0; ker<4; ++ker){
      cout << myMesh.trian[ad] << " ";
      ad++;
    }
    cout << endl;
  }
  
  ad=0;
  cout << "\n\tmyMesh.nL2=" << myMesh.nL2 << endl;
  for( int iCell=0; iCell<myMesh.nL2; ++iCell ) {
    cout << "\tmyMesh.edge" << iCell << ": ";
    for(int ker=0; ker<3; ++ker){
      cout << myMesh.edges[ad] << " " ;
      ad++;
    }
    cout << endl;
  }
  
  cout << "Fin du programme" << endl;
  
  return 0;
}

