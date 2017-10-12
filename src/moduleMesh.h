
struct meshC{
  //-------
  int     nlog;
  //-------
  char   *mesh_name;
  int     geo;
  int     prec;
  int     nVert;
  double *vertx;
  int    *mark;
  int     nH6;
  int    *hexas;
  int     nT4;
  int    *tetra;
  int     nQ4;
  int   *quadr;
  int     nT3;
  int    *trian;
  int     nL2;
  int    *edges;
  double *norms;
  double  xmin,xmax;
  double  ymin,ymax;
  double  zmin,zmax;
  double  baryCenter[3];
  //-------
  bool    solution;
  char   *solu_name;
  int     nSol,nFld;
  double *solu;
  int     kind[20];
  int     ker;
};

extern "C" {  
  void cflAnalyser(meshC*, bool*);
  void read_mesh(meshC*);
}

