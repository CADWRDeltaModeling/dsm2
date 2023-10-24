

#define FORTRAN_NAME(x) __stdcall x

void __stdcall SET_WEIR_OP_COEF(int ndx,int devndx, double val);

void  dsm2_set_weir_op_coef(int ndx, int devndx, double val){
   SET_WEIR_OP_COEF(ndx,devndx,val);
}