#include "klu.h"
#include "klu_version.h"

#define NT
#ifdef NT
#define STDCALL  
#define klu_fortran_init             STDCALL KLU_FORTRAN_INIT
#define klu_fortran_analyze          STDCALL KLU_FORTRAN_ANALYZE
#define klu_fortran_factor           STDCALL KLU_FORTRAN_FACTOR
#define klu_fortran_refactor         STDCALL KLU_FORTRAN_REFACTOR
#define klu_fortran_solve            STDCALL KLU_FORTRAN_SOLVE
#define klu_fortran_condest          STDCALL KLU_FORTRAN_CONDEST
#define klu_fortran_rgrowth          STDCALL KLU_FORTRAN_RGROWTH
#define klu_fortran_rcond			 STDCALL KLU_FORTRAN_RCOND
#define klu_fortran_free_numeric     STDCALL KLU_FORTRAN_FREE_NUMERIC
#define klu_fortran_free             STDCALL KLU_FORTRAN_FREE
#endif

static klu_common Common;

/**
 * Initialize pointer to common with defaults and return handle
**/
long klu_fortran_init()
{
	klu_defaults (&Common);
	Common.scale=-1; // no scaling
	Common.btf=0; // no btf reordering
	Common.tol=1e-13; // pivot tolerance
	return (long) &Common;
}


/**
* Symbolic factorization ( do once only if the non-zero locations remain the same )
**/
long klu_fortran_analyze(int *n, int *Ap, int *Ai, long* common){
	klu_common* Common = (klu_common*) *common;
	klu_symbolic* Symbolic = klu_analyze (*n, Ap, Ai, Common);
	return (long) Symbolic;
}

/**
* Factorize based on symbolic factorization and common defaults
*/
long klu_fortran_factor(int *Ap, int *Ai, double *Ax, long *symbolic, long *common){
	klu_common* Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = klu_factor (Ap, Ai, Ax, Symbolic, Common) ;
	return (long) Numeric;
}

/**
* refactorize based on previous factorization ( good as long as non-zero positions don't change)
*/
void klu_fortran_refactor(int *Ap, int *Ai, double *Ax, long* symbolic, long *numeric, long *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_refactor(Ap,Ai,Ax,Symbolic,Numeric,Common);
}

/**
* solve based on previous factor or refactor.
*/
void klu_fortran_solve(long *symbolic, long *numeric, int* n, int* nrhs, double *b, long *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_solve (Symbolic, Numeric, *n, *nrhs, b, Common) ;
}

//accurate condition number estimation, high condition number is ill-conditioned
double klu_fortran_condest(int *Ap, double *Ax, long *symbolic, long *numeric, long *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_condest(Ap,Ax,Symbolic,Numeric,Common);
	return Common->condest;
}
//cheap reciprocal condition number estimation, low reciprocal is ill-conditioned
double klu_fortran_rcond(long *symbolic, long *numeric, long *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_rcond(Symbolic,Numeric,Common);
	return Common->rcond;
}
//Computes the reciprocal pivot growth, small rgrowth implies inaccurate factorization
double klu_fortran_rgrowth(int *Ap, int *Ai, double *Ax, long *symbolic, long *numeric, long *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_rgrowth(Ap,Ai,Ax,Symbolic,Numeric,Common);
	return Common->rgrowth;
}
/**
* free numeric memory
*/
void klu_fortran_free_numeric(long *numeric, long *common){
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_common *Common = (klu_common*) *common;
    klu_free_numeric (&Numeric, Common) ;
}

/**
* free memory
*/
void klu_fortran_free(long *symbolic, long *numeric, long *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;

	klu_free_symbolic (&Symbolic, Common) ;
    klu_free_numeric (&Numeric, Common) ;
}
