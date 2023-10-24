#include "klu.h"
#include "klu_version.h"
#include <stdint.h>

//#define NT
#ifdef _WIN32
#define STDCALL
#define klu_fortran_init             STDCALL KLU_FORTRAN_INIT
#define klu_fortran_analyze          STDCALL KLU_FORTRAN_ANALYZE
#define klu_fortran_factor           STDCALL KLU_FORTRAN_FACTOR
#define klu_fortran_refactor         STDCALL KLU_FORTRAN_REFACTOR
#define klu_fortran_solve            STDCALL KLU_FORTRAN_SOLVE
#define klu_fortran_condest          STDCALL KLU_FORTRAN_CONDEST
#define klu_fortran_rgrowth          STDCALL KLU_FORTRAN_RGROWTH
#define klu_fortran_rcond            STDCALL KLU_FORTRAN_RCOND
#define klu_fortran_free_numeric     STDCALL KLU_FORTRAN_FREE_NUMERIC
#define klu_fortran_free             STDCALL KLU_FORTRAN_FREE
#else
#define stdcall
#define klu_fortran_init             stdcall klu_fortran_init_
#define klu_fortran_analyze          stdcall klu_fortran_analyze_
#define klu_fortran_factor           stdcall klu_fortran_factor_
#define klu_fortran_refactor         stdcall klu_fortran_refactor_
#define klu_fortran_solve            stdcall klu_fortran_solve_
#define klu_fortran_condest          stdcall klu_fortran_condest_
#define klu_fortran_rgrowth          stdcall klu_fortran_rgrowth_
#define klu_fortran_rcond            stdcall klu_fortran_rcond_
#define klu_fortran_free_numeric     stdcall klu_fortran_free_numeric_
#define klu_fortran_free             stdcall klu_fortran_free_
#endif

static klu_common Common;

/**
 * Initialize pointer to common with defaults and return handle
**/
int64_t klu_fortran_init()
{
	klu_defaults (&Common);
	Common.scale=-1; // no scaling
	Common.btf=0; // no btf reordering
	Common.tol=1e-13; // pivot tolerance
	return &Common;
}


/**
* Symbolic factorization ( do once only if the non-zero locations remain the same )
**/
int64_t klu_fortran_analyze(int *n, int *Ap, int *Ai, int64_t* common){
	klu_common* Common = (klu_common*) *common;
	klu_symbolic* Symbolic = klu_analyze (*n, Ap, Ai, Common);
	return Symbolic;
}

/**
* Factorize based on symbolic factorization and commonS defaults
*/
int64_t klu_fortran_factor(int *Ap, int *Ai, double *Ax, int64_t *symbolic, int64_t *common){
	klu_common* Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = klu_factor (Ap, Ai, Ax, Symbolic, Common) ;
	return Numeric;
}

/**
* refactorize based on previous factorization ( good as int64_t as non-zero positions don't change)
*/
void klu_fortran_refactor(int *Ap, int *Ai, double *Ax, int64_t* symbolic, int64_t *numeric, int64_t *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_refactor(Ap,Ai,Ax,Symbolic,Numeric,Common);
}

/**
* solve based on previous factor or refactor.
*/
void klu_fortran_solve(int64_t *symbolic, int64_t *numeric, int* n, int* nrhs, double *b, int64_t *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_solve (Symbolic, Numeric, *n, *nrhs, b, Common) ;
}

//accurate condition number estimation, high condition number is ill-conditioned
double klu_fortran_condest(int *Ap, double *Ax, int64_t *symbolic, int64_t *numeric, int64_t *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_condest(Ap,Ax,Symbolic,Numeric,Common);
	return Common->condest;
}
//cheap reciprocal condition number estimation, low reciprocal is ill-conditioned
double klu_fortran_rcond(int64_t *symbolic, int64_t *numeric, int64_t *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_rcond(Symbolic,Numeric,Common);
	return Common->rcond;
}
//Computes the reciprocal pivot growth, small rgrowth implies inaccurate factorization
double klu_fortran_rgrowth(int *Ap, int *Ai, double *Ax, int64_t *symbolic, int64_t *numeric, int64_t *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_rgrowth(Ap,Ai,Ax,Symbolic,Numeric,Common);
	return Common->rgrowth;
}
/**
* free numeric memory
*/
void klu_fortran_free_numeric(int64_t *numeric, int64_t *common){
	klu_numeric *Numeric = (klu_numeric*) *numeric;
	klu_common *Common = (klu_common*) *common;
    klu_free_numeric (&Numeric, Common) ;
}

/**
* free memory
*/
void klu_fortran_free(int64_t *symbolic, int64_t *numeric, int64_t *common){
	klu_common *Common = (klu_common*) *common;
	klu_symbolic *Symbolic = (klu_symbolic*) *symbolic;
	klu_numeric *Numeric = (klu_numeric*) *numeric;

	klu_free_symbolic (&Symbolic, Common) ;
    klu_free_numeric (&Numeric, Common) ;
}
