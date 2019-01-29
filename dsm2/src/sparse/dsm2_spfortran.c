/*
 *  SPARSE DSM2 FORTRAN MODULE
 *
 *  Author: 
 *     Eli Ateljevich
 *     California Department of Water resources
 *  Copyright (c) 2007 California Department of Water Resources.
 *  This file is part of dsm2 and is distributed under the same !<license
 *  as dsm2
 */


#define spINSIDE_SPARSE
#include "spConfig.h"
#include "spmatrix.h"
#include "spDefs.h"

/*
 *  Using predefined macros for OS
 */

#ifdef _WIN32
#define STDCALL 
#define sfAdd5Reservoir		STDCALL SFADD5RESERVOIR
#define sfAdd4Equation		STDCALL SFADD4EQUATION
#else
#define sfAdd5Reservoir		 sfadd5reservoir_
#define sfAdd4Equation		 sfadd4equation_
#endif

#if FORTRAN

void
sfAdd4Equation( Template, Value )

long Template[4];
RealVector Value;
{
/* Begin `sfAdd4Equation'. */
    *((RealNumber *)Template[0]) += Value[0];
    *((RealNumber *)Template[1]) += Value[1];
    *((RealNumber *)Template[2]) += Value[2];
    *((RealNumber *)Template[3]) += Value[3];
}


void
sfAdd5Reservoir( Template, Value1, Value2, Value3, Value4, Value5 )

long Template[5];
RealNumber *Value1;
RealNumber *Value2;
RealNumber *Value3;
RealNumber *Value4;
RealNumber *Value5;
{
/* Begin `sfAdd5Reservoir'. */
    *((RealNumber *)Template[0]) += *Value1;
    *((RealNumber *)Template[1]) += *Value2;
    *((RealNumber *)Template[2]) += *Value3;
    *((RealNumber *)Template[3]) += *Value4;
	*((RealNumber *)Template[4]) += *Value5;
}

#endif /* FORTRAN */
