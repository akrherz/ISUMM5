/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/
/* intrinsics.c SCCS(4.1 91/02/18 15:40:11) */

#include <stdio.h>
#include "node.h"

int sw_ansi ;		/* accessed by default main */

struct intrRec {
    char * name , * relation, * argtype , * funtype, * standard  ;
    struct intrRec * next ;
} ;

struct intrRec intrTab[] = {

    "int",	"int",	"arithmetic",	"integer", "ansi", 0,
    "ifix",	"int",	"arithmetic",	"integer", "ansi", 0,
    "idint",	"int",	"arithmetic",	"integer", "ansi", 0,

    "real",	"real",	"arithmetic",	"real", "ansi", 0,
    "float",	"real",	"arithmetic",	"real", "ansi", 0,
    "sngl",	"real",	"arithmetic",	"real", "ansi", 0,

    "real",	"dble",	"arithmetic",	"double", "ansi", 0,
    "float",	"dble",	"arithmetic",	"double", "ansi", 0,
    "sngl",	"dble",	"arithmetic",	"double", "ansi", 0,

    "cmplx",	"cmplx", "arithmetic",	"complex", "ansi", 0,

    "ichar",	"ichar", "character",	"integer", "ansi", 0,
    "char",	"char",	"integer",	"character", "ansi", 0,

    "aint",	"aint",	"real",		"integer", "ansi", 0,
    "dint",	"aint",	"double",	"integer", "ansi", 0,

    "anint",	"anint", "real", 	"real", "ansi", 0,
    "dnint",	"anint", "double", 	"double", "ansi", 0,

    "nint",	"nint",	"real",		"integer", "ansi", 0,
    "idnint",	"nint",	"double",	"integer", "ansi", 0,

    "iabs",	"abs",	"integer",	"integer", "ansi", 0,
    "abs",	"abs",	"real",		"real", "ansi", 0,
    "dabs",	"abs",	"double",	"double", "ansi", 0,
    "cabs",	"abs",	"comples",	"real", "ansi", 0,

    "mod",	"mod",	"integer",	"integer", "ansi", 0,
    "amod",	"mod",	"real",	"real", "ansi", 0,
    "dmod",	"mod",	"double",	"double", "ansi", 0,

    "isign",	"sign",	"integer",	"integer", "ansi", 0,
    "sign",	"sign",	"real",	"real", "ansi", 0,
    "dsign",	"sign",	"double",	"double", "ansi", 0,

    "idim",	"dim",	"integer",	"integer", "ansi", 0,
    "dim",	"dim",	"real",	"real", "ansi", 0,
    "ddim",	"dim",	"double",	"double", "ansi", 0,

    "dprod",	"dprod",	"real",	"double", "ansi", 0,

    "max",	"max",	"arithmetic",	"arithmetic", "ansi", 0,
    "max0",	"max",	"integer",	"integer", "ansi", 0,
    "amax1",	"max",	"real",	"real", "ansi", 0,
    "dmax1",	"max",	"double",	"double", "ansi", 0,
    "amax0",	"max",	"integer",	"real", "ansi", 0,
    "max1",	"max",	"real",	"integer", "ansi", 0,

    "min",	"min",	"arithmetic",	"arithmetic", "ansi", 0,
    "min0",	"min",	"integer",	"integer", "ansi", 0,
    "amin1",	"min",	"real",	"real", "ansi", 0,
    "dmin1",	"min",	"double",	"double", "ansi", 0,
    "amin0",	"min",	"integer",	"real", "ansi", 0,
    "min1",	"min",	"real",	"integer", "ansi", 0,

    "len",	"len",	"character",	"integer", "ansi", 0,

    "index",	"index",	"character",	"integer", "ansi", 0,

    "aimag",	"aimag",	"complex",	"real", "ansi", 0,

    "conjg",	"conjg",	"complex",	"complex", "ansi", 0,

    "sqrt",	"sqrt",	"real",	"real", "ansi", 0,
    "dsqrt",	"sqrt",	"double",	"double", "ansi", 0,
    "csqrt",	"sqrt",	"complex",	"complex", "ansi", 0,

    "exp",	"exp",	"real",	"real", "ansi", 0,
    "dexp",	"exp",	"double",	"double", "ansi", 0,
    "cexp",	"exp",	"complex",	"complex", "ansi", 0,

    "log",	"log",	"real",	"real", "ansi", 0,
    "alog",	"log",	"real",	"real", "ansi", 0,
    "dlog",	"log",	"double",	"doublew", "ansi", 0,
    "clog",	"log",	"complex",	"complex", "ansi", 0,

    "log10",	"log10",	"real",	"real", "ansi", 0,
    "alog10",	"log10",	"real",	"real", "ansi", 0,
    "dlog10",	"log10",	"double",	"double", "ansi", 0,

    "sin",	"sin",	"real",	"real", "ansi", 0,
    "dsin",	"sin",	"double",	"double", "ansi", 0,
    "csin",	"csin",	"complex",	"complex", "ansi", 0,

    "cos",	"cos",	"real",	"real", "ansi", 0,
    "dcos",	"cos",	"double",	"double", "ansi", 0,
    "ccos",	"ccos",	"complex",	"complex", "ansi", 0,

    "tan",	"tan",	"real",	"real", "ansi", 0,
    "dtan",	"tan",	"double",	"double", "ansi", 0,

    "asin",	"asin",	"real",	"real", "ansi", 0,
    "dasin",	"asin",	"double",	"double", "ansi", 0,

    "acos",	"acos",	"real",	"real", "ansi", 0,
    "dacos",	"acos",	"double",	"double", "ansi", 0,

    "atan",	"atan",	"real",	"real", "ansi", 0,
    "datan",	"atan",	"double",	"double", "ansi", 0,

    "atan2",	"atan2",	"real",	"real", "ansi", 0,
    "datan2",	"atan2",	"double",	"double", "ansi", 0,

    "sinh",	"sinh",	"real",	"real", "ansi", 0,
    "dsinh",	"sinh",	"double",	"double", "ansi", 0,

    "cosh",	"cosh",	"real",	"real", "ansi", 0,
    "dcosh",	"cosh",	"double",	"double", "ansi", 0,

    "tanh",	"tanh",	"real",	"real", "ansi", 0,
    "dtanh",	"tanh",	"double",	"double", "ansi", 0,

    "lge",	"lge",	"character",	"logical", "ansi", 0,
    "lgt",	"lgt",	"character",	"logical", "ansi", 0,
    "lle",	"lle",	"character",	"logical", "ansi", 0,
    "llt",	"llt",	"character",	"logical", "ansi", 0,

/* VMS extension intrinsics */

    "qsqrt",	"sqrt",	"real*16",	"real*16",	"vms", 0,
    "cdsqrt",	"sqrt",	"complex*16",	"complex*16",	"vms", 0,

    "qlog",	"log",	"real*16",	"real*16",	"vms", 0,
    "cdlog",	"log",	"complex*16",	"complex*16",	"vms", 0,

    "qlog10",	"log10",	"real*16",	"real*16",	"vms", 0,

    "qexp",	"exp",	"real*16",	"real*16",	"vms", 0,
    "cdexp",	"exp",	"complex*16",	"complex*16",	"vms", 0,

    "qsin",	"sin",	"real*16",	"real*16",	"vms", 0,
    "cdsin",	"sin",	"complex*16",	"complex*16",	"vms", 0,

    "sind",	"sind",	"real",	"real",	"vms", 0,
    "dsind",	"sind",	"double",	"double",	"vms", 0,
    "qsind",	"sind",	"real*16",	"real*16",	"vms", 0,

    "qcos",	"cos",	"real*16",	"real*16",	"vms", 0,
    "cdcos",	"cos",	"complex*16",	"complex*16",	"vms", 0,

    "cosd",	"cosd",	"real",	"real",	"vms", 0,
    "dcosd",	"cosd",	"double",	"double",	"vms", 0,
    "qcosd",	"cosd",	"real*16",	"real*16",	"vms", 0,

    "qtan",	"tan",	"real*16",	"real*16",	"vms", 0,

    "tand",	"tand",	"real",	"real",	"vms", 0,
    "dtand",	"tand",	"double",	"double",	"vms", 0,
    "qtand",	"tand",	"real*16",	"real*16",	"vms", 0,

    "qasin",	"asin",	"real*16",	"real*16",	"vms", 0,

    "asind",	"asind",	"real",	"real",	"vms", 0,
    "dasind",	"asind",	"double",	"double",	"vms", 0,
    "qasind",	"asind",	"real*16",	"real*16",	"vms", 0,

    "qacos",	"acos",	"real*16",	"real*16",	"vms", 0,

    "acosd",	"acosd",	"real",	"real",	"vms", 0,
    "dacosd",	"acosd",	"double",	"double",	"vms", 0,
    "qacosd",	"acosd",	"real*16",	"real*16",	"vms", 0,

    "qatan",	"atan",	"real*16",	"real*16",	"vms", 0,

    "atand",	"atand",	"real",	"real",	"vms", 0,
    "datand",	"atand",	"double",	"double",	"vms", 0,
    "qatand",	"atand",	"real*16",	"real*16",	"vms", 0,

    "qatan2",	"atan2",	"real*16",	"real*16",	"vms", 0,

    "atan2d",	"atan2d",	"real",	"real",	"vms", 0,
    "datan2d",	"atan2d",	"double",	"double",	"vms", 0,
    "qatan2d",	"atan2d",	"real*16",	"real*16",	"vms", 0,

    "qsinh",	"sinh",	"real*16",	"real*16",	"vms", 0,
    "qcosh",	"cosh",	"real*16",	"real*16",	"vms", 0,
    "qtanh",	"tanh",	"real*16",	"real*16",	"vms", 0,

    "iiabs",	"abs",	"integer*2",	"integer*2",	"vms", 0,
    "jiabs",	"abs",	"integer",	"integer",	"vms", 0,
    "qabs",	"abs",	"real*16",	"real*16",	"vms", 0,
    "cdabs",	"abs",	"complex*16",	"complex*16",	"vms", 0,

    "iint",	"int",	"real",	"integer*2",	"vms", 0,
    "jint",	"int",	"real",	"integer",	"vms", 0,
    "iidint",	"int",	"double",	"integer*2",	"vms", 0,
    "jidint",	"int",	"double",	"integer",	"vms", 0,
    "iiqint",	"int",	"real*16",	"integer*2",	"vms", 0,
    "jiqint",	"int",	"real*16",	"integer",	"vms", 0,

    "inint",	"nint",	"real",	"ninteger*2",	"vms", 0,
    "jnint",	"nint",	"real",	"ninteger",	"vms", 0,
    "iidnnt",	"nint",	"double",	"ninteger*2",	"vms", 0,
    "jidnnt",	"nint",	"double",	"ninteger",	"vms", 0,
    "iiqnnt",	"nint",	"real*16",	"ninteger*2",	"vms", 0,
    "jiqnnt",	"nint",	"real*16",	"ninteger",	"vms", 0,

    "izext",	"zext",	"integer",	"integer*2",	"vms", 0,
    "jzext",	"zext",	"integer",	"integer",	"vms", 0,

    "floati",	"real",	"integer*2",	"real",	"vms", 0,
    "floatj",	"real",	"integer",	"real",	"vms", 0,
    "snglq",	"real",	"real*16",	"real",	"vms", 0,

    "dbleq",	"dble",	"real*16",	"double",	"vms", 0,

    "qext",	"qext",	"integer",	"real*16",	"vms", 0,
    "qextd",	"qext",	"real",	"real*16",	"vms", 0,

    "iifix",	"ifix",	"real",	"integer*2",	"vms", 0,
    "jifix",	"ifix",	"real",	"integer",	"vms", 0,

    "dfloti",	"real",	"integer*2",	"double",	"vms", 0,
    "dflotj",	"real",	"integer",	"double",	"vms", 0,

    "dcmplx",	"dcmplx",	"arithmetic",	"complex*16",	"vms", 0,

    "dreal",	"dble",	"complex*16",	"double",	"vms", 0,

    "dconjg",	"comjg",	"complex*16",	"complex*16",	"vms", 0,

    "imax0",	"max",	"integer*2",	"integer*2",	"vms", 0,
    "jmax0",	"max",	"integer",	"integer",	"vms", 0,
    "qmax1",	"max",	"real*16",	"real*16",	"vms", 0,
    "imax1",	"max",	"real",	"integer*2",	"vms", 0,
    "jmax1",	"max",	"real",	"integer",	"vms", 0,
    "aimax0",	"amax0",	"integer*2",	"real",	"vms", 0,
    "ajmax0",	"amax0",	"integer",	"real",	"vms", 0,

    "imin0",	"min",	"integer*2",	"integer*2",	"vms", 0,
    "jmin0",	"min",	"integer",	"integer",	"vms", 0,
    "qmin1",	"min",	"real*16",	"real*16",	"vms", 0,
    "imin1",	"min",	"real",	"integer*2",	"vms", 0,
    "jmin1",	"min",	"real",	"integer",	"vms", 0,
    "aimin0",	"amin0",	"integer*2",	"real",	"vms", 0,
    "ajmin0",	"amin0",	"integer",	"real",	"vms", 0,

    "iidim",	"dim",	"integer*2",	"integer*2",	"vms", 0,
    "jidim",	"dim",	"integer",	"integer",	"vms", 0,
    "qdim",	"dim",	"real*16",	"real*16",	"vms", 0,

    "imod",	"mod",	"integer*2",	"integer*2",	"vms", 0,
    "jmod",	"mod",	"integer",	"integer",	"vms", 0,
    "qmod",	"mod",	"real*16",	"real*16",	"vms", 0,

    "iisign",	"sign",	"integer*2",	"integer*2",	"vms", 0,
    "jisign",	"sign",	"integer",	"integer",	"vms", 0,

    "iiand",	"iand",	"integer*2",	"integer*2",	"vms", 0,
    "jiand",	"iand",	"integer",	"integer",	"vms", 0,

    "iior",	"ior",	"integer*2",	"integer*2",	"vms", 0,
    "jior",	"ior",	"integer",	"integer",	"vms", 0,

    "iieor",	"ieor",	"integer*2",	"integer*2",	"vms", 0,
    "jieor",	"ieor",	"integer",	"integer",	"vms", 0,

    "inot",	"not",	"integer*2",	"integer*2",	"vms", 0,
    "jnot",	"not",	"integer",	"integer",	"vms", 0,

    "iishft",	"ishft",	"integer*2",	"integer*2",	"vms", 0,
    "jishft",	"ishft",	"integer",	"integer",	"vms", 0,

    "iibits",	"ibits",	"integer*2",	"integer*2",	"vms", 0,
    "jibits",	"ibits",	"integer",	"integer",	"vms", 0,

    "iibset",	"ibset",	"integer*2",	"integer*2",	"vms", 0,
    "jibset",	"ibset",	"integer",	"integer",	"vms", 0,

    "bitest",	"btest",	"integer*2",	"integer*2",	"vms", 0,
    "bjtest",	"btest",	"integer",	"integer",	"vms", 0,

    "iibclr",	"ibclr",	"integer*2",	"integer*2",	"vms", 0,
    "jibclr",	"ibclr",	"integer",	"integer",	"vms", 0,

    "iishftc",	"ishftc",	"integer*2",	"integer*2",	"vms", 0,
    "jishftc",	"ishftc",	"integer",	"integer",	"vms", 0,

    0,0,0,0,0,0
} ;

isintr( name )
char * name ;
{
    struct intrRec * getintr() ;
    if ( getintr( name ) != NULL ) return(1) ;
    return(0) ;
}
	
char * rm_len() ;

char *
typeintr( name )
char * name ;
{
    struct intrRec * getintr(), * p ;

    if (( p = getintr( name )) != NULL ) return(rm_len(p->funtype)) ;
    return(NULL) ;
}

char *
standardintr( name )
char * name ;
{   
    struct intrRec * getintr(), * p ;

    if (( p = getintr( name )) != NULL ) return(p->standard) ;
    return(NULL) ;
}

char *
typeintr_ptr( p )
struct intrRec * p ;
{
    if ( p != NULL ) return(rm_len(p->funtype)) ;
    return(NULL) ;
}

char *
standardintr_ptr( p )
struct intrRec * p ;
{   
    if ( p != NULL ) return(p->standard) ;
    return(NULL) ;
}

static int inited = 0 ;

struct intrRec *
getintr( name )
char * name ;
{
    struct intrRec * p, * intrget() ;

    if (!inited) {init_intr() ; inited = 1 ;}

    return( intrget( name, 0 ) )  ;

#ifdef COMMENTOUT
    for (p = intrTab ; p->name ; p++ ) {
	/* if we've specified ansi only and still haven't found then
	   just quit -- 4-27-90 */
	if ( *(p->standard) == 'v' && sw_ansi ) {
	    return(NULL) ;
	}
	if ( strcmp(name, p->name) == 0 ) {
	    return(p) ;
	}
    }
    return( NULL ) ;
#endif
}

/* A hack.  4-27-90.
   Place a null at the first non lower case alphabetic.
   Other parts of the parser at this point can't understand
   length information with the VMS intrinsics, so we'll just
   lop off everything afterwards.  Note, this is lopping on
   the strings in the structure, so beware.
*/
char *
rm_len( str )
char * str ;
{
    char * p ;
    for (p = str ;*p;p++) {
	if ( *p < 'a' || *p > 'z' ) {
	    *p = 0 ;
	    break ;
	}
    }
    return(str) ;
}
	
static struct intrRec ** intrHash ;


struct intrRec *
newintrnode()
{
    struct intrRec * myMalloc(), * p ;
    p = myMalloc(sizeof( struct intrRec )) ;
    return(p) ;
}

char **
intrname(p)
struct intrRec *p ;
{
    char ** x ;
    x = &(p->name) ;
    return(x) ;
}

struct intrRec **
intrnext(p)
struct intrRec * p ;
{
    struct intrRec ** x ;
    x = &(p->next) ;
    return( x ) ;
}

struct intrRec *
intrget( name,flag )
char * name ;
{
    struct intrRec * symget() ;
    return( symget( name,newintrnode,intrname,intrnext,intrHash,flag)) ;
}

init_intr()
{
    struct intrRec * p, * q ;

    create_ht( &intrHash ) ;

    if (intrHash == NULL) fprintf(stderr,"init_intr(), can't make hash tab\n") ;

    /* load up intrinsics table */
    for (p = intrTab ; p->name ; p++ ) {
	q = intrget( p->name, 1 ) ;
	q->name = p->name ;
	q->relation = p->relation ;
	q->argtype = p->argtype ;
	q->funtype = p->funtype ;
	q->standard = p->standard ;
    }
}
