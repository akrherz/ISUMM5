/*
 * Written by D.L.Hansen Jan 1996
 * Input -> configure.user
 * Outputs-> include/config.INCL, include/defines.incl
 * 
 * Why?
 * Well, user sets "configure.user".  There are a number of
 * of variables that are set according to variables in "configure.user"
 * So this program parses "configure.user" and sets those variables!
 * config.INCL is used by include/Makefile and Run/Makefile
 *     to create parame.incl and mmlif respectively
 * defines.incl is included in all .F routines with #ifdef statements
 */

#include <stdio.h>
#include <string.h>

#ifndef FALSE
# define FALSE	0
#ifdef TRUE
# undef TRUE
#endif
# define TRUE	1
#endif

#define MAXDIMS 10
#define FRADDIMS 5

int main(int argc, char **argv)
{
    FILE *fp = 0;
    FILE *fc = 0;
    FILE *fd = 0;
    char line[255];
    int i;
    char *ch = NULL;
    char *substr = "IMPHYS";
    short need_ISOIL_equal_one = FALSE;
    short need_ISOIL_equal_three = FALSE;
    int isldim = 0;
    int ilddim = 0;
    int iplacedim = 0;
    int ipolar = 0;
    int isoil;
    int lx;

    fc = fopen("./include/config.INCL","w");
    /*
    fprintf(fc,"# This file is for Makefiles\n");
    fprintf(fc,"#\n"); */
    fd = fopen("./include/defines.incl.tmp","w");
    /*
    fprintf(fd,"# This file is for cpp\n");
    fprintf(fd,"#\n"); */

    fp = fopen("configure.user","r");
    for (i=0; fgets(line, 255, fp); i++) {
	ch = strstr(&(line[0]), substr);
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int im[MAXDIMS];
	    int imoist[MAXDIMS];
	    int idry[MAXDIMS];
	    int iexice=0,iexgr=0;
	    int iexms = 0;
	    int iice=0,iiceg=0;
	    short id1=0,id2=0,id3=0,id4=0,id5=0,id6=0,id7=0,id8=0,id9=0;

	    /* printf("line #%d -> %s\n", i, line);*/


	    sscanf(ch, "IMPHYS = \"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\"",
		   &im[0],&im[1],&im[2],&im[3],
		   &im[4],&im[5],&im[6],&im[7],
		   &im[8],&im[9]);
	    for (j=0;j<MAXDIMS;j++){
		if (im[j] == 1) {
		    imoist[j] = 1;
		    idry[j] = 1;
		    if (id1++ < 1) 
			fprintf(fd, "#define IMPHYS1 1\n");
		} else if (im[j] == 2) {
		    imoist[j] = 1;
		    idry[j] = 0;
		    if (id2++ < 1) 
			fprintf(fd, "#define IMPHYS2 1\n");
		} else if (im[j] == 3) {
		    imoist[j] = 2;
		    idry[j] = 0;
		    iexms = 1;
		    if (id3++ < 1) 
			fprintf(fd, "#define IMPHYS3 1\n");
		} else if (im[j] == 4) {
		    imoist[j] = 2;
		    idry[j] = 0;
		    iexice = 1;
		    iexms = 1;
		    if (id4++ < 1) 
			fprintf(fd, "#define IMPHYS4 1\n");
		} else if (im[j] == 5) {
		    imoist[j] = 2;
		    idry[j] = 0;
		    iexms = 1;
		    iice = 1;
		    if (id5++ < 1) 
			fprintf(fd, "#define IMPHYS5 1\n");
		} else if (im[j] == 6) {
		    imoist[j] = 2;
		    idry[j] = 0;
		    iexms = 1;
		    iexgr = 1;
		    iice = 1;
		    iiceg = 1;
		    if (id6++ < 1) 
			fprintf(fd, "#define IMPHYS6 1\n");
		} else if (im[j] == 7) {
		    imoist[j] = 2;
		    idry[j] = 0;
		    iexgr = 2;
		    iice = 1;
		    iexms = 1;
		    iiceg = 1;
		    if (id7++ < 1) 
			fprintf(fd, "#define IMPHYS7 1\n");
		} else if (im[j] == 8) {
		    imoist[j] = 2;
		    idry[j] = 0;
		    iexgr = 2;
		    iice = 1;
		    iexms = 1;
		    iiceg = 1;
		    if (id8++ < 1)
			fprintf(fd, "#define IMPHYS8 1\n");
		} else{
		    fprintf(stderr,"=========== ERROR ===============\n");
		    fprintf(stderr,"Illegal Value for IMPHYS!!\n");
		    fprintf(stderr,"IMPHYS[%d] = %d\n",j,im[j]);
		    fprintf(stderr,"=================================\n");
		    exit(-1);
		}
	    }

	    fprintf(fc, "DRY = \"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\"\n",
		   idry[0],idry[1],idry[2],idry[3],
		   idry[4],idry[5],idry[6],idry[7],
		   idry[8],idry[9]);
	    fprintf(fc, "MOIST = \"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\"\n",
		   imoist[0],imoist[1],imoist[2],imoist[3],
		   imoist[4],imoist[5],imoist[6],imoist[7],
		   imoist[8],imoist[9]);
	    fprintf(fc, "EXMS = %d\n",iexms);
	    fprintf(fc, "ICE = %d\n",iice);
	    fprintf(fc, "ICEG = %d\n",iiceg);
	    fprintf(fc, "EXICE = %d\n",iexice);
	    fprintf(fc, "EXGR = %d\n",iexgr);
	}

	/* ICUPA */
	ch = strstr(&(line[0]), "ICUPA");
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int icupa[MAXDIMS];
	    int iarasc = 0;
	    int kffc = 0;
	    short id1=0,id2=0,id3=0,id4=0,id5=0,id6=0,id7=0,id8=0,id9=0;

	    sscanf(ch, "ICUPA = \"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\"",
		   &icupa[0],&icupa[1],&icupa[2],&icupa[3],
		   &icupa[4],&icupa[5],&icupa[6],&icupa[7],
		   &icupa[8],&icupa[9]);
	    for (j=0;j<MAXDIMS;j++){
		if (icupa[j] == 1) {
		    ;
		} else if (icupa[j] == 2) {
		    if (id2++ < 1) 
			fprintf(fd, "#define ICUPA2 1\n");
		} else if (icupa[j] == 3) {
		    if (id3++ < 1) 
			fprintf(fd, "#define ICUPA3 1\n");
		} else if (icupa[j] == 4) {
		    if (id4++ < 1) 
			fprintf(fd, "#define ICUPA4 1\n");
		    iarasc = 1;
		} else if (icupa[j] == 5) {
		    if (id5++ < 1) 
			fprintf(fd, "#define ICUPA5 1\n");
		    kffc = 1;
		} else if (icupa[j] == 6) {
		    if (id6++ < 1) 
			fprintf(fd, "#define ICUPA6 1\n");
		    kffc = 1;
		} else if (icupa[j] == 7) {
		    if (id7++ < 1) 
			fprintf(fd, "#define ICUPA7 1\n");
		} else if (icupa[j] == 8) {
		    if (id8++ < 1) 
			fprintf(fd, "#define ICUPA8 1\n");
		    kffc = 1;
		} else{
		    fprintf(stderr,"=========== ERROR ===============\n");
		    fprintf(stderr,"Illegal Value for ICUPA!!\n");
		    fprintf(stderr,"ICUPA[%d] = %d\n",j,icupa[j]);
		    fprintf(stderr,"=================================\n");
		    exit(-1);
		}
	    }
	    fprintf(fc, "ARASC = %d\n",iarasc);
	    fprintf(fc, "KFFC = %d\n",kffc);
	}
    
	/* FRAD */
	ch = strstr(&(line[0]), "FRAD");
	if (ch != NULL && (line[0] != '#')) {
	    int s1=FALSE,s2=FALSE,s3=FALSE,s4=FALSE;
	    int j;
	    int frad[FRADDIMS];
	    int irrdim = 0;
	    short id1=0,id2=0,id3=0,id4=0,id5=0,id6=0,id7=0,id8=0,id9=0;

	    sscanf(ch, "FRAD = \"%d,%d,%d,%d,%d\"",
		   &frad[0],&frad[1],&frad[2],&frad[3],
		   &frad[4]);
	    for (j=0;j<FRADDIMS;j++){
	      if (frad[j] == 2) {
		irrdim = 1;
		s2 = TRUE;
	      } else if (frad[j] == 3) {
		irrdim = 1;
		s3 = TRUE;
	      } else if (frad[j] == 4) {
		irrdim = 1;
		s4 = TRUE;
	      } else if (frad[j] == 1) {
		s1 = TRUE;
	      }
	    }
	    if (s1 == TRUE) {
	      fprintf(fd,"#define FRAD1 1\n");
	    }
	    if (s2 == TRUE) {
	      fprintf(fd,"#define FRAD2 1\n");
	    }
	    if (s3 == TRUE) {
	      fprintf(fd,"#define FRAD3 1\n");
	    }
	    if (s4 == TRUE) {
	      fprintf(fd,"#define FRAD4 1\n");
	    }
	    fprintf(fc, "RDDIM = %d\n",irrdim);
	}

	/* IBLTYP */
	ch = strstr(&(line[0]), "IBLTYP");
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int iblt[MAXDIMS];
	    int nav = 0;
	    int nav2 = 0;
	    int nav3 = 0;
	    int gspbl = 0;
	    short id0=0,id1=0,id2=0,id3=0,id4=0,id5=0,id6=0,id7=0,id8=0,id9=0;

	    sscanf(ch, "IBLTYP = \"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\"",
		   &iblt[0],&iblt[1],&iblt[2],&iblt[3],
		   &iblt[4],&iblt[5],&iblt[6],&iblt[7],
		   &iblt[8],&iblt[9]);
	    for (j=0;j<MAXDIMS;j++){
		if (iblt[j] == 1) {
		    if (id1++ < 1) 
			fprintf(fd, "#define IBLT1 1\n");
		} else if (iblt[j] == 2) {
		    if (id2++ < 1) 
			fprintf(fd, "#define IBLT2 1\n");
		} else if (iblt[j] == 3) {
		    nav = 1;
		    nav2 = 1;
		    if (id3++ < 1) 
			fprintf(fd, "#define IBLT3 1\n");
		} else if (iblt[j] == 0) {
		    if (id0++ < 1) 
			fprintf(fd, "#define IBLT0 1\n");
		} else if (iblt[j] == 4) {
		    if (id4++ < 1) 
		    nav = 1;
		    nav2 = 1;
			fprintf(fd, "#define IBLT4 1\n");
		} else if (iblt[j] == 5) {
		    if (id5++ < 1) {
			fprintf(fd, "#define IBLT5 1\n");
		      need_ISOIL_equal_one = TRUE;
		    }
		} else if (iblt[j] == 6) {
		    gspbl = 1;
		    if (id6++ < 1)
			fprintf(fd, "#define IBLT6 1\n");
	        } else if (iblt[j] == 7) {
       		     if (id7++ < 1)
			fprintf(fd, "#define IBLT7 1\n");
		      need_ISOIL_equal_three = TRUE;
		} else{
		    fprintf(stderr,"=========== ERROR ===============\n");
		    fprintf(stderr,"Illegal Value for IBLTYP!!\n");
		    fprintf(stderr,"IBLTYP[%d] = %d\n",j,iblt[j]);
		    fprintf(stderr,"=================================\n");
		    exit(-1);
		}
	    }
	    fprintf(fc, "NAV = %d\n",nav);
	    fprintf(fc, "NAV2 = %d\n",nav2);
	    fprintf(fc, "NAV3 = %d\n",nav3);
	    fprintf(fc, "GSPBL = %d\n",gspbl);

	} /* IBLTYP */

	/* IPOLAR */
	ch = strstr(&(line[0]), "IPOLAR");
	if (ch != NULL && (line[0] != '#')) {

	    sscanf(ch, "IPOLAR = %d", &ipolar);
	} /* IPOLAR */

	/* ISOIL */
	ch = strstr(&(line[0]), "ISOIL");
	if (ch != NULL && (line[0] != '#')) {
	    int j;

	    sscanf(ch, "ISOIL = %d", &isoil);
	    if (isoil == 0) {
		isldim = 0;
		ilddim = 0;
		lx = 1;
		fprintf(fd, "#define SOIL0 1\n");
	    } else if (isoil == 1 && ipolar == 0) {
		isldim = 1;
		ilddim = 0;
		lx = 6;
		fprintf(fd, "#define SOIL1 1\n");
	    } else if (isoil == 1 && ipolar == 1) {
		isldim = 1;
		ilddim = 0;
		lx = 8;
		fprintf(fd, "#define SOIL1 1\n");
	    } else if (isoil == 2) {
		isldim = 0;
		ilddim = 1;
		lx = 4;
		fprintf(fd, "#define SOIL2 1\n");
	    } else if (isoil == 3) {
		isldim = 0;
		ilddim = 1;
		lx = 2;
		fprintf(fd, "#define SOIL3 1\n");
	    }
	    fprintf(fc, "SLDIM = %d\n",isldim);
	    fprintf(fc, "LDDIM = %d\n",ilddim);
	    fprintf(fc, "PLACEDIM = %d\n",iplacedim);
	    fprintf(fc, "MLX = %d\n",lx);
	}
	/* ISOIL */

	/* ISHALLO */
	ch = strstr(&(line[0]), "ISHALLO");
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int ishallo[MAXDIMS];
	    short id1=0;
	    for (j=0;j<MAXDIMS;j++) ishallo[j] = 0;

	    sscanf(ch, "ISHALLO = \"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\"",
		   &ishallo[0],&ishallo[1],&ishallo[2],&ishallo[3],
		   &ishallo[4],&ishallo[5],&ishallo[6],&ishallo[7],
		   &ishallo[8],&ishallo[9]);
	    for (j=0;j<MAXDIMS;j++){
		if (ishallo[j] == 1) {
		    if (id1++ < 1) 
			fprintf(fd, "#define ISHALLO1 1\n");
		} else if (ishallo[j] == 0) {
		  continue;
		} else{
		    fprintf(stderr,"=========== ERROR ===============\n");
		    fprintf(stderr,"Illegal Value for ISHALLO!!\n");
		    fprintf(stderr,"ISHALLO[%d] = %d\n",j,ishallo[j]);
		    fprintf(stderr,"=================================\n");
		    exit(-1);
		}
	    }
	} /* ISHALLO */

	/* FDDAGD */
	ch = strstr(&(line[0]), "FDDAGD");
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int FDDAGD;

	    sscanf(ch, "FDDAGD = %d", &FDDAGD);
	    if (FDDAGD == 0) {
		fprintf(fd,"#define FDDAGD0 1\n");
	    } else if (FDDAGD == 1) {
		fprintf(fd,"#define FDDAGD1 1\n");
	    }
	} /*FDDAGD*/

	/* FDDAOBS */
	ch = strstr(&(line[0]), "FDDAOBS");
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int FDDAOBS;
	    sscanf(ch, "FDDAOBS = %d", &FDDAOBS);
	    if (FDDAOBS == 0) {
		fprintf(fd,"#define FDDAOBS0 1\n");
	    } else if (FDDAOBS == 1) {
		fprintf(fd,"#define FDDAOBS1 1\n");
	    }
	} /*FDDAOBS*/

	/* MAXNES */
	ch = strstr(&(line[0]), "MAXNES");
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int MAXNES;
	    sscanf(ch, "MAXNES = %d", &MAXNES);
	    if (MAXNES > 1) {
		fprintf(fd,"#define NESTED 1\n");
	    }
	} /*MAXNES*/

	/* MPHYSTBL */
	ch = strstr(&(line[0]), "MPHYSTBL");
	if (ch != NULL && (line[0] != '#')) {
	    int j;
	    int MPHYSTBL;
	    sscanf(ch, "MPHYSTBL = %d", &MPHYSTBL);
	    if (MPHYSTBL == 0) {
		fprintf(fd,"#define MPHYSTBL0 1\n");
	    } else if (MPHYSTBL == 1) {
		fprintf(fd,"#define MPHYSTBL1 1\n");
	    } else if (MPHYSTBL == 2) {
		fprintf(fd,"#define MPHYSTBL2 1\n");
	    }
	} /*MPHYSTBL*/
/* ADDED to support VECTOR (NEC and FUJI) with MPP code , JM 000412 */
        /* VECTOR */
        ch = strstr(&(line[0]), "VECTOR");
        if (ch != NULL && (line[0] != '#')) {
            int j;
            int VECTOR;
            sscanf(ch, "VECTOR = %d", &VECTOR);
            if (VECTOR == 1) {
                fprintf(fd,"#define VECTOR 1\n");
            }
        } /*MPHYSTBL*/

/* [DMF], added for lack of leap year in GCM driven runs. */
        /* NOLEAP */
        ch = strstr(&(line[0]), "NOLEAP");
        if (ch != NULL && (line[0] != '#')) {
            int j;
            int NOLEAP;
            sscanf(ch, "NOLEAP = %d", &NOLEAP);
            if (NOLEAP == 1) {
                fprintf(fd,"#define NOLEAP 1\n");
            }
        } /*NOLEAP*/
/* [DMF], added for lack of leap year in GCM driven runs. */

/* [DMF], added for exponential b.c. */
        /* EXPOBC */
        ch = strstr(&(line[0]), "EXPOBC");
        if (ch != NULL && (line[0] != '#')) {
            int j;
            int EXPOBC;
            sscanf(ch, "EXPOBC = %d", &EXPOBC);
            if (EXPOBC == 1) {
                fprintf(fd,"#define EXPOBC 1\n");
            }
        } /*EXPOBC*/
/* [DMF], added for exponential b.c. */

/* [DMF], Added to read width of boundary forcing frame.
        /* NSPGX */
        ch = strstr(&(line[0]), "NSPGX");
        if (ch != NULL && (line[0] != '#')) {
            int j;
            int NSPGX;
            sscanf(ch, "NSPGX = %d", &NSPGX);
            fprintf(fc,"NSPGX = %d\n", NSPGX);
        } /*NSPGX*/

        /* NSPGD */
        ch = strstr(&(line[0]), "NSPGD");
        if (ch != NULL && (line[0] != '#')) {
            int j;
            int NSPGD;
            sscanf(ch, "NSPGD = %d", &NSPGD);
            fprintf(fc,"NSPGD = %d\n", NSPGD);
        } /*NSPGD*/
/* [DMF], Added to read width of boundary forcing frame. */


    }

    if (need_ISOIL_equal_one == TRUE) {
      if (isoil == 0) {
	printf("========================= ERROR ========================\n");
	printf("When IBLTYP is set to 5, then ISOIL must equal 1 !!!! \n");
	printf("========================= ERROR ========================\n");
	exit(-1);
      }
    }

    if (need_ISOIL_equal_three == TRUE) {
      if (isoil != 3) {
	printf("========================= ERROR ========================\n");
	printf("When IBLTYP is set to 7, then ISOIL must equal 3 !!!! \n");
	printf("========================= ERROR ========================\n");
	exit(-1);
      }
    }

    fclose(fp);
    fclose(fc);
    fclose(fd);

    exit(0);
}
