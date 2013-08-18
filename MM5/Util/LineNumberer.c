/*
 *
 * Written by d.l.hansen Fall 1996
 * 
 * This program is sensitive to the format that cvs diff produces.
 * If in future releases of cvs the diff output format changes,
 * this program will have to be changed.
 *
 * 1. LineNumberer -F param.F
 *    Will produce a complete set of linenumbers in param.F
 * 2. LineNumberer -U update.mods -d 23AUG96 -v > renumber.log
 *    Will RENUMBER lines that have been CHANGED or ADDED.
 *    This requires the diff file update.mods.  Example:
 *          cvs diff -r test-2-8 > update.mods
 *    So if you added the lines
 * #ifdef NESTED
 * #endif       
 *    to your code (since you tagged test-2-8, that is), the lines are numbered
 * #ifdef NESTED                                                                    23AUG96.1
 * #endif                                                                           23AUG96.2
 * 
 *    This option will also create a script called "rename.csh" which when 
 *    excuted will copy the newly renumbered *.F.l files over the orig *.F files.
 *
 * 3. LineNumberer -U update.mods -C
 *    This will CLEAN UP the update.mods file (output file is "update.mod.new")
 * a. All file entries specfied in "Util/badentries" are removed.
 * b. If a file entry has NO differences from the tagged version, it is removed.
 * c. The ",v" and mesouser prefix are removed from the file names.
 * d. Lines beginning with "retrieving" are removed.
 * e. Lines beginning with "cvs diff" are removed.
 *
 * 4. LineNumberer -c param.F
 *    Will capitalize all lines that are not cpp directives (e.g.,#include) in param.F
 */
#define EXTERN 
#define FRED 0

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAXDIMS 10
#define XNBUFSIZ  255
#define DLVL 1
#define PREFIX_SKIP 15

#include "filecache.h"

typedef enum { ADDED,CHANGED } typeochange;

struct myufile {
  short int doit;
  short int cleanup;
  char name[BUFSIZ];
  char cvsroot[BUFSIZ];
  int  cvsroot_len;
  char linemod[100];				  /* tag to stick at end of line */
  FILE *fd;
  long int count;
  long int lcount;				  /* Where I'm at in the diff file */
  struct m5FileCache* files;
  char *dlines[MAXDLINES];
  int num_dlines;				  /* num of changed lines (indicated by ">") */
  int dl_loc[MAXDLINES];			  /* actual line nums of changed lines */
  int num_dl_loc;				  /* num of changed lines (indicated by "c" line) */
  typeochange tc[MAXDLINES];
} uf;

struct mycfile {
  char oname[BUFSIZ];                             /* Output if cleaning up mods file */
  FILE *fo;
  char *store[MAXDLINES];
  long int numstored;
  char *bad[200];
  int numbad;
} cf;

void ReNumberLines(const char *fname);
void ToUpper(const char *fname);
int Update();
int CleanUp();
int ReadFileEntry();
int GetBadEntries();

void init_uf()
{
  int i=0;
  uf.doit = FALSE;  
  uf.cleanup = FALSE;
  uf.name[0] = '\0';
  uf.linemod[0] = '\0';
  uf.cvsroot[0] = '\0';
  uf.cvsroot_len = 0;
  uf.fd = NULL;
  uf.count = 0;
  uf.lcount = 0;
  for (i=0;i<MAXDLINES;i++) {
    uf.dlines[i] = NULL;
  }
  uf.num_dlines = 0;
  uf.files = new_FileCache();
  uf.num_dl_loc = 0;

  /* Cleanup Structure */
  cf.oname[0] = '\0';
  cf.fo = NULL;
  for (i=0;i<MAXDLINES;i++) cf.store[i] = NULL;
  for (i=0;i<200;i++) cf.bad[i] = NULL;
  cf.numstored=0;
  cf.numbad=0;
}

void Xnerr(const char *ms)
{
  printf("Runtime ERROR: %s caused exit.\n",ms);
  exit(-1);
}

void NumberLines(const char *fname)
{
  FILE *fd = 0;
  FILE *fo = 0;
  char tmps[BUFSIZ];
  char line[255];
  char tackon[BUFSIZ];
  char fnoout[BUFSIZ];
  int i,j;
  char *ch = NULL;
  if (fname == NULL) Xnerr("38383893");
  
  fd = fopen(fname,"r+");
  sprintf(fnoout,"%s.l",fname);
  fo = fopen(fnoout,"w");

  strcpy(tackon,fname);
  ch = strstr(tackon,".F");
  if (ch) {
    *ch='\0';
  } else {
    ch = strstr(tackon,".incl");
    if (ch) *ch='\0';
  }
  for (ch=&(tackon[0]);*ch;ch++) {
    if (islower(*ch)) *ch = toupper(*ch);
  }
  for (j=79; j>=0; j--) line[j] = ' ';

  for (i=1; fgets(line, 255, fd); i++) {
    line[80] = '\0';
    for (j=79; j>=0; j--) {
      if (line[j] == '\n') line[j] = ' ';
      if (line[j] == '\0') line[j] = ' ';
    }
    
    sprintf(tmps,"%-81.81s%s.%d\n",&(line[0]),tackon,i);
    fprintf(fo,"%s",tmps);
    for (j=79; j>=0; j--) line[j] = ' ';
  }
  fclose(fd);
  fclose(fo);
}

int main(int argc, char **argv)
{
  int c, errflg=0;
  char *ch=0;
  char *cptr=0;

  init_uf();
  ch = getenv("CVSROOT");
  if (ch == NULL) {
    Xnerr ("100 (CVSROOT is NOT set!!!)");
  } else {
    strcpy(uf.cvsroot,ch);
    uf.cvsroot_len = strlen(uf.cvsroot); 
  }
  debug = -1;
  while ((c = getopt(argc, argv, "F:c:U:d:vC")) != EOF){
    switch (c) {
    case 'F':
      for (ch = optarg; ch != NULL; ch = cptr+1){
	if((cptr = strchr(ch, ','))) {
	  char tmps[XNBUFSIZ];
	  strncpy(tmps, ch, strlen(ch)-strlen(cptr));
	  tmps[strlen(ch)-strlen(cptr)] = '\0';
	  NumberLines(tmps);
	} else {
	  char tmps[XNBUFSIZ];
	  strcpy(tmps,ch);
	  NumberLines(tmps);
	  break;
	}
      }
      break;
    case 'c':
      for (ch = optarg; ch != NULL; ch = cptr+1){
	if((cptr = strchr(ch, ','))) {
	  char tmps[XNBUFSIZ];
	  strncpy(tmps, ch, strlen(ch)-strlen(cptr));
	  tmps[strlen(ch)-strlen(cptr)] = '\0';
	  ToUpper(tmps);
	} else {
	  char tmps[XNBUFSIZ];
	  strcpy(tmps,ch);
	  ToUpper(tmps);
	  break;
	}
      }
      break;
    case 'U':
      strcpy(uf.name,optarg);
      uf.doit = TRUE;
      break;
    case 'C':
      uf.cleanup = TRUE;
      break;
    case 'd':
      strcpy(uf.linemod,optarg);
      break;
    case 'v':
      debug = 100;
      break;
    default:
      break;
    }
  }
  if (uf.doit == TRUE) {
    if (debug>DLVL)
      printf("CVSROOT = \"%s\" (len=%d)\n",uf.cvsroot,uf.cvsroot_len);
    if (uf.cleanup == TRUE) {
      CleanUp();
      exit(0);
    }
    if (uf.linemod[0] == '\0') {
      printf("You MUST specify the line modifier using the -d option!\n");
      Xnerr("3984849");
    }
    if (debug>DLVL)
      printf("Line Modifier = \"%s\".\n",uf.linemod);
    Update();
    free_FileCache(uf.files);
  }
}

int Update()
{
  char line[BUFSIZ];
  int i;
  if (uf.name == NULL) Xnerr("37595 (No CVS diff file!)");
  if (uf.doit != TRUE) Xnerr("37489");
  if (debug>DLVL)
    printf("CVS diff file --> %s\n",uf.name);

  uf.fd = fopen(uf.name,"r+");
  if (uf.fd == NULL) Xnerr("683949 (open failed!)");
  for (i=0; fgets(line, BUFSIZ, uf.fd); i++) {
    int len=strlen(line);
    uf.lcount += 1;
    if (len>0) {
      if (line[0] == '=') {
	int j = ReadFileEntry();
	if (j == -1) {
	  struct file_header *head=0;
	  FILE *rnsh = 0;
	  int fcount=0;
	  rnsh = fopen("./rename.csh","w");
	  fprintf(rnsh,"#! /bin/csh -f\n");
	  for (head = uf.files->first; head != NULL;head=head->next){
	    if (head->p->active == TRUE) {
	      fcount += 1;
	      printf("Modified file %s\n",head->p->name);
	      fprintf(rnsh,"mv %s.l %s\n",head->p->name, head->p->name);
	    }
	  }	  
	  printf("Modified %d files\n",fcount);
	  printf ("We are done!\n");
	  fclose(rnsh);
	  chmod("./rename.csh",S_IRWXU|S_IRWXG|S_IRWXO);
	  Xnerr("0 (STOP)");
	}
      }
    }
  }
  fclose(uf.fd);
  return(0);
}

void Xnfree(char *befree)
{
  void *lp= befree;
  free(lp);
}

char *Xnmem (unsigned size)
{
  char   *ptr=0;
  if (size <= 0) Xnerr("46578");
  ptr = (char *) malloc((size_t)size);
  if (ptr == NULL) Xnerr("8928 (Out of memory)");
  memset((char *)ptr, 0, (int)size);
  return ptr;
}

int ReadFileEntry()
{
  int j;
  char tmps[BUFSIZ];
  struct m5File *file=NULL;
  char key[BUFSIZ]; 
  char btest[BUFSIZ]; 

  if (uf.fd == NULL) Xnerr("6878484");
  if (uf.num_dlines > 0) Xnerr("399292");
  if (fgets(scratch, BUFSIZ, uf.fd) == NULL)
    return(-1);
  sscanf(scratch,"%s",btest);
  uf.lcount += 1;

  while (strncmp(btest,"Index:",6)) {
    sscanf(scratch,"%s", key);
    if (!strncmp(key,"RCS",3)) {
      char *ch=NULL,*nm=NULL;
      if (debug>DLVL) {
	printf("====================================================\n");
	printf("Found Repository Entry: %s\n",&(scratch[10]));
      }
      ch = strstr(scratch,",v");
      if (ch != NULL) ch[0] = '\0';
      
      /* Different "module" than MM5? */
      ch = strstr(scratch,"TERRAIN");
      if (ch != NULL) {
	nm = &(scratch[PREFIX_SKIP+4+(uf.cvsroot_len)]);          /* 4 ch -> "RAIN" */
      } else {
	ch = strstr(scratch,"REGRID");
	if (ch != NULL) {
	  nm = &(scratch[PREFIX_SKIP+3+(uf.cvsroot_len)]);	  /* 5 ch -> "RID" */
	} else {
	  ch = strstr(scratch,"GRAPH");
	  if (ch != NULL) {
	    nm = &(scratch[PREFIX_SKIP+2+(uf.cvsroot_len)]);	  /* 2 ch -> "PH" */
	  } else {
	    ch = strstr(scratch,"INTERPF");
	    if (ch != NULL) {
	      nm = &(scratch[PREFIX_SKIP+4+(uf.cvsroot_len)]);	  /* 3 ch -> "ERPF" */
	    } else {
	      ch = strstr(scratch,"RAWINS");
	      if (ch != NULL) {
		nm = &(scratch[PREFIX_SKIP+3+(uf.cvsroot_len)]);  /* 3 ch -> "INS" */
	    } else {
	      ch = strstr(scratch,"LITTLE_R");
	      if (ch != NULL) {
		nm = &(scratch[PREFIX_SKIP+5+(uf.cvsroot_len)]);  /* 3 ch -> "TLE_R" */
	    } else {
	      ch = strstr(scratch,"NESTDOWN");
	      if (ch != NULL) {
		nm = &(scratch[PREFIX_SKIP+5+(uf.cvsroot_len)]);  /* 3 ch -> "TDOWN" */
	      } else {
		nm = &(scratch[PREFIX_SKIP+(uf.cvsroot_len)]);	  /* Just "MM5" */
	      }
	    }
	    }
	    }
	  }
	}
      }

      ch = strstr(scratch,".F");
      if (ch != NULL) {
	if (debug>DLVL) 
	  printf("Found File Entry: %s\n",nm);
	file = FileFindorAdd(uf.files,nm);
	if (file == NULL) Xnerr("3884934");
      } else {
	ch = strstr(scratch,".incl");
	if (ch != NULL) {
	  if (debug>DLVL) 
	    printf("Found File Entry: %s\n",nm);
	  file = FileFindorAdd(uf.files,nm);
	  if (file == NULL) Xnerr("37283");
	}
      }
    } else if (isdigit(key[0])) {
      if (file != NULL) {
	char *ch=NULL;
	ch = strstr(scratch,"c");
	if (ch==NULL) {
	  ch = strstr(scratch,"a");
	  if (ch==NULL) {
	    ch = strstr(scratch,"d");
	    if (ch==NULL) {
	      printf("%s",scratch);
	      Xnerr("783293943");
	    } else {
	      ch += 1;
	      printf("%s: Deleted Line(s) %s",file->name,ch);
	    }
	  } else {
	    int lstj = -1;
	    char *cptr=ch+1;	/* skip to beginning of numbers */
	    for (ch=cptr;ch != NULL; ch = cptr+1){
	      if((cptr = strchr(ch, ','))) {
		strncpy(tmps, ch, strlen(ch)-strlen(cptr));
		tmps[strlen(ch)-strlen(cptr)] = '\0';
		if (uf.num_dl_loc > MAXDLINES) Xnerr("7738383292");
		j = atoi(tmps);
		lstj=j;
	      } else {
		strcpy(tmps,ch);
		if (uf.num_dl_loc > MAXDLINES) Xnerr("77383293");
		j = atoi(tmps);
		if (lstj != -1) {
		  for (;lstj<=j;lstj+=1) {
		    uf.tc[uf.num_dl_loc] = ADDED;
		    uf.dl_loc[uf.num_dl_loc++] = lstj;
		  }
		  lstj=j;
		} else {
		  uf.tc[uf.num_dl_loc] = ADDED;
		  uf.dl_loc[uf.num_dl_loc++] = j;
		}
		break;
	      }
	    }
	  }
	} else {
	  int lstj = -1;
	  char *cptr=ch+1;	/* skip to beginning of numbers */
	  for (ch=cptr;ch != NULL; ch = cptr+1){
	    if((cptr = strchr(ch, ','))) {
	      strncpy(tmps, ch, strlen(ch)-strlen(cptr));
	      tmps[strlen(ch)-strlen(cptr)] = '\0';
	      if (uf.num_dl_loc > MAXDLINES) Xnerr("7738383292");
	      j = atoi(tmps);
	      lstj=j;
	    } else {
	      strcpy(tmps,ch);
	      if (uf.num_dl_loc > MAXDLINES) Xnerr("77383293");
	      j = atoi(tmps);
	      if (lstj != -1) {
		for (;lstj<=j;lstj+=1) {
		  uf.tc[uf.num_dl_loc] = CHANGED;
		  uf.dl_loc[uf.num_dl_loc++] = lstj;
		}
		lstj=j;
	      } else {
		uf.tc[uf.num_dl_loc] = CHANGED;
		uf.dl_loc[uf.num_dl_loc++] = j;
	      }
	      break;
	    }
	  }
	}
      }
    } else if (!strncmp(key,">",1)) {
      if (file != NULL) {
	if (uf.num_dlines > MAXDLINES) Xnerr("738383292");
	uf.dlines[uf.num_dlines++] = strdup(&(scratch[2]));
      }
    }
    if (fgets(scratch, BUFSIZ, uf.fd) == NULL) {
      printf("Reached the end of the update file.  You should be processing a .F or .incl file.\n");
      printf("If not, please send a bug report to dhansen@ncar.ucar.edu\n");

      /* -------------------- Begin processing ------------------*/
      if (uf.num_dlines != 0) {
	int i;
	file->active = TRUE;
	for (i=0;i<uf.num_dlines;i++) {
	  printf("%s:%s",file->name,uf.dlines[i]);
	  if (uf.tc[i] == ADDED) {
	    printf("--> line #%d (ADDED)\n",uf.dl_loc[i]);
	  } else if (uf.tc[i] == CHANGED) {
	    printf("--> line #%d (CHANGED)\n",uf.dl_loc[i]);
	  } else{
	    Xnerr("387894499");
	  }
	}
      }
      ModifyFile(file);
      if (uf.num_dlines != 0) {
	printf("Current TOTAL of number lines modified = %d\n",uf.count);
      }
      
      /* Done with that file - Now do cleanup of uf */
      if (uf.num_dlines != 0) {
	int i;
	for (i=0;i<uf.num_dlines;i++) {
	  if (uf.dlines[i] != NULL) Xnfree(uf.dlines[i]);
	}
	uf.num_dlines = uf.num_dl_loc = 0;
      }  
      /* -------------------- End processing ------------------*/
      return(-1);
    }
    sscanf(scratch,"%s",btest);
    uf.lcount += 1;
  }
  if (uf.num_dlines != 0) {
    int i;
    file->active = TRUE;
    for (i=0;i<uf.num_dlines;i++) {
      printf("%s:%s",file->name,uf.dlines[i]);
      if (uf.tc[i] == ADDED) {
	printf("--> line #%d (ADDED)\n",uf.dl_loc[i]);
      } else if (uf.tc[i] == CHANGED) {
	printf("--> line #%d (CHANGED)\n",uf.dl_loc[i]);
      } else{
	Xnerr("387894499");
      }
    }
  }
  ModifyFile(file);
  if (uf.num_dlines != 0) {
    printf("Current TOTAL of number lines modified = %d\n",uf.count);
  }

  /* Done with that file - Now do cleanup of uf */
  if (uf.num_dlines != 0) {
    int i;
    for (i=0;i<uf.num_dlines;i++) {
      if (uf.dlines[i] != NULL) Xnfree(uf.dlines[i]);
    }
    uf.num_dlines = uf.num_dl_loc = 0;
  }  
  return(0);
}

void ModifyFile(struct m5File *file)
{
  int indx=0,i=0;
  int nextline;
  char tmps[255];
  char bline[255];
  char bline_tmp[255];
  if (file==NULL) return;
  if (file->active == FALSE) return;

  file->fd = fopen(file->name,"r+");
  if (file->fd == NULL) {
    printf("Unable to open %s\n",file->name);
    Xnerr("3895493");
  }
  printf("------- Modifying File %s --------\n",file->name);
  sprintf(file->oname,"%s.l",file->name);
  file->fo = fopen(file->oname,"w");
  if (file->fo == NULL) Xnerr("38973983");
  printf("Opening output file %s\n",file->oname);

  nextline = uf.dl_loc[indx];	             /* first line to change */
  for (i=1;fgets(bline_tmp, 255, file->fd);i++) {
    if (i == nextline) {
      /* Now actually modifying the line */
      int j=0;
      if (indx >= uf.num_dlines) {
	printf("indx = %d, uf.num_dlines = %d\n",indx,uf.num_dlines);
	Xnerr("839839022");
      }

      /*DPTEST*/
      memset((void*)bline,0,150);
      strcpy(bline,bline_tmp);
      /*DPTEST*/

      printf("%s",bline);
      
      bline[79] = '\0';
      for (j=78; j>=0; j--) {
	if (bline[j] == '\n') bline[j] = ' ';
	if (bline[j] == '\0') bline[j] = ' ';
      }
      sprintf(tmps,"%-81.81s%s.%d\n",&(bline[0]),uf.linemod,(uf.count+1));
      uf.count += 1;
      printf("becomes...\n%s",tmps);

      fprintf(file->fo,"%s",tmps);
      /* Now set nextline to point to the next line to mod */
      indx += 1;
      if (indx >= uf.num_dl_loc) {
	nextline = -1;
      } else {
	nextline = uf.dl_loc[indx];
      }
    } else {
      fprintf(file->fo,"%s",bline_tmp);
    }
    memset((void*)bline,0,100);
  }

  fclose(file->fd);
  fclose(file->fo);
}

int CleanUp()
{
  char line[BUFSIZ];
  int i;
  if (uf.name == NULL) Xnerr("37595 (No CVS diff file!)");
  if (uf.doit != TRUE) Xnerr("37489");
  sprintf(cf.oname,"%s.new",uf.name);
  if (debug>DLVL)
    printf("CVS diff file --> %s will become --> %s\n",uf.name,cf.oname);

  GetBadEntries();
  uf.fd = fopen(uf.name,"r+");
  if (uf.fd == NULL) Xnerr("683949 (open failed!)");
  cf.fo = fopen(cf.oname,"w");
  if (cf.fo == NULL) Xnerr("378489 (open failed!)");
  for (i=0; fgets(line, BUFSIZ, uf.fd); i++) {
    int len=strlen(line);
    uf.lcount += 1;
    if (len>0) {
      if (line[0] == '=') {
	int j = ReadCleanUpFileEntry();
	if (j == -1) {
	  Xnerr("38945040");
	}
      }
    }
  }
  fclose(uf.fd);
  fclose(cf.fo);
  for (i=0;i<cf.numbad;i++) {
    if (cf.bad[i] != NULL) Xnfree(cf.bad[i]);
  }  
  return(0);
}

int ReadCleanUpFileEntry()
{
  int j,i;
  char tmps[BUFSIZ];
  struct m5File *file=NULL;
  char key[BUFSIZ]; 
  char btest[BUFSIZ]; 

  if (uf.fd == NULL) Xnerr("6878484");
  if (cf.fo == NULL) Xnerr("6878484");
  if (uf.num_dlines > 0) Xnerr("399292");
  if (cf.numstored > 0) Xnerr("399292");
  if (fgets(scratch, BUFSIZ, uf.fd) == NULL)
    return(-1);
  sscanf(scratch,"%s",btest);
  uf.lcount += 1;

  while (strncmp(btest,"Index:",6)) {
    sscanf(scratch,"%s", key);
    if (!strncmp(key,"RCS",3)) {
      short int bad=FALSE;
      char *ch=NULL,*nm=NULL;
      if (debug>DLVL) {
	printf("====================================================\n");
	printf("Found Repository Entry: %s\n",&(scratch[10]));
      }
      ch = strstr(scratch,",v");
      if (ch != NULL) ch[0] = '\0';

      /* Different "module" than MM5? */
      ch = strstr(scratch,"TERRAIN");
      if (ch != NULL) {
	nm = &(scratch[PREFIX_SKIP+4+(uf.cvsroot_len)]);          /* 4 ch -> "RAIN" */
      } else {
	ch = strstr(scratch,"REGRID");
	if (ch != NULL) {
	  nm = &(scratch[PREFIX_SKIP+3+(uf.cvsroot_len)]);	  /* 5 ch -> "RID" */
	} else {
	  ch = strstr(scratch,"GRAPH");
	  if (ch != NULL) {
	    nm = &(scratch[PREFIX_SKIP+2+(uf.cvsroot_len)]);	  /* 2 ch -> "PH" */
	  } else {
	    ch = strstr(scratch,"INTERPF");
	    if (ch != NULL) {
	      nm = &(scratch[PREFIX_SKIP+4+(uf.cvsroot_len)]);	  /* 3 ch -> "ERPF" */
	    } else {
	      ch = strstr(scratch,"RAWINS");
	      if (ch != NULL) {
		nm = &(scratch[PREFIX_SKIP+3+(uf.cvsroot_len)]);  /* 3 ch -> "INS" */
	    } else {
	      ch = strstr(scratch,"LITTLE_R");
	      if (ch != NULL) {
		nm = &(scratch[PREFIX_SKIP+5+(uf.cvsroot_len)]);  /* 3 ch -> "TLE_R" */
	    } else {
	      ch = strstr(scratch,"NESTDOWN");
	      if (ch != NULL) {
		nm = &(scratch[PREFIX_SKIP+5+(uf.cvsroot_len)]);  /* 3 ch -> "TDOWN" */
	      } else {
		nm = &(scratch[PREFIX_SKIP+(uf.cvsroot_len)]);	  /* Just "MM5" */
	      }
	    }
	    }
	    }
	  }
	}
      }

      /*------------*/
      file = FileFindorAdd(uf.files,nm);
      if (file == NULL) Xnerr("3912834");
      if (cf.numbad == 0) {
	file->active = TRUE;
	if (cf.numstored != 0) Xnerr("7838392");
	cf.store[cf.numstored++] = strdup("===================================================================");
	cf.store[cf.numstored++] = strdup(nm);
	if (cf.numstored >= MAXDLINES) {
	  printf("File %s has too many diff lines! (%d)\n",file->name,cf.numstored);
	  Xnerr("382993");
	}
	if (FRED) printf("%d",cf.numstored);
      } else {
	for (i=0;i<cf.numbad;i++) {
	  ch = strstr(scratch,cf.bad[i]);
	  if (ch != NULL) bad = TRUE;
	}
	if (bad == TRUE) {
	  file->active = FALSE;
	  printf("Skipping BAD file entry %s\n",nm);
	} else {
	  file->active = TRUE;
	  if (cf.numstored != 0) Xnerr("728392");
	  cf.store[cf.numstored++] = strdup("===================================================================");
	  cf.store[cf.numstored++] = strdup(nm);
	  if (cf.numstored >= MAXDLINES) {
	    printf("File %s has too many diff lines! (%d)\n",file->name,cf.numstored);
	    Xnerr("382993");
	  }
	  if (FRED) printf("%d",cf.numstored);
	}
      }
    } else if (!strncmp(key,">",1)) {
      if (file->active == TRUE) {
	uf.num_dlines += 1;
	cf.store[cf.numstored++] = strdup(scratch);
	if (cf.numstored >= MAXDLINES) {
	  printf("File %s has too many diff lines! (%d)\n",file->name,cf.numstored);
	  Xnerr("933893");
	}
	if (FRED) printf("%d",cf.numstored);
      }
    } else if (!strncmp(key,"<",1)) {
      if (file->active == TRUE) {
	uf.num_dlines += 1;
	cf.store[cf.numstored++] = strdup(scratch);
	if (cf.numstored >= MAXDLINES) {
	  printf("File %s has too many diff lines! (%d)\n",file->name,cf.numstored);
	  Xnerr("9287393");
	}
	if (FRED) printf("%d",cf.numstored);
      }
    } else {
      if (file->active == TRUE) {
	cf.store[cf.numstored++] = strdup(scratch);
	if (cf.numstored >= MAXDLINES) {
	  printf("File %s has too many diff lines! (%d)\n",file->name,cf.numstored);
	  Xnerr("9283893");
	}
	if (FRED) printf("%d",cf.numstored);
      }
    }
    if (fgets(scratch, BUFSIZ, uf.fd) == NULL) {
      /* Now print out the stored lines */
      if ((file->active == TRUE) && (uf.num_dlines>0)){
	for(i=0;i<cf.numstored;i++) {
	  char *ch=0;
	  if (!strncmp(cf.store[i],"retrieving",10)) 
	    continue;
	  if (!strncmp(cf.store[i],"cvs diff",8))
	    continue;
	  ch = strstr(cf.store[i],"\n");
	  if (ch!=NULL) {
	    fprintf(cf.fo,"%s",cf.store[i]);
	  } else {
	    fprintf(cf.fo,"%s\n",cf.store[i]);
	  }
	}
      } else {
	  printf("File %s is NOT active\n",file->name);
      }
      
      /* Done with that file - Now do cleanup of uf */
      for (i=0;i<cf.numstored;i++) {
	if (cf.store[i] != NULL) Xnfree(cf.store[i]);
      }
      if (debug>DLVL) {
	printf("File %s has %d stored lines.\n",file->name,cf.numstored);
      }
      cf.numstored = 0;
      uf.num_dlines = 0;
      return(-1);
    }
    sscanf(scratch,"%s",btest);
    uf.lcount += 1;
  }
  /* "Index" line should be added */
  /*
  if (file->active == TRUE) {
    cf.store[cf.numstored++] = strdup(scratch);
    if (cf.numstored >= MAXDLINES) {
      printf("File %s has too many diff lines! (%d)\n",file->name,cf.numstored);
      Xnerr("9283893");
    }
    if (FRED) printf("%d",cf.numstored);
  }
  */

  /* Now print out the stored lines */
  if ((file->active == TRUE) && (uf.num_dlines>0)){
    for(i=0;i<cf.numstored;i++) {
      char *ch=0;
      if (!strncmp(cf.store[i],"retrieving",10)) 
	continue;
      if (!strncmp(cf.store[i],"cvs diff",8))
	continue;
      ch = strstr(cf.store[i],"\n");
      if (ch!=NULL) {
	fprintf(cf.fo,"%s",cf.store[i]);
      } else {
	fprintf(cf.fo,"%s\n",cf.store[i]);
      }
    }
  } else {
    printf("File %s is NOT active\n",file->name);
  }
  
  /* Done with that file - Now do cleanup of uf */
  for (i=0;i<cf.numstored;i++) {
    if (cf.store[i] != NULL) Xnfree(cf.store[i]);
  }
  if (debug>DLVL) {
    printf("File %s has %d stored lines.\n",file->name,cf.numstored);
  }
  cf.numstored = 0;
  uf.num_dlines = 0;

  if (uf.files->nfiles > 1000000) {
    fclose(uf.fd);
    fclose(cf.fo);
    exit(0);
  }
  return(0);
}

int GetBadEntries()
{
  int i;
  FILE *fd = 0;
  char line[255];

  fd = fopen("Util/badentries","r");
  if (fd==NULL) {
    printf("No Util/badentries file!\n");
    return(-1);
  }
  for (i=0; fgets(line, 255, fd); i++) {
    char *ch=NULL;
    ch = strstr(line,"\n");
    if (ch!=NULL) ch[0] = '\0';
    cf.bad[cf.numbad++] = strdup(&(line[0]));
    if (cf.numbad>200) Xnerr("38929");
  }
  if (cf.numbad == 1) {
    printf("You list %d bad entry\n",cf.numbad);
  } else {
    printf("You list %d bad entries\n",cf.numbad);
  }
  for (i=0;i<cf.numbad;i++) {
    printf("Bad Entry: %s\n",cf.bad[i]);
  }
  fclose(fd);
}

void ToUpper(const char *fname)
{
  FILE *fd = 0;
  FILE *fo = 0;
  char tmps[BUFSIZ];
  char line[255];
  char fnoout[BUFSIZ];
  int i,j;
  char *ch = NULL;
  if (fname == NULL) Xnerr("38383893");

  fd = fopen(fname,"r+");
  sprintf(fnoout,"%s.l",fname);
  fo = fopen(fnoout,"w");

  for (i=0; fgets(line, 255, fd); i++) {
    if (line[0] != '#') {
      ch = &(line[0]);
      for (j=0;(j<255)&&(*ch)&&(isascii(*ch));j+=1,ch++){
	if (isalpha(*ch)) *ch = toupper(*ch);
      }
    }
    fprintf(fo,"%s",line);
  }
  fclose(fd);
  fclose(fo);
}
