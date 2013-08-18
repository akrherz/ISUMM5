/*
 * Written by d.l.hansen Spring 1996
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifndef TRUE
# define TRUE 1
# define FALSE 0
#endif

#define MAXDIMS 10
#define XNBUFSIZ  255

/* XnLine class put into C */
struct myline {
    int numcomments;
    int numemptylines;
    int numsplit;
    char l[BUFSIZ];		/* The line itself*/
    short initch;
};

void Xnerr(const char *ms)
{
  printf("Runtime ERROR: %s caused exit.\n",ms);
  exit(-1);
}

void splitline(FILE *fo, struct myline *cur)
{
#define LLEN2 66
  int j;
  char tmps[BUFSIZ];
  char line[BUFSIZ];
  char line2[BUFSIZ];
  char *ch;
  ch=&(cur->l[75]);
  if (!strncmp(ch,"       ",5)) {
    cur->l[72] = '\0';
    sprintf(tmps,"%s\n",&(cur->l[0]));
    fprintf(fo,"%s",tmps);
    return;
  }

  cur->numsplit += 1;
  strcpy(line,cur->l);
  ch=&(line[0]);
  j = strlen(ch);

  /* Check FIRST Line */
  if (j>73) {			
    strcpy(line2, &(ch[72]));
    ch[72] = '\0';

    sprintf(tmps,"%s\n",&(ch[0]));
    fprintf(fo,"%s",tmps);
    cur->initch = FALSE;

    strcpy(line, &(line2[0]));
    ch = &(line[0]);
  }

  /* Check REST of Lines (66=72-6)*/
  for (j=strlen(ch); j>LLEN2; j=strlen(ch)) {
    strcpy(line2, &(ch[LLEN2]));
    ch[LLEN2] = '\0';

    sprintf(tmps,"     1%s\n",&(ch[0]));
    fprintf(fo,"%s",tmps);

    strcpy(line, &(line2[0]));
    ch = &(line[0]);
  }
  if (cur->initch == TRUE) {
    j = strlen(cur->l);
    if (j>72) Xnerr("283838"); 
    sprintf(tmps,"%s",&(ch[0]));
    fprintf(fo,"%s",tmps);
  } else {
    /* If ch is nothing but spaces, skip*/
    if (!strncmp(ch,"         ",7)) { 
      fprintf(fo,"\n");
    } else{
      sprintf(tmps,"     1%s",&(ch[0]));
      fprintf(fo,"%s",tmps);
    }
  }
  cur->initch = TRUE;		/* reset for new line */
}

void SplitLines(const char *fname)
{
  FILE *fd = 0;
  FILE *fo = 0;
  char tmps[BUFSIZ];
  char line[BUFSIZ];
  char tackon[BUFSIZ];
  char fnoout[BUFSIZ];
  int i;
  int j;
  int count = 0;
  char *ch = NULL;
  char *emptyline = "                                                                       ";

  struct myline cur;

  cur.l[0] = '\0';
  cur.numsplit=0;
  cur.numcomments = 0;
  cur.numemptylines=0;
  cur.initch = TRUE;

  if (fname == NULL) Xnerr("38383893");

  fd = fopen(fname,"r+");
  sprintf(fnoout,"%s.new",fname);
  fo = fopen(fnoout,"w");

  for (i=0; fgets(cur.l, BUFSIZ, fd); i++) {
    ch = &(cur.l[0]);
    j = strlen(ch);
    if ((j = strlen(ch)) < 74) {
      sprintf(tmps,"%s",&(ch[0]));
      fprintf(fo,"%s",tmps);
    } else if (ch[0] == 'C' || ch[0] == 'c') {
      cur.numcomments += 1;
      sprintf(tmps,"%s",&(ch[0]));
      fprintf(fo,"%s",tmps);
    } else if(!strncmp(&(ch[0]), emptyline, 70)) {
      cur.numemptylines += 1;
      sprintf(tmps,"%s",&(ch[0]));
      fprintf(fo,"%s",tmps);
    } else {
      splitline(fo,&cur);
    }
  }
  /*
  printf("# of comments in file    = %d\n",cur.numcomments);
  printf("# of empty lines in file = %d\n",cur.numemptylines);
  */
  fclose(fd);
  fclose(fo);
}
void usage() {
  printf("usage:: linesplitter72 -F filename\n");
  exit(0);
}

int main(int argc, char **argv)
{
  int c, errflg=0;
  char *ch=0;
  char *cptr;
  short donumbering = FALSE;
  short docaps = FALSE;

  if (argv[1] == NULL) {
    usage();
  }
  if (argv[2] == NULL) {
    usage();
  } else {
    for (ch = argv[2]; ch != NULL; ch = cptr+1){
      if((cptr = strchr(ch, ','))) {
	char tmps[XNBUFSIZ];
	strncpy(tmps, ch, strlen(ch)-strlen(cptr));
	tmps[strlen(ch)-strlen(cptr)] = '\0';
	SplitLines(tmps);
      } else {
	char tmps[XNBUFSIZ];
	strcpy(tmps,ch);
	SplitLines(tmps);
	break;
      }
    }
  }
  return(0);
}

