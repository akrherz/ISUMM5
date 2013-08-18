#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <wait.h>
#include <time.h>
#include <sys/systeminfo.h>
#include <sys/errno.h>

#include "filecache.h"

#ifndef      YES
#define      YES           1
#define      NO            0
#endif

#define DLVL 10
#define MAXLINE 80

int FileCache_insert_before(struct m5FileCache *cache, struct m5File *oproc,struct m5File *proc);

#ifdef USE_PROTOTYPES
struct m5File *new_m5File(void)
#else
     struct m5File *new_m5File()
#endif
{
  struct m5File      *tmp=0;
  unsigned lsize = sizeof(struct m5File);

  tmp = (struct m5File *) Xnmem(lsize);

  tmp->active = FALSE;
  tmp->name[0] = '\0';
  tmp->fd = NULL;
  tmp->fo = NULL;
  return tmp;
}

/*
 * - Pass in a file ptr.
 *   Add it to this file cache (at end).
 * - This does NOT allocate memory for the file.
 */

#ifdef USE_PROTOTYPES
int FileCache_add(struct m5FileCache *lcache, struct m5File *proc)
#else
     int FileCache_add(lcache, proc)
     struct m5FileCache *lcache;
     struct m5File *proc;
#endif
{
  struct file_header *new_header;

  if (proc == NULL) Xnerr("343284");

  new_header = new_file_header(proc);
  if (new_header == NULL)
    return ENOMEM;
  if (lcache->nfiles == 0) {
    lcache->first = new_header;
    lcache->last = new_header;
    new_header->next = NULL;
    new_header->prev = NULL;
  } else {
    new_header->prev = lcache->last;
    new_header->next = NULL;
    lcache->last->next = new_header;
    lcache->last = new_header;
  }
  lcache->nfiles++;

  proc->id = lcache->nfiles;			  /* file "id" */
  return 0;
}

/*
 * This does NOT allocate memory for the file.
 */

#ifdef USE_PROTOTYPES
struct file_header *new_file_header(struct m5File *proc)
#else
     struct file_header *new_file_header(proc)
     struct m5File *proc;
#endif
{
  struct file_header *tmp=NULL;
  unsigned lsize = sizeof(struct file_header);

  if (proc == NULL) Xnerr("8203284");

  tmp = (struct file_header *) Xnmem(lsize);
  if (tmp == NULL)
    return NULL;
  tmp->next = NULL;
  tmp->prev = NULL;
  tmp->p = proc;
  return tmp;
}

/*
 * - Find this file in the file Cache.
 *   Delete it if found.
 *   Reset the file "ids"
 */
#ifdef USE_PROTOTYPES
void FileCache_delete(struct m5FileCache *lcache, struct m5File* proc)
#else
     void FileCache_delete(lcache, proc)
     struct m5FileCache *lcache;
     struct m5File *proc;
#endif
{
  struct file_header *head=0;
  int              i;
  
  if (lcache == NULL) Xnerr("1298");
  for (i=0,head = lcache->first; head != NULL; i++,head=head->next) {
    if (head->p == proc) break;	                                      /* match !! */
  }
  if (!head) return;
  
  if (lcache->nfiles == 1) {					      
    lcache->first = NULL;
    lcache->last = NULL;
  } else {
    if (head == lcache->first) {		  /* top of list, off list */
      lcache->first = head->next;
      lcache->first->prev = NULL;
    } else if (head == lcache->last) {		  /* at least 2, off list */
      lcache->last = lcache->last->prev;
      lcache->last->next = NULL;
    } else {					  /* if the middle, off list */
      head->prev->next = head->next;
      head->next->prev = head->prev;
    }
  }
  free_file_header (head, i);
  lcache->nfiles--;
  
  /*
   * We need to reset the file "ids"
   */
  for (i=1,head = lcache->first; head != NULL; i++,head=head->next){
    head->p->id = i; 
  }

  return;
}

/*
 * - Remove the file from the file list.
 * - "indx" is the offset to the line in the file list.
 */
#ifdef USE_PROTOTYPES
void free_file_header(struct file_header *head, int indx)
#else
     void free_file_header(head, indx)
     struct file_header *head;
     int             indx;
#endif
{
  if (!head->p) Xnerr("56891");
  if (indx <= -1) Xnerr("56892");

  Xnfree((char *)head->p);
  Xnfree((char *)head);
  head = NULL;
}

#ifdef USE_PROTOTYPES
struct m5FileCache *new_FileCache(void)
#else
     struct m5FileCache *new_FileCache()
#endif
{
  struct m5FileCache *tmp=0;
  unsigned lc = sizeof(struct m5FileCache);
  
  tmp = (struct m5FileCache *) Xnmem(lc);
  if (tmp == NULL)
    return NULL;
  tmp->first = NULL;
  tmp->last = NULL;
  tmp->nfiles = 0;				  
  return tmp;
}

/*
 * Free file cache (and all its filees).
 */
#ifdef USE_PROTOTYPES
int free_FileCache(struct m5FileCache *lcache)
#else
     int free_FileCache(lcache)
     struct m5FileCache *lcache;
#endif
{
  struct file_header *head=0, *tmp=0;
  int              i;
  
  if (lcache == NULL) Xnerr("576");
  if (lcache->nfiles == 0) {			  
    if (lcache->first) Xnerr("98384");
  } else {
    for (i=0,head=lcache->first;head != NULL;head=head->next) i++;
    if (i != lcache->nfiles) {
      printf ("Counted %d proc - Cache thinks it has %d\n!",i,lcache->nfiles);
      /* FileCacheTest(); */
      Xnerr ("458");
    }
    
    for (head = lcache->first; head != NULL; head = tmp){
      tmp = head->next;
      FileCache_delete (lcache, head->p);	  /* head DELETED */
    }
  }
  Xnfree((char *) lcache);
  lcache = NULL;
  return(0);
}

#ifdef USE_PROTOTYPES
struct m5File *FileFindorAdd(struct m5FileCache *cache, char *nm)
#else
     struct m5File *FileFindorAdd(cache, nm)
     struct m5FileCache *cache;
     char *nm;
#endif
{
  int i, status;
  struct m5File *tProc=NULL;
  struct file_header *head=0;
  if (debug > DLVL) printf("Trying to find file %s in file list.\n",nm);
  if (cache == NULL) Xnerr("1837754296");
  if (nm == NULL) Xnerr("18299");

  for (head = cache->first; head != NULL; head=head->next){
    i = strcmp(head->p->name, nm);
    if (i == 0) {				  /* Same Procname */
      return(head->p);
    } else if (i > 0) {				  /* curname 'F' > 'f',e.g. */
      tProc = new_m5File();			  
      status = FileCache_insert_before(cache,head->p,tProc);
      if (status == ENOMEM) Xnerr("384933 (outta memory)");
      strcpy(tProc->name, nm);
      return(tProc);      
    }
  }

  tProc = new_m5File();				  /* at enddolist */
  status = FileCache_add(cache,tProc);
  if (status == ENOMEM) Xnerr("384933 (outta memory)");
  strcpy(tProc->name, nm);
  return(tProc);
}

#ifdef USE_PROTOTYPES
int FileCache_insert_before(struct m5FileCache *cache, struct m5File *oproc, 
			    struct m5File *proc)
#else
     int FileCache_insert_before(cache, oproc, proc)
     struct m5FileCache *cache;
     struct m5File *oproc;
     struct m5File *proc;
#endif
{
  int i;
  struct file_header *new_header;
  struct file_header *head=0;

  if (proc == NULL) Xnerr("343237");
  if (oproc == NULL) Xnerr("34328429");
  if (cache->nfiles == 0) Xnerr("2349402");

  new_header = new_file_header(proc);
  if (new_header == NULL) return ENOMEM;

  /* Let's go to the oproc (old file) */
  for (head = cache->first; head != NULL; head=head->next){
    if (head->p == oproc) {
      if (head->prev == NULL) {
	new_header->prev = head->prev;
	new_header->next = head;
	head->prev = new_header;
	cache->first = new_header;
      } else {
	head->prev->next = new_header;
	new_header->prev = head->prev;
	new_header->next = head;
	head->prev = new_header;
      }

      cache->nfiles++;
      /* We need to reset the file "ids" */
      for (i=1,head = cache->first; head != NULL; i++,head=head->next){
	head->p->id = i; 
      }
      return 0;
    }
  }
  Xnerr("28494032");
  return 0;
}

#ifdef USE_PROTOTYPES
struct m5File *FileFind(struct m5FileCache *cache, char *nm)
#else
     struct m5File *FileFind(cache, nm)
     struct m5FileCache *cache;
     char *nm;
#endif
{
  int i;
  struct m5File *tProc=NULL;
  struct file_header *head=0;
  if (debug > DLVL) printf("Trying to find file %s.\n",nm);
  if (cache == NULL) Xnerr("182384996");
  if (nm == NULL) Xnerr("18299");

  for (head = cache->first; head != NULL; head=head->next){
    i = strcmp(head->p->name, nm);
    if (i == 0) {				  /* Same Procname */
      return(head->p);
    } else if (i > 0) {				  /* curname 'F' > 'f',e.g. */
      return(tProc);      
    }
  }
  return(tProc);
}

#ifdef USE_PROTOTYPES
int FileCache_CountByName(struct m5FileCache *cache, char *nm)
#else
     int FileCache_CountByName(cache, nm)
     struct m5FileCache *cache;
     char *nm;
#endif
{
  int i,count=0;
  struct file_header *head=0;
  if (debug > 10) printf("Counting number of occurrences for file %s.\n",nm);
  if (cache == NULL) Xnerr("182994896");
  if (nm == NULL) Xnerr("18299");

  for (head = cache->first; head != NULL; head=head->next){
    i = strcmp(head->p->name, nm);
    if (i == 0) {				  /* Same Procname */
      count += 1 ;
    }
  }
  return(count);
}






