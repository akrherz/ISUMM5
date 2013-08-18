/* Dan Hansen, 1996, MM5 V2 */

#ifndef m5FileCache_DEFINED
#define m5FileCache_DEFINED

#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>

#ifndef FALSE
# define FALSE 0
# define TRUE  1
#endif
#define MAXDLINES 10000
#ifndef EXTERN
# define EXTERN extern
#endif
EXTERN short int debug;
EXTERN char scratch[BUFSIZ];

struct m5File {
  char  name[BUFSIZ];
  short int active;
  int   id;			/* This is an internal 'thang' */
  FILE *fd;			/* Input file */
  char  oname[BUFSIZ];		/* Output File Name */
  FILE *fo;			/* Output after mods */
};

struct file_header {
  struct file_header *prev;
  struct file_header *next;
  struct m5File *p;
};

struct m5FileCache {
  int nfiles;
  struct file_header *first;
  struct file_header *last;
};

#include	"c_varieties.h"

EXTERN_FUNCTION( struct m5File *	new_m5File,	(_VOID_) );
EXTERN_FUNCTION( struct m5File *	FileFindorAdd,	(struct m5FileCache*,char*) );
EXTERN_FUNCTION( struct m5File *	FileFind,	(struct m5FileCache*,char*) );
EXTERN_FUNCTION( int	m5FileCache_add,	(struct m5FileCache*,struct m5File*) );
EXTERN_FUNCTION( int	free_FileCache,	(struct m5FileCache*) );
EXTERN_FUNCTION( void	FileCache_delete,	(struct m5FileCache*,struct m5File*) );
EXTERN_FUNCTION( struct file_header*	new_file_header,	(struct m5File*) );
EXTERN_FUNCTION( void	free_file_header,	(struct file_header*,int) );
EXTERN_FUNCTION( struct m5FileCache*	new_FileCache,	(_VOID_) );
EXTERN_FUNCTION( int	m5FileCache_insert_before,	(struct m5FileCache*,struct m5File*,struct m5File*) );
EXTERN_FUNCTION( int	FileCache_CountByName,	(struct m5FileCache*,char*) );
EXTERN_FUNCTION( void	ModifyFile,	(struct m5File*) );

char *Xnmem (unsigned size);
void Xnfree(char *befree);

#endif
