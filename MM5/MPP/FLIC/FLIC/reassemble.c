#include <stdio.h>
#include <stdlib.h>

/* read from standard input and the one input file.  The input file
   will contain lines that start with CFLICBYE.  Replace the lines that
   contain CFLICBYE on standard input with these, one for one.  If there
   is not one for one correspondence, it's an error. */

#define CFLICBYE "CFLICBYE"

main( argc, argv )
  int argc ;
  char *argv[] ;
{
  FILE *infd ;
  char inline1[4096] , inline2[4096] ;
  int lencflicbye ;
  int gotline1, gotline2 ;

  lencflicbye = strlen( CFLICBYE ) ;
  if ( argc != 2 )
  {
    fprintf(stderr,"usage: %s file\n",argv[0]) ; exit(2) ;
  }
  if (( infd = fopen(argv[1],"r")) == NULL )
  {
    fprintf(stderr,"error: cannot open %s\n",argv[1]) ; exit(2) ;
  }

  while (1)
  {
    gotline1 = 0 ;
    while (! gotline1)
    {
      if ( fgets( inline1, 4095, stdin ) == NULL ) break ;
      if ( ! strncmp( inline1, CFLICBYE, lencflicbye ) ) 
      {
        gotline1 = 1 ;
      }
      else
      {
        fprintf(stdout,"%s",inline1) ;
      }
    }
    if ( gotline1 == 0 ) break ;

    if ( fgets( inline2, 4095, infd ) == NULL )
    {
      fprintf(stderr,"%s: Mismatch in number of replacement lines\n", argv[0]) ; exit(2) ;
    }
    if ( strncmp( inline2, CFLICBYE, lencflicbye ) )
    {
      fprintf(stderr,"%s: Bad line |%s| in %s\n", argv[0],inline2,argv[1]) ; exit(2) ;
    }
    fprintf(stdout,"%s",inline2) ;
  }
  fclose(infd) ;
}

