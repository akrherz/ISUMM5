#include <stdio.h>
#include <fcntl.h>

#define STANDARD_OUTPUT 1
#define STANDARD_ERROR  2

ERROR_DUP( me )
  int *me ;
{
  int newfd ;
  char filename[128] ;
  sprintf(filename,"rsl.out.%04d",*me) ;
  if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
  {
    perror(filename) ;
    return ;
  }
  close(STANDARD_OUTPUT) ;
  dup(newfd) ;
  sprintf(filename,"rsl.error.%04d",*me) ;
  if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
  {
    perror(filename) ;
    return ;
  }
  close(STANDARD_ERROR) ;
  dup(newfd) ;
}

