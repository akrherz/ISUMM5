#include <fenv.h>
#include <signal.h>

T3ETRAPS()
{
   /* ignore traps */

#if 0
   fedisabletrap(FE_DIVBYZERO|FE_INVALID) ;
   signal(SIGFPE,SIG_IGN);
#endif
   return ;
}


