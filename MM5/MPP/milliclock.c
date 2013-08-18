#include <stdio.h>
/* millisecond elapsed time timer 
NOTE -- very different from IBM mclock time function
in fortran.
*/
#include <sys/time.h>
#if defined(rs6000)
milliclock()
{
    struct timestruc_t tb ;
    int isec ;
    int nsec ;
    int msecs ;

    gettimer(TIMEOFDAY, &tb) ;

    isec = tb.tv_sec ;
    nsec = tb.tv_nsec ;

    msecs = 1000 * isec + nsec / 1000000 ;

    return(msecs) ;
}
#else
#  ifdef T3E
MILLICLOCK ()
#  else
#    ifdef NOUNDERSCORE
milliclock()
#    else
milliclock_()
#    endif
#  endif
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;	/* microseconds */
    int msecs ;

    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;

    msecs = 1000 * isec + usec / 1000 ;

    return(msecs) ;
}
#endif

#if ((defined(vpp) || defined(vpp2)) && defined(mpestuff))

#define p4_ustimer usc_MD_clock
#define p4_clock MD_clock
#define usc_time_t unsigned long

static int global_reference_time ;
static usc_time_t usc_MD_rollover_val = 0 ;

usc_time_t usc_MD_clock()
{
        unsigned long ustime;
        struct timeval tp;
        struct timezone tzp;
        unsigned long roll;

        if ( usc_MD_rollover_val == 0 )
        {
        roll = (usc_time_t) ((usc_time_t) 1 << ((sizeof(usc_time_t)*8)-1));
        roll = roll + roll - 1;
        usc_MD_rollover_val = (usc_time_t) (roll / 1000000);
        }

        gettimeofday(&tp,&tzp);
        ustime = (unsigned long) tp.tv_sec;
        ustime = ustime % usc_MD_rollover_val;
        ustime = (ustime * 1000000) + (unsigned long) tp.tv_usec;
        return((usc_time_t) ustime);
}

void MD_set_reference_time()
{
    int i = 0;
    struct timeval tp;
    struct timezone tzp;
    gettimeofday(&tp, &tzp);
    global_reference_time = tp.tv_sec ;
}

int MD_clock()
{
    int i = 0;
    struct timeval tp;
    struct timezone tzp;
    gettimeofday(&tp, &tzp);
    i = (int) (tp.tv_sec - global_reference_time);
    i *= 1000;
    i += (int) (tp.tv_usec / 1000);
    return (i);
}

static int clock_start_ms;
static usc_time_t ustimer_start;
static usc_time_t usrollover;

double
MPI_Wtime( )
{
    int elapsed_ms, q;
    usc_time_t ustimer_end;
    double rc, roll, beginning, end;

    if (usrollover == 0)
        return( .001*p4_clock() );

    elapsed_ms = p4_clock() - clock_start_ms; /* milliseconds */
    ustimer_end = p4_ustimer();               /* terminal segment */

    q  =  elapsed_ms / (int)(usrollover/1000);/* num rollover-sized intervals*/
    /* q+1 is the maximum number of rollovers that could have occurred */

    if (ustimer_start <= ustimer_end)
      q = q - 1;
    /* now q+1 is the number of rollovers that did occur */

    beginning = (double)(usrollover - ustimer_start); /* initial segment */
    end = ustimer_end;                               /* terminal segment */

    roll = (double)(usrollover * 0.000001);           /* rollover in seconds */
    rc = (double) (((beginning + end ) * 0.000001) + (q * roll));

fprintf(stderr,"MPI_Wtime returns %lf\n",rc ) ;

    return(rc) ;
}

int   MPI_Attr_get( comm, keyval, attr_value, flag )
int comm;
int keyval;
void * attr_value;
int * flag;
{
    *flag = 0 ;
}

#endif


#if 0
main()
{
   while(1)
   {
     printf("%d\n",milliclock()) ;
     sleep(1) ;
   }
}
#endif
