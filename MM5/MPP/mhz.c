
/*
 * mhz.c - calculate clock rate and megahertz
 *
 * Usage: mhz [-c]
 *
 * This version of mhz is designed to eliminate the (possibly
 * variable) cost of the cache load and variable instruction
 * counts used by different compilers on different architectures
 * and instruction sets.  We can do this if we can get the compiler
 * to generate (at least) two instruction sequences inside loops
 * where the instruction count is relatively prime.  We have five
 * different loops to increase the chance that two of them will
 * be relatively prime on any given architecture.  Then, we can
 * use the least-common-multiple of the execution frequency as
 * the CPU frequency.
 *
 * We have to try and make sure that the code in the various
 * inner loops does not fall out of the on-chip instruction cache
 * and that the inner loop variables fit inside the register set.
 * The i386 only has six addressable registers, so we had to make
 * sure that the inner loop procedures had fewer variables so they
 * would not spill onto the stack.
 *
 * This technique makes no assumptions about the cost of a LOAD
 * from the on-board cache or the number of instructions used to
 * implement a given command.  We just hope that the compiler gets
 * at least two inner loop instruction sequences with lengths that 
 * are relatively prime.  The "relatively prime" makes the least
 * common multiplier method work.  If all the instructions sequences
 * have a common factor (e.g. 2), then the apparent CPU speed will
 * be off by that common factor.  Also, if there is too much
 * variability in the data so there is no apparent least common
 * multiple within the error bounds set in multiple_approx, then
 * we simply return the maximum clock rate found in the loops.
 *
 * Copyright (c) 1994-1997 Larry McVoy and Carl Staelin.  Distributed 
 * under the FSF GPL with additional restriction that results may 
 * published only if:
 * (1) the benchmark is unmodified, and
 * (2) the version in the sccsid below is included in the report.
 */
char	*id = "$Id: mhz.c,v 3.1 2004/12/01 16:41:02 mesouser Exp $\n";

#include        <stdio.h>
#include        <unistd.h>
#include        <sys/types.h>
#include        <signal.h>
#include        <errno.h>
#include	<sys/time.h>

#define	uint64	unsigned long long


#define	SHORT	 1000000
#define	MEDIUM	 5000000
#define	LONGER	10000000	/* for networking, etc */
#define	ENOUGH	SHORT

/*
 * Standard timing loop.  Usage:
 *
 *	LOOP_FIRST(N, usecs, time)
 *	<code that you want timed>
 *	LOOP_LAST(N, usecs, time)
 *
 * time is how long you think it should run to be accurate.
 * "N" is a variable that will be set to the number of times it 
 * took to get "usecs" duration.  You then use N & usecs to print
 * out your results.
 * 
 * Notes: 
 *
 * Adjust the amount of time proportional to how
 * far we need to go.  We want time/usecs to be ~1.
 *
 * For systems with low resolution clocks, usecs can
 * be 0 or very close to 0.  We don't know how 
 * much time we spent, it could be anywhere from
 * 1 to 9999 usecs.  We pretend it was 1000 usecs.
 * The 129 value is because some systems bump the
 * timeval each time you call gettimeofday().
 */
#define	LOOP_FIRST(N, usecs, time)			\
	N = 0;						\
	do {						\
		if (!N) {				\
			N = 1;				\
		} else {				\
			double	adj;			\
			int	n;			\
			if (usecs <= 129) {		\
				usecs = 1000;		\
			}				\
			adj = (time * 1.5)/usecs;	\
			n = N * adj;			\
			N = n <= N ? N+1 : n;		\
		}					\
timit:		usecs = N;				\
		start(0);				\
		while (usecs--) {

#define	LOOP_LAST(N, usecs, time)			\
		}					\
		usecs = stop(0,0);			\
		/*printf("\tN=%.2f u=%.2f c=%.2f\n",	\
		(double)N, (double)usecs, (double)usecs/N);  \
		*/ \
	} while (usecs < time);				


static struct timeval start_tv, stop_tv;

void
tvsub(struct timeval * tdiff, struct timeval * t1, struct timeval * t0)
{
	tdiff->tv_sec = t1->tv_sec - t0->tv_sec;
	tdiff->tv_usec = t1->tv_usec - t0->tv_usec;
	if (tdiff->tv_usec < 0)
		tdiff->tv_sec--, tdiff->tv_usec += 1000000;
}

/*
 * Start ftiming now.
 */
void
start(struct timeval *tv)
{
	if (tv == NULL) {
		tv = &start_tv;
	}
	(void) gettimeofday(tv, (struct timezone *) 0);
}

/*
 * Stop ftiming and return real time in microseconds.
 */
uint64
stop(struct timeval *begin, struct timeval *end)
{
	struct timeval tdiff;
	uint64	m;

	if (end == NULL) {
		end = &stop_tv;
	}
	(void) gettimeofday(end, (struct timezone *) 0);

	if (begin == NULL) {
		begin = &start_tv;
	}
	tvsub(&tdiff, end, begin);
	m = tdiff.tv_sec;
	m *= 1000000;
	m += tdiff.tv_usec;
	return (m);
}


#define	TEN(one)	one one one one one one one one one one
#define	HUNDRED(one)	TEN(one) TEN(one) TEN(one) TEN(one) TEN(one) \
			TEN(one) TEN(one) TEN(one) TEN(one) TEN(one) 

void use_p(int **p) {}

/*
 * We figure that the inner loop overhead is about 3 instructions, and
 * that the outer loop and function call overhead is about 8 instructions.
 *
 * We ignore the outer loop and function call overhead because we only
 * do them once per measurement period and they are far smaller than the 
 * clock granularity.
 *
 * We assume that the inner loop actions are about 3 clock ticks each (on
 * systems we have looked at they varied between 2 and 6 clock ticks), so
 * the overhead is roughly equal to one extra loop action.
 */
#define LOOP_CONTENTS(one)	for (; n > 0; --n) {			\
					HUNDRED(one) HUNDRED(one)	\
				}
#define LOOP_SIZE		200
#define INSTRUCTIONS		(N * 5000 * (LOOP_SIZE + 1))

#define MHZ_INNER(M, one)						\
int ** 									\
_mhz_##M (register int n, register int **p, register int **q, register int o) \
{ 									\
	LOOP_CONTENTS(one)						\
	return p;							\
}

#define MHZ_OUTER(M) 							\
double 									\
mhz_##M (register int **p, register int **q) 				\
{									\
	register int i;							\
	int	usecs, N;						\
	double	mhz;							\
									\
	/* warm the instruction cache */				\
	_mhz_##M(1, p, q, 1);						\
									\
	LOOP_FIRST(N, usecs, ENOUGH);					\
	p = _mhz_##M(5000 * N, p, q, 1);				\
	usecs = 0;							\
	LOOP_LAST(N, usecs, ENOUGH);					\
									\
	use_p(p);							\
	use_p(q);							\
									\
	return (double)INSTRUCTIONS / (double)usecs ;			\
}

#define MHZ(M, ONE)							\
MHZ_INNER(M, ONE)							\
MHZ_OUTER(M)

/* this loop should simply be a series of LOAD instructions */
MHZ(0, p = (int **)*p;)

/* This loop should be a series of LOAD, SUB, ADD instructions */
#ifdef crayx1
MHZ(1, p = (int **)*p; p = p + ((long)q - (long)p);)
#else
MHZ(1, p = (int **)*p; p = p + ((int)q - (int)p);)
#endif

/* This loop should be a series of LOAD, SUB, ADD and COPY instructions */
#ifdef crayx1
MHZ(2, p = (int **)*p; p = p + ((long)q - (long)p); q = p;)
#else
MHZ(2, p = (int **)*p; p = p + ((int)q - (int)p); q = p;)
#endif

/* This loop should be a series of LOAD and ADD instructions */
MHZ(3, p = (int **)*p; p += o;)

/* This loop should be a series of LOAD, ADD, and SUB instructions */
#ifdef crayx1
MHZ(4, p = (int **)*(q + o + ((long)p - (long)q));)
#else
MHZ(4, p = (int **)*(q + o + ((int)p - (int)q));)
#endif

typedef double (*loop_f)(register int **, register int **);
loop_f loops[5] = { mhz_0, mhz_1, mhz_2, mhz_3, mhz_4 };

/*
 * Predicate: "multiple" is a multiple of "value", within a margin of error
 */
int
multiple_approx(double multiple, double value)
{
	double	mult = multiple / value;
	int	m = (int)(mult + 0.5);
	double	diff = (double)m - mult;

	/* scale the error margin by the multiple */
	diff /= m;

	if (-0.05 < diff && diff < 0.05) return 1;
	return 0;
}


/*
 * least common multiple
 *
 * return the least common multiple of the passed values (within a
 * margin of error because these are experimental results, not
 * theoretical numbers)
 */
double 
lcm(double values[], int size)
{
	int i;
	int mult;
	double max;

	for (max = values[0], i = 1; i < size; ++i) {
		if (max < values[i]) max = values[i];
	}

	for (mult = 1; mult < 20; ++mult) {
	    int valid = 1;
	    for (i = 0; i < size; ++i) {
		if (!multiple_approx((double)mult * max, values[i])) {
		    valid = 0;
		    break;
		}
	    }
	    if (valid) return (double)mult * max;
	}
	/* best guess */
	return max;
}

#if 0
int
main(int ac, char **av)
#else
mhz( meghz )
  float * meghz ;
#endif
{
	int	i;
	int    *x[2];
	int	size = sizeof(loops) / sizeof(loop_f);
	double	mlines[sizeof(loops) / sizeof(loop_f)];
	double	mhz;

	x[0] = (int *)&x[0];
	x[1] = (int *)&x[0];

	/*
	 * compute the frequency of execution of each inner loop's
	 * action.  So, if the inner loop is just a LOAD instruction,
	 * then this will return the number of LOADs per second.
	 */
	for (i = 0; i < size; ++i) {
	    mlines[i] = (*loops[i])((int **)x[0], (int **)x[0]);
	}

#ifdef DEBUG
	for (i = 0; i < size; ++i) {
	    fprintf(stderr, "mlines[%d] = %f\n", i, mlines[i]);
	}
#endif /* DEBUG */

	/*
	 * the processor's clock speed is the least common multiple
	 * of the execution frequencies of the various loops.  For
	 * example, suppose we are trying to compute the clock speed
	 * for a 120Mhz processor, and we have two loops:
	 *	LOAD		--- two cycles to LOAD from on-board cache
	 *	LOAD;ADD	--- three cycles to LOAD and add
	 * then the loop execution frequencies will be:
	 *	LOAD		60Mhz (2 cycles/LOAD)
	 *	LOAD;ADD	40Mhz (3 cycles/LOAD;ADD)
	 * so the least common multiple is 120Mhz.  Aside from
	 * extraneous variability added by poor benchmarking
	 * hygiene, this method should always work when we are able
	 * to get loops with cycle counts that are relatively prime.
	 *
	 * Suppose we are unlucky, and we have our two loops do
	 * not have relatively prime instruction counts.  Suppose
	 * our two loops are:
	 *	LOAD		60Mhz (2 cycles/LOAD)
	 *	LOAD;ADD;SUB	30Mhz (4 cycles/LOAD;ADD;SUB)
	 * then the least common multiple will be 60Mhz.
	 *
	 * The five loops provided above should have at least two
	 * relatively prime loops on nearly all architectures.
	 */
	mhz = lcm(mlines, size);

#if 0
	if (ac == 2 && !strcmp(av[1], "-c")) {
		printf("%.4f\n", 1000 / mhz);
	} else {
		printf("%.0f Mhz, %.2f nanosec clock\n", mhz, 1000 / mhz);
	}
	exit(0);
#else
# if (ASSUME_HOMOGENEOUS_ENVIRONMENT == 1) 
        *meghz = 100.0 ;
# else
        *meghz = mhz ;
# endif
        return ;
#endif
}


