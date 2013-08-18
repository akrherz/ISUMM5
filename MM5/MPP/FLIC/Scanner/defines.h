/* defines.h SCCS(4.1 91/02/18 15:42:53) */

#define MAXSTACK 8*4096   /* pushback stack sizes */
#define NCONT	50	/* Number of allowable cards per statement */
#define CARDLEN 256	/* Number of characters per card */
#define LABELN   6	/* Number of characters in label */
#define STATN   250	/* Number of characters in statement */
#define RESTN  CARDLEN - LABELN - STATN /* N of chars to end of card */
#define YYLMAX 1000 


extern int CardLen ;
extern int LabelN ;
extern int StatN ;
extern int RestN ;
