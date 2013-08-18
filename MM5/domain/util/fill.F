#ifndef MPP1                                                                     FILL.1
      SUBROUTINE FILL(HT,HSCR1,IYY,JXX,IYYN,JXXN,IEND,JEND)                      FILL.2
#     include <parame.incl>                                                      FILL.3
      DIMENSION HT(MIX,MJX),HSCR1(MIX,MJX)                                       FILL.4
      DO 10 I=1,IEND                                                             FILL.5
        DO 10 J=1,JEND                                                           FILL.6
          HSCR1(I,J)=HT(I,J)                                                     FILL.7
   10   CONTINUE                                                                 FILL.8
                                                                                 FILL.9
      RETURN                                                                     FILL.10
      END                                                                        FILL.11
                                                                                 FILL.12
#else                                                                            FILL.13
C WARNING -- FILL IS JUST A COPY ON MPP VERSION.  LAST 6 ARGS IGNORED.           FILL.14
C            CALL ONLY FROM INITNEST.                                            FILL.15
      SUBROUTINE FILL(HT,HSCR1,IDUM0,IDUM1,IDUM2,IDUM3,IDUM4,IDUM5)              FILL.16
#     include <parame.incl>                                                      FILL.17
      DIMENSION HT(MIX,MJX),HSCR1(MIX,MJX)                                       FILL.18
C      don't do anything                                                         FILL.19
      RETURN                                                                     FILL.20
      DO 10 J=1,MJX                                                              FILL.21
        DO 10 I=1,MIX                                                            FILL.22
          HSCR1(I,J)=HT(I,J)                                                     FILL.23
   10   CONTINUE                                                                 FILL.24
      RETURN                                                                     FILL.25
      END                                                                        FILL.26
#endif                                                                           FILL.27
                                                                                 FILL.28
