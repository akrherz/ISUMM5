#ifndef MPP1                                                                     WHENFGT.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             WHENFGT.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             WHENFGT.3
      SUBROUTINE WHENFGT(NMBR,TIN,LEN,TMIN,INDEX,NVAL)                           WHENFGT.4
C                                                                                WHENFGT.5
C....... DUMMY ROUTINE FOR CRAY ROUTINE                                          WHENFGT.6
C                                                                                WHENFGT.7
      DIMENSION TIN(NMBR),INDEX(NMBR)                                            WHENFGT.8
      NVAL=0                                                                     WHENFGT.9
      INDEX(1)=0                                                                 WHENFGT.10
      DO 100 N=1,NMBR                                                            WHENFGT.11
        IF(TIN(N).GT.TMIN)THEN                                                   WHENFGT.12
          NVAL=NVAL+1                                                            WHENFGT.13
          INDEX(NVAL)=N                                                          WHENFGT.14
        ENDIF                                                                    WHENFGT.15
  100 CONTINUE                                                                   WHENFGT.16
      RETURN                                                                     WHENFGT.17
      END                                                                        WHENFGT.18
                                                                                 WHENFGT.19
                                                                                 WHENFGT.20
#else                                                                            WHENFGT.21
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             WHENFGT.22
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             WHENFGT.23
      SUBROUTINE WHENFGT(NMBR,TIN,LEN,TMIN,INDEX,NVAL)                           WHENFGT.24
C                                                                                WHENFGT.25
C....... DUMMY ROUTINE FOR CRAY ROUTINE                                          WHENFGT.26
C                                                                                WHENFGT.27
# include <parame.incl>                                                          WHENFGT.28
# include <various.incl>                                                         WHENFGT.29
      FLIC_RUN_DECL                                                              WHENFGT.30
      DIMENSION TIN(*),INDEX(*)                                                  05DEC01.1510
      NVAL=0                                                                     WHENFGT.32
      INDEX(1)=0                                                                 WHENFGT.33
      DO 100 N=is_x0(1),ie_x0(ilx)                                               WHENFGT.34
        IF(TIN(N).GT.TMIN)THEN                                                   WHENFGT.35
          NVAL=NVAL+1                                                            WHENFGT.36
          INDEX(NVAL)=FLIC_L2G_M(N)                                              WHENFGT.37
        ENDIF                                                                    WHENFGT.38
  100 CONTINUE                                                                   WHENFGT.39
      RETURN                                                                     WHENFGT.40
      END                                                                        WHENFGT.41
                                                                                 WHENFGT.42
#endif                                                                           WHENFGT.43
