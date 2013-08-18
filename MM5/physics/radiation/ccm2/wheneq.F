#ifndef MPP1                                                                     WHENEQ.1
C                                                                                WHENEQ.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC            WHENEQ.3
      SUBROUTINE WHENEQ(PLON,IPOS,I1,I2,INDX,NPTS)                               WHENEQ.4
C....... DUMMY ROUTINE FOR CRAY ROUTINE                                          WHENEQ.5
      INTEGER PLON,IPOS(PLON),INDX(PLON)                                         WHENEQ.6
      NPTS=0                                                                     WHENEQ.7
      DO 10 I=1,PLON                                                             WHENEQ.8
        IF(IPOS(I).EQ.I2)THEN                                                    WHENEQ.9
          NPTS=NPTS+1                                                            WHENEQ.10
          INDX(NPTS)=I                                                           WHENEQ.11
        ENDIF                                                                    WHENEQ.12
   10 CONTINUE                                                                   WHENEQ.13
      RETURN                                                                     WHENEQ.14
      END                                                                        WHENEQ.15
                                                                                 WHENEQ.16
#else                                                                            WHENEQ.17
C                                                                                WHENEQ.18
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC            WHENEQ.19
C                                                                                WHENEQ.20
C DO NOT RUN THIS ROUTINE THROUGH FLIC                                           WHENEQ.21
C                                                                                WHENEQ.22
                                                                                 WHENEQ.23
      SUBROUTINE WHENEQ(PLON,IPOS,I1,I2,INDX,NPTS)                               WHENEQ.24
C....... DUMMY ROUTINE FOR CRAY ROUTINE                                          WHENEQ.25
#include <parame.incl>                                                           WHENEQ.26
#include <various.incl>                                                          WHENEQ.27
      FLIC_RUN_DECL                                                              WHENEQ.28
      INTEGER PLON,IPOS(*),INDX(*)                                               05DEC01.1509
      NPTS=0                                                                     WHENEQ.30
      DO 10 I=is_x0(1),ie_x0(ilx)                                                WHENEQ.31
        IF(IPOS(I).EQ.I2)THEN                                                    WHENEQ.32
          NPTS=NPTS+1                                                            WHENEQ.33
          INDX(NPTS)=FLIC_L2G_M(I)                                               WHENEQ.34
        ENDIF                                                                    WHENEQ.35
   10 CONTINUE                                                                   WHENEQ.36
      RETURN                                                                     WHENEQ.37
      END                                                                        WHENEQ.38
                                                                                 WHENEQ.39
#endif                                                                           WHENEQ.40
