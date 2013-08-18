      SUBROUTINE SMTH121(TF,IYYF,JXXF,NP)                                        SMTH121.1
C                                                                                SMTH121.2
C   PURPOSE :  PERFORMS THE 1-2-1 SMOOTHING TO REMOVE PRIMARILY THE              SMTH121.3
C              2DX WAVES FROM THE FIELDS TF.                                     SMTH121.4
C   ON ENTRY : TF(IYYF,JXXF): 2-D FIELD TO BE SMOOTHED.                          SMTH121.5
C                  IYYF,JXXF: THE DIMENSIONS OF THE FIELD TF.                    SMTH121.6
C                         NP: (NP-1) ROWS AND COLUMNS NEAR THE                   SMTH121.7
C                             BOUNDARY ARE NOT TO BE SMOOTHED.                   SMTH121.8
C   ON EXIT :  TF(IYYF,JXXF): THE SMOOTHED FIELD.                                SMTH121.9
C   CALLS :    SUBROUTINE SMTH121 CALLS NO OTHER SUBROUTINES.                    SMTH121.10
C   CALLED BY: SUBROUTINE TERDRV.                                                SMTH121.11
C   COMMENTS : SEE SECTION 3.4 FOR DETAILS.                                      SMTH121.12
C                                                                                SMTH121.13
# include <parame.incl>                                                          SMTH121.14
      DIMENSION TSCR(IIMX,JJMX)                                                  SMTH121.15
      DIMENSION TF(IYYF,JXXF)                                                    SMTH121.16
C                                                                                SMTH121.17
      DO 492 I=1,IYYF                                                            SMTH121.18
         DO 492 J=1,JXXF                                                         SMTH121.19
            TSCR(I,J)=TF(I,J)                                                    SMTH121.20
 492  CONTINUE                                                                   SMTH121.21
      DO 493 I=NP,IYYF-NP                                                        SMTH121.22
         DO 493 J=NP,JXXF-NP                                                     SMTH121.23
            TSCR(I,J)=.25*(2.*TF(I,J)+TF(I,J+1)+TF(I,J-1))                       SMTH121.24
 493  CONTINUE                                                                   SMTH121.25
      DO 494 J=NP,JXXF-NP                                                        SMTH121.26
         DO 495 I=NP,IYYF-NP                                                     SMTH121.27
            TF(I,J)=.25*(2.*TSCR(I,J)+TSCR(I+1,J)+TSCR(I-1,J))                   SMTH121.28
 495     CONTINUE                                                                SMTH121.29
 494  CONTINUE                                                                   SMTH121.30
C                                                                                SMTH121.31
      RETURN                                                                     SMTH121.32
      END                                                                        SMTH121.33
