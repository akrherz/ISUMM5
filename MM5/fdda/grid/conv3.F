      SUBROUTINE CONV3(SLAB,SFCOBS,MIXFG,MJXFG,TIMANL,SFCTIM,MTIM,MVAR,          CONV3.1
     1IMAX,JMAX,NT,NV)                                                           CONV3.2
C THIS SUBROUTINE BUFFERS A 2-D FIELD INTO THE 4-D ARRAY SFCOBS                  CONV3.3
C AND PLACES THE MODEL-RELATIVE TIME OF THE 2-D FIELD INTO                       CONV3.4
C THE 1-D ARRAY SFCTIM                                                           CONV3.5
      DIMENSION SLAB(MIXFG,MJXFG),SFCOBS(MTIM,MVAR,MIXFG,MJXFG),                 CONV3.6
     1SFCTIM(MTIM)                                                               CONV3.7
      IDC=0                                                                      CONV3.8
      IF(NV.GT.2)IDC=1                                                           CONV3.9
      DO 10 I=1,IMAX-IDC                                                         CONV3.10
        DO 10 J=1,JMAX-IDC                                                       CONV3.11
          SFCOBS(NT,NV,I,J)=SLAB(I,J)                                            CONV3.12
   10   CONTINUE                                                                 CONV3.13
                                                                                 CONV3.14
      SFCTIM(NT)=TIMANL                                                          CONV3.15
      RETURN                                                                     CONV3.16
      END                                                                        CONV3.17
                                                                                 CONV3.18
