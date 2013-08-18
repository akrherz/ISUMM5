         SUBROUTINE FILSLB(ANEW,AOLD,ISOUTH,ISOUTHO,JWEST,JWESTO,                FILSLB.1
     1                     IIX,JJX,ICRSDOT)                                      FILSLB.2
                                                                                 FILSLB.3
#     include <parame.incl>                                                      FILSLB.4
                                                                                 FILSLB.5
      DIMENSION ANEW(MIX,MJX),AOLD(MIX,MJX)                                      FILSLB.6
      IF(ICRSDOT.EQ.0)THEN                                                       FILSLB.7
        ICUT=2                                                                   FILSLB.8
      ELSEIF(ICRSDOT.EQ.1)THEN                                                   FILSLB.9
        ICUT=3                                                                   FILSLB.10
      ENDIF                                                                      FILSLB.11
      IBEG=(ISOUTH-ISOUTHO)*3+1                                                  FILSLB.12
      JBEG=(JWEST-JWESTO)*3+1                                                    FILSLB.13
      IF(IBEG.LT.0)THEN                                                          FILSLB.14
        IEND=IIX                                                                 FILSLB.15
        IBEG1=2-IBEG                                                             FILSLB.16
        IIBE=1-IBEG                                                              FILSLB.17
      ELSE                                                                       FILSLB.18
        IBEG1=1                                                                  FILSLB.19
        IEND=IIX-(IBEG-1)                                                        FILSLB.20
        IIBE=1-IBEG                                                              FILSLB.21
      ENDIF                                                                      FILSLB.22
      IF(JBEG.LT.0)THEN                                                          FILSLB.23
        JEND=JJX                                                                 FILSLB.24
        JBEG1=2-JBEG                                                             FILSLB.25
        JJBE=1-JBEG                                                              FILSLB.26
      ELSE                                                                       FILSLB.27
        JBEG1=1                                                                  FILSLB.28
        JEND=JJX-(JBEG-1)                                                        FILSLB.29
        JJBE=1-JBEG                                                              FILSLB.30
      ENDIF                                                                      FILSLB.31
      IBEG1=IBEG1+ICUT                                                           FILSLB.32
      JBEG1=JBEG1+ICUT                                                           FILSLB.33
      IEND=IEND-ICUT                                                             FILSLB.34
      JEND=JEND-ICUT                                                             FILSLB.35
      DO I=IBEG1,IEND                                                            FILSLB.36
        DO J=JBEG1,JEND                                                          FILSLB.37
          ANEW(I,J)=AOLD(I-IIBE,J-JJBE)                                          FILSLB.38
        ENDDO                                                                    FILSLB.39
      ENDDO                                                                      FILSLB.40
      RETURN                                                                     FILSLB.41
      END                                                                        FILSLB.42
                                                                                 FILSLB.43
