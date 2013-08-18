C                                                                                EXCHANJ.1
C                                                                                EXCHANJ.2
       SUBROUTINE EXCHANJ(JARG,JARGN,JOVE,ISTART,ISTARTN,IENDN,                  EXCHANJ.3
     1        K,ARR,ARRN,ARRB,ARRNB,ARRBB,ARRBT,KZZ,NSPG,DT,ISTOP)               EXCHANJ.4
C                                                                                EXCHANJ.5
C--- EXCHANGE BOUNDARY VALUES BETWEEN NESTS FOR PARTICULAR                       EXCHANJ.6
C--- J`S FROM ISTARTN TO IENDN                                                   EXCHANJ.7
C                                                                                EXCHANJ.8
C---   ON INPUT:                                                                 EXCHANJ.9
C                                                                                EXCHANJ.10
C----     JARGN    :    WHERE TO START EXCHANGING. SINCE THIS                    EXCHANJ.11
C---                    IS BOUNDARY, JARGN IS 1 OR JL,OR JLX.                    EXCHANJ.12
C---                                                                             EXCHANJ.13
C---      JOVE     :    HOW MANY POINS TO BE EXCHANGED                           EXCHANJ.14
C---                                                                             EXCHANJ.15
C---      ISTARTN  :    WHAT I-VALUE O START WITH IN THE NEST                    EXCHANJ.16
C---                    THAT IS TO BE OVERWRITTEN                                EXCHANJ.17
C---                                                                             EXCHANJ.18
C---      JARG     :    SEE JARGN, EXCEPT FOR THE NEST THAT IS                   EXCHANJ.19
C---                    OVERWRITTEN WITH                                         EXCHANJ.20
C---                                                                             EXCHANJ.21
C---      ISTART   :    SEE ISTARTN, EXCEPT FOR THE NEST THAT IS TO              EXCHANJ.22
C---                    BE OVERWRITTEN WITH                                      EXCHANJ.23
C                                                                                EXCHANJ.24
C                                                                                EXCHANJ.25
C                                                                                EXCHANJ.26
C                                                                                EXCHANJ.27
#     include <parame.incl>                                                      EXCHANJ.28
C                                                                                EXCHANJ.29
C                                                                                EXCHANJ.30
       DIMENSION ARR(MIX,MJX,KZZ),ARRN(MIX,MJX,KZZ),                             EXCHANJ.31
     1   ARRNB(MIX,MJX,KZZ),ARRB(MIX,MJX,KZZ),ARRBB(MIX,KZZ,NSPG),               EXCHANJ.32
     2   ARRBT(MIX,KZZ,NSPG)                                                     EXCHANJ.33
      JBOUN=0                                                                    EXCHANJ.34
      ISTARTNN=ISTARTN                                                           EXCHANJ.35
      IENDNN=IENDN                                                               EXCHANJ.36
      IF(ISTARTN.EQ.1)THEN                                                       EXCHANJ.37
        ISTARTNN=ISTARTN+ISTOP                                                   EXCHANJ.38
      ELSE                                                                       EXCHANJ.39
        IENDNN=IENDN-ISTOP                                                       EXCHANJ.40
      ENDIF                                                                      EXCHANJ.41
      DO 100 J=JARGN,JARGN+JOVE                                                  EXCHANJ.42
        JBOUN=JBOUN+1                                                            EXCHANJ.43
        IF(JBOUN.GT.2)THEN                                                       EXCHANJ.44
          IF(ISTARTN.EQ.1)THEN                                                   EXCHANJ.45
            ISTARTNN=1                                                           EXCHANJ.46
          ELSE                                                                   EXCHANJ.47
            IENDNN=IENDN                                                         EXCHANJ.48
          ENDIF                                                                  EXCHANJ.49
        ENDIF                                                                    EXCHANJ.50
        DO 100 I=ISTARTNN,IENDNN                                                 EXCHANJ.51
          ARRN(I,J,K)=ARR(I+ISTART,JBOUN-1+JARG,K)                               EXCHANJ.52
          ARRNB(I,J,K)=ARRB(I+ISTART,JBOUN-1+JARG,K)                             EXCHANJ.53
          ARRBB(I,K,JBOUN)=ARRN(I,J,K)                                           EXCHANJ.54
          ARRBT(I,K,JBOUN)=(ARRNB(I,J,K)-ARRN(I,J,K))/(.5*DT)                    EXCHANJ.55
  100   CONTINUE                                                                 EXCHANJ.56
                                                                                 EXCHANJ.57
      RETURN                                                                     EXCHANJ.58
      END                                                                        EXCHANJ.59
                                                                                 EXCHANJ.60
