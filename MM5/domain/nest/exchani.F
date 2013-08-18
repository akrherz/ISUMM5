       SUBROUTINE EXCHANI(IARG,IARGN,IOVE,JSTART,JSTARTN,JENDN,                  EXCHANI.1
     1        K,ARR,ARRN,ARRB,ARRNB,ARRBB,ARRBT,KZZ,NSPG,DT,ISTOP)               EXCHANI.2
C                                                                                EXCHANI.3
C--- EXCHANGE BOUNDARY VALUES BETWEEN NESTS FOR PARTICULAR                       EXCHANI.4
C--- I`S FROM ISTARTN TO IENDN                                                   EXCHANI.5
C                                                                                EXCHANI.6
C---   ON INPUT:                                                                 EXCHANI.7
C                                                                                EXCHANI.8
C----     IARGN    :    WHERE TO START EXCHANGING. SINCE THIS                    EXCHANI.9
C---                    IS BOUNDARY, JARGN IS 1 OR JL,OR JLX.                    EXCHANI.10
C---                                                                             EXCHANI.11
C---      IOVE     :    HOW MANY POINS TO BE EXCHANGED                           EXCHANI.12
C---                                                                             EXCHANI.13
C---      JSTARTN  :    WHAT I-VALUE O START WITH IN THE NEST                    EXCHANI.14
C---                    THAT IS TO BE OVERWRITTEN                                EXCHANI.15
C---                                                                             EXCHANI.16
C---      IARG     :    SEE JARGN, EXCEPT FOR THE NEST THAT IS                   EXCHANI.17
C---                    OVERWRITTEN WITH                                         EXCHANI.18
C---                                                                             EXCHANI.19
C---      JSTART   :    SEE ISTARTN, EXCEPT FOR THE NEST THAT IS TO              EXCHANI.20
C---                    BE OVERWRITTEN WITH                                      EXCHANI.21
C                                                                                EXCHANI.22
C                                                                                EXCHANI.23
C                                                                                EXCHANI.24
C                                                                                EXCHANI.25
#     include <parame.incl>                                                      EXCHANI.26
C                                                                                EXCHANI.27
C                                                                                EXCHANI.28
       DIMENSION ARR(MIX,MJX,KZZ),ARRN(MIX,MJX,KZZ),                             EXCHANI.29
     1   ARRNB(MIX,MJX,KZZ),ARRB(MIX,MJX,KZZ),ARRBB(MJX,KZZ,NSPG),               EXCHANI.30
     2   ARRBT(MJX,KZZ,NSPG)                                                     EXCHANI.31
      IBOUN=0                                                                    EXCHANI.32
      JSTARTNN=JSTARTN                                                           EXCHANI.33
      JENDNN=JENDN                                                               EXCHANI.34
      IF(JSTARTN.EQ.1)THEN                                                       EXCHANI.35
        JSTARTNN=JSTARTN+ISTOP                                                   EXCHANI.36
      ELSE                                                                       EXCHANI.37
        JENDNN=JENDN-ISTOP                                                       EXCHANI.38
      ENDIF                                                                      EXCHANI.39
      DO 100 I=IARGN,IARGN+IOVE                                                  EXCHANI.40
        IBOUN=IBOUN+1                                                            EXCHANI.41
        IF(IBOUN.GT.2)THEN                                                       EXCHANI.42
          IF(JSTARTN.EQ.1)THEN                                                   EXCHANI.43
            JSTARTNN=1                                                           EXCHANI.44
          ELSE                                                                   EXCHANI.45
            JENDNN=JENDN                                                         EXCHANI.46
          ENDIF                                                                  EXCHANI.47
        ENDIF                                                                    EXCHANI.48
        DO 100 J=JSTARTNN,JENDNN                                                 EXCHANI.49
          ARRN(I,J,K)=ARR(IBOUN-1+IARG,J+JSTART,K)                               EXCHANI.50
          ARRNB(I,J,K)=ARRB(IBOUN-1+IARG,J+JSTART,K)                             EXCHANI.51
          ARRBB(J,K,IBOUN)=ARRN(I,J,K)                                           EXCHANI.52
          ARRBT(J,K,IBOUN)=(ARRNB(I,J,K)-ARRN(I,J,K))/(.5*DT)                    EXCHANI.53
  100   CONTINUE                                                                 EXCHANI.54
                                                                                 EXCHANI.55
      RETURN                                                                     EXCHANI.56
      END                                                                        EXCHANI.57
                                                                                 EXCHANI.58
