       SUBROUTINE EXAINT(TA,IYY,JXX,TAN,IYYN,JXXN,ISOUTH,                        EXAINT.1
     1            JWEST,ICRSDOT)                                                 EXAINT.2
#     include <parame.incl>                                                      EXAINT.3
      DIMENSION TA(MIX,MJX),TAN(MIX,MJX)                                         EXAINT.4
      DIMENSION PSCA(MJX,MIX,9)                                                  EXAINT.5
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)                    EXAINT.6
      JENDC=INT(FLOAT(JXXN-1)/3.+.001)+JWEST                                     EXAINT.7
      IENDC=INT(FLOAT(IYYN-1)/3.+.001)+ISOUTH                                    EXAINT.8
      JW2=JWEST-4                                                                EXAINT.9
      IS2=ISOUTH-4                                                               EXAINT.10
      JC2=JENDC+4                                                                EXAINT.11
      IC2=IENDC+4                                                                EXAINT.12
      JW3=JWEST-3                                                                EXAINT.13
      IS3=ISOUTH-3                                                               EXAINT.14
      JC3=JENDC+3                                                                EXAINT.15
      IC3=IENDC+3                                                                EXAINT.16
      IORD=3                                                                     EXAINT.17
      IFCT=1                                                                     EXAINT.18
      DO 4 NF=1,9                                                                EXAINT.19
        DO 4 I=1,MIX                                                             EXAINT.20
          DO 4 J=1,MJX                                                           EXAINT.21
            PSCA(J,I,NF)=0.                                                      EXAINT.22
    4     CONTINUE                                                               EXAINT.23
                                                                                 EXAINT.24
                                                                                 EXAINT.25
C                                                                                EXAINT.26
C--- INTERPOLATE FIELD, FIRST CROSS POINT                                        EXAINT.27
C                                                                                EXAINT.28
      IF(ICRSDOT.EQ.1)THEN                                                       EXAINT.29
        DO 1245 NF=1,9                                                           EXAINT.30
          DO 1245 I=IS2,IC2                                                      EXAINT.31
            DO 1245 J=JW2,JC2                                                    EXAINT.32
              PSCA(J,I,NF)=TA(I,J)                                               EXAINT.33
 1245       CONTINUE                                                             EXAINT.34
                                                                                 EXAINT.35
                                                                                 EXAINT.36
        CALL SINT(PSCA,MJX,MIX,JW3,JC3,IS3,IC3)                                  EXAINT.37
        DO 1270 J=1,JXXN-1                                                       EXAINT.38
          J2=(J-1)/3+JWEST                                                       EXAINT.39
          DO 1250 I=1,IYYN-1                                                     EXAINT.40
            II=(I-1)/3+ISOUTH                                                    EXAINT.41
            JJ=MOD(I-1,3)*3+MOD(J-1,3)+1                                         EXAINT.42
            TAN(I,J)=PSCA(J2,II,JJ)                                              EXAINT.43
 1250     CONTINUE                                                               EXAINT.44
 1270   CONTINUE                                                                 EXAINT.45
C                                                                                EXAINT.46
C                                                                                EXAINT.47
C                                                                                EXAINT.48
      ELSEIF(ICRSDOT.EQ.0)THEN                                                   EXAINT.49
        DO 1220 NF=1,9                                                           EXAINT.50
          DO 1220 I=IS2,IC2                                                      EXAINT.51
            DO 1220 J=JW2,JC2                                                    EXAINT.52
              PSCA(J,I,NF)=TA(I,J)                                               EXAINT.53
 1220       CONTINUE                                                             EXAINT.54
                                                                                 EXAINT.55
                                                                                 EXAINT.56
        CALL SINT(PSCA,MJX,MIX,JW3,JC3,IS3,IC3)                                  EXAINT.57
        DO 1240 J=1,JXXN                                                         EXAINT.58
          J2=J/3+JWEST                                                           EXAINT.59
          DO 1230 I=1,IYYN                                                       EXAINT.60
            II=I/3+ISOUTH                                                        EXAINT.61
            JJ=3*MOD(I,3)+MOD(J,3)+1                                             EXAINT.62
            TAN(I,J)=PSCA(J2,II,JJ)                                              EXAINT.63
 1230     CONTINUE                                                               EXAINT.64
 1240   CONTINUE                                                                 EXAINT.65
      ENDIF                                                                      EXAINT.66
C                                                                                EXAINT.67
      RETURN                                                                     EXAINT.68
      END                                                                        EXAINT.69
                                                                                 EXAINT.70
