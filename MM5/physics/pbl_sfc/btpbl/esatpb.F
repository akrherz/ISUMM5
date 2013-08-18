      SUBROUTINE ESATPB(ES1,T1)                                                  ESATPB.1
C  COMPUTE SATURATION VAPOR PRESSURE BASED ON                                    ESATPB.2
C  LOWE (JAM, 16, 100-103, 1977).                                                ESATPB.3
      T=T1                                                                       ESATPB.4
      IF(T.LT.173.)T=173.                                                        ESATPB.5
      IF(T.LT.223.)GOTO 10                                                       ESATPB.6
      AA0=6984.505294                                                            ESATPB.7
      AA1=-188.9039310                                                           ESATPB.8
      AA2=2.133357675                                                            ESATPB.9
      AA3=-1.288580973E-2                                                        ESATPB.10
      AA4=4.393587233E-5                                                         ESATPB.11
      AA5=-8.023923082E-8                                                        ESATPB.12
      AA6=6.136820929E-11                                                        ESATPB.13
      GOTO 20                                                                    ESATPB.14
   10 CONTINUE                                                                   ESATPB.15
      T=T-273.16                                                                 ESATPB.16
      AA0=4.866786841                                                            ESATPB.17
      AA1=3.152625546E-1                                                         ESATPB.18
      AA2=8.640188586E-3                                                         ESATPB.19
      AA3=1.279669658E-4                                                         ESATPB.20
      AA4=1.077955914E-6                                                         ESATPB.21
      AA5=4.886796102E-9                                                         ESATPB.22
      AA6=9.296950850E-12                                                        ESATPB.23
   20 CONTINUE                                                                   ESATPB.24
      ES1=AA0+T*(AA1+T*(AA2+T*(AA3+T*(AA4+T*(AA5+AA6*T)))))                      ESATPB.25
      RETURN                                                                     ESATPB.26
      END                                                                        ESATPB.27
                                                                                 ESATPB.28
