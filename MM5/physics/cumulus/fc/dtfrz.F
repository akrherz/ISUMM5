      FUNCTION DTFRZ (T,P,Q,SD,D273)                                             DTFRZ.1
C                                                                                DTFRZ.2
C...COMPUTE FREEZING EFFECT.                                                     DTFRZ.3
C                                                                                DTFRZ.4
      RLC=2.5E6-2369.276*(T-273.16)                                              DTFRZ.5
      RLS=2833922.-259.532*(T-273.16)                                            DTFRZ.6
      TOTRU=SD+Q                                                                 DTFRZ.7
      BT=1./T                                                                    DTFRZ.8
      DT=D273-BT                                                                 DTFRZ.9
      QS1=611.*EXP(RLC*DT/461.5)                                                 DTFRZ.10
      QS2=611.*EXP(RLS*DT/461.5)                                                 DTFRZ.11
      QS=.622*RLS*QS2/(P-QS2)                                                    DTFRZ.12
      X=2.1672E-3*RLS*QS*BT*BT                                                   DTFRZ.13
      DTFRZ=(SD*(RLS-RLC)+.622*RLS*QS1/(P-QS1)-QS)/(1004.+1952.*TOTRU+X)         DTFRZ.14
      RETURN                                                                     DTFRZ.15
      END                                                                        DTFRZ.16
                                                                                 DTFRZ.17
