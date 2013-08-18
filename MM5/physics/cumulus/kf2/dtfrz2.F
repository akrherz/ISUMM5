      SUBROUTINE DTFRZ2(TU,P,THTEU,QU,QFRZ,QICE,ALIQ,BLIQ,CLIQ,DLIQ)             DTFRZ2.1
C                                                                                DTFRZ2.2
C...ALLOW THE FREEZING OF LIQUID WATER IN THE UPDRAFT TO PROCEED AS AN           DTFRZ2.3
C...APPROXIMATELY LINEAR FUNCTION OF TEMPERATURE IN THE TEMPERATURE RANGE        DTFRZ2.4
C...TTFRZ TO TBFRZ...                                                            DTFRZ2.5
C...FOR COLDER TERMPERATURES, FREEZE ALL LIQUID WATER...                         DTFRZ2.6
C...THERMODYNAMIC PROPERTIES ARE STILL CALCULATED WITH RESPECT TO LIQUID WATER   DTFRZ2.7
C...TO ALLOW THE USE OF LOOKUP TABLE TO EXTRACT TMP FROM THETAE...               DTFRZ2.8
C                                                                                DTFRZ2.9
      RLC=2.5E6-2369.276*(TU-273.16)                                             DTFRZ2.10
      RLS=2833922.-259.532*(TU-273.16)                                           DTFRZ2.11
      RLF=RLS-RLC                                                                DTFRZ2.12
      CP=1004.*(1.+0.89*QU)                                                      23DEC04.3155
C                                                                                DTFRZ2.14
C  A = D(es)/DT IS THAT CALCULATED FROM BUCK'S (1981) EMPERICAL FORMULAS         DTFRZ2.15
C  FOR SATURATION VAPOR PRESSURE...                                              DTFRZ2.16
C                                                                                DTFRZ2.17
      A=(CLIQ-BLIQ*DLIQ)/((TU-DLIQ)*(TU-DLIQ))                                   DTFRZ2.18
      DTFRZ = RLF*QFRZ/(CP+RLS*QU*A)                                             DTFRZ2.19
      TU = TU+DTFRZ                                                              DTFRZ2.20
                                                                                 DTFRZ2.21
      ES = ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                                    DTFRZ2.22
      QS = ES*0.622/(P-ES)                                                       DTFRZ2.23
C                                                                                DTFRZ2.24
C...FREEZING WARMS THE AIR AND IT BECOMES UNSATURATED...ASSUME THAT SOME OF THE  DTFRZ2.25
C...LIQUID WATER THAT IS AVAILABLE FOR FREEZING EVAPORATES TO MAINTAIN SATURA-   DTFRZ2.26
C...TION...SINCE THIS WATER HAS ALREADY BEEN TRANSFERRED TO THE ICE CATEGORY,    DTFRZ2.27
C...SUBTRACT IT FROM ICE CONCENTRATION, THEN SET UPDRAFT MIXING RATIO AT THE NEW DTFRZ2.28
C...TEMPERATURE TO THE SATURATION VALUE...                                       DTFRZ2.29
C                                                                                DTFRZ2.30
      DQEVAP = QS-QU                                                             DTFRZ2.31
      QICE = QICE-DQEVAP                                                         DTFRZ2.32
      QU = QU+DQEVAP                                                             DTFRZ2.33
      PI=(1.E5/P)**(0.2854*(1.-0.28*QU))                                         DTFRZ2.34
      THTEU=TU*PI*                                                               DTFRZ2.35
     *      EXP((3374.6525/TU-2.5403)*QU*(1.+0.81*QU))                           DTFRZ2.36
      RETURN                                                                     DTFRZ2.37
      END                                                                        DTFRZ2.38
