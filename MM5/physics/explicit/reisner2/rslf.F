                                                                                 RSLF.1
C                                                                                RSLF.2
C ---------------------------------------------------------------------          RSLF.3
C THIS FUNCTION CALCULATES THE LIQUID SATURATION VAPOR MIXING RATIO AS           RSLF.4
C A FUNCTION OF TEMPERATURE AND PRESSURE                                         RSLF.5
C                                                                                RSLF.6
      FUNCTION RSLF(P,T)                                                         RSLF.7
                                                                                 RSLF.8
      IMPLICIT NONE                                                              RSLF.9
      REAL ESL,RSLF,X,T,P,C0,C1,C2,C3,C4,C5,C6,C7,C8                             RSLF.10
      PARAMETER (C0= .611583699E03)                                              RSLF.11
      PARAMETER (C1= .444606896E02)                                              RSLF.12
      PARAMETER (C2= .143177157E01)                                              RSLF.13
      PARAMETER (C3= .264224321E-1)                                              RSLF.14
      PARAMETER (C4= .299291081E-3)                                              RSLF.15
      PARAMETER (C5= .203154182E-5)                                              RSLF.16
      PARAMETER (C6= .702620698E-8)                                              RSLF.17
      PARAMETER (C7= .379534310E-11)                                             RSLF.18
      PARAMETER (C8=-.321582393E-13)                                             RSLF.19
                                                                                 RSLF.20
      X=MAX(-80.,T-273.16)                                                       RSLF.21
                                                                                 RSLF.22
C     ESL=612.2*EXP(17.67*X/(T-29.65))                                           RSLF.23
      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))               RSLF.24
      RSLF=.622*ESL/(P-ESL)                                                      RSLF.25
                                                                                 RSLF.26
      RETURN                                                                     RSLF.27
      END                                                                        RSLF.28
C                                                                                RSLF.29
C ---------------------------------------------------------------------          RSLF.30
C THIS FUNCTION CALCULATES THE ICE SATURATION VAPOR MIXING RATIO AS A            RSLF.31
C FUNCTION OF TEMPERATURE AND PRESSURE                                           RSLF.32
C                                                                                RSLF.33
      FUNCTION RSIF(P,T)                                                         RSLF.34
                                                                                 RSLF.35
      IMPLICIT NONE                                                              RSLF.36
      REAL ESI,RSIF,X,T,P,C0,C1,C2,C3,C4,C5,C6,C7,C8                             RSLF.37
      PARAMETER (C0= .609868993E03)                                              RSLF.38
      PARAMETER (C1= .499320233E02)                                              RSLF.39
      PARAMETER (C2= .184672631E01)                                              RSLF.40
      PARAMETER (C3= .402737184E-1)                                              RSLF.41
      PARAMETER (C4= .565392987E-3)                                              RSLF.42
      PARAMETER (C5= .521693933E-5)                                              RSLF.43
      PARAMETER (C6= .307839583E-7)                                              RSLF.44
      PARAMETER (C7= .105785160E-9)                                              RSLF.45
      PARAMETER (C8= .161444444E-12)                                             RSLF.46
                                                                                 RSLF.47
      X=MAX(-80.,T-273.16)                                                       RSLF.48
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))               RSLF.49
      RSIF=.622*ESI/(P-ESI)                                                      RSLF.50
                                                                                 RSLF.51
      RETURN                                                                     RSLF.52
      END                                                                        RSLF.53
