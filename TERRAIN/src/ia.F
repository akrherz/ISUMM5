      INTEGER FUNCTION IA(CHR,N,ISPVAL)                                          IA.1
C                                                                                IA.2
C  PURPOSE: TO CONVERT A N-BYTES CHARACTER (CHR) TO INTEGER IA.                  IA.3
C        ** THE INTEGER DATA FILE IS SAVED AS A N-BYTE CHARACTER                 IA.4
C           DATA FILE. THIS FUNCTION IS USED TO RECOVER THE                      IA.5
C           CHARACTER DATA TO THE INTEGER DATA.                                  IA.6
C                                                                                IA.7
C  N      --- THE NUMBER OF BYTES IN CHR                                         IA.8
C  ISPVAL --- DEFAULT VALUE FOR THE NEGATIVE INTEGER.                            IA.9
C                                                                                IA.10
      CHARACTER*(*) CHR                                                          IA.11
      INTEGER BIT_1, BIT_2                                                       IA.12
C                                                                                IA.13
      BIT_1 = '200'O     ! BINARY '10000000'                                     IA.14
      BIT_2 = '377'O     ! BINARY '11111111'                                     IA.15
      IA    = 0                                                                  IA.16
C                                                                                IA.17
      II1 = ICHAR(CHR(1:1))                                                      IA.18
C .. GET THE SIGN -- ISN=0 POSITIVE, ISN=1 NEGATIVE:                             IA.19
      JJ  = IAND(II1,BIT_1)                                                      IA.20
      ISN = ISHFT(JJ,-7)                                                         IA.21
C                                                                                IA.22
C .. FOR NEGATIVE NUMBER:                                                        IA.23
C    BECAUSE THE NEGATIVE INTEGERS ARE REPRESENTED BY THE SUPPLEMENTARY          IA.24
C    BINARY CODE INSIDE MACHINE.                                                 IA.25
C                                                                                IA.26
        IF (ISN.EQ.1) THEN                                                       IA.27
#ifdef CRAY                                                                      IA.28
           DO M = N+1,8                                                          IA.29
#else                                                                            IA.30
           DO M = N+1,4                                                          IA.31
#endif                                                                           IA.32
             NBIT = (M-1)*8                                                      IA.33
             JJ = ISHFT(BIT_2,NBIT)                                              IA.34
             IA = IEOR(JJ,IA)                                                    IA.35
           END DO                                                                IA.36
        ENDIF                                                                    IA.37
C                                                                                IA.38
C   .. GET THE BYTE FROM CHR:                                                    IA.39
         DO M = 1,N                                                              IA.40
           II2 = ICHAR(CHR(M:M))                                                 IA.41
C new IBM xlf 8.1 compiler fix: thanks to Jim Edwards                            19DEC02.1
           if (ii2.lt.0) ii2 = ii2 + 256                                         19DEC02.2
           MSHFT = (N-M)*8                                                       IA.42
           IA2   = ISHFT(II2,MSHFT)                                              IA.43
C   .. THE ABS(INTEGER):                                                         IA.44
           IA = IEOR(IA,IA2)                                                     IA.45
         END DO                                                                  IA.46
C                                                                                IA.47
      IF (IA.LT.0) IA = ISPVAL                                                   IA.48
C                                                                                IA.49
      RETURN                                                                     IA.50
      END                                                                        IA.51
C                                                                                IA.52
