C                                                                                ZX4LP.1
C----------------------------------------------------------------------          ZX4LP.2
C                                                                                ZX4LP.3
      SUBROUTINE ZX4LP (A, IA, B, C, N, M1, M2, S, PSOL, DSOL, RW, IW,           ZX4LP.4
     &                  IER)                                                     ZX4LP.5
C                                  SPECIFICATIONS FOR ARGUMENTS                  ZX4LP.6
      INTEGER    IA, N, M1, M2, IER, IW(*)                                       ZX4LP.7
      REAL S, A(IA,*), B(*), C(*), PSOL(*), DSOL(*), RW(*)                       ZX4LP.8
C      DOUBLE PRECISION S, A(IA,*), B(*), C(*), PSOL(*), DSOL(*), RW(*)          ZX4LP.9
C                                  SPECIFICATIONS FOR LOCAL VARIABLES            ZX4LP.10
      INTEGER    IWKIRT, K, M, NCODE, NTYPE                                      ZX4LP.11
C                                  SPECIFICATIONS FOR SPECIAL CASES              ZX4LP.12
C                                  SPECIFICATIONS FOR COMMON /WORKSP/            ZX4LP.13
      REAL       RWKSP(5000)                                                     ZX4LP.14
      REAL RDWKSP(2500)                                                          ZX4LP.15
      REAL DWKSP(2500)                                                           ZX4LP.16
C      DOUBLE PRECISION RDWKSP(2500)                                             ZX4LP.17
C      DOUBLE PRECISION DWKSP(2500)                                              ZX4LP.18
      COMPLEX    CWKSP(2500)                                                     ZX4LP.19
      COMPLEX    *16 CZWKSP(1250)                                                ZX4LP.20
      COMPLEX    *16 ZWKSP(1250)                                                 ZX4LP.21
      INTEGER    IWKSP(5000)                                                     ZX4LP.22
      LOGICAL    LWKSP(5000)                                                     ZX4LP.23
      EQUIVALENCE (DWKSP(1), RWKSP(1))                                           ZX4LP.24
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))                     ZX4LP.25
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))                     ZX4LP.26
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))                   ZX4LP.27
      COMMON     /WORKSP/ DWKSP                                                  ZX4LP.28
C                                  SPECIFICATIONS FOR INTRINSICS                 ZX4LP.29
C     INTRINSIC  DABS                                                            ZX4LP.30
      INTRINSIC  DABS                                                            ZX4LP.31
C      DOUBLE PRECISION DABS                                                     ZX4LP.32
C      DOUBLE PRECISION DABS                                                     ZX4LP.33
C                                  SPECIFICATIONS FOR SUBROUTINES                ZX4LP.34
C      EXTERNAL   E1POP, E1PSH, ISET, DSCAL, DSET, DDLPRS                        ZX4LP.35
      EXTERNAL   E1POP, E1PSH, ISET, DSCAL, DSET, DLPRS                          ZX4LP.36
C                                  SPECIFICATIONS FOR FUNCTIONS                  ZX4LP.37
      EXTERNAL   I1KGT, N1RCD, N1RTY                                             ZX4LP.38
      INTEGER    I1KGT, N1RCD, N1RTY                                             ZX4LP.39
C                                                                                ZX4LP.40
      CALL E1PSH ('ZX4LP ')                                                      ZX4LP.41
C                                  SET M, XLB, AND XUB                           ZX4LP.42
C                                  RW(I), I = 1,...,N CONTAINS XLB               ZX4LP.43
C                                  RW(I), I = N+1,...,2*N CONTAINS XUB           ZX4LP.44
      IER=0                                                                      ZX4LP.45
      M=M1+M2                                                                    ZX4LP.46
      IF(N.LT.M2)THEN                                                            ZX4LP.47
        IER=130                                                                  ZX4LP.48
        GOTO 9000                                                                ZX4LP.49
      ELSE                                                                       ZX4LP.50
        DO 10 I=1,M                                                              ZX4LP.51
          IF(B(I).NE.0.0E0)GOTO 20                                               ZX4LP.52
          IF(I.EQ.M)THEN                                                         ZX4LP.53
            IER=133                                                              ZX4LP.54
            GOTO 9000                                                            ZX4LP.55
          ENDIF                                                                  ZX4LP.56
   10   CONTINUE                                                                 ZX4LP.57
      ENDIF                                                                      ZX4LP.58
C                                                                                ZX4LP.59
   20 CONTINUE                                                                   ZX4LP.60
      CALL SSET(N,0.0E0,RW,1)                                                    ZX4LP.61
      CALL SSET(N,-1.0E30,RW(N+1),1)                                             ZX4LP.62
C                                  SET IRTYPE                                    ZX4LP.63
      IWKIRT=I1KGT(M,2)                                                          ZX4LP.64
      CALL ISET(M1,1,IWKSP(IWKIRT),1)                                            ZX4LP.65
      CALL ISET(M2,0,IWKSP(IWKIRT+M1),1)                                         ZX4LP.66
C                                  NEGATE C FOR A MINIMIZE PROBLEM               ZX4LP.67
      CALL SSCAL(N,-1.0E0,C,1)                                                   ZX4LP.68
C                                  CALL NEW ROUTINE DDLPRS                       ZX4LP.69
      CALL DLPRS(M,N,A,IA,B,B,C,IWKSP(IWKIRT),RW,RW(N+1),S,PSOL,DSOL)            ZX4LP.70
C                                  NEGATE C, DSOL AND S FOR A MAXIMIZE           ZX4LP.71
C                                  PROBLEM                                       ZX4LP.72
      CALL SSCAL(N,-1.0E0,C,1)                                                   ZX4LP.73
      CALL SSCAL(M,-1.0E0,DSOL,1)                                                ZX4LP.74
      S=-S                                                                       ZX4LP.75
C                                  CHECK FOR ERRORS                              ZX4LP.76
      NTYPE=N1RTY(1)                                                             ZX4LP.77
      NCODE=N1RCD(1)                                                             ZX4LP.78
      IF(NTYPE.EQ.4)THEN                                                         ZX4LP.79
        IF(NCODE.EQ.3.OR.NCODE.EQ.5.OR.NCODE.EQ.7)THEN                           ZX4LP.80
          IER=134                                                                ZX4LP.81
        ELSEIF(NCODE.EQ.6)THEN                                                   ZX4LP.82
          IER=135                                                                ZX4LP.83
        ELSEIF(NCODE.EQ.8)THEN                                                   ZX4LP.84
          IER=131                                                                ZX4LP.85
        ENDIF                                                                    ZX4LP.86
      ELSEIF(NTYPE.EQ.5.AND.NCODE.EQ.3)THEN                                      ZX4LP.87
        IER=129                                                                  ZX4LP.88
      ELSEIF(NTYPE.EQ.3.AND.NCODE.EQ.1)THEN                                      ZX4LP.89
        IER=136                                                                  ZX4LP.90
      ENDIF                                                                      ZX4LP.91
C                                                                                ZX4LP.92
 9000 CALL E1POP ('ZX4LP ')                                                      ZX4LP.93
      RETURN                                                                     ZX4LP.94
      END                                                                        ZX4LP.95
                                                                                 ZX4LP.96
