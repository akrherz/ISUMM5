#include <defines.incl>                                                          07NOV00.1966
      SUBROUTINE TRIDI2(LS,L,N,CL,CM,CU,R1,R2,AU,A1,A2)                          07NOV00.1967
      IMPLICIT NONE                                                              07NOV00.1968
#     include <parame.incl>                                                      TRIDI2.2
      INTEGER LS,L,N,I,K                                                         07NOV00.1969
      REAL      CL(MIX,2:MKX),CM(MIX,MKX),CU(MIX,MKX-1),R1(MIX,MKX),             07NOV00.1970
     &          R2(MIX,MKX),AU(MIX,MKX-1),A1(MIX,MKX),A2(MIX,MKX)                TRIDI2.4
      REAL FK                                                                    07NOV00.1971
#ifdef IBMopt                                                                    23DEC04.3599
      real temp1(mix)                                                            23DEC04.3600
      INTEGER nn,is                                                              23DEC04.3601
#ifdef MPP1                                                                      23DEC04.3602
#define nf(A) noflic(A) 
#else                                                                            23DEC04.3604
#define nf(A) A        
#endif                                                                           23DEC04.3606
#endif                                                                           23DEC04.3607
                                                                                 23DEC04.3608
#ifdef VECTOR                                                                    07NOV00.1972
C These are used for the vector version, even though you cannot see them         07NOV00.1973
#ifndef IBMopt                                                                   23DEC04.3609
      INTEGER IST,IEN                                                            07NOV00.1974
#else                                                                            23DEC04.3610
      INTEGER IST,IEN,nn,is                                                      23DEC04.3611
#endif                                                                           23DEC04.3612
      IST=LS                                                                     07NOV00.1975
      IEN=L                                                                      07NOV00.1976
#endif                                                                           07NOV00.1977
                                                                                 23DEC04.3613
#ifdef IBMopt                                                                    23DEC04.3614
#ifdef MPP1                                                                      23DEC04.3615
      nn = ie_x0(L) - is_x0(LS) + 1                                              23DEC04.3616
      is = is_x0(LS)                                                             23DEC04.3617
      call vsrec(temp1(noflic(is)),cm(noflic(is),1),nn)                          23DEC04.3618
#else                                                                            23DEC04.3619
      nn = L-LS + 1                                                              23DEC04.3620
      is = LS                                                                    23DEC04.3621
      call vsrec(temp1(is),cm(is,1),nn)                                          23DEC04.3622
#endif                                                                           23DEC04.3623
#endif                                                                           23DEC04.3624
C-----------------------------------------------------------------------         TRIDI2.5
      DO I=LS,L                                                                  07NOV00.1978
#ifndef IBMopt                                                                   23DEC04.3625
        FK=1./CM(I,1)                                                            TRIDI2.7
#else                                                                            23DEC04.3626
        fk = temp1(i)                                                            23DEC04.3627
#endif                                                                           23DEC04.3628
        AU(I,1)=FK*CU(I,1)                                                       TRIDI2.8
        A1(I,1)=FK*R1(I,1)                                                       TRIDI2.9
        A2(I,1)=FK*R2(I,1)                                                       TRIDI2.10
      ENDDO                                                                      TRIDI2.11
      DO K=2,N-1                                                                 TRIDI2.12
        DO I=LS,L                                                                07NOV00.1979
#ifndef IBMopt                                                                   23DEC04.3629
          FK=1./(CM(I,K)-CL(I,K)*AU(I,K-1))                                      TRIDI2.14
#else                                                                            23DEC04.3630
           temp1(i) = CM(I,K)-CL(I,K)*AU(I,K-1)                                  23DEC04.3631
        ENDDO                                                                    23DEC04.3632
        call vsrec(temp1(nf(is)),temp1(nf(is)),nn)                               23DEC04.3633
                                                                                 23DEC04.3634
        DO I=LS,L                                                                23DEC04.3635
          FK= temp1(i)                                                           23DEC04.3636
#endif                                                                           23DEC04.3637
          AU(I,K)=FK*CU(I,K)                                                     TRIDI2.15
          A1(I,K)=FK*(R1(I,K)-CL(I,K)*A1(I,K-1))                                 TRIDI2.16
          A2(I,K)=FK*(R2(I,K)-CL(I,K)*A2(I,K-1))                                 TRIDI2.17
        ENDDO                                                                    TRIDI2.18
      ENDDO                                                                      TRIDI2.19
      DO I=LS,L                                                                  07NOV00.1980
#ifndef IBMopt                                                                   23DEC04.3638
        FK=1./(CM(I,N)-CL(I,N)*AU(I,N-1))                                        TRIDI2.21
#else                                                                            23DEC04.3639
         temp1(i) = CM(I,N)-CL(I,N)*AU(I,N-1)                                    23DEC04.3640
      ENDDO                                                                      23DEC04.3641
      call vsrec(temp1(nf(is)),temp1(nf(is)),nn)                                 23DEC04.3642
                                                                                 23DEC04.3643
      DO I=LS,L                                                                  23DEC04.3644
        FK= temp1(i)                                                             23DEC04.3645
#endif                                                                           23DEC04.3646
        A1(I,N)=FK*(R1(I,N)-CL(I,N)*A1(I,N-1))                                   TRIDI2.22
        A2(I,N)=FK*(R2(I,N)-CL(I,N)*A2(I,N-1))                                   TRIDI2.23
      ENDDO                                                                      TRIDI2.24
      DO K=N-1,1,-1                                                              TRIDI2.25
        DO I=LS,L                                                                07NOV00.1981
          A1(I,K)=A1(I,K)-AU(I,K)*A1(I,K+1)                                      TRIDI2.27
          A2(I,K)=A2(I,K)-AU(I,K)*A2(I,K+1)                                      TRIDI2.28
        ENDDO                                                                    TRIDI2.29
      ENDDO                                                                      TRIDI2.30
C-----------------------------------------------------------------------         TRIDI2.31
      RETURN                                                                     TRIDI2.32
      END                                                                        TRIDI2.33
                                                                                 TRIDI2.34
