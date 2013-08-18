      SUBROUTINE SMT2(HSC1,IYY,JXX,IST,IEN,JST,JEN)                              SMT2.1
#     include <parame.incl>                                                      SMT2.2
       DIMENSION HSC1(IYY,JXX),HSC2(MIX,MJX)                                     SMT2.3
      DO 100 I=IST-1,IEN+1                                                       SMT2.4
        DO 100 J=JST-1,JEN+1                                                     SMT2.5
          HSC2(I,J)=.25*(HSC1(I,J-1)+HSC1(I,J)*2.+HSC1(I,J+1))                   SMT2.6
  100   CONTINUE                                                                 SMT2.7
                                                                                 SMT2.8
      DO 200 J=JST,JEN                                                           SMT2.9
        DO 200 I=IST,IEN                                                         SMT2.10
          HSC1(I,J)=.25*(HSC2(I-1,J)+2.*HSC2(I,J)+HSC2(I+1,J))                   SMT2.11
  200   CONTINUE                                                                 SMT2.12
                                                                                 SMT2.13
      RETURN                                                                     SMT2.14
      END                                                                        SMT2.15
                                                                                 SMT2.16
