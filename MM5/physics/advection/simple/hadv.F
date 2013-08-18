      SUBROUTINE HADV(KZZ,FTEN,UA,VA,F,MSF,DXX,IND,IN,VAR)                       23DEC04.2755
      IMPLICIT NONE                                                              HADV.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          HADV.3
C                                                                     C          HADV.4
C                                                                     C          HADV.5
C     THIS SUBROUTINE COMPUTES THE HORIZONTAL FLUX-DIVERGENCE TERMS.  C          HADV.6
C     SECOND-ORDER DIFFERENCE IS USED.                                C          HADV.7
C                                                                     C          HADV.8
C                                                                     C          HADV.9
C     IYY, JXX, AND KZZ : DIMENSIONS FOR ALL THE VARIABLES.           C          HADV.10
C                                                                     C          HADV.11
C     FTEN   : IS THE TENDENCY FOR VARIABLE 'F'.                      C          HADV.12
C                                                                     C          HADV.13
C     UA, VA : ARE P*U AND P*V.                                       C          HADV.14
C                                                                     C          HADV.15
C     MSF    : IS THE MAP SCALE FACTOR.                               C          HADV.16
C                                                                     C          HADV.17
C     DXX    : IS THE HORIZONTAL DISTANCE.                            C          HADV.18
C              = DX4  FOR IND=1.                                      C          HADV.19
C              = DX   FOR IND=2.                                      C          HADV.20
C              = DX16 FOR IND=3.                                      C          HADV.21
C                                                                     C          HADV.22
C     IND = 1 : FOR T AND QV.                                         C          HADV.23
C         = 2 : FOR QC AND QR.                                        C          HADV.24
C         = 3 : FOR U AND V.                                          C          HADV.25
C                                                                     C          HADV.26
C                                                                     C          HADV.27
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          HADV.28
#       include <parame.incl>                                                    HADV.29
#       include <param3.incl>                                                    HADV.30
#       include <various.incl>                                                   HADV.31
C                                                                                HADV.32
      INTEGER KZZ,IND,IN                                                         HADV.33
C                                                                                HADV.34
      REAL    UA(MIX,MJX,MKX),VA(MIX,MJX,MKX),                                   HADV.35
     1        FTEN(MIX,MJX,KZZ), F(MIX,MJX,KZZ)                                  HADV.36
      REAL    MSF(MIX,MJX)                                                       HADV.37
      REAL    DXX                                                                HADV.38
      character*1 VAR                                                            23DEC04.2756
C                                                                                HADV.39
      REAL    DXXMSF(MIX,MJX)                                                    HADV.40
      REAL    UAVG1,UAVG2,FX1,FX2,VAVG1,VAVG2,FY1,FY2,UCMONA,VCMONA,             HADV.41
     1        UCMONB,VCMONB,UCMONC,VCMONC                                        HADV.42
      INTEGER I,J,K,ICUT                                                         HADV.43
C                                                                                HADV.44
      ICUT=0                                                                     HADV.45
      IF(IN.GT.1)ICUT=1                                                          HADV.46
C                                                                                HADV.47
cmic$ do all autoscope                                                           HADV.48
cmic$1 shared(ILX,JLX,ICUT,DXX,MSF,DXXMSF)                                       HADV.49
cmic$2 private(I,J)                                                              HADV.50
c$doacross                                                                       HADV.51
c$& local(i,j)                                                                   HADV.52
c$omp parallel do default(shared)                                                HADV.53
c$omp&private(i,j)                                                               HADV.54
      DO J=2+ICUT,JLX-ICUT                                                       HADV.55
        DO I=2+ICUT,ILX-ICUT                                                     HADV.56
          DXXMSF(I,J)=1.0/(DXX*MSF(I,J)*MSF(I,J))                                HADV.57
        ENDDO                                                                    HADV.58
      ENDDO                                                                      HADV.59
C                                                                                HADV.60
C----------------------------------------------------------------------          HADV.61
C                                                                                HADV.62
      GOTO (10,30,50,70,90),IND                                                  23DEC04.2757
C                                                                                HADV.64
C-----FOR T AND QV:                                                              HADV.65
C                                                                                HADV.66
   10 CONTINUE                                                                   HADV.67
cmic$ do all autoscope                                                           HADV.68
cmic$1 shared(ILXM,JLXM,KL,ICUT,FTEN,F,UA,VA,DXXMSF)                             HADV.69
cmic$2 private(I,J,K)                                                            HADV.70
c$doacross                                                                       HADV.71
c$& local(i,j,k)                                                                 HADV.72
c$omp parallel do default(shared)                                                HADV.73
c$omp&private(i,j,k)                                                             HADV.74
      DO 20 J=2+ICUT,JLXM-ICUT                                                   HADV.75
        DO 20 K=1,KL                                                             HADV.76
          DO 20 I=2+ICUT,ILXM-ICUT                                               HADV.77
             FTEN(I,J,K)=                                                        HADV.78
     1           -((UA(I+1,J+1,K)+UA(I,J+1,K))*(F(I,J+1,K)+F(I,J,K))             HADV.79
     2           - (UA(I+1,J,  K)+UA(I,J,  K))*(F(I,J-1,K)+F(I,J,K))             HADV.80
     3           + (VA(I+1,J+1,K)+VA(I+1,J,K))*(F(I+1,J,K)+F(I,J,K))             HADV.81
     4           - (VA(I,  J+1,K)+VA(I,  J,K))*(F(I-1,J,K)+F(I,J,K)))            HADV.82
     5           *DXXMSF(I,J)                                                    HADV.83
   20     CONTINUE                                                               HADV.84
                                                                                 HADV.85
                                                                                 HADV.86
C                                                                                HADV.87
      RETURN                                                                     HADV.88
C                                                                                HADV.89
C-----FOR QC AND QR:                                                             HADV.90
C     UP-WIND VALUES OF QC AND QR ARE USED.                                      HADV.91
C                                                                                HADV.92
   30 CONTINUE                                                                   HADV.93
cmic$ do all autoscope                                                           HADV.94
cmic$1 shared(ILXM,JLXM,KL,FTEN,F,UA,VA,DXXMSF)                                  HADV.95
cmic$2 private(I,J,K)                                                            HADV.96
c$doacross                                                                       HADV.97
c$& local(i,j,k,uavg1,uavg2,vavg1,vavg2,fx1,fx2,fy1,fy2)                         HADV.98
c$omp parallel do default(shared)                                                HADV.99
c$omp&private(i,j,k,uavg1,uavg2,vavg1,vavg2,fx1,fx2,fy1,fy2)                     HADV.100
      DO 40 J=2+ICUT,JLXM-ICUT                                                   24SEP99.411
        DO 40 K=1,KL                                                             HADV.102
          DO 40 I=2+ICUT,ILXM-ICUT                                               24SEP99.412
            UAVG1=0.5*(UA(I+1,J,K)+UA(I,J,K))                                    HADV.104
            UAVG2=0.5*(UA(I+1,J+1,K)+UA(I,J+1,K))                                HADV.105
C                                                                                HADV.106
            IF(UAVG1.GE.0.)THEN                                                  HADV.107
              FX1=F(I,J-1,K)                                                     HADV.108
            ELSE                                                                 HADV.109
              FX1=F(I,J,K)                                                       HADV.110
            ENDIF                                                                HADV.111
C                                                                                HADV.112
            IF(UAVG2.GE.0.)THEN                                                  HADV.113
              FX2=F(I,J,K)                                                       HADV.114
            ELSE                                                                 HADV.115
              FX2=F(I,J+1,K)                                                     HADV.116
            ENDIF                                                                HADV.117
C                                                                                HADV.118
            VAVG1=0.5*(VA(I,J+1,K)+VA(I,J,K))                                    HADV.119
            VAVG2=0.5*(VA(I+1,J+1,K)+VA(I+1,J,K))                                HADV.120
C                                                                                HADV.121
            IF(VAVG1.GE.0.)THEN                                                  HADV.122
              FY1=F(I-1,J,K)                                                     HADV.123
            ELSE                                                                 HADV.124
              FY1=F(I,J,K)                                                       HADV.125
            ENDIF                                                                HADV.126
C                                                                                HADV.127
            IF(VAVG2.GE.0.)THEN                                                  HADV.128
              FY2=F(I,J,K)                                                       HADV.129
            ELSE                                                                 HADV.130
              FY2=F(I+1,J,K)                                                     HADV.131
            ENDIF                                                                HADV.132
C                                                                                HADV.133
            FTEN(I,J,K)=-(UAVG2*FX2-UAVG1*FX1+VAVG2*FY2-VAVG1*FY1)*              HADV.134
     +                  DXXMSF(I,J)                                              HADV.135
   40     CONTINUE                                                               HADV.136
                                                                                 HADV.137
                                                                                 HADV.138
C                                                                                HADV.139
      RETURN                                                                     HADV.140
C                                                                                HADV.141
C-----FOR U AND V:                                                               HADV.142
C                                                                                HADV.143
   50 CONTINUE                                                                   HADV.144
C                                                                                HADV.145
cmic$ do all autoscope                                                           HADV.146
cmic$1 shared(ILX,JLX,KL,ICUT,                                                   HADV.147
cmic$1        FTEN,F,UA,VA,DXXMSF)                                               HADV.148
cmic$2 private(I,J,K)                                                            HADV.149
c$doacross                                                                       HADV.150
c$& local(i,j,k,ucmona,vcmona,ucmonb,vcmonb,ucmonc,vcmonc)                       HADV.151
c$omp parallel do default(shared)                                                HADV.152
c$omp&private(i,j,k,ucmona,vcmona,ucmonb,vcmonb,ucmonc,vcmonc)                   HADV.153
      DO 60 J=2+ICUT,JLX-ICUT                                                    HADV.154
        DO 60 K=1,KL                                                             HADV.155
          DO 60 I=2+ICUT,ILX-ICUT                                                HADV.156
            UCMONA=UA(I+1,J,K)+2.*UA(I,J,K)+UA(I-1,J,K)                          HADV.157
            VCMONA=VA(I,J+1,K)+2.*VA(I,J,K)+VA(I,J-1,K)                          HADV.158
            UCMONB=UA(I+1,J+1,K)+2.*UA(I,J+1,K)+UA(I-1,J+1,K)+UCMONA             HADV.159
            VCMONB=VA(I+1,J+1,K)+2.*VA(I+1,J,K)+VA(I+1,J-1,K)+VCMONA             HADV.160
            UCMONC=UA(I+1,J-1,K)+2.*UA(I,J-1,K)+UA(I-1,J-1,K)+UCMONA             HADV.161
            VCMONC=VA(I-1,J+1,K)+2.*VA(I-1,J,K)+VA(I-1,J-1,K)+VCMONA             HADV.162
            FTEN(I,J,K)=                                                         HADV.163
     1           -((F(I,J+1,K)+F(I,J,K))*UCMONB                                  HADV.164
     2            -(F(I,J-1,K)+F(I,J,K))*UCMONC                                  HADV.165
     3            +(F(I+1,J,K)+F(I,J,K))*VCMONB                                  HADV.166
     4            -(F(I-1,J,K)+F(I,J,K))*VCMONC)                                 HADV.167
     5            *DXXMSF(I,J)                                                   HADV.168
   60     CONTINUE                                                               HADV.169
                                                                                 HADV.170
                                                                                 HADV.171
C                                                                                HADV.172
      RETURN                                                                     HADV.173
C     PASS IN DX4 AS DXX                                                         HADV.174
   70 CONTINUE                                                                   HADV.175
cmic$ do all autoscope                                                           HADV.176
cmic$1 shared(ILXM,JLXM,KL,icut,FTEN,F,UA,VA,DXXMSF)                             HADV.177
cmic$2 private(I,J,K)                                                            HADV.178
c$doacross                                                                       HADV.179
c$& local(i,j,k)                                                                 HADV.180
c$omp parallel do default(shared)                                                HADV.181
c$omp&private(i,j,k)                                                             HADV.182
      DO 80 J=2+ICUT,JLXM-ICUT                                                   HADV.183
        DO 80 K=2,KL                                                             HADV.184
          DO 80 I=2+ICUT,ILXM-ICUT                                               HADV.185
C     USE TWT, CONSISTENT WITH REST OF MODEL                                     HADV.186
            FTEN(I,J,K)=                                                         HADV.187
     1     -(((UA(I+1,J+1,K-1)+UA(I,J+1,K-1))*TWT(K,2)                           HADV.188
     2       +(UA(I+1,J+1,K)  +UA(I,J+1,K))*TWT(K,1))                            HADV.189
     3       *( F(I,J,K)+F(I,J+1,K))                                             HADV.190
     4      -((UA(I+1,J,K-1)+UA(I,J,K-1))*TWT(K,2)                               HADV.191
     5       +(UA(I+1,J,K)  +UA(I,J,K))*TWT(K,1))                                HADV.192
     6       *( F(I,J,K)+F(I,J-1,K))                                             HADV.193
     7      +((VA(I+1,J,K-1)+VA(I+1,J+1,K-1))*TWT(K,2)                           HADV.194
     8       +(VA(I+1,J,K)  +VA(I+1,J+1,K))*TWT(K,1))                            HADV.195
     9       *( F(I,J,K)+F(I+1,J,K))                                             HADV.196
     &      -((VA(I,J,K-1)+VA(I,J+1,K-1))*TWT(K,2)                               HADV.197
     1       +(VA(I,J,K)  +VA(I,J+1,K))*TWT(K,1))                                HADV.198
     2       *( F(I,J,K)+F(I-1,J,K)))                                            HADV.199
     3       *DXXMSF(I,J)                                                        HADV.200
   80     CONTINUE                                                               HADV.201
                                                                                 HADV.202
                                                                                 HADV.203
C                                                                                HADV.204
      RETURN                                                                     HADV.205
c                                                                                23DEC04.2758
c advection of cross-point variables with divergence already removed             23DEC04.2759
c                                                                                23DEC04.2760
   90 CONTINUE                                                                   23DEC04.2761
cmic$ do all autoscope                                                           23DEC04.2762
cmic$1 shared(ILXM,JLXM,KL,ICUT,FTEN,F,UA,VA,DXXMSF)                             23DEC04.2763
cmic$2 private(I,J,K)                                                            23DEC04.2764
c$omp parallel do default(shared)                                                23DEC04.2765
c$omp&private(i,j,k)                                                             23DEC04.2766
      DO 100 J=2+ICUT,JLXM-ICUT                                                  23DEC04.2767
        DO 100 K=1,KL                                                            23DEC04.2768
          DO 100 I=2+ICUT,ILXM-ICUT                                              23DEC04.2769
             FTEN(I,J,K)=                                                        23DEC04.2770
     1           -((UA(I+1,J+1,K)+UA(I,J+1,K))*(F(I,J+1,K)-F(I,J,K))             23DEC04.2771
     2           + (UA(I+1,J,  K)+UA(I,J,  K))*(F(I,J,K)-F(I,J-1,K))             23DEC04.2772
     3           + (VA(I+1,J+1,K)+VA(I+1,J,K))*(F(I+1,J,K)-F(I,J,K))             23DEC04.2773
     4           + (VA(I,  J+1,K)+VA(I,  J,K))*(F(I,J,K)-F(I-1,J,K)))            23DEC04.2774
     5           *DXXMSF(I,J)                                                    23DEC04.2775
  100     CONTINUE                                                               23DEC04.2776
                                                                                 23DEC04.2777
c  Instability correction (local extrema exceeding a certain threshold           23DEC04.2778
c                   must not grow further due to advection)                      23DEC04.2779
      if (var.eq.'T') then  ! Temperature                                        23DEC04.2780
C                                                                                23DEC04.2781
c$omp parallel do default(shared)                                                23DEC04.2782
c$omp&private(i,j,k)                                                             23DEC04.2783
      DO 220 J=2+ICUT,JLXM-ICUT                                                  23DEC04.2784
        DO 220 K=1,KL                                                            23DEC04.2785
          DO 220 I=2+ICUT,ILXM-ICUT                                              23DEC04.2786
      if (abs(F(I+1,J,K)+F(I-1,J,K)-2*F(I,J,K)).gt.5) then                       23DEC04.2787
      if ((F(I,J,K).gt.F(I+1,J,K)).and.(F(I,J,K).gt.F(I-1,J,K))) then            23DEC04.2788
        FTEN(I,J,K) = min(FTEN(I,J,K),0.)                                        23DEC04.2789
      else if ((F(I,J,K).lt.F(I+1,J,K)).and.(F(I,J,K).lt.F(I-1,J,K)))            23DEC04.2790
     f then                                                                      23DEC04.2791
      FTEN(I,J,K) = max(FTEN(I,J,K),0.)                                          23DEC04.2792
      endif                                                                      23DEC04.2793
      endif                                                                      23DEC04.2794
      if (abs(F(I,J+1,K)+F(I,J-1,K)-2*F(I,J,K)).gt.5) then                       23DEC04.2795
      if ((F(I,J,K).gt.F(I,J+1,K)).and.(F(I,J,K).gt.F(I,J-1,K))) then            23DEC04.2796
        FTEN(I,J,K) = min(FTEN(I,J,K),0.)                                        23DEC04.2797
      else if ((F(I,J,K).lt.F(I,J+1,K)).and.(F(I,J,K).lt.F(I,J-1,K)))            23DEC04.2798
     f then                                                                      23DEC04.2799
      FTEN(I,J,K) = max(FTEN(I,J,K),0.)                                          23DEC04.2800
      endif                                                                      23DEC04.2801
      endif                                                                      23DEC04.2802
 220   continue                                                                  23DEC04.2803
C                                                                                23DEC04.2804
       else if (var.eq.'Q') then   ! WV mixing ratio                             23DEC04.2805
C                                                                                23DEC04.2806
c$omp parallel do default(shared)                                                23DEC04.2807
c$omp&private(i,j,k)                                                             23DEC04.2808
      DO 230 J=2+ICUT,JLXM-ICUT                                                  23DEC04.2809
        DO 230 K=1,KL                                                            23DEC04.2810
          DO 230 I=2+ICUT,ILXM-ICUT                                              23DEC04.2811
      if (abs(F(I+1,J,K)+F(I-1,J,K)-2*F(I,J,K))/F(I,J,K).gt.0.2) then            23DEC04.2812
      if ((F(I,J,K).gt.F(I+1,J,K)).and.(F(I,J,K).gt.F(I-1,J,K))) then            23DEC04.2813
        FTEN(I,J,K) = min(FTEN(I,J,K),0.)                                        23DEC04.2814
      else if ((F(I,J,K).lt.F(I+1,J,K)).and.(F(I,J,K).lt.F(I-1,J,K)))            23DEC04.2815
     f then                                                                      23DEC04.2816
      FTEN(I,J,K) = max(FTEN(I,J,K),0.)                                          23DEC04.2817
      endif                                                                      23DEC04.2818
      endif                                                                      23DEC04.2819
      if (abs(F(I,J+1,K)+F(I,J-1,K)-2*F(I,J,K))/F(I,J,K).gt.0.2) then            23DEC04.2820
      if ((F(I,J,K).gt.F(I,J+1,K)).and.(F(I,J,K).gt.F(I,J-1,K))) then            23DEC04.2821
        FTEN(I,J,K) = min(FTEN(I,J,K),0.)                                        23DEC04.2822
      else if ((F(I,J,K).lt.F(I,J+1,K)).and.(F(I,J,K).lt.F(I,J-1,K)))            23DEC04.2823
     f then                                                                      23DEC04.2824
      FTEN(I,J,K) = max(FTEN(I,J,K),0.)                                          23DEC04.2825
      endif                                                                      23DEC04.2826
      endif                                                                      23DEC04.2827
 230  continue                                                                   23DEC04.2828
c                                                                                23DEC04.2829
        else if (var.eq.'C') then   ! CLW mixing ratio                           23DEC04.2830
c                                                                                23DEC04.2831
c$omp parallel do default(shared)                                                23DEC04.2832
c$omp&private(i,j,k)                                                             23DEC04.2833
      DO 240 J=2+ICUT,JLXM-ICUT                                                  23DEC04.2834
        DO 240 K=1,KL                                                            23DEC04.2835
          DO 240 I=2+ICUT,ILXM-ICUT                                              23DEC04.2836
      if (abs(F(I+1,J,K)+F(I-1,J,K)-2*F(I,J,K)).gt.2.e-4) then                   23DEC04.2837
      if ((F(I,J,K).gt.F(I+1,J,K)).and.(F(I,J,K).gt.F(I-1,J,K))) then            23DEC04.2838
        FTEN(I,J,K) = min(FTEN(I,J,K),0.)                                        23DEC04.2839
      else if ((F(I,J,K).lt.F(I+1,J,K)).and.(F(I,J,K).lt.F(I-1,J,K)))            23DEC04.2840
     f then                                                                      23DEC04.2841
      FTEN(I,J,K) = max(FTEN(I,J,K),0.)                                          23DEC04.2842
      endif                                                                      23DEC04.2843
      endif                                                                      23DEC04.2844
      if (abs(F(I,J+1,K)+F(I,J-1,K)-2*F(I,J,K)).gt.2.e-4) then                   23DEC04.2845
      if ((F(I,J,K).gt.F(I,J+1,K)).and.(F(I,J,K).gt.F(I,J-1,K))) then            23DEC04.2846
        FTEN(I,J,K) = min(FTEN(I,J,K),0.)                                        23DEC04.2847
      else if ((F(I,J,K).lt.F(I,J+1,K)).and.(F(I,J,K).lt.F(I,J-1,K)))            23DEC04.2848
     f then                                                                      23DEC04.2849
      FTEN(I,J,K) = max(FTEN(I,J,K),0.)                                          23DEC04.2850
      endif                                                                      23DEC04.2851
      endif                                                                      23DEC04.2852
 240  continue                                                                   23DEC04.2853
                                                                                 23DEC04.2854
      endif                                                                      23DEC04.2855
c                                                                                23DEC04.2856
      return                                                                     23DEC04.2857
                                                                                 23DEC04.2858
      END                                                                        HADV.206
                                                                                 HADV.207
                                                                                 23DEC04.2859
                                                                                 23DEC04.2860
C----------------------------------------------------------------------------    23DEC04.2861
      SUBROUTINE ADVDUV( UTEN, VTEN, UA, VA, QDOT, DXX, REARTH,                  23DEC04.2862
     &                   WA, DIVX, U3D, V3D, UCD, VCD, IN )                      23DEC04.2863
      IMPLICIT NONE                                                              23DEC04.2864
                                                                                 23DEC04.2865
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          23DEC04.2866
C  UNIFIED 3-D ADVECTION OF U,V                                       C          23DEC04.2867
C                                                                     C          23DEC04.2868
C     This subroutine computes the horizontal and vertical            C          23DEC04.2869
C     flux-divergence terms, divergence terms, curvature terms,       C          23DEC04.2870
C     and Coriolis terms for wind components U and V                  C          23DEC04.2871
C     Second-order difference is used for flux-divergence terms.      C          23DEC04.2872
C                                                                     C          23DEC04.2873
C     Adapted from SOLVE parallel loop 6, and optimized HADV          C          23DEC04.2874
C     and VADV by CJC, March. 2004                                    C          23DEC04.2875
C                                                                     C          23DEC04.2876
C INPUT ARGUMENTS                                                     C          23DEC04.2877
C                                                                     C          23DEC04.2878
C     IN     : Nest subscript                                         C          23DEC04.2879
C                                                                     C          23DEC04.2880
C     UA, VA, WA : are P*U, P*V, P*W.                                 C          23DEC04.2881
C                                                                     C          23DEC04.2882
C     QDOT   : is the vertical SIGMA-velocity                         C          23DEC04.2883
C                                                                     C          23DEC04.2884
C     DIVX   : is the (3D) divergence of (U,V,W)                      C          23DEC04.2885
C                                                                     C          23DEC04.2886
C     DXX    : is the horizontal distance.                            C          23DEC04.2887
C              = DX16 FOR IND=3.                                      C          23DEC04.2888
C                                                                     C          23DEC04.2889
C     REARTH : is the radius of the Earth.                            C          23DEC04.2890
C                                                                     C          23DEC04.2891
C     U3D, V3D : are decoupled U and V.                               C          23DEC04.2892
C                                                                     C          23DEC04.2893
C     UCD, VCD : are  coupled  U and V.                               C          23DEC04.2894
C                                                                     C          23DEC04.2895
C OUTPUT ARGUMENTS                                                    C          23DEC04.2896
C                                                                     C          23DEC04.2897
C     UTEN,VTEN : is the tendency for variables U, V                  C          23DEC04.2898
C                                                                     C          23DEC04.2899
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          23DEC04.2900
                                                                                 23DEC04.2901
#       include <parame.incl>                                                    23DEC04.2902
#       include <param3.incl>                                                    23DEC04.2903
#       include <various.incl>                                                   23DEC04.2904
#       include <point2d.incl>                                                   23DEC04.2905
#       include <rpstar.incl>                                                    23DEC04.2906
                                                                                 23DEC04.2907
C.......  Arguments:                                                             23DEC04.2908
                                                                                 23DEC04.2909
      INTEGER IN                                                                 23DEC04.2910
      REAL    UTEN(MIX,MJX,MKX), VTEN(MIX,MJX,MKX)                               23DEC04.2911
      REAL    UA(MIX,MJX,MKX),   VA(MIX,MJX,MKX)                                 23DEC04.2912
      REAL    QDOT(MIX,MJX,KXP1),WA(MIX,MJX,KXP1)                                23DEC04.2913
      REAL   DIVX(MIX,MJX,MKX),  MSF(MIX,MJX)                                    23DEC04.2914
      REAL    U3D(MIX,MJX,MKX),  V3D(MIX,MJX,MKX)                                23DEC04.2915
      REAL    UCD(MIX,MJX,MKX),  VCD(MIX,MJX,MKX)                                23DEC04.2916
      REAL    DXX, REARTH                                                        23DEC04.2917
                                                                                 23DEC04.2918
C.......  Local Variables:                                                       23DEC04.2919
                                                                                 23DEC04.2920
      REAL    DXXMSF(MIX), EFC(MIX), EFS(MIX)                                    23DEC04.2921
      REAL    DDS(KXP1)                                                          23DEC04.2922
      REAL    WADOT(MIX,KXP1)                                                    23DEC04.2923
      INTEGER ICUT,I,J,K                                                         23DEC04.2924
                                                                                 23DEC04.2925
      REAL    UU, VV, QQ                                                         23DEC04.2926
      REAL    UCMONA, VCMONA, UCMONB, VCMONB, UCMONC, VCMONC                     23DEC04.2927
      REAL    DIVD, DIAG, WABAR, AMFAC, DUV                                      23DEC04.2928
                                                                                 23DEC04.2929
C----------------------------------------------------------------------          23DEC04.2930
                                                                                 23DEC04.2931
      ICUT=0                                                                     23DEC04.2932
      IF ( IN.GT.1 ) ICUT=1                                                      23DEC04.2933
                                                                                 23DEC04.2934
      DO K=1,KL                                                                  23DEC04.2935
          DDS(K) = 1.0 / DSIGMA(K)                                               23DEC04.2936
      END DO                                                                     23DEC04.2937
                                                                                 23DEC04.2938
cmic$ do all autoscope                                                           23DEC04.2939
cmic$1    shared(ILX,JLX,KL,ICUT,                                                23DEC04.2940
cmic$2           UTEN,VTEN,U3D,V3D,UCD,VCD,UA,VA,QDOT,MSF,DXX,DDS)               23DEC04.2941
cmic$3   private(I,J,K,ucmona,vcmona,ucmonb,vcmonb,ucmonc,vcmonc,                23DEC04.2942
cmic$4           WADOT,WABAR,AMFAC,DUV,DIVD,DIAG,DXXMSF,EFC,EFS,UU,VV,QQ)        23DEC04.2943
                                                                                 23DEC04.2944
c$doacross 
c$&  local(i,j,k,ucmona,vcmona,ucmonb,vcmonb,ucmonc,vcmonc,     
c$&        WADOT,WABAR,AMFAC,DUV,DIVD,DIAG,DXXMSF,EFC,EFS,UU,VV,QQ) 
                                                                                 23DEC04.2947
c$omp parallel do                                                                23DEC04.2948
c$omp&   default(shared)                                                         23DEC04.2949
c$omp&   private(i,j,k,ucmona,vcmona,ucmonb,vcmonb,ucmonc,vcmonc,                23DEC04.2950
c$omp&          WADOT,WABAR,AMFAC,DUV,DIVD,DIAG,DXXMSF,EFC,EFS,UU,VV,QQ)         23DEC04.2951
                                                                                 23DEC04.2952
      DO  J=2+ICUT,JLX-ICUT                                                      23DEC04.2953
                                                                                 23DEC04.2954
         DO I=2+ICUT,ILX-ICUT                                                    23DEC04.2955
            DXXMSF(I) = 1.0/(DXX*MSFD(I,J)*MSFD(I,J))                            23DEC04.2956
            EFC(I)    = EF(I,J)*COSROT(I,J)                                      23DEC04.2957
            EFS(I)    = EF(I,J)*SINROT(I,J)                                      23DEC04.2958
         ENDDO                                                                   23DEC04.2959
                                                                                 23DEC04.2960
         DO K=1,KL+1                                                             23DEC04.2961
         DO I=2+ICUT,ILX-ICUT                                                    23DEC04.2962
            WADOT(I,K)=0.125*(WA(I-1,J-1,K) + WA(I,J-1,K)+                       23DEC04.2963
     &                        WA(I-1,J  ,K) + WA(I,J  ,K))                       23DEC04.2964
         ENDDO                                                                   23DEC04.2965
         ENDDO                                                                   23DEC04.2966
                                                                                 23DEC04.2967
         DO  K=1,KL                                                              23DEC04.2968
                                                                                 23DEC04.2969
            !!         Horizontal advection                                      23DEC04.2970
                                                                                 23DEC04.2971
            DO  I=2+ICUT,ILX-ICUT                                                23DEC04.2972
               DIVD = 0.25*(DIVX(I,J  ,K) + DIVX(I-1,J  ,K)+                     23DEC04.2973
     &                      DIVX(I,J-1,K) + DIVX(I-1,J-1,K))                     23DEC04.2974
               UCMONA = UA(I+1,J,K)   + 2.*UA(I,J,K)   + UA(I-1,J,K)             23DEC04.2975
               VCMONA = VA(I,J+1,K)   + 2.*VA(I,J,K)   + VA(I,J-1,K)             23DEC04.2976
               UCMONB = UA(I+1,J+1,K) + 2.*UA(I,J+1,K) + UA(I-1,J+1,K)           23DEC04.2977
               VCMONB = VA(I+1,J+1,K) + 2.*VA(I+1,J,K) + VA(I+1,J-1,K)           23DEC04.2978
               UCMONC = UA(I+1,J-1,K) + 2.*UA(I,J-1,K) + UA(I-1,J-1,K)           23DEC04.2979
               VCMONC = VA(I-1,J+1,K) + 2.*VA(I-1,J,K) + VA(I-1,J-1,K)           23DEC04.2980
               DIAG   = DIVD - DXXMSF(I)*( (UCMONB - UCMONC)                     23DEC04.2981
     &                                   + (VCMONB - VCMONC) )                   23DEC04.2982
               UCMONB = UCMONB + UCMONA                                          23DEC04.2983
               VCMONB = VCMONB + VCMONA                                          23DEC04.2984
               UCMONC = UCMONC + UCMONA                                          23DEC04.2985
               VCMONC = VCMONC + VCMONA                                          23DEC04.2986
               UTEN(I,J,K) =  U3D(I,J,K)*DIAG -                                  23DEC04.2987
     &                        DXXMSF(I)*( U3D(I,J+1,K)*UCMONB                    23DEC04.2988
     &                                  - U3D(I,J-1,K)*UCMONC                    23DEC04.2989
     &                                  + U3D(I+1,J,K)*VCMONB                    23DEC04.2990
     &                                  - U3D(I-1,J,K)*VCMONC )                  23DEC04.2991
               VTEN(I,J,K) =  V3D(I,J,K)*DIAG -                                  23DEC04.2992
     &                        DXXMSF(I)*( V3D(I,J+1,K)*UCMONB                    23DEC04.2993
     &                                  - V3D(I,J-1,K)*UCMONC                    23DEC04.2994
     &                                  + V3D(I+1,J,K)*VCMONB                    23DEC04.2995
     &                                  - V3D(I-1,J,K)*VCMONC )                  23DEC04.2996
            ENDDO                                                                23DEC04.2997
                                                                                 23DEC04.2998
            !!         Coriolis and curvature terms                              23DEC04.2999
                                                                                 23DEC04.3000
            DO  I=2+ICUT,ILX-ICUT                                                23DEC04.3001
               WABAR = WADOT(I,K) + WADOT(I,K+1)                                 23DEC04.3002
               AMFAC = WABAR*RPDOTA(I,J)*REARTH                                  23DEC04.3003
               DUV   = UCD(I,J,K)*DMDY(I,J) - VCD(I,J,K)*DMDX(I,J)               23DEC04.3004
               UTEN(I,J,K) = UTEN(I,J,K) + F(I,J)*VCD(I,J,K)    !  H. Coriolis   23DEC04.3005
     &                                   - EFC(I)*WABAR         !  V. Coriolis   23DEC04.3006
     &                                   + V3D(I,J,K)*DUV       !  XY-curvature  23DEC04.3007
     &                                   - UCD(I,J,K)*AMFAC     !  Z-curvature   23DEC04.3008
               VTEN(I,J,K) = VTEN(I,J,K) - F(I,J)*UCD(I,J,K)                     23DEC04.3009
     &                                   + EFS(I)*WABAR                          23DEC04.3010
     &                                   - U3D(I,J,K)*DUV                        23DEC04.3011
     &                                   - VCD(I,J,K)*AMFAC                      23DEC04.3012
            ENDDO                                                                23DEC04.3013
                                                                                 23DEC04.3014
            IF ( K .EQ. 1 ) GO TO  99   !! no vertical advection at k=1          23DEC04.3015
                                                                                 23DEC04.3016
            !!         Vertical advection                                        23DEC04.3017
                                                                                 23DEC04.3018
            DO  I=2+ICUT,ILX-ICUT                                                23DEC04.3019
               QQ  = 0.25*( QDOT(I,J  ,K) + QDOT(I-1,J  ,K)                      23DEC04.3020
     &                    + QDOT(I,J-1,K) + QDOT(I-1,J-1,K))                     23DEC04.3021
               UU = QQ*( TWT(K,1)*UCD(I,J,K) + TWT(K,2)*UCD(I,J,K-1) )           23DEC04.3022
               UTEN(I,J,K-1) = UTEN(I,J,K-1) - UU*DDS(K-1)                       23DEC04.3023
               UTEN(I,J,K  ) = UTEN(I,J,K  ) + UU*DDS(K  )                       23DEC04.3024
               VV = QQ*( TWT(K,1)*VCD(I,J,K) + TWT(K,2)*VCD(I,J,K-1) )           23DEC04.3025
               VTEN(I,J,K-1) = VTEN(I,J,K-1) - VV*DDS(K-1)                       23DEC04.3026
               VTEN(I,J,K  ) = VTEN(I,J,K  ) + VV*DDS(K  )                       23DEC04.3027
            END DO                                                               23DEC04.3028
                                                                                 23DEC04.3029
  99        CONTINUE    !!  target of "if K=1" (no vertical advection)           23DEC04.3030
                                                                                 23DEC04.3031
         END DO         !!  end loop on K                                        23DEC04.3032
                                                                                 23DEC04.3033
      ENDDO             !!  end parallel loop on J                               23DEC04.3034
                                                                                 23DEC04.3035
      RETURN                                                                     23DEC04.3036
                                                                                 23DEC04.3037
      END                                                                        23DEC04.3038
                                                                                 23DEC04.3039
