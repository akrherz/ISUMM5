      SUBROUTINE VDIFF(PSTAR,UST,KL,KLM,J,DTPBL,ILX,IMOISTX)                     VDIFF.1
C**********************************************************************          VDIFF.2
C-- VERTICAL EDDY DIFFUSION                                                      VDIFF.3
C-- THIS SUBROUTINE IS CALLED WHEN AN ENTIRE I-SLAB HAS NO FREE CONVECTIVE       VDIFF.4
C-- CONDITIONS.  OTHERWISE, SUBROUTINE ACM IS CALLED WHICH INCLUDES A VDIFF      VDIFF.5
C-- PORTION.  THEREFORE, THE FRONT END OF THIS SUBROUTINE WHERE SURFACE          VDIFF.6
C-- FLUX BC'S ARE SET UP IS IDENTICAL TO ACM.  THE EDDY MIXING IS IDENTICAL      VDIFF.7
C-- TO THE VDIFF PORTION OF SUBROUTINE ACM.                                      VDIFF.8
C-- THE SOLE PURPOSE OF THIS SUBROUTINE IS TO ALLOW FOR MORE EFFICIENT           VDIFF.9
C-- WHEN ACM IS NOT USED FOR AN ENTIRE I-SLAB.                                   VDIFF.10
C**********************************************************************          VDIFF.11
      IMPLICIT NONE                                                              VDIFF.12
C                                                                                VDIFF.13
      INTEGER NSP                                                                VDIFF.14
      PARAMETER (NSP = 6)                                                        VDIFF.15
#     include <parame.incl>                                                      VDIFF.16
#     include <comicl.incl>                                                      VDIFF.17
C                                                                                VDIFF.18
C...........Arguments                                                            VDIFF.19
      REAL PSTAR(MIX,MJX),UST(MIX,MJX),DTPBL                                     VDIFF.20
      INTEGER KL,ILX,KLM,J,IMOISTX                                               VDIFF.21
C                                                                                VDIFF.22
C.......... Local arrays                                                         VDIFF.23
      REAL*8 AI(MIX,MKX),BI(MIX,MKX),EI(MIX,MKX)                                 VDIFF.24
      REAL*8 DI(MIX,MKX),UI(MIX,MKX)                                             VDIFF.25
      REAL*8 BETA(MIX)                                                           VDIFF.26
      REAL VCI(MIX,MKX,NSP),FS(MIX,NSP)                                          VDIFF.27
      REAL GAM(MIX,MKX)                                                          VDIFF.28
      REAL BCBOTN(MIX,NSP),DTS                                                   VDIFF.29
C..........Local variables                                                       VDIFF.30
      REAL DTLIM,DTRAT                                                           VDIFF.31
      REAL EKZ,PSTARI,FM,WSPD                                                    VDIFF.32
C..........indexes                                                               VDIFF.33
      INTEGER K,I,L,NL,NLP,NSPX                                                  VDIFF.34
C **                                                                             VDIFF.35
C                                                                                VDIFF.36
      DO 10 K=1,KL                                                               VDIFF.37
      DO 10 I=1,ILX                                                              VDIFF.38
        VCI(I,K,1) = THETA(I,K)                                                  VDIFF.39
        VCI(I,K,2) = QVS(I,K)                                                    VDIFF.40
        VCI(I,K,3) = US(I,K)                                                     VDIFF.41
        VCI(I,K,4) = VS(I,K)                                                     VDIFF.42
10    CONTINUE                                                                   VDIFF.43
      NSPX=4                                                                     VDIFF.44
                                                                                 VDIFF.45
C -- Also mix cloud water and ice if necessary                                   VDIFF.46
      IF (IMOISTX.NE.1.AND.IMOISTX.NE.3) THEN                                    VDIFF.47
        DO 15 K=1,KL                                                             VDIFF.48
        DO 15 I=1,ILX                                                            VDIFF.49
          VCI(I,K,5) = QCS(I,K)                                                  VDIFF.50
          VCI(I,K,6) = QIS(I,K)                                                  VDIFF.51
15      CONTINUE                                                                 VDIFF.52
        NSPX=6                                                                   VDIFF.53
      ENDIF                                                                      VDIFF.54
C                                                                                VDIFF.55
55      DTLIM = DTPBL                                                            VDIFF.56
        DO 60 K = 1,KLM                                                          VDIFF.57
          DO 65 I = 1,ILX                                                        VDIFF.58
              EKZ   = EDDYZ(I,K) / DTPBL * DSIGFI(K)                             VDIFF.59
              IF(EKZ.LT.1.0E-10)                                                 VDIFF.60
     >          PRINT *,' EKZ,EDDY=',EKZ,EDDYZ(I,K),I,K                          VDIFF.61
              DTLIM = AMIN1(0.75 / EKZ,DTLIM)                                    VDIFF.62
65        CONTINUE                                                               VDIFF.63
60      CONTINUE                                                                 VDIFF.64
        IF (DTLIM .EQ. DTPBL) THEN                                               VDIFF.65
          NLP = 1                                                                VDIFF.66
         ELSE                                                                    VDIFF.67
          NLP = INT(DTPBL / DTLIM + 1.0)                                         VDIFF.68
        END IF                                                                   VDIFF.69
        DTS   = DTPBL / NLP                                                      VDIFF.70
      DTRAT = DTS / DTPBL                                                        VDIFF.71
C **                                                                             VDIFF.72
      DO 17 I=1,ILX                                                              VDIFF.73
        PSTARI = 1.0 / PSTAR(I,J)                                                VDIFF.74
        FS(I,1) = -UST(I,J) * TST(I,J) * DENSX(I,1) * DTS * PSTARI               19DEC02.1875
        FS(I,2) = -UST(I,J) * QST(I) * DENSX(I,1) * DTS * PSTARI                 VDIFF.76
        FM    = -USTM(I) * USTM(I) * DENSX(I,1) * DTS * PSTARI                   19DEC02.1876
C        ALPH  = ATAN2(VS(I,1), US(I,1))                                         VDIFF.78
C        FS(I,3) = FM * COS(ALPH)                                                VDIFF.79
C        FS(I,4) = FM * SIN(ALPH)                                                VDIFF.80
        WSPD =SQRT(US(I,1)*US(I,1)+VS(I,1)*VS(I,1))+1.E-9                        VDIFF.81
        FS(I,3) = FM * US(I,1)/WSPD                                              VDIFF.82
        FS(I,4) = FM * VS(I,1)/WSPD                                              VDIFF.83
        FS(I,5) = 0.0                                                            VDIFF.84
        FS(I,6) = 0.0    ! SURFACE FLUXES OF CLOUD WATER AND ICE = 0             VDIFF.85
17    CONTINUE                                                                   VDIFF.86
C                                                                                VDIFF.87
C                                                                                VDIFF.88
      DO 70 L = 1,NSPX                                                           VDIFF.89
        DO 75 I = 1,ILX                                                          VDIFF.90
             BCBOTN(I,L) = -G1000 * FS(I,L) * DSIGHI(1)                          VDIFF.91
75      CONTINUE                                                                 VDIFF.92
70    CONTINUE                                                                   VDIFF.93
                                                                                 VDIFF.94
C                                                                                VDIFF.95
          DO 80 K = 2,KL                                                         VDIFF.96
            DO 85 I = 1,ILX                                                      VDIFF.97
C------- note that we are using eddyz(k-1) instead of eddyz(k) here              VDIFF.98
C------- keep dsighi(k)                                                          VDIFF.99
                AI(I,K) = -EDDYZ(I,K-1) * CRANKP * DSIGHI(K) * DTRAT             VDIFF.100
85          CONTINUE                                                             VDIFF.101
80        CONTINUE                                                               VDIFF.102
C                                                                                VDIFF.103
          DO 90 K = 1,KLM                                                        VDIFF.104
            DO 95 I = 1,ILX                                                      VDIFF.105
                EI(I,K) = -EDDYZ(I,K) * CRANKP * DSIGHI(K) * DTRAT               VDIFF.106
95          CONTINUE                                                             VDIFF.107
90        CONTINUE                                                               VDIFF.108
C                                                                                VDIFF.109
          DO 100 K = 2,KLM                                                       VDIFF.110
            DO 105 I = 1,ILX                                                     VDIFF.111
                BI(I,K) = 1.0 - EI(I,K) - AI(I,K)                                VDIFF.112
105         CONTINUE                                                             VDIFF.113
100       CONTINUE                                                               VDIFF.114
C                                                                                VDIFF.115
          DO 109 I = 1,ILX                                                       VDIFF.116
            BI(I,1) = 1.0 - EI(I,1)                                              VDIFF.117
            BI(I,KL)  = 1.0 - AI(I,KL)                                           VDIFF.118
109       CONTINUE                                                               VDIFF.119
C                                                                                VDIFF.120
      DO 4000 NL = 1,NLP          ! SUB TIME LOOP IF KZ TOO BIG                  VDIFF.121
        DO 3000 L = 1,NSPX        ! loop for thetav, q, u and v                  VDIFF.122
          DO 110 K = 2,KLM                                                       VDIFF.123
            DO 115 I = 1,ILX                                                     VDIFF.124
                DI(I,K) = VCI(I,K,L) - (1.0 - CRANKP) * EI(I,K) /                VDIFF.125
     &              CRANKP * (VCI(I,K+1,L) - VCI(I,K,L))  +                      VDIFF.126
     &              (1.0 - CRANKP) * AI(I,K) / CRANKP *                          VDIFF.127
     &              (VCI(I,K,L) - VCI(I,K-1,L))                                  VDIFF.128
115         CONTINUE                                                             VDIFF.129
110       CONTINUE                                                               VDIFF.130
C                                                                                VDIFF.131
                                                                                 VDIFF.132
          DO 116 I = 1,ILX                                                       VDIFF.133
            DI(I,1) = VCI(I,1,L) - (1.0 - CRANKP) *                              VDIFF.134
     &             EI(I,1) / CRANKP *                                            VDIFF.135
     &             (VCI(I,2,L) - VCI(I,1,L))+BCBOTN(I,L)                         VDIFF.136
            DI(I,KL)  = VCI(I,KL,L) + (1.0 - CRANKP) *                           VDIFF.137
     &             AI(I,KL) / CRANKP *                                           VDIFF.138
     &             (VCI(I,KL,L) - VCI(I,KLM,L))                                  VDIFF.139
116       CONTINUE                                                               VDIFF.140
C                                                                                VDIFF.141
C.... SOLVE TRIDIAGONAL SYSTEM                                                   VDIFF.142
          DO 117 I = 1,ILX                                                       VDIFF.143
C            IF (BI(I,1) .EQ. 0.0) STOP 2345                                     VDIFF.144
            BETA(I)  = BI(I,1)                                                   VDIFF.145
            UI(I,1) = DI(I,1) / BETA(I)                                          VDIFF.146
117       CONTINUE                                                               VDIFF.147
C                                                                                VDIFF.148
C     ** DECOMPOSITION AND FORWARD SUBSTITUTION                                  VDIFF.149
          DO 120 K = 2,KL                                                        VDIFF.150
            DO 122 I = 1,ILX                                                     VDIFF.151
                GAM(I,K) = EI(I,K-1) / BETA(I)                                   VDIFF.152
                BETA(I)  = BI(I,K) - AI(I,K) * GAM(I,K)                          VDIFF.153
122         CONTINUE                                                             VDIFF.154
            DO 125 I = 1,ILX                                                     VDIFF.155
                UI(I,K)  = (DI(I,K) - AI(I,K) * UI(I,K-1)) / BETA(I)             VDIFF.156
125         CONTINUE                                                             VDIFF.157
120       CONTINUE                                                               VDIFF.158
C     ** BACK SUBSTITUTION                                                       VDIFF.159
          DO 130 K = KLM,1,-1                                                    VDIFF.160
            DO 135 I = 1,ILX                                                     VDIFF.161
                 UI(I,K) = UI(I,K) - GAM(I,K+1) * UI(I,K+1)                      VDIFF.162
135         CONTINUE                                                             VDIFF.163
130       CONTINUE                                                               VDIFF.164
C                                                                                VDIFF.165
C.... BACK TO VCI                                                                VDIFF.166
C                                                                                VDIFF.167
          DO 140 K = 1,KL                                                        VDIFF.168
            DO 145 I = 1,ILX                                                     VDIFF.169
                VCI(I,K,L)= UI(I,K)                                              VDIFF.170
145         CONTINUE                                                             VDIFF.171
140       CONTINUE                                                               VDIFF.172
C**                                                                              VDIFF.173
3000    CONTINUE       ! END LOOP FOR THETAV, Q, U AND V                         VDIFF.174
4000  CONTINUE         ! END OF SUB-TIME LOOP                                    VDIFF.175
C2200  CONTINUE         ! END OF I LOOP                                          VDIFF.176
C                                                                                VDIFF.177
      DO 150 K = 1,KL                                                            VDIFF.178
      DO 150 I = 1,ILX                                                           VDIFF.179
        THETA(I,K) = VCI(I,K,1)                                                  VDIFF.180
        QVS(I,K)    = VCI(I,K,2)                                                 VDIFF.181
        US(I,K)     = VCI(I,K,3)                                                 VDIFF.182
        VS(I,K)     = VCI(I,K,4)                                                 VDIFF.183
150   CONTINUE                                                                   VDIFF.184
C                                                                                VDIFF.185
      IF (IMOISTX.NE.1.AND.IMOISTX.NE.3) THEN                                    VDIFF.186
        DO 155 K=1,KL                                                            VDIFF.187
        DO 155 I=1,ILX                                                           VDIFF.188
          QCS(I,K) = VCI(I,K,5)                                                  VDIFF.189
          QIS(I,K) = VCI(I,K,6)                                                  VDIFF.190
155      CONTINUE                                                                VDIFF.191
      ENDIF                                                                      VDIFF.192
                                                                                 VDIFF.193
C **                                                                             VDIFF.194
      RETURN                                                                     VDIFF.195
      END                                                                        VDIFF.196
