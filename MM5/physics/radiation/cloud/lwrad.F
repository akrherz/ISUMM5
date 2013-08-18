      SUBROUTINE LWRAD(J,RTTEN,PSA,GLW,OLW,XLAT,XLONG,TGA,EMISS,                 07NOV00.2183
     &                 T,QV,QC,QR,QI,QNI,PP,INEST)                               LWRAD.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         LWRAD.3
C                                                                                LWRAD.4
C     TO CALCULATE LONG WAVE RADIATION THROUGH CLOUDS AND WATER                  LWRAD.5
C     VAPOUR - BASED ON STEPHENS (1978) AND GARAND (1983)                        LWRAD.6
C                                                                                LWRAD.7
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         LWRAD.8
#       include <parame.incl>                                                    LWRAD.9
#       include <param2.incl>                                                    LWRAD.10
#       include <param3.incl>                                                    LWRAD.11
#       include <nhcnst.incl>                                                    23DEC04.3891
#       include <various.incl>                                                   LWRAD.13
#       include <rpstar.incl>                                                    LWRAD.14
C                                                                                LWRAD.15
      DIMENSION RTTEN(MIXR,MJXR,MKXR)                                            LWRAD.16
      DIMENSION PSA(MIX,MJX),GLW(MIX,MJX),OLW(MIX,MJX),TGA(MIX,MJX),             07NOV00.2184
     &         XLAT(MIX,MJX),XLONG(MIX,MJX),EMISS(MIX,MJX),                      LWRAD.18
     &            T(MIX,MJX,MKX),                                                LWRAD.19
     &           QV(MIX,MJX,MKX), QC(MIXM,MJXM,MKXM),                            LWRAD.20
     &           PP(MIX,MJX,MKX), QR(MIXM,MJXM,MKXM),                            LWRAD.21
     &           QI(MIXIC,MJXIC,MKXIC),QNI(MIXIC,MJXIC,MKXIC)                    LWRAD.22
C                                                                                LWRAD.23
C  NEW DIMENSION STATEMENTS FOR RADIATION CODE                                   LWRAD.24
C                                                                                LWRAD.25
      DIMENSION FTOP(MIXR),FBOT(MIXR),P(MIXR,MKX),DZ(MIXR,MKX),                  LWRAD.26
     *          XLWP(MIXR,MKX),XRWP(MIXR,MKX),XWVP(MIXR,MKX),                    LWRAD.27
     *          XWVEP(MIXR,MKX),XCO2P(MIXR,MKX),BCO2(MIXR,KXP1),                 LWRAD.28
     *          FDOWN(MIXR,KXP1),FUP(MIXR,KXP1),FDIV(MIXR,MKX)                   LWRAD.29
C                                                                                LWRAD.30
      DIMENSION BCO2B(MIXR)                                                      LWRAD.31
      DIMENSION RO(MIXR,MKXR)                                                    LWRAD.32
C                                                                                LWRAD.33
      DIMENSION UILWP(MIXR,0:MKXR)                                               LWRAD.34
      DIMENSION UIRWP(MIXR,0:MKXR)                                               LWRAD.35
      DIMENSION UIWVP(MIXR,0:MKXR)                                               LWRAD.36
      DIMENSION UIWVT(MIXR,0:MKXR)                                               LWRAD.37
      DIMENSION UIWVEP(MIXR,0:MKXR)                                              LWRAD.38
      DIMENSION UIWVPP(MIXR,0:MKXR)                                              LWRAD.39
      DIMENSION UICO2P(MIXR,0:MKXR)                                              LWRAD.40
      DIMENSION UICO2PP(MIXR,0:MKXR)                                             LWRAD.41
C                                                                                LWRAD.42
      DIMENSION DBH2O(MIXR,MKXR+1)                                               LWRAD.43
      DIMENSION DOVLP(MIXR,MKXR+1)                                               LWRAD.44
      DIMENSION ETOTD(MIXR,MKXR+1)                                               LWRAD.45
C                                                                                LWRAD.46
      DIMENSION UBH2O(MIXR,0:MKXR)                                               LWRAD.47
      DIMENSION UOVLP(MIXR,0:MKXR)                                               LWRAD.48
      DIMENSION ETOTU(MIXR,0:MKXR)                                               LWRAD.49
#ifdef IBMopt                                                                    23DEC04.3892
      integer is,nn                                                              23DEC04.3893
      integer icount4,icount5,icount6                                            23DEC04.3894
      dimension ind4(mixr),ind5(mixr),ind6(mixr)                                 23DEC04.3895
      dimension temp1(mixr),temp2(mixr),temp3(mixr)                              23DEC04.3896
      dimension temp4(mixr),temp5(mixr),temp6(mixr)                              23DEC04.3897
      dimension DLTWVP0(mixr),DLTWVP0r(mixr)                                     23DEC04.3898
      dimension DLTCO2P0(mixr),DLTCO2P0r(mixr)                                   23DEC04.3899
      dimension DLTLWP0(mixr),DLTWVEP0(mixr)                                     23DEC04.3900
      dimension DLTRWP0(mixr)                                                    23DEC04.3901
#endif                                                                           23DEC04.3902
C                                                                                LWRAD.50
#ifdef IBMopt                                                                    23DEC04.3903
#ifdef MPP1                                                                      23DEC04.3904
#define nf(A) noflic(A)   
#else                                                                            23DEC04.3906
#define nf(A) A          
#endif                                                                           23DEC04.3908
#endif                                                                           23DEC04.3909
      ABUP=.13                                                                   LWRAD.51
      ABDOWN=.158                                                                LWRAD.52
C     BASED ON EXTINCTION DISTANCES (SEE STEPHENS (1984))                        LWRAD.53
C     WATER ASSUMED BLACK W.R.T. LONGWAVE . NO EXTRA SCATTERING.                 LWRAD.54
C           AB=(1.66/2)*(PI*N0/RHOW**3)**0.25/1000.                              LWRAD.55
      ABRN=0.330E-3                                                              LWRAD.56
      ABSN=2.34E-3                                                               LWRAD.57
      ABICE=.0735                                                                LWRAD.58
      QCO2=0.5*1.E-3                                                             LWRAD.59
C                                                                                LWRAD.60
      ABSNOVABRN=ABSN/ABRN                                                       LWRAD.61
      ABICEOVABRN=ABICE/ABRN                                                     LWRAD.62
#ifdef IBMopt                                                                    23DEC04.3910
#ifdef MPP1                                                                      23DEC04.3911
      nn = ie_x0(ILX) - is_x0(1) + 1                                             23DEC04.3912
      is = is_x0(1)                                                              23DEC04.3913
#else                                                                            23DEC04.3914
      nn = ILX                                                                   23DEC04.3915
      is = 1                                                                     23DEC04.3916
#endif                                                                           23DEC04.3917
#endif                                                                           23DEC04.3918
C                                                                                LWRAD.63
C --- Note: P is in mb.                                                          LWRAD.64
C           DZ is meter.                                                         LWRAD.65
C                                                                                LWRAD.66
      DO K=1,MKX                                                                 LWRAD.67
        DO I=1,ILX                                                               LWRAD.68
          P(I,K)=0.01*(PR0(I,J,K)+PP(I,J,K))                                     LWRAD.69
          RO(I,K)=100.0*P(I,K)/(R*T(I,J,K))                                      LWRAD.70
          DZ(I,K)=PSA(I,J)*DSIGMA(K)*1.0E6/(G*RHO0(I,J,K))                       LWRAD.71
        ENDDO                                                                    LWRAD.72
      ENDDO                                                                      LWRAD.73
      IF(ICLOUD(INEST).EQ.0)THEN                                                 LWRAD.74
        DO K=1,MKX                                                               LWRAD.75
          DO I=1,ILX                                                             LWRAD.76
            XLWP(I,K)=0.                                                         LWRAD.77
            XRWP(I,K)=0.                                                         LWRAD.78
          ENDDO                                                                  LWRAD.79
        ENDDO                                                                    LWRAD.80
      ELSE                                                                       LWRAD.81
        DO K=1,MKX                                                               LWRAD.82
          DO I=1,ILX                                                             LWRAD.83
            XLWP(I,K)=RO(I,K)*QC(I,J,K)*DZ(I,K)                                  LWRAD.84
C                                                                                LWRAD.85
C EFFECTIVE PATH LENGTH                                                          LWRAD.86
C                                                                                LWRAD.87
            QR(I,J,K)=AMAX1(QR(I,J,K),0.)                                        24SEP99.543
            XRWP(I,K)=(RO(I,K)*QR(I,J,K))**0.75*DZ(I,K)                          LWRAD.88
          ENDDO                                                                  LWRAD.89
        ENDDO                                                                    LWRAD.90
C                                                                                LWRAD.91
        IF(IICE.EQ.1)THEN                                                        LWRAD.92
          DO K=1,MKX                                                             LWRAD.93
            DO I=1,ILX                                                           LWRAD.94
              DZSN=DZ(I,K)*ABSNOVABRN                                            LWRAD.95
              DZIC=DZ(I,K)*ABICEOVABRN                                           LWRAD.96
              ROQI=RO(I,K)*QI(I,J,K)                                             LWRAD.97
              ROQN=RO(I,K)*QNI(I,J,K)                                            LWRAD.98
C                                                                                LWRAD.99
              XRWP(I,K)=XRWP(I,K)+ROQI*DZIC+ROQN**0.75*DZSN                      LWRAD.100
            ENDDO                                                                LWRAD.101
          ENDDO                                                                  LWRAD.102
        ENDIF                                                                    LWRAD.103
C                                                                                LWRAD.104
C     FOR SNOW AND ICE INCREASE EFFECTIVE PATH LENGTH                            LWRAD.105
C     INCLUDE ICE CRYSTALS IN RAIN TERM                                          LWRAD.106
C                                                                                LWRAD.107
        IF(IMPHYS(INEST).EQ.4)THEN                                               LWRAD.108
          DO K=1,MKX                                                             LWRAD.109
            DO I=1,ILX                                                           LWRAD.110
              IF(T(I,J,K).LT.273.15)THEN                                         LWRAD.111
                XRWP(I,K)=XRWP(I,K)*ABSNOVABRN+XLWP(I,K)*ABICEOVABRN             LWRAD.112
                XLWP(I,K)=0.                                                     LWRAD.113
              ENDIF                                                              LWRAD.114
            ENDDO                                                                LWRAD.115
          ENDDO                                                                  LWRAD.116
        ENDIF                                                                    LWRAD.117
      ENDIF                                                                      LWRAD.118
      DO K=1,MKX                                                                 LWRAD.119
#ifdef IBMopt                                                                    23DEC04.3919
        DO I=1,ILX                                                               23DEC04.3920
           temp2(i)=P(I,K)/1013.0                                                23DEC04.3921
        ENDDO                                                                    23DEC04.3922
                                                                                 23DEC04.3923
        call vslog(temp2(nf(is)),temp2(nf(is)),nn)                               23DEC04.3924
        call vsrec(temp3(nf(is)),T(nf(IS),J,K),nn)                               23DEC04.3925
        call vsrec(temp4(nf(is)),TGA(nf(IS),J),nn)                               23DEC04.3926
                                                                                 23DEC04.3927
        DO I=1,ILX                                                               23DEC04.3928
           temp1(i)=temp2(i)*0.86                                                23DEC04.3929
           temp2(i)=temp2(i)*1.75                                                23DEC04.3930
           temp3(i)=959.6*temp3(i)                                               23DEC04.3931
           temp4(i)=959.6*temp4(i)                                               23DEC04.3932
        ENDDO                                                                    23DEC04.3933
                                                                                 23DEC04.3934
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               23DEC04.3935
        call vsexp(temp2(nf(is)),temp2(nf(is)),nn)                               23DEC04.3936
        call vsexp(temp3(nf(is)),temp3(nf(is)),nn)                               23DEC04.3937
        call vsexp(temp4(nf(is)),temp4(nf(is)),nn)                               23DEC04.3938
                                                                                 23DEC04.3939
#endif                                                                           23DEC04.3940
        DO I=1,ILX                                                               LWRAD.120
          QVTMP=AMAX1(QV(I,J,K),1.0E-6)                                          07NOV00.2185
          PPREF=P(I,K)/1013.0                                                    LWRAD.122
#ifndef IBMopt                                                                   23DEC04.3941
          XWVP(I,K)=RO(I,K)*QVTMP*DZ(I,K)*PPREF**0.86                            07NOV00.2186
#else                                                                            23DEC04.3942
          XWVP(I,K)=RO(I,K)*QVTMP*DZ(I,K)*temp1(i)                               23DEC04.3943
#endif                                                                           23DEC04.3944
          XWVEP(I,K)=RO(I,K)*QVTMP*QVTMP*DZ(I,K)*PPREF/0.622                     07NOV00.2187
#ifndef IBMopt                                                                   23DEC04.3945
          XCO2P(I,K)=RO(I,K)*QCO2*DZ(I,K)*PPREF**1.75                            LWRAD.125
          DIVDND=EXP(959.6/T(I,J,K))-1.                                          LWRAD.126
#else                                                                            23DEC04.3946
          XCO2P(I,K)=RO(I,K)*QCO2*DZ(I,K)*temp2(i)                               23DEC04.3947
          DIVDND=temp3(i)-1.                                                     23DEC04.3948
#endif                                                                           23DEC04.3949
          BCO2(I,K)=2.2234E3/DIVDND                                              LWRAD.127
        ENDDO                                                                    LWRAD.128
      ENDDO                                                                      LWRAD.129
C                                                                                LWRAD.130
C     STRATOSPHERIC CO2 EMISSION AT 190 K(DUDHIA) OR T(1)                        LWRAD.131
C                                                                                LWRAD.132
      DO I=1,ILX                                                                 LWRAD.133
        FTOP(I)=BCO2(I,1)                                                        LWRAD.134
        FBOT(I)=EMISS(I,J)*STBOLT*TGA(I,J)**4                                    LWRAD.135
C                                                                                LWRAD.136
        FDOWN(I,1)=FTOP(I)                                                       LWRAD.137
        FUP(I,KXP1)=FBOT(I)                                                      LWRAD.138
#ifndef IBMopt                                                                   23DEC04.3950
        DIVDND=EXP(959.6/TGA(I,J))-1.                                            LWRAD.139
#else                                                                            23DEC04.3951
        DIVDND=temp4(i)-1.                                                       23DEC04.3952
#endif                                                                           23DEC04.3953
        BCO2B(I)=2.2234E3/DIVDND                                                 LWRAD.140
      ENDDO                                                                      LWRAD.141
C                                                                                LWRAD.142
C     WV ABSORPTION IS APPROXIMATED BY EMPIRICAL FUNCTION                        LWRAD.143
C     DIWVP(I,K) IS DOWNWARD INTEGRATED WATER VAPOR PATH (G/M**2)                LWRAD.144
C                                                                                LWRAD.145
      DO I=1,ILX                                                                 LWRAD.146
        UILWP(I,0)=0.                                                            LWRAD.147
        UIRWP(I,0)=0.                                                            LWRAD.148
        UIWVP(I,0)=0.                                                            LWRAD.149
        UIWVEP(I,0)=0.                                                           LWRAD.150
        UIWVT(I,0)=0.                                                            LWRAD.151
        UIWVPP(I,0)=0.                                                           LWRAD.152
        UICO2P(I,0)=0.                                                           LWRAD.153
        UICO2PP(I,0)=0.                                                          LWRAD.154
      ENDDO                                                                      LWRAD.155
C                                                                                LWRAD.156
      DO K=1,MKX                                                                 LWRAD.157
        DO I=1,ILX                                                               LWRAD.158
          UILWP(I,K)=UILWP(I,K-1)+XLWP(I,K)                                      LWRAD.159
          UIRWP(I,K)=UIRWP(I,K-1)+XRWP(I,K)                                      LWRAD.160
          UIWVP(I,K)=UIWVP(I,K-1)+XWVP(I,K)                                      LWRAD.161
          UIWVEP(I,K)=UIWVEP(I,K-1)+XWVEP(I,K)                                   LWRAD.162
          UIWVT(I,K)=UIWVT(I,K-1)+XWVP(I,K)*(T(I,J,K)-250.)                      LWRAD.163
          UIWVPP(I,K)=UIWVPP(I,K-1)+XWVP(I,K)*P(I,K)                             LWRAD.164
          UICO2P(I,K)=UICO2P(I,K-1)+XCO2P(I,K)                                   LWRAD.165
          UICO2PP(I,K)=UICO2PP(I,K-1)+XCO2P(I,K)*P(I,K)                          LWRAD.166
        ENDDO                                                                    LWRAD.167
      ENDDO                                                                      LWRAD.168
C                                                                                LWRAD.169
      DO 100 II=2,KXP1                                                           LWRAD.170
C                                                                                LWRAD.171
        DO I=1,ILX                                                               LWRAD.172
          FDOWN(I,II)=0.                                                         LWRAD.173
          ETOTD(I,II)=0.                                                         LWRAD.174
          DBH2O(I,II)=0.                                                         LWRAD.175
          DOVLP(I,II)=0.                                                         LWRAD.176
        ENDDO                                                                    LWRAD.177
C                                                                                LWRAD.178
        IIM1=II-1                                                                LWRAD.179
C                                                                                LWRAD.180
        DO 11 K=IIM1,1,-1                                                        LWRAD.181
          DO I=1,ILX                                                             LWRAD.182
#ifndef IBMopt                                                                   23DEC04.3954
            DLTWVP=UIWVP(I,IIM1)-UIWVP(I,K-1)                                    LWRAD.183
            DLTLWP=UILWP(I,IIM1)-UILWP(I,K-1)                                    LWRAD.184
            DLTCO2P=UICO2P(I,IIM1)-UICO2P(I,K-1)                                 LWRAD.185
            DLTRWP=UIRWP(I,IIM1)-UIRWP(I,K-1)                                    LWRAD.186
            DLTWVEP=UIWVEP(I,IIM1)-UIWVEP(I,K-1)                                 LWRAD.187
            TBAR=(UIWVT(I,IIM1)-UIWVT(I,K-1))/DLTWVP                             LWRAD.188
#else                                                                            23DEC04.3955
            DLTWVP0(i)=UIWVP(I,IIM1)-UIWVP(I,K-1)                                23DEC04.3956
            DLTLWP0(i)=UILWP(I,IIM1)-UILWP(I,K-1)                                23DEC04.3957
            DLTCO2P0(i)=UICO2P(I,IIM1)-UICO2P(I,K-1)                             23DEC04.3958
            DLTRWP0(i)=UIRWP(I,IIM1)-UIRWP(I,K-1)                                23DEC04.3959
            DLTWVEP0(i)=UIWVEP(I,IIM1)-UIWVEP(I,K-1)                             23DEC04.3960
#endif                                                                           23DEC04.3961
C                                                                                LWRAD.189
C     OVERLAP BETWEEN CO2 AND H2O                                                LWRAD.190
C                                                                                LWRAD.191
#ifndef IBMopt                                                                   23DEC04.3962
            UH2O=DLTWVP*0.0001                                                   LWRAD.192
            UCO2=DLTCO2P*0.0001                                                  LWRAD.193
            PH2O=(UIWVPP(I,IIM1)-UIWVPP(I,K-1))/DLTWVP                           LWRAD.194
            PCO2=(UICO2PP(I,IIM1)-UICO2PP(I,K-1))/DLTCO2P                        LWRAD.195
#else                                                                            23DEC04.3963
            PH2O=DLTWVP0(i)/(UIWVPP(I,IIM1)-UIWVPP(I,K-1))                       23DEC04.3964
            PCO2=DLTCO2P0(i)/(UICO2PP(I,IIM1)-UICO2PP(I,K-1))                    23DEC04.3965
#endif                                                                           23DEC04.3966
C                                                                                LWRAD.196
#ifndef IBMopt                                                                   23DEC04.3967
            UHPHSQ=1.+142.47*1013.*UH2O/PH2O                                     LWRAD.197
            UHPHSQ=SQRT(UHPHSQ)                                                  LWRAD.198
            TH2O=-7.345*UH2O/UHPHSQ                                              LWRAD.199
            UCPCSQ=1.+1604.2*1013.*UCO2/PCO2                                     LWRAD.200
            UCPCSQ=SQRT(UCPCSQ)                                                  LWRAD.201
            TCO2=-718.7*UCO2/UCPCSQ                                              LWRAD.202
            TLIQ=-ABDOWN*DLTLWP-ABRN*DLTRWP                                      LWRAD.203
            DOVLP(I,K)=1.-EXP(TH2O+TCO2+TLIQ)                                    LWRAD.204
            DBH2O(I,K)=1.-EXP(TH2O+TLIQ)                                         LWRAD.205
#else                                                                            23DEC04.3968
            temp1(i)=1.+14.432211 * DLTWVP0(i) * PH2O                            23DEC04.3969
            temp2(i)=1.+162.50546 * DLTCO2P0(i) * PCO2                           23DEC04.3970
          ENDDO                                                                  23DEC04.3971
                                                                                 23DEC04.3972
          call vsrsqrt(temp1(nf(is)),temp1(nf(is)),nn)                           23DEC04.3973
          call vsrsqrt(temp2(nf(is)),temp2(nf(is)),nn)                           23DEC04.3974
                                                                                 23DEC04.3975
          DO I=1,ILX                                                             23DEC04.3976
            TH2O = -0.0007345 * DLTWVP0(i) * temp1(i)                            23DEC04.3977
            TCO2 = -0.07187   * DLTCO2P0(i) * temp2(i)                           23DEC04.3978
            TLIQ=-ABDOWN*DLTLWP0(i)-ABRN*DLTRWP0(i)                              23DEC04.3979
            temp1(i) = TH2O+TCO2+TLIQ                                            23DEC04.3980
            temp2(i) = TH2O+TLIQ                                                 23DEC04.3981
          ENDDO                                                                  23DEC04.3982
                                                                                 23DEC04.3983
          call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                             23DEC04.3984
          call vsexp(temp2(nf(is)),temp2(nf(is)),nn)                             23DEC04.3985
                                                                                 23DEC04.3986
          icount4=is - 1                                                         23DEC04.3987
          icount5=is - 1                                                         23DEC04.3988
          icount6=is - 1                                                         23DEC04.3989
                                                                                 23DEC04.3990
          DO I=1,ILX                                                             23DEC04.3991
            IF(DLTWVP0(i).LT.10.)THEN                                            23DEC04.3992
                icount4 = icount4 +1                                             23DEC04.3993
                ind4(i) = icount4                                                23DEC04.3994
                temp4(nf(icount4)) = DLTWVP0(i)*0.0001                           23DEC04.3995
             else                                                                23DEC04.3996
                icount5 = icount5 +1                                             23DEC04.3997
                ind5(i) = icount5                                                23DEC04.3998
                temp5(nf(icount5)) = DLTWVP0(i)*0.0001                           23DEC04.3999
             end if                                                              23DEC04.4000
             IF(DLTWVEP0(i).GT.8.)THEN                                           23DEC04.4001
                icount6 = icount6 +1                                             23DEC04.4002
                ind6(i) = icount6                                                23DEC04.4003
                temp6(nf(icount6)) = DLTWVEP0(i)*0.0001                          23DEC04.4004
             end if                                                              23DEC04.4005
          ENDDO                                                                  23DEC04.4006
                                                                                 23DEC04.4007
          if(icount4.gt.0) then                                                  23DEC04.4008
             call vssqrt(temp4(nf(is)),temp4(nf(is)),icount4)                    23DEC04.4009
          endif                                                                  23DEC04.4010
          if(icount5.gt.0) then                                                  23DEC04.4011
             call  vslog(temp5(nf(is)),temp5(nf(is)),icount5)                    23DEC04.4012
          endif                                                                  23DEC04.4013
          if(icount6.gt.0) then                                                  23DEC04.4014
             call  vslog(temp6(nf(is)),temp6(nf(is)),icount6)                    23DEC04.4015
          endif                                                                  23DEC04.4016
                                                                                 23DEC04.4017
          DO I=1,ILX                                                             23DEC04.4018
            DOVLP(I,K)=1.- temp1(i)                                              23DEC04.4019
            DBH2O(I,K)=1.- temp2(i)                                              23DEC04.4020
#endif                                                                           23DEC04.4021
C                                                                                LWRAD.206
#ifndef IBMopt                                                                   23DEC04.4022
            DDOVLP=DOVLP(I,K)-DOVLP(I,K+1)                                       LWRAD.207
            DDBH2O=DBH2O(I,K)-DBH2O(I,K+1)                                       LWRAD.208
#endif                                                                           23DEC04.4023
C                                                                                LWRAD.209
C     CORRECT FROM ORIGINAL PROG 2.1917-2.9177                                   LWRAD.210
C                                                                                LWRAD.211
#ifndef IBMopt                                                                   23DEC04.4024
            IF(DLTWVP.LT.10.)THEN                                                LWRAD.212
              AX=SQRT(UH2O)                                                      LWRAD.213
#else                                                                            23DEC04.4025
            TBAR=(UIWVT(I,IIM1)-UIWVT(I,K-1))/DLTWVP0(i)                         23DEC04.4026
            IF(DLTWVP0(i).LT.10.)THEN                                            23DEC04.4027
               ax = temp4(nf(ind4(i)))                                           23DEC04.4028
#endif                                                                           23DEC04.4029
              ETOTD(I,K)=(AX*(5.384+AX*(71.+AX*(-4823.+AX*53000.))))+            LWRAD.214
     +                   TBAR*(AX*(-0.062+AX*(3.23+AX*(-34.4+AX*(-1065.)         LWRAD.215
     +                   ))))                                                    LWRAD.216
            ELSE                                                                 LWRAD.217
#ifndef IBMopt                                                                   23DEC04.4030
              AX=ALOG(UH2O)                                                      LWRAD.218
#else                                                                            23DEC04.4031
               ax = temp5(nf(ind5(i)))                                           23DEC04.4032
#endif                                                                           23DEC04.4033
              ETOTD(I,K)=(0.6267+AX*(0.10303+AX*(1.5967E-2+AX*(2.9177E-3         LWRAD.219
     +                   +AX*1.8734E-4))))+TBAR*(9.95E-4+AX*(3.27E-4+AX*         LWRAD.220
     +                   (-1.6533E-4+AX*(-5.2942E-5+AX*(-4.0313E-6)))))          LWRAD.221
            ENDIF                                                                LWRAD.222
C                                                                                LWRAD.223
#ifndef IBMopt                                                                   23DEC04.4034
            IF(DLTWVEP.GT.8.)THEN                                                LWRAD.224
              AX=ALOG(DLTWVEP*0.0001)                                            LWRAD.225
#else                                                                            23DEC04.4035
            IF(DLTWVEP0(i).GT.8.)THEN                                            23DEC04.4036
               ax = temp6(nf(ind6(i)))                                           23DEC04.4037
#endif                                                                           23DEC04.4038
              ETOTD(I,K)=ETOTD(I,K)+(.2837+AX*(-.1231+AX*(-.10568+AX*(-          LWRAD.226
     +                   .0199+AX*(-1.16E-3)))))                                 LWRAD.227
            ENDIF                                                                LWRAD.228
            IF(ETOTD(I,K).GT.1.)THEN                                             LWRAD.229
              ETOTD(I,K)=1.                                                      LWRAD.230
            ENDIF                                                                LWRAD.231
C                                                                                LWRAD.232
C     ADD CONTRIBUTION DUE TO LIQUID WATER                                       LWRAD.233
C                                                                                LWRAD.234
#ifndef IBMopt                                                                   23DEC04.4039
            ALIWPI=-ABDOWN*DLTLWP-ABRN*DLTRWP                                    LWRAD.235
            ETOTD(I,K)=1.-(1.-ETOTD(I,K))*EXP(ALIWPI)                            LWRAD.236
#else                                                                            23DEC04.4040
            temp1(i)=-ABDOWN*DLTLWP0(i)-ABRN*DLTRWP0(i)                          23DEC04.4041
          ENDDO                                                                  23DEC04.4042
          call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                             23DEC04.4043
                                                                                 23DEC04.4044
          DO I=1,ILX                                                             23DEC04.4045
            DDOVLP=DOVLP(I,K)-DOVLP(I,K+1)                                       23DEC04.4046
            DDBH2O=DBH2O(I,K)-DBH2O(I,K+1)                                       23DEC04.4047
            ETOTD(I,K)=1.-(1.-ETOTD(I,K))* temp1(i)                              23DEC04.4048
#endif                                                                           23DEC04.4049
C                                                                                LWRAD.237
            FDOWN(I,II)=FDOWN(I,II)+(ETOTD(I,K)-ETOTD(I,K+1))*STBOLT*            LWRAD.238
     +                  T(I,J,K)**4+(DDOVLP-DDBH2O)*BCO2(I,K)                    LWRAD.239
          ENDDO                                                                  LWRAD.240
C                                                                                LWRAD.241
   11   CONTINUE                                                                 LWRAD.242
C                                                                                LWRAD.243
        DO I=1,ILX                                                               LWRAD.244
          FDOWN(I,II)=FDOWN(I,II)+FTOP(I)+(DBH2O(I,1)-DOVLP(I,1)-                LWRAD.245
     +                ETOTD(I,1))*FTOP(I)                                        LWRAD.246
        ENDDO                                                                    LWRAD.247
  100 CONTINUE                                                                   LWRAD.248
C                                                                                LWRAD.249
C     REFLECTED I.R. COMPONENT AT SURFACE                                        LWRAD.250
C                                                                                LWRAD.251
      DO I=1,ILX                                                                 LWRAD.252
        FUP(I,KXP1)=FUP(I,KXP1)+(1.-EMISS(I,J))*FDOWN(I,KXP1)                    LWRAD.253
        FBOT(I)=FUP(I,KXP1)                                                      LWRAD.254
      ENDDO                                                                      LWRAD.255
C                                                                                LWRAD.256
      DO 150 II=1,MKX                                                            LWRAD.257
        IIM1=II-1                                                                LWRAD.258
        DO I=1,ILX                                                               LWRAD.259
          UBH2O(I,IIM1)=0.                                                       LWRAD.260
          ETOTU(I,IIM1)=0.                                                       LWRAD.261
          UOVLP(I,IIM1)=0.                                                       LWRAD.262
          FUP(I,II)=0.                                                           LWRAD.263
        ENDDO                                                                    LWRAD.264
C                                                                                LWRAD.265
        DO 10 K=II,MKX                                                           LWRAD.266
C                                                                                LWRAD.267
C     WV ABSORPTION IS APPROXIMATED BY EMPIRICAL FUNCTION                        LWRAD.268
C     DIWVP(I) IS DOWNWARD INTEGRATED WATER VAPOR PATH (G/M**2)                  LWRAD.269
C                                                                                LWRAD.270
          DO I=1,ILX                                                             LWRAD.271
#ifndef IBMopt                                                                   23DEC04.4050
            DLTWVT=UIWVT(I,K)-UIWVT(I,IIM1)                                      LWRAD.272
            DLTWVP=UIWVP(I,K)-UIWVP(I,IIM1)                                      LWRAD.273
            DLTLWP=UILWP(I,K)-UILWP(I,IIM1)                                      LWRAD.274
            DLTRWP=UIRWP(I,K)-UIRWP(I,IIM1)                                      LWRAD.275
            DLTCO2P=UICO2P(I,K)-UICO2P(I,IIM1)                                   LWRAD.276
            DLTWVEP=UIWVEP(I,K)-UIWVEP(I,IIM1)                                   LWRAD.277
            TBAR=DLTWVT/DLTWVP                                                   LWRAD.278
#else                                                                            23DEC04.4051
            DLTWVP0(i)=UIWVP(I,K)-UIWVP(I,IIM1)                                  23DEC04.4052
            DLTLWP0(i)=UILWP(I,K)-UILWP(I,IIM1)                                  23DEC04.4053
            DLTRWP0(i)=UIRWP(I,K)-UIRWP(I,IIM1)                                  23DEC04.4054
            DLTCO2P0(i)=UICO2P(I,K)-UICO2P(I,IIM1)                               23DEC04.4055
            DLTWVEP0(i)=UIWVEP(I,K)-UIWVEP(I,IIM1)                               23DEC04.4056
#endif                                                                           23DEC04.4057
C                                                                                LWRAD.279
C     OVERLAP BETWEEN CO2 AND H2O                                                LWRAD.280
C                                                                                LWRAD.281
#ifndef IBMopt                                                                   23DEC04.4058
            UH2O=DLTWVP*0.0001                                                   LWRAD.282
            UCO2=DLTCO2P*0.0001                                                  LWRAD.283
            PH2O=(UIWVPP(I,K)-UIWVPP(I,IIM1))/DLTWVP                             LWRAD.284
            PCO2=(UICO2PP(I,K)-UICO2PP(I,IIM1))/DLTCO2P                          LWRAD.285
            UHPHSQ=1.+142.47*1013.*UH2O/PH2O                                     LWRAD.286
            UHPHSQ=SQRT(UHPHSQ)                                                  LWRAD.287
#else                                                                            23DEC04.4059
            PH2O=DLTWVP0(i)/(UIWVPP(I,K)-UIWVPP(I,IIM1))                         23DEC04.4060
            PCO2=DLTCO2P0(i)/(UICO2PP(I,K)-UICO2PP(I,IIM1))                      23DEC04.4061
            temp1(i)=1.+ 14.432211*DLTWVP0(i) *PH2O                              23DEC04.4062
            temp2(i)=1.+ 162.50546*DLTCO2P0(i)* PCO2                             23DEC04.4063
          end do                                                                 23DEC04.4064
                                                                                 23DEC04.4065
        call vsrsqrt(temp1(nf(is)),temp1(nf(is)),nn)                             23DEC04.4066
        call vsrsqrt(temp2(nf(is)),temp2(nf(is)),nn)                             23DEC04.4067
#endif                                                                           23DEC04.4068
C                                                                                LWRAD.288
#ifndef IBMopt                                                                   23DEC04.4069
            TH2O=-7.345*UH2O/UHPHSQ                                              LWRAD.289
            UCPCSQ=1.+1604.2*1013.*UCO2/PCO2                                     LWRAD.290
            UCPCSQ=SQRT(UCPCSQ)                                                  LWRAD.291
            TCO2=-718.7*UCO2/UCPCSQ                                              LWRAD.292
            TLIQ=-ABDOWN*DLTLWP-ABRN*DLTRWP                                      LWRAD.293
            UOVLP(I,K)=1.-EXP(TH2O+TCO2+TLIQ)                                    LWRAD.294
            UBH2O(I,K)=1.-EXP(TH2O+TLIQ)                                         LWRAD.295
#else                                                                            23DEC04.4070
          DO I=1,ILX                                                             23DEC04.4071
            TH2O=-0.0007345*DLTWVP0(i) * temp1(i)                                23DEC04.4072
            TCO2=-0.07187*DLTCO2P0(i) * temp2(i)                                 23DEC04.4073
            TLIQ=-ABDOWN*DLTLWP0(i)-ABRN*DLTRWP0(i)                              23DEC04.4074
            temp1(i) = TH2O+TCO2+TLIQ                                            23DEC04.4075
            temp2(i) = TH2O+TLIQ                                                 23DEC04.4076
          end do                                                                 23DEC04.4077
                                                                                 23DEC04.4078
          call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                             23DEC04.4079
          call vsexp(temp2(nf(is)),temp2(nf(is)),nn)                             23DEC04.4080
                                                                                 23DEC04.4081
          icount4=is - 1                                                         23DEC04.4082
          icount5=is - 1                                                         23DEC04.4083
          icount6=is - 1                                                         23DEC04.4084
                                                                                 23DEC04.4085
          DO I=1,ILX                                                             23DEC04.4086
            IF(DLTWVP0(i).LT.10.)THEN                                            23DEC04.4087
                icount4 = icount4 +1                                             23DEC04.4088
                ind4(i) = icount4                                                23DEC04.4089
                temp4(nf(icount4)) = DLTWVP0(i)*0.0001                           23DEC04.4090
             else                                                                23DEC04.4091
                icount5 = icount5 +1                                             23DEC04.4092
                ind5(i) = icount5                                                23DEC04.4093
                temp5(nf(icount5)) = DLTWVP0(i)*0.0001                           23DEC04.4094
             end if                                                              23DEC04.4095
             IF(DLTWVEP0(i).GT.8.)THEN                                           23DEC04.4096
                icount6 = icount6 +1                                             23DEC04.4097
                ind6(i) = icount6                                                23DEC04.4098
                temp6(nf(icount6)) = DLTWVEP0(i)*0.0001                          23DEC04.4099
             end if                                                              23DEC04.4100
          end do                                                                 23DEC04.4101
                                                                                 23DEC04.4102
          if(icount4.gt.0) then                                                  23DEC04.4103
             call vssqrt(temp4(nf(is)),temp4(nf(is)),icount4)                    23DEC04.4104
          endif                                                                  23DEC04.4105
          if(icount5.gt.0) then                                                  23DEC04.4106
             call  vslog(temp5(nf(is)),temp5(nf(is)),icount5)                    23DEC04.4107
          endif                                                                  23DEC04.4108
          if(icount6.gt.0) then                                                  23DEC04.4109
             call  vslog(temp6(nf(is)),temp6(nf(is)),icount6)                    23DEC04.4110
          endif                                                                  23DEC04.4111
                                                                                 23DEC04.4112
          DO I=1,ILX                                                             23DEC04.4113
            UOVLP(I,K)=1.- temp1(i)                                              23DEC04.4114
            UBH2O(I,K)=1.- temp2(i)                                              23DEC04.4115
#endif                                                                           23DEC04.4116
C                                                                                LWRAD.296
#ifndef IBMopt                                                                   23DEC04.4117
            DUOVLP=UOVLP(I,K)-UOVLP(I,K-1)                                       LWRAD.297
            DUBH2O=UBH2O(I,K)-UBH2O(I,K-1)                                       LWRAD.298
#endif                                                                           23DEC04.4118
C                                                                                LWRAD.299
C     H2O ABSORPTION                                                             LWRAD.300
C                                                                                LWRAD.301
#ifndef IBMopt                                                                   23DEC04.4119
            IF(DLTWVP.LT.10.)THEN                                                LWRAD.302
              AX=SQRT(UH2O)                                                      LWRAD.303
#else                                                                            23DEC04.4120
            TBAR=(UIWVT(I,K)-UIWVT(I,IIM1))/DLTWVP0(i)                           23DEC04.4121
            IF(DLTWVP0(i).LT.10.)THEN                                            23DEC04.4122
              ax = temp4(nf(ind4(i)))                                            23DEC04.4123
#endif                                                                           23DEC04.4124
              ETOTU(I,K)=(AX*(2.364+AX*(252.7+AX*(-21411.+AX*481500.))))         LWRAD.304
     +                   +TBAR*(AX*(-0.193+AX*(28.7+AX*(-1309.8+AX*              LWRAD.305
     +                   18166.))))                                              LWRAD.306
            ELSE                                                                 LWRAD.307
#ifndef IBMopt                                                                   23DEC04.4125
              AX=ALOG(UH2O)                                                      LWRAD.308
#else                                                                            23DEC04.4126
              ax = temp5(nf(ind5(i)))                                            23DEC04.4127
#endif                                                                           23DEC04.4128
              ETOTU(I,K)=(0.584+AX*(0.14943+AX*(3.7405E-2+AX*(7.5518E-3+         LWRAD.309
     +                   AX*5.6412E-4))))+TBAR*(7.05E-4+AX*(-2.05E-5+AX*         LWRAD.310
     +                   (-1.912E-4+AX*(-5.9241E-5+AX*(-5.2059E-6)))))           LWRAD.311
            ENDIF                                                                LWRAD.312
C                                                                                LWRAD.313
#ifndef IBMopt                                                                   23DEC04.4129
            IF(DLTWVEP.GT.8.)THEN                                                LWRAD.314
              AX=ALOG(DLTWVEP*0.0001)                                            LWRAD.315
#else                                                                            23DEC04.4130
            IF(DLTWVEP0(i).GT.8.)THEN                                            23DEC04.4131
              ax = temp6(nf(ind6(i)))                                            23DEC04.4132
#endif                                                                           23DEC04.4133
              ETOTU(I,K)=ETOTU(I,K)+(0.21669+AX*(-9.185E-2+AX*(-7.971E-2         LWRAD.316
     +                   +AX*(-1.502E-2+AX*(-8.754E-4)))))                       LWRAD.317
            ENDIF                                                                LWRAD.318
C                                                                                LWRAD.319
            IF(ETOTU(I,K).GT.1.)THEN                                             LWRAD.320
              ETOTU(I,K)=1.                                                      LWRAD.321
            ENDIF                                                                LWRAD.322
C                                                                                LWRAD.323
C     ADD LIQUID WATER CONTRIBUTION                                              LWRAD.324
C                                                                                LWRAD.325
#ifndef IBMopt                                                                   23DEC04.4134
            UPRNLRWP=-ABUP*DLTLWP-ABRN*DLTRWP                                    LWRAD.326
            ETOTU(I,K)=1.-(1.-ETOTU(I,K))*EXP(UPRNLRWP)                          LWRAD.327
#else                                                                            23DEC04.4135
            temp1(i)=-ABUP*DLTLWP0(i)-ABRN*DLTRWP0(i)                            23DEC04.4136
          ENDDO                                                                  23DEC04.4137
          call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                             23DEC04.4138
                                                                                 23DEC04.4139
          DO I=1,ILX                                                             23DEC04.4140
            ETOTU(I,K)=1.-(1.-ETOTU(I,K))*  temp1(i)                             23DEC04.4141
            DUOVLP=UOVLP(I,K)-UOVLP(I,K-1)                                       23DEC04.4142
            DUBH2O=UBH2O(I,K)-UBH2O(I,K-1)                                       23DEC04.4143
#endif                                                                           23DEC04.4144
C                                                                                LWRAD.328
            FUP(I,II)=FUP(I,II)+(ETOTU(I,K)-ETOTU(I,K-1))*STBOLT*                LWRAD.329
     +                T(I,J,K)**4+(DUOVLP-DUBH2O)*BCO2(I,K)                      LWRAD.330
          ENDDO                                                                  LWRAD.331
   10   CONTINUE                                                                 LWRAD.332
C                                                                                LWRAD.333
        DO I=1,ILX                                                               LWRAD.334
          FUP(I,II)=FUP(I,II)+(1.-ETOTU(I,MKX))*FBOT(I)+(UBH2O(I,MKX)-           LWRAD.335
     +              UOVLP(I,MKX))*BCO2B(I)                                       LWRAD.336
        ENDDO                                                                    LWRAD.337
  150 CONTINUE                                                                   LWRAD.338
C                                                                                LWRAD.339
C     ONLY ABSORBED PART IN SURFACE ENERGY BUDGET                                LWRAD.340
C                                                                                LWRAD.341
      DO 18 I=1,ILX                                                              LWRAD.342
        GLW(I,J)=FDOWN(I,KXP1)                                                   LWRAD.343
        OLW(I,J)=FUP(I,1)                                                        07NOV00.2188
   18 CONTINUE                                                                   LWRAD.344
C                                                                                LWRAD.345
      DO 20 K=1,MKX                                                              LWRAD.346
        DO I=1,ILX                                                               LWRAD.347
          FDIV(I,K)=1000.0*(FUP(I,K+1)-FUP(I,K)+FDOWN(I,K)-FDOWN(I,K+1))         LWRAD.348
     +              /DZ(I,K)                                                     LWRAD.349
          RTTEN(I,J,K)=FDIV(I,K)/(CP*RO(I,K))                                    LWRAD.350
        ENDDO                                                                    LWRAD.351
   20 CONTINUE                                                                   LWRAD.352
C                                                                                LWRAD.353
      RETURN                                                                     LWRAD.354
      END                                                                        LWRAD.355
                                                                                 LWRAD.356
