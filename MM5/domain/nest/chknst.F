      SUBROUTINE CHKNST(IEXEC,NESCOU)                                            CHKNST.1
C                                                                                CHKNST.2
C--- THIS SUBROUTINE INITIALIZES NEW FIRST LEVEL NEST                            CHKNST.3
C                                                                                CHKNST.4
#     include <parame.incl>                                                      CHKNST.5
#     include <varia.incl>                                                       CHKNST.6
#     include <param2.incl>                                                      CHKNST.7
#     include <param3.incl>                                                      CHKNST.8
#     include <pmoist.incl>                                                      CHKNST.9
#     include <addr0.incl>                                                       CHKNST.10
#     include <various.incl>                                                     CHKNST.11
#     include <variousn.incl>                                                    CHKNST.12
#     include <nestl.incl>                                                       CHKNST.13
#     include <defines.incl>                                                     CHKNST.14
#ifdef MPP1                                                                      CHKNST.15
      INTEGER MILLICLOCK,CLOCKSTART                                              CHKNST.16
      EXTERNAL MILLICLOCK                                                        CHKNST.17
#endif                                                                           CHKNST.18
      DIMENSION HTNO(MIX,MJX)                                                    CHKNST.19
      INTEGER IEXEC(MAXNES)                                                      CHKNST.20
C                                                                                CHKNST.21
C--- SORT OUT WHERE YOU ARE, WHAT LEVEL(IDLEV),                                  CHKNST.22
C--- WHAT MOTHER DOMAIN (NUMNC), ALL                                             CHKNST.23
C--- NECESSARY ADDRESSES                                                         CHKNST.24
C                                                                                CHKNST.25
      N=NESCOU                                                                   CHKNST.26
      IDLEV=LEVIDN(N)                                                            CHKNST.27
      ICOARS=NUMNC(N)                                                            CHKNST.28
      CALL ADDRX1C(IAXALL(1,ICOARS))                                             CHKNST.29
      CALL ADDRX1N(IAXALL(1,N))                                                  CHKNST.30
      DT5=5.*DT                                                                  CHKNST.31
C                                                                                CHKNST.32
C--- IF THIS NEST ALREADY ACTIVE, OR INITIALIZATION TIME NOT YET                 CHKNST.33
C--- REACHED, SKIP                                                               CHKNST.34
C                                                                                CHKNST.35
      XTIMET=XTIME+0.01                                                          CHKNST.36
      IF(XTIMET.LT.XSTNES(N).OR.(XTIMET.GT.XENNES(N).AND.IACTIV(N).EQ.0)         CHKNST.37
     +  )GOTO 100                                                                CHKNST.38
C                                                                                CHKNST.39
C--- MOVE THE NEST???                                                            CHKNST.40
      IF(IMOVE(N).EQ.1.AND.KTAU.EQ.KTAUR)THEN                                    CHKNST.41
        IF(IMOVET(N,IMOVCO(N)).LT.IFIX(XTIME+.001).AND.                          CHKNST.42
     +    IMOVET(N,IMOVCO(N)).GT.0)THEN                                          CHKNST.43
          PRINT999,IMOVET(N,IMOVCO(N)),XTIME,IMOVCO(N),N                         CHKNST.44
          STOP 'CHKNST IMOVCO?'                                                  CHKNST.45
        ENDIF                                                                    CHKNST.46
C                                                                                CHKNST.47
        IXHE2=IMOVET(N,IMOVCO(N))/10                                             CHKNST.48
        IXHE=IFIX(XTIME/10.+.001)                                                CHKNST.49
        TNEST=KTAUN*DTN/60.                                                      CHKNST.50
        IF(IXHE2.EQ.IXHE.AND.TNEST.LT.10.)THEN                                   CHKNST.51
          IMOVCO(N)=IMOVCO(N)+1                                                  CHKNST.52
          PRINT998,N,TNEST,IMOVCO(N),IMOVET(N,IMOVCO(N))                         CHKNST.53
        ENDIF                                                                    CHKNST.54
      ENDIF                                                                      CHKNST.55
  998 FORMAT(1X,'NEST ',I3,' JUST MOVED ',F6.0,'MIN AGO. ',                      CHKNST.56
     -     'RESET IMOVCO=',I4,'. NEXT MOVE AT ',I6,'MIN')                        CHKNST.57
  999 FORMAT(1X,'FIRST MOVE AT ',I6,' BUT XTIME = ',F6.0,                        CHKNST.58
     -     '. CHECK IMOVCO=',I4,' FOR NEST ',I4)                                 CHKNST.59
C                                                                                CHKNST.60
      IF(IACTIV(N).EQ.1.AND.XTIMET.LT.XENNES(N))THEN                             CHKNST.61
        IXHE2=IMOVET(N,IMOVCO(N))/10                                             CHKNST.62
        IXHE=IFIX(XTIME/10.+.001)                                                CHKNST.63
        IXHOUR=IFIX(XTIME/60.+.001)                                              CHKNST.64
C                                                                                CHKNST.65
        IF(IMOVE(N).EQ.1.AND.IXHE2.EQ.IXHE)THEN                                  CHKNST.66
          PRINT *,'!!! NEST * ',N,' * IS MOVED AT HOUR ',IXHOUR                  CHKNST.67
          ISOUTHO=NESTI(N)                                                       CHKNST.68
          JWESTO=NESTJ(N)                                                        CHKNST.69
          NESTI(N)=NESTI(N)+IMOVEI(N,IMOVCO(N))                                  CHKNST.70
          NESTJ(N)=NESTJ(N)+IMOVEJ(N,IMOVCO(N))                                  CHKNST.71
          PRINT *,'NEW NESTI,NESTJ ARE * ',NESTI(N),NESTJ(N),' *'                CHKNST.72
          PRINT *,'THIS IS THE ** ',IMOVCO(N),' ** MOVE !'                       CHKNST.73
          IF(IOVERW(N).EQ.1)PRINT*,                                              CHKNST.74
     +      '!!! MOVING A READ-IN NEST (IOVERW=1) !!!'                           CHKNST.75
          IMOVCO(N)=IMOVCO(N)+1                                                  CHKNST.76
          INSTES=1                                                               CHKNST.77
          IF(NUMLV(IDLEV+1,N).GT.0)INSTES=11                                     CHKNST.78
          IX=NESTIX(ICOARS)                                                      CHKNST.79
          JX=NESTJX(ICOARS)                                                      CHKNST.80
          INX=NESTIX(N)                                                          CHKNST.81
          JNX=NESTJX(N)                                                          CHKNST.82
          IXM=1                                                                  CHKNST.83
          JXM=1                                                                  CHKNST.84
          KXM=1                                                                  CHKNST.85
          INXM=1                                                                 CHKNST.86
          JNXM=1                                                                 CHKNST.87
          KNXM=1                                                                 CHKNST.88
          IF(IMOIST(N).EQ.2)THEN                                                 CHKNST.89
            IXM=IX                                                               CHKNST.90
            JXM=JX                                                               CHKNST.91
            KXM=MKX                                                              CHKNST.92
            INXM=INX                                                             CHKNST.93
            JNXM=JNX                                                             CHKNST.94
            KNXM=MKX                                                             CHKNST.95
          ENDIF                                                                  CHKNST.96
          CALL INITNEST(IX,JX,MKX,INX,JNX,MKX,IXM,JXM,KXM,INXM,JNXM,KNXM         CHKNST.97
     +         ,NESTI(N),NESTJ(N),N,HTNO,INSTES,ISOUTHO,JWESTO)                  CHKNST.98
          IUTNEW=(N-1)+41                                                        CHKNST.99
          CALL ADDRX1C(IAXALL(1,N))                                              CHKNST.100
          KTAUH=KTAU                                                             CHKNST.101
          KTAU=0                                                                 CHKNST.102
          IUTNEW=N-1+41                                                          CHKNST.103
          IF(IFTAPE.EQ.1)CALL OUTTAP(IUTNEW,N,IL,JL,MKX,1)                       CHKNST.104
          KTAU=KTAUH                                                             CHKNST.105
        ENDIF                                                                    CHKNST.106
        GOTO 100                                                                 CHKNST.107
C                                                                                CHKNST.108
C--- IF FADE OUT TIME                                                            CHKNST.109
C--- REACHED, DEACTIVATE.                                                        CHKNST.110
C                                                                                CHKNST.111
      ELSEIF(IACTIV(N).EQ.1.AND.XTIMET.GE.XENNES(N))THEN                         CHKNST.112
        NUMLV(IDLEV,ICOARS)=NUMLV(IDLEV,ICOARS)-1                                CHKNST.113
        NSTTOT=NSTTOT-1                                                          CHKNST.114
        PRINT *,'THIS NEST SWITCHED OFF AT XTIME = ',XTIME                       CHKNST.115
        IF(KTAU.GE.NINT((TAPTIM-TAPFRQ+5.)/DTMIN))THEN                           07NOV00.809
          IUTNEW=N-1+41                                                          CHKNST.117
          CALL ADDRX1C(IAXALL(1,N))                                              CHKNST.118
          IF(IFTAPE.EQ.1)CALL OUTTAP(IUTNEW,N,IL,JL,MKX,0)                       CHKNST.119
        ENDIF                                                                    CHKNST.120
        IACTIV(N)=0                                                              CHKNST.121
        GOTO 100                                                                 CHKNST.122
C                                                                                CHKNST.123
C--- IF INITIALIZATION TIME REACHED, ACTIVATE                                    CHKNST.124
C                                                                                CHKNST.125
      ELSEIF(IACTIV(N).EQ.0.AND.XTIMET.GE.XSTNES(N))THEN                         CHKNST.126
        IACTIV(N)=1                                                              CHKNST.127
        NUMLV(IDLEV,ICOARS)=NUMLV(IDLEV,ICOARS)+1                                CHKNST.128
C       PRINT *,'NUMLV = ',NUMLV(IDLEV,ICOARS),IDLEV,ICOARS                      07NOV00.810
        IF(XTIME*60..LE.DT5)THEN                                                 CHKNST.130
          PRINT *,'         '                                                    CHKNST.131
          PRINT *,'THIS NEST INITIALIZED AT START UP '                           CHKNST.132
          IEXEC(N)=1                                                             CHKNST.133
          INSTES=-1                                                              CHKNST.134
        ELSE                                                                     CHKNST.135
          PRINT *,'         '                                                    CHKNST.136
          PRINT *,'THIS NEST INITIALIZED AT XTIME = ',XTIME                      CHKNST.137
          IEXEC(N)=1                                                             CHKNST.138
          INSTES=-3                                                              CHKNST.139
        ENDIF                                                                    CHKNST.140
        ISOUTHO=1                                                                CHKNST.141
        JWESTO=1                                                                 CHKNST.142
        IX=NESTIX(ICOARS)                                                        CHKNST.143
        JX=NESTJX(ICOARS)                                                        CHKNST.144
        INX=NESTIX(N)                                                            CHKNST.145
        JNX=NESTJX(N)                                                            CHKNST.146
#ifndef MPP1                                                                     CHKNST.147
        IF(INX.GT.MIX.OR.JNX.GT.MJX)STOP 'NEST TOO LARGE FOR MIX,MJX'            CHKNST.148
#else                                                                            CHKNST.149
        IF(INX.GT.MIX_G.OR.JNX.GT.MJX_G)STOP                                     CHKNST.150
     +    'NEST TOO LARGE FOR MIX,MJX'                                           CHKNST.151
#endif                                                                           CHKNST.152
        IXM=1                                                                    CHKNST.153
        JXM=1                                                                    CHKNST.154
        KXM=1                                                                    CHKNST.155
        INXM=1                                                                   CHKNST.156
        JNXM=1                                                                   CHKNST.157
        KNXM=1                                                                   CHKNST.158
        IF(IMOIST(N).EQ.2)THEN                                                   CHKNST.159
          IXM=IX                                                                 CHKNST.160
          JXM=JX                                                                 CHKNST.161
          KXM=MKX                                                                CHKNST.162
          INXM=INX                                                               CHKNST.163
          JNXM=JNX                                                               CHKNST.164
          KNXM=MKX                                                               CHKNST.165
        ENDIF                                                                    CHKNST.166
#ifdef MPP1                                                                      CHKNST.167
        CLOCKSTART=MILLICLOCK()                                                  CHKNST.168
#endif                                                                           CHKNST.169
        CALL INITNEST(IX,JX,MKX,INX,JNX,MKX,IXM,JXM,KXM,INXM,JNXM,KNXM,          CHKNST.170
     +       NESTI(N),NESTJ(N),N,HTNO,INSTES,ISOUTHO,JWESTO)                     CHKNST.171
#ifdef MPP1                                                                      CHKNST.172
          PRINT*,'INITNEST TOOK ',MILLICLOCK()-CLOCKSTART                        CHKNST.173
#endif                                                                           CHKNST.174
        NSTTOT=NSTTOT+1                                                          CHKNST.175
C                                                                                CHKNST.176
C--- OUTPUT INITIAL STUFF                                                        CHKNST.177
C                                                                                CHKNST.178
        IUTNEW=(N-1)+41                                                          CHKNST.179
        CALL ADDRX1C(IAXALL(1,N))                                                CHKNST.180
        KTAUH=KTAU                                                               CHKNST.181
        KTAU=0                                                                   CHKNST.182
        IF(IFTAPE.EQ.1)CALL OUTTAP(IUTNEW,N,IL,JL,MKX,1)                         CHKNST.183
C RESET IOVERW TO ZERO AFTER INITIALIZING NESTS                                  CHKNST.184
        IF(IOVERW(N).EQ.1)IOVERW(N)=0                                            CHKNST.185
        KTAU=KTAUH                                                               CHKNST.186
      ENDIF                                                                      CHKNST.187
C                                                                                CHKNST.188
  100 CONTINUE                                                                   CHKNST.189
      RETURN                                                                     CHKNST.190
      END                                                                        CHKNST.191
                                                                                 CHKNST.192
