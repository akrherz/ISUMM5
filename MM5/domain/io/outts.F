      SUBROUTINE OUTTS(INEST,IX,JX,XTIME)                                        OUTTS.1
      IMPLICIT NONE                                                              OUTTS.2
C                                                                                OUTTS.3
C-----OUTPUT VARIABLES IN TIME SERIES                                            OUTTS.4
C                                                                                OUTTS.5
#     include <parame.incl>                                                      OUTTS.6
#     include <param2.incl>                                                      OUTTS.7
#     include <param3.incl>                                                      OUTTS.8
#     include <point3d.incl>                                                     OUTTS.9
#     include <point2d.incl>                                                     OUTTS.10
#     include <rpstar.incl>                                                      OUTTS.11
#     include <nonhyd.incl>                                                      OUTTS.12
#     include <hdrv3.incl>                                                       19DEC02.679
      integer iproj                                                              19DEC02.680
      real uhold, vhold                                                          19DEC02.681
      real confac, pole, cenlon, ang                                             19DEC02.682
                                                                                 OUTTS.13
      INTEGER ITSUNIT,ILX,IX,JLX,JX,NTS,ITS,INEST,JTS,K                          OUTTS.14
      REAL    CLW,UATS,VATS,TSCRN,QATS,XTIME                                     OUTTS.15
                                                                                 OUTTS.16
#ifdef MPP1                                                                      OUTTS.17
      INTEGER RETVAL                                                             OUTTS.18
#endif                                                                           OUTTS.19
                                                                                 OUTTS.20
#ifndef MPP1                                                                     OUTTS.21
      itsunit=25+inest                                                           OUTTS.22
#else                                                                            OUTTS.23
      itsunit=6                                                                  OUTTS.24
#endif                                                                           OUTTS.25
c     print *, 'OUTPUT T-S FOR DOMAIN ', inest, ' FROM UNIT ',itsunit            OUTTS.26
c     if (inest.eq.1) print *, 'xtime = ', xtime, inest,ix,jx                    OUTTS.27
      ilx = ix-1                                                                 OUTTS.28
      jlx = jx-1                                                                 OUTTS.29
      do nts=1,itsdim                                                            OUTTS.30
        if(tslat(nts).lt.900..and.tslon(nts).lt.900.)then                        OUTTS.31
          its=itsn(inest,nts)                                                    OUTTS.32
          jts=jtsn(inest,nts)                                                    OUTTS.33
#ifndef MPP1                                                                     OUTTS.34
          if(its.ne.1.and.its.ne.ilx.and.jts.ne.1.and.jts.ne.jlx)then            OUTTS.35
#else                                                                            OUTTS.36
          CALL RSL_POINT_ON_PROC(DOMAINS(INEST),ITS,JTS,RETVAL)                  OUTTS.37
          if(its.ne.1.and.its.ne.ilx.and.jts.ne.1.and.jts.ne.jlx                 OUTTS.38
     -       .and. retval.eq.1 )then                                             OUTTS.39
#endif                                                                           OUTTS.40
                                                                                 OUTTS.41
c     calculate column-integrated liquid/ice  (kg/m^2 or mm)                     OUTTS.42
            clw=0.                                                               OUTTS.43
            if(imoist(inest).eq.2)then                                           OUTTS.44
              do k=1,kx                                                          OUTTS.45
                clw=clw+(qca(its,jts,k)+qra(its,jts,k))*dsigma(k)                OUTTS.46
     -               *1000./g                                                    OUTTS.47
                if(iice.eq.1)                                                    OUTTS.48
     -          clw=clw+(qia(its,jts,k)+qnia(its,jts,k))*dsigma(k)               OUTTS.49
     -               *1000./g                                                    OUTTS.50
              enddo                                                              OUTTS.51
            endif                                                                OUTTS.52
                                                                                 OUTTS.53
            if (ibltyp(inest).ne.5.and.ibltyp(inest).ne.2.and.                   05DEC01.289
     -          ibltyp(inest).ne.4) then                                         05DEC01.290
              uats=0.25*(ua(its,jts,kx)+ua(its+1,jts,kx)                         OUTTS.55
     -                  +ua(its,jts+1,kx)+ua(its+1,jts+1,kx))                    OUTTS.56
     -                  *rpsa(its,jts)                                           OUTTS.57
              vats=0.25*(va(its,jts,kx)+va(its+1,jts,kx)                         OUTTS.58
     -                  +va(its,jts+1,kx)+va(its+1,jts+1,kx))                    OUTTS.59
     -                  *rpsa(its,jts)                                           OUTTS.60
              if (tga(its,jts) .le. ta(its,jts,kx)*rpsa(its,jts) ) then          OUTTS.61
                tscrn = ta(its,jts,kx)*rpsa(its,jts)                             OUTTS.62
              else                                                               OUTTS.63
                tscrn = (ta(its,jts,kx)*rpsa(its,jts)+tga(its,jts))*0.5          OUTTS.64
              endif                                                              OUTTS.65
              qats=qva(its,jts,kx)*rpsa(its,jts)                                 OUTTS.66
            else                                                                 05DEC01.291
              uats=ua10(its,jts)                                                 OUTTS.68
              vats=va10(its,jts)                                                 OUTTS.69
              tscrn=ta2(its,jts)                                                 OUTTS.70
              qats=qa2(its,jts)                                                  OUTTS.71
            end if                                                               OUTTS.72
C Rotate the winds from grid coordinates to earth coordinates.                   19DEC02.683
            iproj = bhi(7,1) ! projection flag                                   19DEC02.684
            if (iproj .eq. 3) then                                               19DEC02.685
               ! Mercator grid, no rotation necessary                            19DEC02.686
            else                                                                 19DEC02.687
               ! Lambert Conformal and Polar Stereographic projections           19DEC02.688
               uhold = uats                                                      19DEC02.689
               vhold = vats                                                      19DEC02.690
               confac = bhr(4,1)                                                 19DEC02.691
               pole   = bhr(7,1)                                                 19DEC02.692
               cenlon = bhr(3,1)                                                 19DEC02.693
               ang = xlong(its,jts)-cenlon                                       19DEC02.694
               ang = ang * confac * degrad                                       19DEC02.695
               if (pole .lt. 0) ang=-ang                                         19DEC02.696
               uats = uhold*cos(ang)+vhold*sin(ang)                              19DEC02.697
               vats = vhold*cos(ang)-uhold*sin(ang)                              19DEC02.698
            endif                                                                19DEC02.699
                                                                                 OUTTS.73
#ifndef MPP1                                                                     OUTTS.74
            write(itsunit,987) xtime,nts,its,jts,                                OUTTS.75
     &         tscrn, qats, uats, vats,                                          OUTTS.76
     &         psa(its,jts), ppa(its,jts,kx)*rpsa(its,jts),                      OUTTS.77
     &         rainc(its,jts),rainnc(its,jts),clw,                               OUTTS.78
     &         glw(its,jts),hfx(its,jts),qfx(its,jts),                           OUTTS.79
     &         gsw(its,jts)/(1.-alb(its,jts)),tga(its,jts)                       OUTTS.80
#else                                                                            OUTTS.81
C note that this calls a C routine. If the output fields are                     OUTTS.82
C changed here, you must also change the routine MPP/outts_c.c                   OUTTS.83
            call outts_c(inest,xtime,nts,its,jts,                                OUTTS.84
     &         tscrn, qats, uats, vats,                                          OUTTS.85
     &         psa(its,jts), ppa(its,jts,kx)*rpsa(its,jts),                      OUTTS.86
     &         rainc(its,jts),rainnc(its,jts),clw,                               OUTTS.87
     &         glw(its,jts),hfx(its,jts),qfx(its,jts),                           OUTTS.88
     &         gsw(its,jts)/(1.-alb(its,jts)),tga(its,jts))                      OUTTS.89
#endif                                                                           OUTTS.90
                                                                                 OUTTS.91
          endif                                                                  OUTTS.92
        endif                                                                    OUTTS.93
      enddo                                                                      OUTTS.94
                                                                                 OUTTS.95
  987 FORMAT(f6.1,3i5,15f20.10)                                                  OUTTS.96
      return                                                                     OUTTS.97
                                                                                 OUTTS.98
      end                                                                        OUTTS.99
