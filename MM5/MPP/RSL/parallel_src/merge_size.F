      subroutine merge_size( inest, icurs )
C------------------------------Description------------------------------
C------------------------------Parameters-------------------------------
#include <rsl.inc>
#include <parame.incl>
#include <param2.incl>
      integer inest
      integer icurs
C---------------------------Local workspace-----------------------------
C------------------------------Externals--------------------------------
C------------------------------Executable-------------------------------


      icurs = 1

C New for r11 (see changes to mp_feedback)
      icurs = icurs+1                   ! ht
      icurs = icurs+1                   ! satbrt
      icurs = icurs+1                   ! psa
      icurs = icurs+1                   ! psb
      icurs = icurs+1                   ! rpsa
      icurs = icurs+1                   ! rpsb
      icurs = icurs+1                   ! rpsc
      icurs = icurs+1                   ! pdota
      icurs = icurs+1                   ! pdotb
      icurs = icurs+1                   ! rpdota
      icurs = icurs+1                   ! rpdotb
      icurs = icurs+1                   ! ps0
      icurs = icurs+mkx                 ! t0
      icurs = icurs+mkx                 ! pr0
      icurs = icurs+mkx                 ! rho0
C Added 2000/09/01 jm
      icurs = icurs+1                   ! rainc
      icurs = icurs+1                   ! rainnc
      icurs = icurs+1                   ! MAVAIL
C Added SNOWC and SNOA 2001/11/09 ajb
      if (ifsnow(inest).eq.2) then
        icurs = icurs+1                   ! SNOWC
      endif
      if (isoil.eq.2) then
        icurs = icurs+1                   ! SNOA
        icurs = icurs+1                   ! SNOWHA
      endif
C
      icurs = icurs+1                   ! psa
      icurs = icurs+1                   ! tga
      icurs = icurs+1                   ! tgsi
      if(inav.eq.1)then
          icurs = icurs+mkx             ! turke
          icurs = icurs+mkx             ! pqwsb
          icurs = icurs+mkx             ! pthlqwb
          icurs = icurs+mkx             ! pthlsb
      endif
      if(ibltyp(inest).eq.6)then
          icurs = icurs+mkx+1           ! tkea (note addit. level)
      endif
      icurs = icurs+mkx                 ! ta
      if (inhyd.eq.1) then
          icurs = icurs+mkx+1           ! wa (note addit. level)
          icurs = icurs+mkx             ! ppa
      endif

      if ( idry(inest) .eq. 0 ) then
          icurs = icurs+mkx             ! qva
          if (imoist(inest).eq.2) then
            icurs = icurs+mkx           ! qca
            icurs = icurs+mkx           ! qra
            if ( iice .eq. 1 ) then
              icurs = icurs+mkx           ! qia
              icurs = icurs+mkx           ! qnia
              if ( iice .eq. 1 ) then
                icurs = icurs+mkx           ! qga
                icurs = icurs+mkx           ! qnca
              endif
            endif
          endif
      endif

      icurs = icurs+mkx               ! ua
      icurs = icurs+mkx               ! va

      return
      end
