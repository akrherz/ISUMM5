      subroutine bcast_size( inest, icurs )
#include <rsl.inc>
#include <parame.incl>
#include <param2.incl>
#include <soilp.incl>
      integer inest
      integer icurs

      icurs = 1

      icurs = icurs+1                   ! ht 
      icurs = icurs+1                   ! psa
      icurs = icurs+1                   ! msfx
      icurs = icurs+1                   ! msfd
      icurs = icurs+1                   ! xlat
      icurs = icurs+1                   ! xlon
      icurs = icurs+1                   ! help1  (R11, initnest)
      icurs = icurs+1                   ! xlongd (R11, initnest)
      icurs = icurs+1                   ! snowc
      icurs = icurs+1                   ! f
      icurs = icurs+1                   ! tga
      icurs = icurs+1                   ! tgsi
      icurs = icurs+1                   ! tmn
      icurs = icurs+1                   ! tss, added 19991220
      icurs = icurs+1                   ! rainc
      icurs = icurs+1                   ! rainnc
      icurs = icurs+1                   ! mavail
      if ( inhyd .eq. 1 ) then
        icurs = icurs+1                 ! ps0
      endif
      if ( isoil .eq. 2 .or. isoil .eq. 3 ) then
        icurs = icurs+1                   ! IVGTYP
        icurs = icurs+1                   ! ISLTYP
        icurs = icurs+1                   ! VEGFRC
        icurs = icurs+1                   ! XVEGMN
        icurs = icurs+1                   ! XICE, added 20001024
        icurs = icurs+1                   ! CMCA
        icurs = icurs+1                   ! SNOA
        icurs = icurs+1                   ! SNOWHA
        icurs = icurs+mlx                 ! STCA
        icurs = icurs+mlx                 ! SMCA
        icurs = icurs+mlx                 ! SWCA
        icurs = icurs+nmo                 ! VEGFMO, added 20011030
      endif

      icurs = icurs+mkx                 ! ta
      icurs = icurs+mkx                 ! ta_sav
      if ( idry(inest) .eq. 0 ) then
        icurs = icurs+mkx               ! qva
        icurs = icurs+mkx               ! qva_sav
        if (imoist(inest).eq.2) then
            icurs = icurs+mkx           ! qca
            icurs = icurs+mkx           ! qca_sav
            icurs = icurs+mkx           ! qra
            icurs = icurs+mkx           ! qra_sav
            if(iice.eq.1)then
              icurs = icurs+mkx         ! qia
              icurs = icurs+mkx         ! qia_sav
              icurs = icurs+mkx         ! qnia
              icurs = icurs+mkx         ! qnia_sav
              if(iiceg.eq.1)then
                icurs = icurs+mkx         ! qga
                icurs = icurs+mkx         ! qga_sav
                icurs = icurs+mkx         ! qnca
                icurs = icurs+mkx         ! qnca_sav
              endif
            endif
        endif
      endif

      if(inav.eq.1)then
          icurs = icurs+mkx             ! turke
          icurs = icurs+mkx             ! pqwsb
          icurs = icurs+mkx             ! pthlsb
          icurs = icurs+mkx             ! pthlqwb
      endif
      if(ibltyp(inest).eq.6)then
          icurs = icurs+mkx+1           ! tkea (note addit. level)
          icurs = icurs+mkx+1           ! tkea_sav (note addit. level)
          icurs = icurs+mkx+1           ! tkeb (note addit. level)
      endif
      if(isoil.eq.1)then
          icurs = icurs+mlx             ! tlsa
      endif

      if ( isoil .eq. 2 .or. isoil .eq. 3 ) then
          icurs = icurs+mlx             ! stca_sav, added 20001024
          icurs = icurs+mlx             ! smca_sav, added 20001024
          icurs = icurs+mlx             ! swca_sav, added 20001024
      endif

      if (inhyd.eq.1) then
          icurs = icurs+mkx             ! t0
          icurs = icurs+mkx             ! ppa
          icurs = icurs+mkx             ! ppa_sav
          icurs = icurs+mkx+1           ! wa (note addit. level)
          icurs = icurs+mkx+1           ! wa_sav (note addit. level)
      endif

        icurs = icurs+mkx               ! ua
        icurs = icurs+mkx               ! ua_sav
        icurs = icurs+mkx               ! va
        icurs = icurs+mkx               ! va_sav

      return
      end
