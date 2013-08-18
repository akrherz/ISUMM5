SUBROUTINE proc_tc_bogus 

! === Program to perform TC bogussing ===  
!     Developed by S. Low-Nam and C. Davis, NCAR - March 2001

!  The procedure calls for removing an ill-defined initial vortex and
!  replacing it with an axi-symmetric vortex with location and strength
!  as reported and merging it to the original background fields. The
!  storm at initial time is in balanced with its intensity being a 
!  function of the grid resolution under consideration. 

   USE constants
   USE file_data
   USE first_guess_data
   USE gridded_data
   USE header_data
   USE namelist_info
   USE tc_bogus

   IMPLICIT NONE

   REAL , DIMENSION(jxf,ixf,number_of_original_levels+rec2%number_of_new_levels) :: u11 , v11 , t11 , rh11 , phi11
   REAL , DIMENSION(jxf,ixf) :: pslxx , psldd , ter , mfx , mfd , cor , xlat , latd , xlon , lond 
   REAL , DIMENSION(jxf,ixf,number_of_original_levels+rec2%number_of_new_levels) :: dum3d
   REAL , DIMENSION(jxf,ixf) :: dum2d

   TYPE(sh) :: sh_dummy

   INTEGER ::  index_u , index_v , index_t , index_rh , index_h ,               &
               index_pslx , index_psld , index_ter , index_cor , index_mfx ,    &
               index_mfd , index_xlat , index_latd , index_xlon , index_lond
   INTEGER ::  name_loop
   INTEGER ::  kx
   INTEGER ::  bogus_year , bogus_month , bogus_day , bogus_hour ,      &
               num_storm  
   REAL    ::  latc_loc(num_max) , lonc_loc(num_max) , vmax(num_max)

   kx = number_of_original_levels+rec2%number_of_new_levels

   all_3d_names : DO name_loop = 1 , fg_variables_up_index

      dum3d = all_3d(:,:,:,name_loop)
      sh_dummy = sh_up(name_loop)
      IF      ( sh_dummy%name .eq. 'U' ) THEN
         u11 = dum3d
         index_u = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'V' ) THEN
         v11 = dum3d
         index_v = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'T' ) THEN
         t11 = dum3d
         index_t = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'RH' ) THEN
         rh11 = dum3d
         index_rh = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'H' ) THEN
         phi11 = dum3d
         index_h = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE
         IF ( rec4%print_echo ) THEN
            print *,' skipping ',sh_dummy%name
         END IF
      END IF
   END DO all_3d_names

   all_2d_names : DO name_loop = 1 , fg_variables_sfc_index
      dum2d = all_2d(:,:,name_loop)
      sh_dummy = sh_sfc(name_loop)
      IF      ( sh_dummy%name .eq. 'CORIOLIS' ) THEN
         cor = dum2d
         index_cor = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'TERRAIN ' ) THEN
         ter = dum2d
         index_ter = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'MAPFACCR' ) THEN
         mfx = dum2d
         index_mfx = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'MAPFACDT' ) THEN
         mfd = dum2d
         index_mfd = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'LATITCRS' ) THEN
         xlat = dum2d
         index_xlat = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'LATITDOT' ) THEN
         latd = dum2d
         index_latd = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'LONGICRS' ) THEN
         xlon = dum2d
         index_xlon = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'LONGIDOT' ) THEN
         lond = dum2d
         index_lond = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'PSEALVLC' ) THEN
         pslxx = dum2d
         index_pslx = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE IF ( sh_dummy%name .eq. 'PSEALVLD' ) THEN
         psldd = dum2d
         index_psld = name_loop
         IF ( rec4%print_echo ) THEN
            print *,' found ',sh_dummy%name
         END IF
      ELSE
         IF ( rec4%print_echo ) THEN
            print *,' skipping ',sh_dummy%name
         END IF
      endif
   END DO all_2d_names

   !  Add a bogus tropical storm.

   CALL do_bogus ( jxf , ixf , kx , hdate(1:16) ,                                 &
                   u11 , v11 , t11 , rh11 , phi11 , pslxx , psldd ,               &
                   ter , mfx , mfd , cor , xlat , latd , xlon , lond , pressure , &
                   rec5%num_storm , rec5%latc_loc , rec5%lonc_loc ,               &
                   rec5%vmax )

   !  Re-assign the computed values for the 3d data and SLP back to the original
   !  storage locations.

   all_3d(:,:,:,index_u)  = u11
   all_3d(:,:,:,index_v)  = v11
   all_3d(:,:,:,index_t)  = t11
   all_3d(:,:,:,index_rh) = rh11
   all_3d(:,:,:,index_h)  = phi11

   all_2d(:,:,index_pslx) = pslxx
   all_2d(:,:,index_psld) = psldd

END SUBROUTINE proc_tc_bogus
