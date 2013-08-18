PROGRAM interp

   USE all_io
   USE bdy
   USE date_pack
   USE diags
   USE file
   USE header_data
   USE hydro_interp
   USE lateral_bdy
   USE nh_interp
   USE phys_consts
   USE util

   IMPLICIT NONE

   LOGICAL :: daily_mean_sst , repeat

   LOGICAL :: cloud_and_rain       = .FALSE. , &
              snow_and_ice         = .FALSE. , &
              graupel              = .FALSE. , &
              number_concentration = .FALSE. 

   INTEGER :: endtime ,  excgflag ,  i ,  ialready ,  ifirst ,  immout ,  imx ,  imxexcg ,  imxm ,  index , &
              input_count ,  ioffset ,  iprocess ,  itimes ,  iunit ,  iusesfc ,  j ,  jmx ,  jmxexcg , &
              jmxm ,  joffset ,  k ,  k_loop ,  kx ,  kxs ,  levidn ,  loop ,  loop_count ,  lp ,  mdate , &
              mdateold ,  nesti  ,  nestix ,  nestj  ,  nestjx ,  numnc  ,  num_lowbdy_fields

   REAL :: ds , nestds ,  pt

   REAL , DIMENSION(100) :: newcoord

   REAL , ALLOCATABLE , DIMENSION(:) :: a , dsigma , p , pup , sigh , sighup , vcoord

   REAL , ALLOCATABLE , DIMENSION(:,:) :: coriolis , ground_t , ksafec , ksafed , land_use , latitcrs , &
                                          latitdot , longicrs , longidot , mapfaccr , mapfacdt , ps0 , &
                                          ps0csmall , ps0dot , ps0dsmall , pscsmall , psdsmall , psealvlc , &
                                          psealvld , pstarcrs , pstardot , sfc_p , sfc_rh , sfc_t , &
                                          sfc_theta , sfc_u , sfc_v , snowcovr , terrain , terrdot , &
                                          tseasfc , w_top

   REAL , ALLOCATABLE , DIMENSION(:,:,:) :: height , pcorig , pp_sig , ppclin , ppclog , ppdlin , ppdlog , &
                                            ppeb3d , ppnb3d , ppsb3d , ppwb3d , pr0 , psclin , psclog , psdlin , &
                                            psdlog , ptrapcrs , ptrapdot , q , q_sig , qeb3d , qnb3d , qsb3d , &
                                            qwb3d , rh , rh_sig , rho0 , t , t0 , t_sig , teb3d , the_sig , thetap , &
                                            tnb3d , tsb3d , tv_sig , twb3d , u , u_sig , ueb3d , unb3d , usb3d , &
                                            uwb3d , v , v_sig , veb3d , vnb3d , vsb3d , vwb3d , w_sig , web2d , &
                                            web3d , wnb2d , wnb3d , wsb2d , wsb3d , wwb2d , wwb3d

   REAL , ALLOCATABLE , DIMENSION(:,:)   :: sfc_qc , sfc_qr , sfc_qs , sfc_qi , sfc_qg , sfc_qn

   REAL , ALLOCATABLE , DIMENSION(:,:,:) :: qc , qc_sig , &
                                            qr , qr_sig , &
                                            qs , qs_sig , &
                                            qi , qi_sig , &
                                            qg , qg_sig , &
                                            qn , qn_sig
   
   TYPE(sh) :: ground_t_sh , tseasfc_sh , sfc_t_sh

   !  NAMELIST local data.

   CHARACTER (LEN=132) , DIMENSION(100) :: input_file

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute=0 , start_second=0 , start_frac=0
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute=0 ,   end_second=0 ,   end_frac=0
   INTEGER :: interval
   LOGICAL :: less_than_24h=.FALSE.

   REAL , DIMENSION(1000) :: sigma_f_bu = -1
   REAL :: ptop = -1
   INTEGER :: isfc = 0

   REAL :: p0 , tlp , ts0 , tiso=0

!KWM START
   INTEGER :: psfc_method
   REAL , ALLOCATABLE , DIMENSION(:,:) :: avgsfct
!KWM END
   LOGICAL :: removediv , usesfc , wrth2o   

   INTEGER :: ifdatim

   !  Process the namelist file.  The namelist is read, the data is
   !  brought back to the main program, a few tests are run on
   !  the local variables, and the sigma is tested.

   ifdatim = -1
   CALL do_namelist ( input_file ,  &
                      start_year , start_month , start_day , start_hour , &
                      start_minute , start_second , start_frac , &
                      end_year ,   end_month ,   end_day ,   end_hour , &
                      end_minute ,   end_second ,   end_frac , &
                      interval , less_than_24h , &
                      sigma_f_bu , ptop , isfc ,  &
                      p0, tlp , ts0 , tiso , &
                      removediv , usesfc , wrth2o , &
!KWM START
                      psfc_method ,  &
!KWM END
                      ifdatim )

   !  The date info was contained in the namelist.  When to start and
   !  when to end, interval.

   WRITE ( start_date ,  &
           '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
           start_year , start_month , start_day , start_hour , &
           start_minute , start_second

   current_date = start_date

   WRITE ( end_date ,  &
           '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
           end_year , end_month , end_day , end_hour , &
           end_minute , end_second

   PRINT '(A)','Time periods to process'
   loop_count = 0
   which_times : DO 

      loop_count = loop_count + 1
      IF ( loop_count .GT. 1000 ) THEN
         PRINT '(A)','Seems like a lot of loops.'
         STOP 'lots_of_loops_check_dates'
      END IF

      IF ( current_date .EQ. end_date ) THEN
         PRINT '(A,A,A)','Found ending time ',current_date,'.'
         iprocess = loop_count
         EXIT which_times
      ELSE IF ( current_date .LT. end_date ) THEN
            PRINT '(A,A,A)','Found valid time ',current_date,'.'
      ELSE
         iprocess = loop_count -1 
         EXIT which_times
      END IF

      CALL geth_newdate ( new_date , current_date , interval ) 
      current_date = new_date

   END DO which_times

   !  For the lower boundary condition, we need at least one full
   !  day's worth of processing.

   IF ( iprocess * interval .LT. 86400 ) THEN
      PRINT '(A)','The lower boundary condition needs at least one full day of data.'
      PRINT '(A,I2,A,I5,A)','You have specified only ',iprocess,' time periods at ',interval,' s.'
      IF ( less_than_24h ) THEN
         PRINT '(A)','You asked for less than 24 h of data, so we are proceeding.'
      ELSE
         PRINT '(A)','If you REALLY want less than 24 h of data, set less_than_24h = .TRUE. in NAMELIST record1.'
         STOP 'need_full_day'
      END IF
   END IF

   IF ( usesfc) THEN
      iusesfc = 1
   ELSE
      iusesfc = 0
   END IF

   !  Compute the number of half sigma layers.

   kxs = 0
   find_sigma : DO k_loop = 1 , 1000
      IF ( sigma_f_bu(k_loop) .LT. -0.5 ) THEN
         kxs = k_loop -2 
         EXIT find_sigma
      END IF
   END DO find_sigma
   IF ( kxs .LT. 1 ) THEN
      PRINT '(A)','Error in finding how many sigma levels there are.'
      PRINT *,sigma_f_bu
      STOP 'how_many_sigma_levels'
   END IF
   check_sigma : DO k_loop = 2 , kxs
      IF ( sigma_f_bu(k_loop) .GE. sigma_f_bu(k_loop-1) ) THEN
         PRINT '(A)','Error in defining full sigma levels in namelist'
         PRINT '(A,I3,A,F6.4,A,I3,A,F6.4)','sigma_f_bu(',k_loop,')=',sigma_f_bu(k_loop), &
                ' is greater than sigma_f_bu(',k_loop-1,')=',sigma_f_bu(k_loop-1)
         PRINT '(A)','Sigma levels must start with 1 and end with 0, and they must vary monotonically'
         STOP
      END IF
   END DO check_sigma
!  kxs = MINLOC(sigma_f_bu,DIM=1,MASK=sigma_f_bu.LT.-0.5) - 2
   newcoord(1:kxs+1) = sigma_f_bu(1:kxs+1)

   !  Make sure the sigma levels are OK, descending order, less than 1.0.

   CALL check (kxs, newcoord)

   !  This is some more I/O set up.  This OPENS the pressure level
   !  data set with logical unit 50 (input_press), and with the name specified
   !  in the NAMELIST file.

   input_count = 1
   CALL get_fg_file ( input_file(input_count) , input_press ) 

   !  Once we have the pressure level data ready to read, we
   !  can procees.  We process the data in the big header first.
   !  The big header data comes out of this routine, and is available
   !  via the USE association for header_data.

   CALL read_bh ( input_press )

   !  Some of this big header information is important.  Particularly, we
   !  pick up the horizontal size of the domain.  This allows us to go ahead
   !  and ALLOCATE some space a bit later.

   imx = bhi(16,1)
   jmx = bhi(17,1)
   kx  = bhi(12,bhi(1,1)) - 1
   ds  = bhr( 9,1)
   index = bhi(1,1)
   IF      ( ptop .LT. 0 ) THEN
      ptop = bhr(2,2)
      PRINT '(A,F8.2,A)','Using PTOP from the REGRID header: ',ptop,' Pa.'
   ELSE IF ( ptop .GT. bhr(2,2) ) THEN
      PRINT '(A,F8.2,A)','Using PTOP from the namelist.input file: ',ptop,' Pa.'
      bhr(2,2) = ptop
   ELSE IF ( ABS ( ptop - bhr(2,2) ) .LT. 0.1 ) THEN
      PRINT '(A,F8.2,A)','Using PTOP from the namelist.input file: ',ptop,' Pa.'
      bhr(2,2) = ptop
   ELSE
      PRINT '(A,F8.2,A)','PTOP from header = ',bhr(2,2),' Pa.'
      PRINT '(A,F8.2,A)','PTOP from the namelist.input file = ',ptop,' Pa.'
      PRINT '(A)','Namelist value must be .GE. header value.'
      STOP 'bad_ptop_choice'
   END IF

   imxm = imx - 1
   jmxm = jmx - 1
  
   !  Some more I/O things.  This sets up the names of the output files
   !  for the INTERP program.  UNIT=20 is the MMINPUT_DOMAINx, UNIT=30
   !  is the BDYOUT_DOMAINx, UNIT=40 is the LOWBDY_DOMAINx file.
   !  The bhi(13,1) is the domain ID, so that we can use a number for 
   !  the "x" in DOMAINx.
 
   CALL open_out_file ( output_mminput , output_bdyout , output_lowerbc , bhi(13,1) )

   !  ALLOCATE space for all of the 2d arrays and the 3d arrays that
   !  are a function of sigma - not pressure.

   ALLOCATE ( GROUND_T    ( IMX , JMX ) )
   ALLOCATE ( PSTARCRS    ( IMX , JMX ) ) 
   ALLOCATE ( PSTARDOT    ( IMX , JMX ) )
   ALLOCATE ( CORIOLIS    ( IMX , JMX ) )
   ALLOCATE ( LAND_USE    ( IMX , JMX ) )
   ALLOCATE ( LATITCRS    ( IMX , JMX ) )
   ALLOCATE ( LATITDOT    ( IMX , JMX ) )
   ALLOCATE ( LONGICRS    ( IMX , JMX ) )
   ALLOCATE ( LONGIDOT    ( IMX , JMX ) )
   ALLOCATE ( MAPFACCR    ( IMX , JMX ) )
   ALLOCATE ( MAPFACDT    ( IMX , JMX ) )
   ALLOCATE ( PSEALVLC    ( IMX , JMX ) )
   ALLOCATE ( PSEALVLD    ( IMX , JMX ) )
   ALLOCATE ( SFC_P       ( IMX , JMX ) )
   ALLOCATE ( SFC_RH      ( IMX , JMX ) )
   ALLOCATE ( SFC_T       ( IMX , JMX ) )
   ALLOCATE ( SFC_U       ( IMX , JMX ) )
   ALLOCATE ( SFC_V       ( IMX , JMX ) )
   ALLOCATE ( SNOWCOVR    ( IMX , JMX ) )
   ALLOCATE ( TERRAIN     ( IMX , JMX ) )
   ALLOCATE ( TSEASFC     ( IMX , JMX ) )

   ALLOCATE ( KSAFEC      ( IMX , JMX ) )
   ALLOCATE ( KSAFED      ( IMX , JMX ) )
   ALLOCATE ( PS0         ( IMX , JMX ) )
   ALLOCATE ( PS0CSMALL   ( IMX , JMX ) )
   ALLOCATE ( PS0DOT      ( IMX , JMX ) )
   ALLOCATE ( PS0DSMALL   ( IMX , JMX ) )
   ALLOCATE ( PSCSMALL    ( IMX , JMX ) )
   ALLOCATE ( PSDSMALL    ( IMX , JMX ) )
   ALLOCATE ( SFC_THETA   ( IMX , JMX ) )
   ALLOCATE ( TERRDOT     ( IMX , JMX ) )
   ALLOCATE ( W_TOP       ( IMX , JMX ) )

   ALLOCATE ( PP_SIG      ( IMX , JMX , KXS ) )
   ALLOCATE ( PR0         ( IMX , JMX , KXS ) )
   ALLOCATE ( PSCLIN      ( IMX , JMX , KXS ) )
   ALLOCATE ( PSCLOG      ( IMX , JMX , KXS ) )
   ALLOCATE ( PSDLIN      ( IMX , JMX , KXS ) )
   ALLOCATE ( PSDLOG      ( IMX , JMX , KXS ) )
   ALLOCATE ( PTRAPCRS    ( IMX , JMX , KXS ) )
   ALLOCATE ( PTRAPDOT    ( IMX , JMX , KXS ) )
   ALLOCATE ( Q_SIG       ( IMX , JMX , KXS ) )
   ALLOCATE ( RH_SIG      ( IMX , JMX , KXS ) )
   ALLOCATE ( RHO0        ( IMX , JMX , KXS ) )
   ALLOCATE ( T0          ( IMX , JMX , KXS ) )
   ALLOCATE ( T_SIG       ( IMX , JMX , KXS ) )
   ALLOCATE ( THE_SIG     ( IMX , JMX , KXS ) )
   ALLOCATE ( TV_SIG      ( IMX , JMX , KXS ) )
   ALLOCATE ( U_SIG       ( IMX , JMX , KXS ) )
   ALLOCATE ( V_SIG       ( IMX , JMX , KXS ) )
   ALLOCATE ( W_SIG       ( IMX , JMX , KXS ) )

   ALLOCATE ( PPEB3D      ( IMX , KXS , NSPGD ) )
   ALLOCATE ( PPNB3D      ( JMX , KXS , NSPGD ) )
   ALLOCATE ( PPSB3D      ( JMX , KXS , NSPGD ) )
   ALLOCATE ( PPWB3D      ( IMX , KXS , NSPGD ) )
   ALLOCATE ( QEB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( QNB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( QSB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( QWB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( TEB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( TNB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( TSB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( TWB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( UEB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( UNB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( USB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( UWB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( VEB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( VNB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( VSB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( VWB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( WEB2D       ( IMX , KXS , 1     ) )
   ALLOCATE ( WEB3D       ( IMX , KXS , NSPGD ) )
   ALLOCATE ( WNB2D       ( JMX , KXS , 1     ) )
   ALLOCATE ( WNB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( WSB2D       ( JMX , KXS , 1     ) )
   ALLOCATE ( WSB3D       ( JMX , KXS , NSPGD ) )
   ALLOCATE ( WWB2D       ( IMX , KXS , 1     ) )
   ALLOCATE ( WWB3D       ( IMX , KXS , NSPGD ) )

   !  Loop over all of the time periods in the pressure level input
   !  data.  We only pick the times that match the requested values 
   !  from the NAMELIST file.  The number of vertical pressure levels
   !  is permitted to change between times, so the ALLOCATION of 3D
   !  pressure data occurs when we know how big the vertical component
   !  will be.
 
   ALLOCATE ( allp_3d( 20) )
   ALLOCATE ( alls_3d( 20) )
   ALLOCATE ( allm_3d(  6) )
   ALLOCATE ( all_2d (100) )
   ALLOCATE ( all_1d ( 20) )

!Wei
!  Need to temporarily close input file unit
   print *, 'temporarily close file with unit ', input_press
   CLOSE (input_press)
   print *, 'input_file ', input_file(1), imx,jmx
!KWM_START
   ALLOCATE ( avgsfct     ( IMX , JMX ) )
   if ( ( psfc_method .eq. 1 ) .and. ( .not. less_than_24h ) ) then
      CALL average_surface_T_over_time(input_file, imx, jmx, kxs, avgsfct)
   endif
!KWM_END
!Wei
!  After calculating average surface temp, reopen file
   print *, 'Re-opening input file'
   input_count = 1
   CALL get_fg_file ( input_file(input_count) , input_press )
   CALL read_bh ( input_press )
!Wei_End

   itimes = 0
   current_date = start_date
   time_loop : DO 
 
      !  READ the data for this time.  This is a loop over an unknown
      !  number of variables.  We READ flags, headers and data until we
      !  find the flag that signifies the end of the time.  The data for
      !  the allp_3d, all_2d and all_1d is just the input data from the
      !  pressure level data set.  This is available from USE association
      !  of the input_data module.

      DO loop = 1 , 20
         NULLIFY(allp_3d(loop)%array)
         NULLIFY(alls_3d(loop)%array)
         NULLIFY(all_1d(loop)%array)
      END DO
      DO loop = 1 , 100
         NULLIFY(all_2d(loop)%array)
      END DO

      bhi(1,1) = index

      CALL read_data ( input_press , input_file , input_count , kx , daily_mean_sst , &
                       cloud_and_rain , snow_and_ice , graupel , number_concentration )

      !  What was the date in the small header for this time period?

      test_date = sh_date

      !  Is this one of the times that we requested?

      IF ( test_date .LT. current_date ) THEN
         PRINT '(A,A,A)','Skipping ',test_date,'.  Reading another time.'
    
         !  We need to deallocate the following fields since they are pressure-level
         !  based, and we are allowed to change the number of pressure levels between
         !  time periods.
   
         DO loop = 1 , 20
            IF ( ASSOCIATED(allp_3d(loop)%array) ) THEN
               DEALLOCATE ( allp_3d(loop)%array )
            END IF
            IF ( ASSOCIATED(alls_3d(loop)%array) ) THEN
               DEALLOCATE ( alls_3d(loop)%array )
            END IF
            IF ( ASSOCIATED(all_1d(loop)%array ) ) THEN
               DEALLOCATE ( all_1d(loop)%array )
            END IF
         END DO
         DO loop = 1 , 100
            IF ( ASSOCIATED ( all_2d(loop)%array ) ) THEN
               DEALLOCATE ( all_2d(loop)%array )
            END IF
         END DO
         CYCLE time_loop
      ELSE IF ( test_date .GT. current_date ) THEN
         PRINT '(A)','Seemed to have gone past the right time.'
         STOP 'past_right_time'
      ELSE 
         PRINT '(A,A,A)','Will process time ',test_date,'.'
      END IF

      !  We need to allocate the  space for the pressure level input.

      ALLOCATE ( Q           ( IMX , JMX , KX ) )
      ALLOCATE ( HEIGHT      ( IMX , JMX , KX ) )
      ALLOCATE ( P           ( KX ) )
      ALLOCATE ( RH          ( IMX , JMX , KX ) )
      ALLOCATE ( T           ( IMX , JMX , KX ) )
      ALLOCATE ( U           ( IMX , JMX , KX ) )
      ALLOCATE ( V           ( IMX , JMX , KX ) )
      ALLOCATE ( PCORIG      ( IMX , JMX , KX ) )
      ALLOCATE ( PPCLIN      ( IMX , JMX , KX + 1 ) )
      ALLOCATE ( PPCLOG      ( IMX , JMX , KX + 1 ) )
      ALLOCATE ( PPDLIN      ( IMX , JMX , KX + 1 ) )
      ALLOCATE ( PPDLOG      ( IMX , JMX , KX + 1 ) )
      ALLOCATE ( THETAP      ( IMX , JMX , KX ) )
      ALLOCATE ( PUP (KX) )

      !  Put all of the input data into the mnemonically obvious choices from
      !  the large arrays of all of the 3d, 2d and 1d data.
     
      CALL assign_data ( t, u, v, height, rh, &
                         terrain, land_use, mapfaccr, mapfacdt, coriolis, &
                         latitcrs, longicrs, latitdot, longidot, snowcovr, &
                         psealvld, psealvlc, tseasfc, &
                         sfc_rh, sfc_t, sfc_u, sfc_v , &
                         p , kx , daily_mean_sst )

      !  Since this is one of the time periods that we have requested, bump the counter
      !  that tells us howm many of these time periods we actually have under our belt.

      itimes = itimes + 1

      !  Allocate space for the pressure level moisture fields (if we have any of them).

      IF ( cloud_and_rain ) THEN
         ALLOCATE ( qc     ( imx , jmx , kx ) )
         ALLOCATE ( qr     ( imx , jmx , kx ) )
         ALLOCATE ( sfc_qc ( imx , jmx      ) )
         ALLOCATE ( sfc_qr ( imx , jmx      ) )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice ) THEN
         ALLOCATE ( qs     ( imx , jmx , kx ) )
         ALLOCATE ( qi     ( imx , jmx , kx ) )
         ALLOCATE ( sfc_qs ( imx , jmx      ) )
         ALLOCATE ( sfc_qi ( imx , jmx      ) )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
         ALLOCATE ( qg     ( imx , jmx , kx ) )
         ALLOCATE ( sfc_qg ( imx , jmx      ) )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
         ALLOCATE ( qn     ( imx , jmx , kx ) )
         ALLOCATE ( sfc_qn ( imx , jmx      ) )
      END IF

      !  During the first time period, we get the space for the sigma level data and the lateral
      !  boundary arrays.

      IF ( itimes .EQ. 1 ) THEN

         IF ( cloud_and_rain ) THEN
            ALLOCATE ( qc_sig          ( imx , jmx , kxs   ) )
            ALLOCATE ( allm_3d(1)%array( imx , jmx , kxs   ) ) 
            ALLOCATE ( allm_3d(1)%ebdy ( imx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(1)%nbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(1)%sbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(1)%wbdy ( imx , kxs , nspgd ) )
            ALLOCATE ( qr_sig          ( imx , jmx , kxs   ) )
            ALLOCATE ( allm_3d(2)%array( imx , jmx , kxs   ) ) 
            ALLOCATE ( allm_3d(2)%ebdy ( imx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(2)%nbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(2)%sbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(2)%wbdy ( imx , kxs , nspgd ) )
         END IF
   
         IF ( cloud_and_rain .AND. snow_and_ice ) THEN
            ALLOCATE ( qs_sig          ( imx , jmx , kxs   ) )
            ALLOCATE ( allm_3d(3)%array( imx , jmx , kxs   ) ) 
            ALLOCATE ( allm_3d(3)%ebdy ( imx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(3)%nbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(3)%sbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(3)%wbdy ( imx , kxs , nspgd ) )
            ALLOCATE ( qi_sig          ( imx , jmx , kxs   ) )
            ALLOCATE ( allm_3d(4)%array( imx , jmx , kxs   ) ) 
            ALLOCATE ( allm_3d(4)%ebdy ( imx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(4)%nbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(4)%sbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(4)%wbdy ( imx , kxs , nspgd ) )
         END IF
   
         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
            ALLOCATE ( qg_sig          ( imx , jmx , kxs   ) )
            ALLOCATE ( allm_3d(5)%array( imx , jmx , kxs   ) ) 
            ALLOCATE ( allm_3d(5)%ebdy ( imx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(5)%nbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(5)%sbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(5)%wbdy ( imx , kxs , nspgd ) )
         END IF
   
         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
            ALLOCATE ( qn_sig          ( imx , jmx , kxs   ) )
            ALLOCATE ( allm_3d(6)%array( imx , jmx , kxs   ) ) 
            ALLOCATE ( allm_3d(6)%ebdy ( imx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(6)%nbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(6)%sbdy ( jmx , kxs , nspgd ) )
            ALLOCATE ( allm_3d(6)%wbdy ( imx , kxs , nspgd ) )
         END IF

      END IF
 
      !  Assign the moisture data.

      IF ( cloud_and_rain ) THEN
         CALL assign_cloud_and_rain ( qc , qr , sfc_qc , sfc_qr , kx )
      END IF
      IF ( cloud_and_rain .AND. snow_and_ice ) THEN
         CALL assign_snow_and_ice ( qs , qi , sfc_qs , sfc_qi , kx )
      END IF
      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
         CALL assign_graupel ( qg , sfc_qg , kx )
      END IF
      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
         CALL assign_number_concentration ( qn , sfc_qn , kx )
      END IF

      bhr(2,2) = ptop

      !  Invert pressure array to correspond to inverted 3d arrays.

      DO k = 1, kx
         pup(k) = p(kx + 1 - k)
      END DO

      PRINT '(A,I3,A,I3,A)','Data read for grid #',bhi(13,1),', time period:',itimes,'.'

      !  During the first trip in, we set all sorts of things up for the sigma level
      !  data values - thickness, full levels, upside down things.  All of these are
      !  allocated once.

      IF ( itimes .EQ. 1 ) THEN

         ALLOCATE ( SIGH       ( KXS ) )
         ALLOCATE ( SIGHUP     ( KXS ) )
         ALLOCATE ( VCOORD     ( KXS + 1 ) )
         ALLOCATE ( A          ( KXS ) )
         ALLOCATE ( DSIGMA     ( KXS ) )
   
         !  Compute half sigma layer values from input full sigma values.

         DO lp = 1, kxs
            sigh(lp) = (newcoord(lp) + newcoord(lp+1)) * 0.5
         END DO

         !  Invert half sigma layer value array.

         DO k = 1, kxs
            sighup(k) = sigh(kxs + 1 - k)
         END DO

         !  Invert full sigma level value and store in vcoord array.

         DO k = 1, kxs + 1
            vcoord(k) = newcoord(kxs + 2 - k)
         END DO

         !  Compute thickness of sigma layers for weighted averages.

         DO k = 1, kxs
            dsigma(k) = vcoord(k+1) - vcoord(k)
         END DO

      ENDIF

      !  Make a 3d pressure array for later use

      DO k = 1, kx
         pcorig(:,:,k) = pup(k)
      END DO

      PRINT '(A)','Pressure and sigma arrays constructed.'

      !  Compute mixing ratio on pressure surfaces (q on p).

      CALL mxratprs (rh, t, pup, imxm, jmxm, kx, WRTH2O , q)

      !  Fill last row and column.

      q(:,jmx,:) = q(:,jmxm,:)
      q(imx,:,:) = q(imxm,:,:)

      PRINT '(A)','Mixing ratio computed on pressure.'

      !  Compute the surface pressure.
!KWM REMOVED      CALL sfcprs ( t, q, height, psealvlc, terrain, pup, imx, jmx, kx, sfc_p )
!KWM START
      CALL sfcprs ( t, q, height, psealvlc, terrain, avgsfct, pup, imx, jmx, kx, sfc_p , psfc_method , &
           less_than_24h )
!KWM END

      PRINT '(A)','Surface pressure field computed.'

      !  Compute hydrostatic pstar on cross and dot points.

      pstarcrs = sfc_p - ptop

      pstardot = pstarcrs
      CALL crs2dot (pstardot, imx, jmx)

      PRINT '(A)','Pstar computed on cross and dot points.'

      !  Vertically interpolate a few fields, either linear in 
      !  pressure, or linear in ln pressure.  The only fields interpolated 
      !  are horizontal components of wind, theta (not T) and RH (not Q).

      !  Signal that no interpolation has yet been done.  This computes
      !  a few pressure sorts of fields in the intdrvr routine.

      ialready = 0

      !  U-wind.

      CALL intdrvr (u, sfc_u, sfc_p, imx, jmx, kx, 0, 0,  &
                    iusesfc, ialready, 1, sighup, pstarcrs, &
                    ptop, pup, u_sig, kxs, imxm, jmxm, &
                    ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                    psdlin, psdlog, psclin, psclog, ptrapdot, &
                    ptrapcrs, pcorig, 0, isfc)

      PRINT '(A)','U wind interpolated to sigma.'

      !  V-wind.

      CALL intdrvr (v, sfc_v, sfc_p, imx, jmx, kx, 0, 0,  &
                    iusesfc, ialready, 1, sighup, pstarcrs, &
                    ptop, pup, v_sig, kxs, imxm, jmxm, &
                    ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                    psdlin, psdlog, psclin, psclog, ptrapdot, &
                    ptrapcrs, pcorig, 1, isfc)

      PRINT '(A)','V wind interpolated to sigma.'

      !  Potential temperature is vertically interpolated since it is more
      !  monotonically behaved in the vertical than is temperature.

      thetap    =     t * (100000. / pcorig) ** rovcp
      sfc_theta = sfc_t * (100000. / sfc_p)  ** rovcp

      CALL intdrvr (thetap, sfc_theta, sfc_p, imx, jmx,  kx, 1, 1,  &
                    iusesfc, ialready, 2, sighup,  pstarcrs,  &
                    ptop, pup, the_sig, kxs, imxm, jmxm,  &
                    ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog,  &
                    psdlin, psdlog, psclin, psclog, ptrapdot,  &
                    ptrapcrs, pcorig, 0, isfc)

      t_sig = the_sig   * (psclin / 100000.) ** rovcp

      PRINT '(A)','Temperature interpolated to sigma.'

      !  Build ground temperature based on the water vs land index.  Where
      !  the land use array is equal to the defined water index, the ground
      !  temperature is assigned the sea surface temperature.  Where the 
      !  land use is not water, the surface temperaute is assigned to the
      !  ground temperature.  If a request was made to NOT use the surface
      !  data (from the namelist.input file), we then must use a created
      !  surface temperature field.

      IF ( usesfc ) THEN
         ground_t = MERGE ( tseasfc , sfc_t          , NINT(land_use) .EQ. bhi(23,1) )
      ELSE
         ground_t = MERGE ( tseasfc , t_sig(:,:,kxs) , NINT(land_use) .EQ. bhi(23,1) )
      END IF
          
      !  Interpolate RH, then diagnose Q.

      CALL intdrvr (rh, sfc_rh, sfc_p, imx, jmx, kx, 1, 1, &
                    iusesfc, ialready, 1, sighup, pstarcrs, &
                    ptop, pup, rh_sig, kxs, imxm, jmxm, &
                    ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                    psdlin, psdlog, psclin, psclog, ptrapdot, &
                    ptrapcrs, pcorig, 1, isfc)

      CALL mxratsig (rh_sig, t_sig, sighup, pstarcrs, ptop, imxm, jmxm, kxs, wrth2o , q_sig)

      PRINT '(A)','Mixing ratio computed on sigma.'
          
      !  Interpolate the possibly available additional moisture fields.

      IF ( cloud_and_rain ) THEN

         CALL intdrvr (qc, sfc_qc, sfc_p, imx, jmx, kx, 1, 1, &
                       iusesfc, ialready, 1, sighup, pstarcrs, &
                       ptop, pup, qc_sig, kxs, imxm, jmxm, &
                       ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                       psdlin, psdlog, psclin, psclog, ptrapdot, &
                       ptrapcrs, pcorig, 1, isfc)

         CALL intdrvr (qr, sfc_qr, sfc_p, imx, jmx, kx, 1, 1, &
                       iusesfc, ialready, 1, sighup, pstarcrs, &
                       ptop, pup, qr_sig, kxs, imxm, jmxm, &
                       ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                       psdlin, psdlog, psclin, psclog, ptrapdot, &
                       ptrapcrs, pcorig, 1, isfc)

         CALL clean_rh ( qc_sig , imx , jmx , kxs , 0.0 , 40.0E-3 )
         CALL clean_rh ( qr_sig , imx , jmx , kxs , 0.0 , 40.0E-3 )

         allm_3d(1)%array = qc_sig
         allm_3d(2)%array = qr_sig
         allm_3d(1)%small_header = allp_3d(index_qc)%small_header
         allm_3d(2)%small_header = allp_3d(index_qr)%small_header

      END IF

      IF ( cloud_and_rain .AND. snow_and_ice ) THEN

         CALL intdrvr (qs, sfc_qs, sfc_p, imx, jmx, kx, 1, 1, &
                       iusesfc, ialready, 1, sighup, pstarcrs, &
                       ptop, pup, qs_sig, kxs, imxm, jmxm, &
                       ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                       psdlin, psdlog, psclin, psclog, ptrapdot, &
                       ptrapcrs, pcorig, 1, isfc)

         CALL intdrvr (qi, sfc_qi, sfc_p, imx, jmx, kx, 1, 1, &
                       iusesfc, ialready, 1, sighup, pstarcrs, &
                       ptop, pup, qi_sig, kxs, imxm, jmxm, &
                       ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                       psdlin, psdlog, psclin, psclog, ptrapdot, &
                       ptrapcrs, pcorig, 1, isfc)

         CALL clean_rh ( qs_sig , imx , jmx , kxs , 0.0 , 40.0E-3 )
         CALL clean_rh ( qi_sig , imx , jmx , kxs , 0.0 , 40.0E-3 )

         allm_3d(3)%array = qs_sig
         allm_3d(4)%array = qi_sig
         allm_3d(3)%small_header = allp_3d(index_qs)%small_header
         allm_3d(4)%small_header = allp_3d(index_qi)%small_header

      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN

         CALL intdrvr (qg, sfc_qg, sfc_p, imx, jmx, kx, 1, 1, &
                       iusesfc, ialready, 1, sighup, pstarcrs, &
                       ptop, pup, qg_sig, kxs, imxm, jmxm, &
                       ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                       psdlin, psdlog, psclin, psclog, ptrapdot, &
                       ptrapcrs, pcorig, 1, isfc)

         CALL clean_rh ( qg_sig , imx , jmx , kxs , 0.0 , 40.0E-3 )

         allm_3d(5)%array = qg_sig
         allm_3d(5)%small_header = allp_3d(index_qg)%small_header

      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN

         CALL intdrvr (qn, sfc_qn, sfc_p, imx, jmx, kx, 1, 1, &
                       iusesfc, ialready, 1, sighup, pstarcrs, &
                       ptop, pup, qn_sig, kxs, imxm, jmxm, &
                       ksafec, ksafed, ppdlin, ppdlog, ppclin, ppclog, &
                       psdlin, psdlog, psclin, psclog, ptrapdot, &
                       ptrapcrs, pcorig, 1, isfc)

         CALL clean_rh ( qn_sig , imx , jmx , kxs , 0.0 , 1.0E20 )

         allm_3d(6)%array = qn_sig
         allm_3d(6)%small_header = allp_3d(index_qn)%small_header

      END IF

      !  Remove the integrated mean divergence from each column.  

      IF ( removediv ) THEN
         CALL meandiv (u_sig, v_sig, pstardot, mapfacdt, sighup, imx, jmx, kxs, ds, imxm, jmxm)
         PRINT '(A)','Integrated mean divergent wind removed.'
      ELSE
         PRINT '(A)','Integrated mean divergent wind NOT removed.'
      END IF

      !  Nonhydrostatic part.  First compute the reference/base state.

      pscsmall = pstarcrs
      psdsmall = pstardot
  
      CALL nhbase (terrain, imx, jmx, kxs, ps0, pr0, t0, rho0, sighup, ptop, pt, p0, tlp, ts0, tiso)

      PRINT '(A)','Non-hydrostatic base state computed.'

      t_sig(imx,:,:) = t_sig(imx-1,:,:)
      t_sig(:,jmx,:) = t_sig(:,jmx-1,:)
      q_sig(imx,:,:) = q_sig(imx-1,:,:)
      q_sig(:,jmx,:) = q_sig(:,jmx-1,:)
      tv_sig = t_sig * (1. + 0.608 * q_sig)

      !  Compute nonhydrostatic vertical velocity (w) on full sigma levels.

      CALL nhw (u_sig, v_sig, tv_sig, rho0, pscsmall, psdsmall, &
                ps0, mapfaccr, terrain, vcoord, imx, jmx, kxs, &
                w_sig, w_top, ds, sighup, dsigma, ptop, pt, p0, &
                tlp, ts0, tiso)

      PRINT '(A)','Vertical velocity computed on full sigma levels.'

      !  Put terrain and ps0 on dot points.

      terrdot = terrain
      CALL crs2dot (terrdot, imx, jmx )
      ps0dot = ps0
      CALL crs2dot (ps0dot, imx, jmx )

      !  Interpolate nonhydrostatic u, v, t, and q

      CALL nhinterp (u_sig, tv_sig, terrdot, psdsmall, ps0dot, vcoord, 1, imx, jmx, kxs, 0,  &
                     sighup, ptop, pt, p0, tlp, ts0, tiso)

      PRINT '(A)','Non-hydrostatic interp of u complete.'

      CALL nhinterp (v_sig, tv_sig, terrdot, psdsmall, ps0dot, vcoord, 1, imx, jmx, kxs, 0, &
                     sighup, ptop, pt, p0, tlp, ts0, tiso)

      PRINT '(A)','Non-hydrostatic interp of v complete.'

      CALL nhinterp (t_sig, tv_sig, terrain, pscsmall, ps0, vcoord, 1, imx, jmx, kxs, 1, &
                     sighup, ptop, pt, p0, tlp, ts0, tiso)

      PRINT '(A)','Non-hydrostatic interp of temperature complete.'

      CALL nhinterp (q_sig, tv_sig, terrain, pscsmall, ps0, vcoord, 2, imx, jmx, kxs, 1, &
                     sighup, ptop, pt, p0, tlp, ts0, tiso)

      PRINT '(A)','Non-hydrostatic interp of mixing ratio complete.'

      !  Interpolate the additional moisture fields.

      IF ( cloud_and_rain ) THEN
         CALL nhinterp (allm_3d(1)%array, tv_sig, terrain, pscsmall, ps0, vcoord, 2, imx, jmx, kxs, 1,  &
                        sighup, ptop, pt, p0, tlp, ts0, tiso)

         PRINT '(A)','Non-hydrostatic interp of qc complete.'

         CALL nhinterp (allm_3d(2)%array, tv_sig, terrain, pscsmall, ps0, vcoord, 2, imx, jmx, kxs, 1,  &
                        sighup, ptop, pt, p0, tlp, ts0, tiso)

         PRINT '(A)','Non-hydrostatic interp of qr complete.'

         CALL clean_rh ( allm_3d(1)%array , imx , jmx , kxs , 0.0 , 40.0E-3 )
         CALL clean_rh ( allm_3d(2)%array , imx , jmx , kxs , 0.0 , 40.0E-3 )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice ) THEN
         CALL nhinterp (allm_3d(3)%array, tv_sig, terrain, pscsmall, ps0, vcoord, 2, imx, jmx, kxs, 1,  &
                        sighup, ptop, pt, p0, tlp, ts0, tiso)

         PRINT '(A)','Non-hydrostatic interp of qs complete.'

         CALL nhinterp (allm_3d(4)%array, tv_sig, terrain, pscsmall, ps0, vcoord, 2, imx, jmx, kxs, 1,  &
                        sighup, ptop, pt, p0, tlp, ts0, tiso)

         PRINT '(A)','Non-hydrostatic interp of qi complete.'

         CALL clean_rh ( allm_3d(3)%array , imx , jmx , kxs , 0.0 , 40.0E-3 )
         CALL clean_rh ( allm_3d(4)%array , imx , jmx , kxs , 0.0 , 40.0E-3 )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
         CALL nhinterp (allm_3d(5)%array, tv_sig, terrain, pscsmall, ps0, vcoord, 2, imx, jmx, kxs, 1,  &
                        sighup, ptop, pt, p0, tlp, ts0, tiso)

         PRINT '(A)','Non-hydrostatic interp of qg complete.'

         CALL clean_rh ( allm_3d(5)%array , imx , jmx , kxs , 0.0 , 40.0E-3 )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
         CALL nhinterp (allm_3d(6)%array, tv_sig, terrain, pscsmall, ps0, vcoord, 2, imx, jmx, kxs, 1,  &
                        sighup, ptop, pt, p0, tlp, ts0, tiso)

         PRINT '(A)','Non-hydrostatic interp of qn complete.'

         CALL clean_rh ( allm_3d(6)%array , imx , jmx , kxs , 0.0 , 1.0E20 )
      END IF

      !  Recompute virtual temperature on non hydrostatic sigma.

      tv_sig = t_sig * (1. + 0.608 * q_sig)

      PRINT '(A)','Virtual temp recomputed on non-hydrostatic sigma.'

      !   Compute the nonhydrostatic perturbation pressure field (pp).

      CALL nhpp (t_sig, pr0, t0, tv_sig, pstarcrs, ps0, vcoord, imx, jmx, kxs, pt, pp_sig)

      PRINT '(A)','Non-hydrostatic pressure perturbation computed.'

      !  The date in the ground_t_sh data structure is used in several places.
      !  It is set in the outmodel routine, but that does not have to be called
      !  for every time period.  This is some bullet-proofing.

      ground_t_sh%current_date = current_date // '.0000'

      !  We're done for this time period: output initial conditions, possibly for only  
      !  the initial time period.  Hey, if no FDDA is taking place, why output the IC?

      IF ( ( itimes .LE. ifdatim ) .OR. ( ifdatim .EQ. -1 ) ) THEN

         CALL outmodel (current_date,imx,jmx,kxs,output_mminput,itimes,interval, &
                        start_year, start_month, start_day, start_hour, &
                        start_minute, start_second, start_frac, &
                        p0, tlp, ts0, tiso, &
                        sighup, &
                        ps0, ground_t , ground_t_sh , &
                        pp_sig, q_sig, t_sig, u_sig, v_sig, w_sig, w_top, &
                        cloud_and_rain , snow_and_ice , graupel , number_concentration )

          PRINT '(A,A,A,I1,A)','Initial conditions written for ',current_date, &
                               ' on MMINPUT_DOMAIN',bhi(13,1),'.'
      ENDIF

      !  Build the lower boundary condition file for the model.  This file will
      !  have a summation of fields, and the daily mean of each field will be
      !  output to the final lower boundary condition file.  If the surface data
      !  is not to be used, we substitute the lowest level sigma temperature data
      !  for the surface temperature.

      IF ( usesfc ) THEN
         CALL add_lbc ( current_date , tseasfc , sfc_t          , tseasfc_sh ,         &
                        sfc_t_sh , ground_t_sh , output_lowerbc , imx , jmx , itimes , & 
                        interval , less_than_24h , num_lowbdy_fields , iprocess , daily_mean_sst )
      ELSE
         CALL add_lbc ( current_date , tseasfc , t_sig(:,:,kxs) , tseasfc_sh ,         &
                        sfc_t_sh , ground_t_sh , output_lowerbc , imx , jmx , itimes , & 
                        interval , less_than_24h , num_lowbdy_fields , iprocess , daily_mean_sst )
      END IF

      !  The 3d fields coming out of the boundary have to be coupled.
 
      DO k = 1 , kxs
          u_sig(:,:,k)           =  u_sig(:,:,k)           * ps0dot             * 0.001
          v_sig(:,:,k)           =  v_sig(:,:,k)           * ps0dot             * 0.001
          t_sig(:imx-1,:jmx-1,k) =  t_sig(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
          q_sig(:imx-1,:jmx-1,k) =  q_sig(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
          w_sig(:imx-1,:jmx-1,k) =  w_sig(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
         pp_sig(:imx-1,:jmx-1,k) = pp_sig(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
         IF ( cloud_and_rain ) THEN
            allm_3d(1)%array(:imx-1,:jmx-1,k) =  allm_3d(1)%array(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
            allm_3d(2)%array(:imx-1,:jmx-1,k) =  allm_3d(2)%array(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
         END IF
         IF ( cloud_and_rain .AND. snow_and_ice ) THEN
            allm_3d(3)%array(:imx-1,:jmx-1,k) =  allm_3d(3)%array(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
            allm_3d(4)%array(:imx-1,:jmx-1,k) =  allm_3d(4)%array(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
         END IF
         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
            allm_3d(5)%array(:imx-1,:jmx-1,k) =  allm_3d(5)%array(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
         END IF
         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
            allm_3d(6)%array(:imx-1,:jmx-1,k) =  allm_3d(6)%array(:imx-1,:jmx-1,k) * ps0(:imx-1,:jmx-1) * 0.001
         END IF
      END DO
      w_top(:imx-1,:jmx-1) = w_top(:imx-1,:jmx-1) * ps0(:imx-1,:jmx-1) * 0.001

      !  Compute the lateral boundary conditions and tendencies.  

      CALL bdyout (output_bdyout , imx, jmx, kxs, itimes, interval , &
                   start_year , start_month , start_day , start_hour , &
                   start_minute , start_second , start_frac , &
                   w_top,  web2d,  wnb2d,  wsb2d,  wwb2d, &
                  pp_sig, ppeb3d, ppnb3d, ppsb3d, ppwb3d, &
                   q_sig,  qeb3d,  qnb3d,  qsb3d,  qwb3d, &
                   t_sig,  teb3d,  tnb3d,  tsb3d,  twb3d, &
                   u_sig,  ueb3d,  unb3d,  usb3d,  uwb3d, &
                   v_sig,  veb3d,  vnb3d,  vsb3d,  vwb3d, &
                   w_sig,  web3d,  wnb3d,  wsb3d,  wwb3d, &
                   cloud_and_rain , snow_and_ice , graupel , number_concentration ) 
   
      !  Boundary conditions are valid through the following times.

      IF ( itimes .GT. 1 ) THEN
         CALL geth_newdate ( old_date , current_date , -1* interval )
         PRINT '(A,A,A,A,A)','Lateral boundary conditions valid from ',old_date,' through ',current_date,'.'
      END IF

      !  Go back and process the next time until finished with all requested time periods.

      PRINT '(A)','-----------------------'
      PRINT *,'FINISHED TIME PERIOD:',ITIMES
      PRINT '(A)','-----------------------'
 
      !  We need to deallocate the following fields since they are pressure-level
      !  based, and we are allowed to change the number of pressure levels between
      !  time periods.

      DO loop = 1 , 20
         IF ( ASSOCIATED(allp_3d(loop)%array) ) THEN
            DEALLOCATE ( allp_3d(loop)%array )
         END IF
         IF ( ASSOCIATED(all_1d(loop)%array ) ) THEN
            DEALLOCATE ( all_1d(loop)%array )
         END IF
      END DO
      DO loop = 1 , 100
         IF ( ASSOCIATED ( all_2d(loop)%array ) ) THEN
            DEALLOCATE ( all_2d(loop)%array )
         END IF
      END DO
      DEALLOCATE ( Q )
      DEALLOCATE ( HEIGHT )
      DEALLOCATE ( RH )
      DEALLOCATE ( P )
      DEALLOCATE ( T )
      DEALLOCATE ( U )
      DEALLOCATE ( V )
      DEALLOCATE ( PCORIG )
      DEALLOCATE ( PPCLIN )
      DEALLOCATE ( PPCLOG ) 
      DEALLOCATE ( PPDLIN )
      DEALLOCATE ( PPDLOG ) 
      DEALLOCATE ( THETAP )
      DEALLOCATE (PUP)

      IF ( cloud_and_rain ) THEN
         DEALLOCATE ( qc )
         DEALLOCATE ( qr )
         DEALLOCATE ( sfc_qc )
         DEALLOCATE ( sfc_qr )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice ) THEN
         DEALLOCATE ( qs )
         DEALLOCATE ( qi )
         DEALLOCATE ( sfc_qs )
         DEALLOCATE ( sfc_qi )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
         DEALLOCATE ( qg )
         DEALLOCATE ( sfc_qg )
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
         DEALLOCATE ( qn )
         DEALLOCATE ( sfc_qn )
      END IF

      !  What we really need is a brand new time for our processing.  Given the
      !  "current" date and the interval specified in the namelist, we now know
      !  the "new" time that this next loop will process.

      CALL geth_newdate ( new_date , current_date , interval ) 
      current_date = new_date
           
      !  Of course, we may not want to process the "new" time if it is beyond the
      !  range of the valid times that were specified in the namelist.

      IF ( current_date .gt. end_date ) THEN
         EXIT time_loop
      END IF

   END DO time_loop

   !  After all of the time periods have been processed, we need to do the summing
   !  and averaging for the lower boundary condition file.

   CALL sum_lbc ( output_lowerbc , itimes , interval , less_than_24h , &
                  start_year , start_month , start_day , start_hour , &
                  start_minute , start_second , start_frac , num_lowbdy_fields , &
                  imx , jmx , daily_mean_sst )

   !  We are almost finished.  Just a few mop up activities.  Clean up our heap.

   DEALLOCATE ( GROUND_T )
   DEALLOCATE ( PSTARCRS )
   DEALLOCATE ( PSTARDOT )
   DEALLOCATE ( CORIOLIS )
   DEALLOCATE ( LAND_USE )
   DEALLOCATE ( LATITCRS )
   DEALLOCATE ( LATITDOT )
   DEALLOCATE ( LONGICRS )
   DEALLOCATE ( LONGIDOT )
   DEALLOCATE ( MAPFACCR )
   DEALLOCATE ( MAPFACDT )
   DEALLOCATE ( PSEALVLC )
   DEALLOCATE ( PSEALVLD )
   DEALLOCATE ( SFC_P )
   DEALLOCATE ( SFC_RH )
   DEALLOCATE ( SFC_T )
   DEALLOCATE ( SFC_U )
   DEALLOCATE ( SFC_V )
   DEALLOCATE ( SNOWCOVR )
   DEALLOCATE ( TERRAIN )
   DEALLOCATE ( TSEASFC )
   DEALLOCATE ( KSAFEC )
   DEALLOCATE ( KSAFED )
   DEALLOCATE ( PP_SIG )
   DEALLOCATE ( PPEB3D )
   DEALLOCATE ( PPNB3D )
   DEALLOCATE ( PPSB3D )
   DEALLOCATE ( PPWB3D )
   DEALLOCATE ( PR0 )
   DEALLOCATE ( PS0 )
   DEALLOCATE ( PS0CSMALL )
   DEALLOCATE ( PS0DOT )
   DEALLOCATE ( PS0DSMALL )
   DEALLOCATE ( PSCLIN )
   DEALLOCATE ( PSCLOG )
   DEALLOCATE ( PSCSMALL )
   DEALLOCATE ( PSDLIN )
   DEALLOCATE ( PSDLOG )
   DEALLOCATE ( PSDSMALL )
   DEALLOCATE ( PTRAPCRS )
   DEALLOCATE ( PTRAPDOT )
   DEALLOCATE ( Q_SIG )
   DEALLOCATE ( QEB3D )
   DEALLOCATE ( QNB3D )
   DEALLOCATE ( QSB3D )
   DEALLOCATE ( QWB3D )
   DEALLOCATE ( RH_SIG )
   DEALLOCATE ( RHO0 )
   DEALLOCATE ( SFC_THETA )
   DEALLOCATE ( T0 )
   DEALLOCATE ( T_SIG )
   DEALLOCATE ( TEB3D )
   DEALLOCATE ( TERRDOT )
   DEALLOCATE ( THE_SIG )
   DEALLOCATE ( TNB3D )
   DEALLOCATE ( TSB3D )
   DEALLOCATE ( TV_SIG )
   DEALLOCATE ( TWB3D )
   DEALLOCATE ( U_SIG )
   DEALLOCATE ( UEB3D )
   DEALLOCATE ( UNB3D )
   DEALLOCATE ( USB3D )
   DEALLOCATE ( UWB3D )
   DEALLOCATE ( V_SIG )
   DEALLOCATE ( VEB3D )
   DEALLOCATE ( VNB3D )
   DEALLOCATE ( VSB3D )
   DEALLOCATE ( VWB3D )
   DEALLOCATE ( W_SIG )
   DEALLOCATE ( W_TOP )
   DEALLOCATE ( WEB2D )
   DEALLOCATE ( WEB3D )
   DEALLOCATE ( WNB2D )
   DEALLOCATE ( WNB3D )
   DEALLOCATE ( WSB2D )
   DEALLOCATE ( WSB3D )
   DEALLOCATE ( WWB2D )
   DEALLOCATE ( WWB3D )
!KWM START
   DEALLOCATE ( avgsfct )
!KWM END
        
   !  Close the output files. 20=MMINPUT_DOMAIN, 30=BDYOUT_DOMAIN, 40=LOWBDY_DOMAIN

   CLOSE ( output_mminput )
   CLOSE ( output_bdyout  )
   CLOSE ( output_lowerbc )

   PRINT '(A)','-------------------------------------------------'
   PRINT '(A,I1)','FINISHED INTERP FOR DOMAIN ID #',bhi(13,1)
   PRINT '(A)','-------------------------------------------------'
   PRINT '(A)','STOP 99999'

END PROGRAM interp
