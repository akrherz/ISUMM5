MODULE lateral_bdy
   
   USE bdy

CONTAINS

   SUBROUTINE bdyout (iunit ,imx, jmx, kxs, itimes, interval , &
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

      USE all_io
      USE date_pack
      USE header_data

      IMPLICIT NONE

      INTEGER :: iunit , start_year , start_month , start_day , start_hour , &
                 start_minute , start_second , start_frac

      LOGICAL :: cloud_and_rain , snow_and_ice , graupel , number_concentration

      !  Data for small header.

      INTEGER                :: num_dims
      INTEGER , DIMENSION(4) :: start_dims
      INTEGER , DIMENSION(4) :: end_dims
      REAL                   :: xtime
      CHARACTER (LEN= 4)     :: staggering
      CHARACTER (LEN= 4)     :: ordering
      CHARACTER (LEN= 9)     :: name
      CHARACTER (LEN=25)     :: units
      CHARACTER (LEN=46)     :: description

      INTEGER :: i , ibdy , imx , inum , interval , itimes , j , jmx , k , kxs , m1 , &
                 m2 , nodiurnl , numdays

      REAL :: dts , t1 , t2 , tbegin , timend

      REAL , DIMENSION(imx,jmx) :: tmin

      REAL , DIMENSION(:,:) :: w_top 

      REAL , DIMENSION( imx,   1, nspgd ) :: et2d , wt2d  

      REAL , DIMENSION( imx, kxs, nspgd ) :: et3d , wt3d

      REAL , DIMENSION( jmx,   1, nspgd ) :: nt2d , st2d

      REAL , DIMENSION( jmx, kxs, nspgd ) :: nt3d , st3d

      REAL , DIMENSION( imx, kxs+1, nspgx ) :: web , wet , wwb , wwt

      REAL , DIMENSION( jmx, kxs+1, nspgx ) :: wnb , wnt , wsb , wst

      REAL , DIMENSION(:,:,:) :: pp_sig , ppeb3d , ppnb3d , ppsb3d , ppwb3d , q_sig , qeb3d , qnb3d , &
                                 qsb3d , qwb3d , t_sig , teb3d , tnb3d , tsb3d , twb3d , u_sig , ueb3d , &
                                 unb3d , usb3d , uwb3d , v_sig , veb3d , vnb3d , vsb3d , vwb3d , w_sig , &
                                 web2d , web3d , wnb2d , wnb3d , wsb2d , wsb3d , wwb2d , wwb3d

      IF (ITIMES .EQ. 1) THEN

         !  Create the 5 rows and columns along the boundary for each of the 3d fields.  This is what we are
         !  going to refer to a "frame".  It is in 4 pieces and contained in the arrays named either YXb3d or 
         !  allm_3d(n)%Xbdy (where Y is a variable name: u, v, etc; X is a compass direction: n, s, e, w).  We 
         !  have the "always" fields (u_sig, v_sig, etc.) and the "sometimes" fields (additional and optional moisture arrays).

         CALL bound (u_sig, ueb3d, uwb3d, unb3d, usb3d, imx, jmx, kxs, 0)
         CALL bound (v_sig, veb3d, vwb3d, vnb3d, vsb3d, imx, jmx, kxs, 0)
         CALL bound (t_sig, teb3d, twb3d, tnb3d, tsb3d, imx, jmx, kxs, 1)
         CALL bound (q_sig, qeb3d, qwb3d, qnb3d, qsb3d, imx, jmx, kxs, 1)
         CALL bound (w_sig, web3d, wwb3d, wnb3d, wsb3d, imx, jmx, kxs, 1)
         CALL bound (w_top, web2d, wwb2d, wnb2d, wsb2d, imx, jmx,   1, 1)
         CALL bound (pp_sig, ppeb3d, ppwb3d, ppnb3d, ppsb3d, imx, jmx, kxs, 1)

         IF ( cloud_and_rain ) THEN
            CALL bound (allm_3d(1)%array, allm_3d(1)%ebdy, allm_3d(1)%wbdy, allm_3d(1)%nbdy, allm_3d(1)%sbdy, imx, jmx, kxs, 1 )
            CALL bound (allm_3d(2)%array, allm_3d(2)%ebdy, allm_3d(2)%wbdy, allm_3d(2)%nbdy, allm_3d(2)%sbdy, imx, jmx, kxs, 1 )
         END IF

         IF ( cloud_and_rain .AND. snow_and_ice ) THEN
            CALL bound (allm_3d(3)%array, allm_3d(3)%ebdy, allm_3d(3)%wbdy, allm_3d(3)%nbdy, allm_3d(3)%sbdy, imx, jmx, kxs, 1 )
            CALL bound (allm_3d(4)%array, allm_3d(4)%ebdy, allm_3d(4)%wbdy, allm_3d(4)%nbdy, allm_3d(4)%sbdy, imx, jmx, kxs, 1 )
         END IF

         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
            CALL bound (allm_3d(5)%array, allm_3d(5)%ebdy, allm_3d(5)%wbdy, allm_3d(5)%nbdy, allm_3d(5)%sbdy, imx, jmx, kxs, 1 )
         END IF

         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
            CALL bound (allm_3d(6)%array, allm_3d(6)%ebdy, allm_3d(6)%wbdy, allm_3d(6)%nbdy, allm_3d(6)%sbdy, imx, jmx, kxs, 1 )
         END IF

         !  The first time in we do the big header jazz.

         WRITE ( iunit ) bh_flag

         !  Just in case no one else cleared the big header, we should do it.  All
         !  of the header values for the lateral boundary condition index can be set to -999.
   
         bhi (:,7) = -999
         bhr (:,7) = -999
         bhic(:,7) = '                                                                                '
         bhrc(:,7) = '                                                                                '
   
         !  Identify the data as the lateral boundary condition for MM5.
   
         bhi( 1,1) = 7 ; bhic( 1,1) = 'Model lateral boundary condition for MM5                                        '
   
         !  A few lines about data format and version numbers.  If we make a change in the
         !  IC INTERPF output version numbers, those will now be reflected here.
   
         bhi( 2,7) = bhi(2,5) ; bhic( 2,7) = bhic(2,5)
         bhi( 3,7) = bhi(3,5) ; bhic( 3,7) = bhic(3,5)
         bhi( 4,7) = bhi(4,5) ; bhic( 4,7) = bhic(4,5)
   
         !  Starting date of the INTERPF data, this was specified in the namelist.
   
         bhi ( 5,7) = start_year
         bhic( 5,7) = 'Four-digit year of start time                                                   '
         bhi ( 6,7) = start_month
         bhic( 6,7) = 'Month of the year of the start time (1-12)                                      '
         bhi ( 7,7) = start_day
         bhic( 7,7) = 'Day of the month of the start time (1-31)                                       '
         bhi ( 8,7) = start_hour
         bhic( 8,7) = 'Hour of the day of the start time (0-23)                                        '
         bhi ( 9,7) = start_minute
         bhic( 9,7) = 'Minute of the start time (0-59)                                                 '
         bhi (10,7) = start_second
         bhic(10,7) = 'Second of the start time (0-59)                                                 '
         bhi (11,7) = start_frac
         bhic(11,7) = 'Ten thousandths of a second of the start time (0-9999)                          '
   
         !  How many levels to expect.
   
         bhi (12,7) = 1
         bhic(12,7) = 'Number of levels in the lower boundary condition file                           '
   
         !  Time interval between output files.  Notice that this is the length of the
         !  entire INTERPF program run, not just a single interval.
   
         bhr ( 1,7) = interval                
         bhrc( 1,7) = 'Time difference (seconds) during which the lateral boundary condition is valid  '
   
         !  Output the big header.
   
         WRITE ( iunit ) bhi , bhr , bhic , bhrc

      ELSE IF (ITIMES .GT. 1) THEN

         CALL geth_newdate(old_date , current_date , -1*interval )
         dts = interval

         !  For itimes > 1, do the following for each variable:

         !  1) Write the frame made last time to disk. This turns out to be 4 separate writes for each variable.

         !  2) Send the frame made last time and the current time's entire field to subroutine bndtend.  This 
         !  subroutine calculates the boundary tendencies and stores them in et2d, wt2d, nt2d, st2d.  Each of these
         !  4 arrays are then output.

         !  3) Call subroutine bound to make a new frame from the current time's field.  The new frame will overwrite 
         !  the old frame's location.

         !  U-component.

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'UEB      '
         units           = 'kPa m/s                  '
         description     = 'U-component of velocity          east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) ueb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'UWB      '
         units           = 'kPa m/s                  '
         description     = 'U-component of velocity          west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) uwb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'UNB      '
         units           = 'kPa m/s                  '
         description     = 'U-component of velocity         north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) unb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'USB      '
         units           = 'kPa m/s                  '
         description     = 'U-component of velocity         south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) usb3d

         CALL bndtend (u_sig, ueb3d, uwb3d, unb3d, usb3d, imx, jmx, kxs, 0, dts, et3d, wt3d, nt3d, st3d)

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'UEBT     '
         units           = 'kPa m/s/s                '
         description     = 'U-component of velocity    tend  east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) et3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'UWBT     '
         units           = 'kPa m/s/s                '
         description     = 'U-component of velocity    tend  west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'UNBT     '
         units           = 'kPa m/s/s                '
         description     = 'U-component of velocity    tend north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) nt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'USBT     '
         units           = 'kPa m/s/s                '
         description     = 'U-component of velocity    tend south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) st3d

         CALL bound (u_sig, ueb3d, uwb3d, unb3d, usb3d, imx, jmx, kxs, 0)

         !  V-component of velocity.

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'VEB      '
         units           = 'kPa m/s                  '
         description     = 'V-component of velocity          east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) veb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'VWB      '
         units           = 'kPa m/s                  '
         description     = 'V-component of velocity          west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) vwb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'VNB      '
         units           = 'kPa m/s                  '
         description     = 'V-component of velocity         north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) vnb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'VSB      '
         units           = 'kPa m/s                  '
         description     = 'V-component of velocity         south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) vsb3d
 
         CALL bndtend (v_sig, veb3d, vwb3d, vnb3d, vsb3d, imx, jmx, kxs, 0, dts, et3d, wt3d, nt3d, st3d)

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'VEBT     '
         units           = 'kPa m/s/s                '
         description     = 'V-component of velocity    tend  east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) et3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'YSB '
         name            = 'VWBT     '
         units           = 'kPa m/s/s                '
         description     = 'V-component of velocity    tend  west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'VNBT     '
         units           = 'kPa m/s/s                '
         description     = 'V-component of velocity    tend north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) nt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgd, 1 /)
         xtime           = 0
         staggering      = 'D   '
         ordering        = 'XSB '
         name            = 'VSBT     '
         units           = 'kPa m/s/s                '
         description     = 'V-component of velocity    tend south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) st3d

         CALL bound (v_sig, veb3d, vwb3d, vnb3d, vsb3d, imx, jmx, kxs, 0)

         !  Temperature.

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'TEB      '
         units           = 'kPa K                    '
         description     = 'Temperature                      east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) teb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'TWB      '
         units           = 'kPa K                    '
         description     = 'Temperature                      west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) twb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'TNB      '
         units           = 'kPa K                    '
         description     = 'Temperature                     north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) tnb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'TSB      '
         units           = 'kPa K                    '
         description     = 'Temperature                     south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) tsb3d
 
         CALL bndtend (t_sig, teb3d, twb3d, tnb3d, tsb3d, imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'TEBT     '
         units           = 'kPa K/s                  '
         description     = 'Temperature                tend  east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) et3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'TWBT     '
         units           = 'kPa K/s                  '
         description     = 'Temperature                tend  west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'TNBT     '
         units           = 'kPa K/s                  '
         description     = 'Temperature                tend north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) nt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'TSBT     '
         units           = 'kPa K/s                  '
         description     = 'Temperature                tend south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) st3d

         CALL bound (t_sig, teb3d, twb3d, tnb3d, tsb3d, imx, jmx, kxs, 1)

         !  Water vapor mixing ratio

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'QEB      '
         units           = 'kPa kg/kg                '
         description     = 'Mixing ratio                     east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) qeb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'QWB      '
         units           = 'kPa kg/kg                '
         description     = 'Mixing ratio                     west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) qwb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'QNB      '
         units           = 'kPa kg/kg                '
         description     = 'Mixing ratio                    north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) qnb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'QSB      '
         units           = 'kPa kg/kg                '
         description     = 'Mixing ratio                    south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) qsb3d
 
         CALL bndtend (q_sig, qeb3d, qwb3d, qnb3d, qsb3d, imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'QEBT     '
         units           = 'kPa kg/kg/s              '
         description     = 'Mixing ratio               tend  east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) et3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'QWBT     '
         units           = 'kPa kg/kg/s              '
         description     = 'Mixing ratio               tend  west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'QNBT     '
         units           = 'kPa kg/kg/s              '
         description     = 'Mixing ratio               tend north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) nt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'QSBT     '
         units           = 'kPa kg/kg/s              '
         description     = 'Mixing ratio               tend south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) st3d

         CALL bound (q_sig, qeb3d, qwb3d, qnb3d, qsb3d, imx, jmx, kxs, 1)
 
         !  Vertical velocity.

         CALL wbound (web2d, web3d, imx, kxs, web)
         CALL wbound (wwb2d, wwb3d, imx, kxs, wwb)
         CALL wbound (wnb2d, wnb3d, jmx, kxs, wnb)
         CALL wbound (wsb2d, wsb3d, jmx, kxs, wsb)

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YWB '
         name            = 'WEB      '
         units           = 'kPa m/s                  '
         description     = 'W-component of velocity          east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) web

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YWB '
         name            = 'WWB      '
         units           = 'kPa m/s                  '
         description     = 'W-component of velocity          west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wwb

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XWB '
         name            = 'WNB      '
         units           = 'kPa m/s                  '
         description     = 'W-component of velocity         north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wnb

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XWB '
         name            = 'WSB      '
         units           = 'kPa m/s                  '
         description     = 'W-component of velocity         south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wsb

         CALL bndtend (w_top, web2d, wwb2d, wnb2d, wsb2d, imx, jmx,   1, 1, dts, et2d, wt2d, nt2d, st2d)
         CALL bndtend (w_sig, web3d, wwb3d, wnb3d, wsb3d, imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)
         CALL wbound (et2d, et3d, imx, kxs, wet)
         CALL wbound (wt2d, wt3d, imx, kxs, wwt)
         CALL wbound (nt2d, nt3d, jmx, kxs, wnt)
         CALL wbound (st2d, st3d, jmx, kxs, wst)

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YWB '
         name            = 'WEBT     '
         units           = 'kPa m/s/s                '
         description     = 'W-component of velocity    tend  east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wet

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YWB '
         name            = 'WWBT     ' 
         units           = 'kPa m/s/s                '
         description     = 'W-component of velocity    tend  west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wwt

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XWB '
         name            = 'WNBT     '
         units           = 'kPa m/s/s                '
         description     = 'W-component of velocity    tend north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wnt

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs+1, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XWB '
         name            = 'WSBT     '
         units           = 'kPa m/s/s                '
         description     = 'W-component of velocity    tend south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wst

         CALL bound (w_sig, web3d, wwb3d, wnb3d, wsb3d, imx, jmx, kxs, 1)
         CALL bound (w_top, web2d, wwb2d, wnb2d, wsb2d, imx, jmx,   1, 1)

         !  Pressure perturbation.

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'PPEB     '
         units           = 'kPa Pa                   '
         description     = 'Pressure perturbation            east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) ppeb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'PPWB     '
         units           = 'kPa Pa                   '
         description     = 'Pressure perturbation            west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) ppwb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'PPNB     '
         units           = 'kPa Pa                   '
         description     = 'Pressure perturbation           north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) ppnb3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'PPSB     '
         units           = 'kPa Pa                   '
         description     = 'Pressure perturbation           south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) ppsb3d

         CALL bndtend (pp_sig, ppeb3d, ppwb3d, ppnb3d, ppsb3d, imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'PPEBT    '
         units           = 'kPa Pa/s                 '
         description     = 'Pressure perturbation      tend  east boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) et3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YSB '
         name            = 'PPWBT    '
         units           = 'kPa Pa/s                 '
         description     = 'Pressure perturbation      tend  west boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) wt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'PPNBT    '
         units           = 'kPa Pa/s                 '
         description     = 'Pressure perturbation      tend north boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) nt3d

         WRITE ( iunit ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ jmx, kxs, nspgx, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'XSB '
         name            = 'PPSBT    '
         units           = 'kPa Pa/s                 '
         description     = 'Pressure perturbation      tend south boundary'
         WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                         old_date//'.0000' , name , units , description
         WRITE ( iunit ) st3d

         CALL bound (pp_sig, ppeb3d, ppwb3d, ppnb3d, ppsb3d, imx, jmx, kxs, 1)

         IF ( cloud_and_rain ) THEN

            !  Cloud Water

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QCEB     '
            units           = 'kPa kg/kg                '
            description     = 'Cloud water                      east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(1)%ebdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QCWB     '
            units           = 'kPa kg/kg                '
            description     = 'Cloud water                      west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(1)%wbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QCNB     '
            units           = 'kPa kg/kg                '
            description     = 'Cloud water                     north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(1)%nbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QCSB     '
            units           = 'kPa kg/kg                '
            description     = 'Mixing ratio                    south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(1)%sbdy
    
            CALL bndtend (allm_3d(1)%array, allm_3d(1)%ebdy, allm_3d(1)%wbdy, allm_3d(1)%nbdy, allm_3d(1)%sbdy, &
                          imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QCEBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud water                tend  east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) et3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QCWBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud water                tend  west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) wt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QCNBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud water                tend north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) nt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QCSBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud water                tend south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) st3d

            CALL bound (allm_3d(1)%array, allm_3d(1)%ebdy, allm_3d(1)%wbdy, allm_3d(1)%nbdy, allm_3d(1)%sbdy, &
                          imx, jmx, kxs, 1)

            !  Rain Water

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QREB     '
            units           = 'kPa kg/kg                '
            description     = 'Rain water                       east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(2)%ebdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QRWB     '
            units           = 'kPa kg/kg                '
            description     = 'Rain water                       west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(2)%wbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QRNB     '
            units           = 'kPa kg/kg                '
            description     = 'Rain water                      north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(2)%nbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QRSB     '
            units           = 'kPa kg/kg                '
            description     = 'Mixing ratio                    south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(2)%sbdy
    
            CALL bndtend (allm_3d(2)%array, allm_3d(2)%ebdy, allm_3d(2)%wbdy, allm_3d(2)%nbdy, allm_3d(2)%sbdy, &
                          imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QREBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Rain water                 tend  east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) et3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QRWBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Rain water                 tend  west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) wt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QRNBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Rain water                 tend north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) nt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QRSBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Rain water                 tend south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) st3d

            CALL bound (allm_3d(2)%array, allm_3d(2)%ebdy, allm_3d(2)%wbdy, allm_3d(2)%nbdy, allm_3d(2)%sbdy, &
                          imx, jmx, kxs, 1)

         END IF

         IF ( cloud_and_rain .AND. snow_and_ice ) THEN

            !  Snow

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNIEB    '
            units           = 'kPa kg/kg                '
            description     = 'Snow                             east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(3)%ebdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNIWB    '
            units           = 'kPa kg/kg                '
            description     = 'Snow                             west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(3)%wbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNINB    '
            units           = 'kPa kg/kg                '
            description     = 'Snow                            north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(3)%nbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNISB    '
            units           = 'kPa kg/kg                '
            description     = 'Mixing ratio                    south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(3)%sbdy
    
            CALL bndtend (allm_3d(3)%array, allm_3d(3)%ebdy, allm_3d(3)%wbdy, allm_3d(3)%nbdy, allm_3d(3)%sbdy, &
                          imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNIEBT   '
            units           = 'kPa kg/kg/s              '
            description     = 'Snow                       tend  east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) et3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNIWBT   '
            units           = 'kPa kg/kg/s              '
            description     = 'Snow                       tend  west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) wt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNINBT   '
            units           = 'kPa kg/kg/s              '
            description     = 'Snow                       tend north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) nt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNISBT   '
            units           = 'kPa kg/kg/s              '
            description     = 'Snow                       tend south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) st3d

            CALL bound (allm_3d(3)%array, allm_3d(3)%ebdy, allm_3d(3)%wbdy, allm_3d(3)%nbdy, allm_3d(3)%sbdy, &
                          imx, jmx, kxs, 1)

            !  Cloud Ice

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QIEB     '
            units           = 'kPa kg/kg                '
            description     = 'Cloud ice                        east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(4)%ebdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QIWB     '
            units           = 'kPa kg/kg                '
            description     = 'Cloud ice                        west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(4)%wbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QINB     '
            units           = 'kPa kg/kg                '
            description     = 'Cloud ice                       north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(4)%nbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QISB     '
            units           = 'kPa kg/kg                '
            description     = 'Mixing ratio                    south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(4)%sbdy
    
            CALL bndtend (allm_3d(4)%array, allm_3d(4)%ebdy, allm_3d(4)%wbdy, allm_3d(4)%nbdy, allm_3d(4)%sbdy, &
                          imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QIEBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud ice                  tend  east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) et3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QIWBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud ice                  tend  west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) wt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QINBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud ice                  tend north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) nt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QISBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Cloud ice                  tend south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) st3d

            CALL bound (allm_3d(4)%array, allm_3d(4)%ebdy, allm_3d(4)%wbdy, allm_3d(4)%nbdy, allm_3d(4)%sbdy, &
                          imx, jmx, kxs, 1)

         END IF

         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN

            !  Graupel

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QGEB     '
            units           = 'kPa kg/kg                '
            description     = 'Graupel                          east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(5)%ebdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QGWB     '
            units           = 'kPa kg/kg                '
            description     = 'Graupel                          west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(5)%wbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QGNB     '
            units           = 'kPa kg/kg                '
            description     = 'Graupel                         north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(5)%nbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QGSB     '
            units           = 'kPa kg/kg                '
            description     = 'Mixing ratio                    south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(5)%sbdy
    
            CALL bndtend (allm_3d(5)%array, allm_3d(5)%ebdy, allm_3d(5)%wbdy, allm_3d(5)%nbdy, allm_3d(5)%sbdy, &
                          imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QGEBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Graupel                    tend  east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) et3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QGWBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Graupel                    tend  west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) wt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QGNBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Graupel                    tend north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) nt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QGSBT    '
            units           = 'kPa kg/kg/s              '
            description     = 'Graupel                    tend south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) st3d

            CALL bound (allm_3d(5)%array, allm_3d(5)%ebdy, allm_3d(5)%wbdy, allm_3d(5)%nbdy, allm_3d(5)%sbdy, &
                          imx, jmx, kxs, 1)

         END IF

         IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN

            !  Number Concentration

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNCEB    '
            units           = 'kPa #/m^3                '
            description     = 'Number concentration of ice      east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(6)%ebdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNCWB    '
            units           = 'kPa #/m^3                '
            description     = 'Number concentration of ice      west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(6)%wbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNCNB    '
            units           = 'kPa #/m^3                '
            description     = 'Number concentration of ice     north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(6)%nbdy

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNCSB    '
            units           = 'kPa #/m^3                '
            description     = 'Number concentration of ice     south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) allm_3d(6)%sbdy
    
            CALL bndtend (allm_3d(6)%array, allm_3d(6)%ebdy, allm_3d(6)%wbdy, allm_3d(6)%nbdy, allm_3d(6)%sbdy, &
                          imx, jmx, kxs, 1, dts, et3d, wt3d, nt3d, st3d)

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNCEBT   '
            units           = 'kPa #/m^3/s              '
            description     = 'Number concentration of icetend  east boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) et3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'YSB '
            name            = 'QNCWBT   '
            units           = 'kPa #/m^3/s              '
            description     = 'Number concentration of icetend  west boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) wt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNCNBT   '
            units           = 'kPa #/m^3/s              '
            description     = 'Number concentration of icetend north boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) nt3d

            WRITE ( iunit ) sh_flag
            num_dims        = 3
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ jmx, kxs, nspgx, 1 /)
            xtime           = 0
            staggering      = 'C   '
            ordering        = 'XSB '
            name            = 'QNCSBT   '
            units           = 'kPa #/m^3/s              '
            description     = 'Number concentration of icetend south boundary'
            WRITE ( iunit ) num_dims , start_dims , end_dims , xtime , staggering , ordering , &
                            old_date//'.0000' , name , units , description
            WRITE ( iunit ) st3d

            CALL bound (allm_3d(6)%array, allm_3d(6)%ebdy, allm_3d(6)%wbdy, allm_3d(6)%nbdy, allm_3d(6)%sbdy, &
                          imx, jmx, kxs, 1)

         END IF

         !  Always end the file with the end of time flag.

         WRITE ( iunit ) eot_flag

      END IF

   END SUBROUTINE bdyout

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE bndtend (f, east, west, north, south, imx, jmx, kxs, icrsdot, dts, teast, twest, tnorth, tsouth)

      IMPLICIT NONE

      INTEGER :: i , icrsdot , imx , j , jmx , k , kxs , n

      REAL :: dts , oneovdt

      REAL , DIMENSION( imx, kxs, nspgd ) :: east , teast , twest , west

      REAL , DIMENSION( jmx, kxs, nspgd ) :: north , south , tnorth , tsouth

      REAL , DIMENSION(imx, jmx, kxs ) :: f


      teast = 0.0
      twest = 0.0
      tnorth = 0.0
      tsouth = 0.0
      oneovdt = 1.0 / dts

      DO n = 1, nspgd
         DO k = 1, kxs
            DO i = 1, imx - icrsdot
               twest(i,k,n) = (f(i,n,k) - west(i,k,n)) * oneovdt
               teast(i,k,n) = (f(i,jmx-icrsdot-n+1,k) - east(i,k,n)) * oneovdt
            END DO
         END DO

         DO k = 1, kxs
            DO j = 1, jmx - icrsdot
               tsouth(j,k,n) = (f(n,j,k) - south(j,k,n)) * oneovdt
               tnorth(j,k,n) = (f(imx-icrsdot-n+1,j,k) - north(j,k,n)) * oneovdt
            END DO
         END DO
      END DO

   END SUBROUTINE bndtend

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE bound (f, east, west, north, south, imx, jmx, kxs, icrsdot)

      IMPLICIT NONE

      INTEGER :: i , icrsdot , imx , j , jmx , k , kxs , n

      REAL , DIMENSION( imx, kxs, nspgd ) :: east , west

      REAL , DIMENSION( jmx, kxs, nspgd ) :: north , south

      REAL , DIMENSION( imx, jmx, kxs ) :: f

      east = 0.0
      west = 0.0
      north = 0.0
      south = 0.0

      DO n = 1, nspgd
         DO k = 1, kxs
            DO i = 1, imx - icrsdot
               west(i,k,n) = f(i,n,k)
               east(i,k,n) = f(i,jmx-icrsdot-n+1,k)
            END DO
         END DO

         DO k = 1, kxs
            DO j = 1, jmx - icrsdot
               south(j,k,n) = f(n,j,k)
               north(j,k,n) = f(imx-icrsdot-n+1,j,k)
            END DO
         END DO
      END DO

   END SUBROUTINE bound

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wbound (w2din, w3din, imx, kxs, w3dout)

      IMPLICIT NONE

      INTEGER :: i , imx , k , kxs , n

      REAL , DIMENSION( imx,   1  , nspgx ) :: w2din
      REAL , DIMENSION( imx,   kxs, nspgx ) :: w3din
      REAL , DIMENSION( imx, kxs+1, nspgx ) :: w3dout

      w3dout = 0.0

      DO n = 1, nspgx
         DO i = 1, imx
            w3dout(i,1,n) = w2din(i,1,n)
            DO k = 2, kxs + 1
               w3dout(i,k,n) = w3din(i,k-1,n)
            END DO
         END DO
      END DO

   END SUBROUTINE wbound

END MODULE lateral_bdy
