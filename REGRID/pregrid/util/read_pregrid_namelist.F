subroutine read_pregrid_namelist(hstart, hend, delta_time, ntimes,&
     ordered_by_date, print_flag, iprint)
  implicit none
  character(len=19) :: hstart, hend
  integer :: delta_time
  integer :: ntimes
  logical :: ordered_by_date
  logical :: print_flag
  integer :: iprint

  integer :: ierr
  integer :: idts
  logical :: stop_flag = .FALSE.

! Declare the namelist variables:

  integer :: start_year = 0
  integer :: start_month = 0
  integer :: start_day = 0
  integer :: start_hour = 0
  integer :: start_minute = 0
  integer :: start_second = 0

  integer :: end_year = 0
  integer :: end_month = 0
  integer :: end_day = 0
  integer :: end_hour = 0
  integer :: end_minute = 0
  integer :: end_second = 0

  integer :: interval = 0

  logical :: debug_print = .FALSE.

  namelist /record1/ start_year, start_month, start_day, start_hour, &
       start_minute, start_second, &
       end_year, end_month, end_day, end_hour, &
       end_minute, end_second,&
       interval, &
       ordered_by_date, &
       debug_print

! Start routine:


! Open the namelist file:
  open(10, file='pregrid.namelist', status='old', iostat=ierr)
  if (ierr.ne.0) then
     write(*,'(80(''-''), /, ''**** ERROR:'')')
     write(*,'(''**** File ''''pregrid.namelist'''' not found.'')')
     write(*,'(''**** Please be sure that the ''''pregrid.namelist'''' file &
          &was properly created.'')')
     write(*,'(/,80("-"))')
     stop
  endif

  ! set default:
  ordered_by_date = .TRUE.

  read(10,NML=record1)

! Build the Starting date HSTART and the ending date HEND from the namelist
! date/time information.

  call build_hdate(hstart, start_year, start_month, start_day, start_hour,&
       start_minute, start_second)
  call build_hdate(hend, end_year, end_month, end_day, end_hour,&
       end_minute, end_second)

! Compute the time difference between start date and end date:

  call geth_idts(hend, hstart, idts)

! Check that INTERVAL is greater than zero:

  if (interval <= 0) then
     write(*,'(//,80("*"),/)')
     write(*,'(2x,"ERROR STOP",/)')
     write(*,'(6x,"INTERVAL must be greater than zero:")')
     write(*,'(6x,"Start time: ", A19)') hstart
     write(*,'(6x,"End time  : ", A19)') hend
     write(*,'(6x,"INTERVAL  : ", I10, " seconds")')interval
     write(*,'(/,6x,"Change your namelist, and resubmit")')
     write(*,'(/,80("*"),//)')
     stop_flag = .TRUE.
  endif

! Check that the selected INTERVAL evenly fills difference between 
! start time and end time:

  if ((idts/interval)*interval /= idts) then
     write(*,'(//,80("*"),/)')
     write(*,'(2x,"ERROR STOP",/)')
     write(*,'(6x,"INTERVAL must fit evenly between start time and end time:")')
     write(*,'(6x,"Start time: ", A19)') hstart
     write(*,'(6x,"End time  : ", A19)') hend
     write(*,'(6x,"INTERVAL  : ", I10, " seconds = ", F15.8, " hours")')&
          interval, float(interval)/3600.
     write(*,'(/,6x,"Change your namelist, and resubmit")')
     write(*,'(/,80("*"),//)')
     stop_flag = .TRUE.
  endif

! Check that start time is not later than end time:

  if (hstart > hend) then
     write(*,'(//,80("*"),/)')
     write(*,'(2x,"ERROR STOP",/)')
     write(*,'(6x,"Start time must not be later than end time:")')
     write(*,'(6x,"Start time: ", A19)') hstart
     write(*,'(6x,"End time  : ", A19)') hend
     write(*,'(6x,"INTERVAL  : ", I10, " seconds")') interval
     write(*,'(/,6x,"Change your namelist, and resubmit")')
     write(*,'(/,80("*"),//)')
     stop_flag = .TRUE.
  endif

! Compute the number of time periods to process:

  if (stop_flag) stop

  ntimes = idts/interval + 1

  print*, 'HSTART, HEND = ', hstart//'    '//hend

  if (debug_print) then
     iprint=1
     write(*,'(//)')
     write(*,'("Namelist information: ")')
     write(*,'(5x,"start_year = ", I8)') start_year
     write(*,'(5x,"start_month = ", I8)') start_month
     write(*,'(5x,"start_day = ", I8)') start_day
     write(*,'(5x,"start_hour = ", I8)') start_hour
     write(*,'(5x,"start_minute = ", I8)') start_minute
     write(*,'(5x,"start_second = ", I8)') start_second
     write(*,'(5x,"end_year = ", I8)') end_year
     write(*,'(5x,"end_month = ", I8)') end_month
     write(*,'(5x,"end_day = ", I8)') end_day
     write(*,'(5x,"end_hour = ", I8)') end_hour
     write(*,'(5x,"end_minute = ", I8)') end_minute
     write(*,'(5x,"end_second = ", I8)') end_second
     write(*,'(5x,"interval = ", I10)') interval
     write(*,'(5x,"ordered_by_date = ", L4)') ordered_by_date
     write(*,'(5x,"debug_print = ", L4)') debug_print
     write(*,'(//)')
  else
     iprint=0
  endif

  print_flag = debug_print
  delta_time = interval

! Close the namelist file:

  close(10)
  
end subroutine read_pregrid_namelist
