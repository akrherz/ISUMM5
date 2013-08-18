MODULE first_guess_data

   INTEGER           :: version
   CHARACTER(LEN=19) :: hdate
   REAL              :: xfcst
   CHARACTER(LEN=8)  :: field
   CHARACTER(LEN=24) :: units
   CHARACTER(LEN=46) :: description
   INTEGER           :: idim, jdim, llflag
   REAL              :: level
   REAL              :: startlat, startlon, deltalat, deltalon
   REAL              :: deltalon_save
   INTEGER           :: nlat
   REAL              :: lat1, lon1, lon2, deltax, deltay, xlonc, truelat1, truelat2
   REAL              :: xlatc , cone_fac

   INTEGER           :: idim_old, jdim_old, llflag_old
   REAL              :: lat1_old, lon1_old, deltax_old, deltay_old, xlonc_old, truelat1_old, truelat2_old

   REAL              :: wind_xlonc , wind_xlatc , wind_cone_fac
   INTEGER           :: wind_llflag
END MODULE first_guess_data
