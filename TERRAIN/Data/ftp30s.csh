#!/bin/csh -f
#
set echo
#
#     Obtain 30 sec global elevation data, and preprocess the data
#
set where30s = $1        # ftp or a directory
set users    = $2        # MMM or Others
#
set ForUnit = fort.
#
# (1) Generate the "dem_read" for getting the 30s data:
#
      src/data_area.exe >! data_area.out
      set toast = $status
      if ( $toast != 0 ) then
         echo "error in running data_area, stopping"
         exit(1)
      endif
      rm data_area.out

# if file dem_read is not created, no need to do the following

  if ( -e dem_read ) then

      echo "---------------------------------------------------"
      echo "Global 30-sec data files needed"
      echo "   FileName     MSS_file      FTP_file    FortranUnit"
      cat dem_read
      echo "---------------------------------------------------"
#
# (2) Get the data from MSS or ftp site or "ln" to the specified directory:
#
      mkdir -p Data30s
      cd Data30s ; mv ../dem_read .
        set File30s = `cat dem_read`
        set Nfiles = ${#File30s}
        echo $Nfiles $File30s
#
        if ( $where30s == ftp ) then

#  ----- Data from ftp edcftp.cr.usgs.gov:

            if ( $users == MMM ) then

               set dhost=gate.ucar.edu
               echo "About to contact $dhost ..."
               cat >! ftp.tmp << EOF
               user anonymous@edcftp.cr.usgs.gov ${USER}@
               cd /pub/data/gtopo30/global
               bi
EOF

            else 

               set dhost=edcftp.cr.usgs.gov
               echo "About to contact $dhost ..."
               cat >! ftp.tmp << EOF
               user anonymous ${USER}@
               cd /pub/data/gtopo30/global
               bi
EOF
            endif

            set Num0 = 1
            while ( $Num0 <= $Nfiles )
              @ Num1 = $Num0 + 1
              @ Num2 = $Num0 + 2
              @ Num3 = $Num0 + 3

              if ( ! -e $File30s[$Num0] ) then

                  cat >! ftp$Num0 << EOF
                  get ${File30s[$Num2]}.gz
EOF
                  cat ftp$Num0 >> ftp.tmp
              endif
                  @ Num0 = $Num0 + 4
            end

            cat >! ftp_end << EOF
            quit
EOF
            cat ftp_end >> ftp.tmp

            cat ftp.tmp
            if ( -e ftp1 || -e ftp2 || -e ftp3 || -e ftp4 ) then
               echo "ftping ..."
               ftp -v -n -i $dhost < ftp.tmp >&! ftp.output
               rm -rf ftp.tmp ftp.output ftp*
            endif

        endif
#
        set Num0 = 1
        while ( $Num0 <= $Nfiles )
           @ Num1 = $Num0 + 1
           @ Num2 = $Num0 + 2
           @ Num3 = $Num0 + 3

        if ( $where30s == ftp ) then

           if ( ! -e ${File30s[$Num0]} ) then
              if ( ! -e ${File30s[$Num2]}.gz ) then
                  echo "ftp is failed for file ${File30s[$Num2]}.gz"
              endif
              echo "untarring and decompressing..."
              gunzip -c ${File30s[$Num2]}.gz | tar xvf -
              rm *.DMW *.GIF *.HDR *.PRJ *.SCH *.SRC *.STX
#             rm *.tar.gz *.DMW *.GIF *.HDR *.PRJ *.SCH *.SRC *.STX

           endif

           if ( -e ${ForUnit}$File30s[$Num3] ) rm ${ForUnit}$File30s[$Num3]
           ln -s $File30s[$Num0]  ${ForUnit}$File30s[$Num3]

        else 

#  ----- access data file in directory:

           echo "accessing data from specified directory"
           if ( -e ${where30s}/$File30s[$Num0] ) then

               if ( -e ${ForUnit}$File30s[$Num3] ) rm ${ForUnit}$File30s[$Num3]
               ln -s ${where30s}/$File30s[$Num0]  ${ForUnit}$File30s[$Num3]

           else if ( -e ${where30s}/${File30s[$Num2]}.gz ) then

               echo "untarring and decompressing..."
               gunzip -c ${where30s}/${File30s[$Num2]}.gz | tar xvf -
               rm *.tar.gz *.DMW *.GIF *.HDR *.PRJ *.SCH *.SRC *.STX
               ln -s $File30s[$Num0]  ${ForUnit}$File30s[$Num3]

           else if ( -e ${where30s}/${File30s[$Num2]}) then

               echo "untarring ..."
               tar xvf ${where30s}/${File30s[$Num2]}
               rm *.tar.gz *.DMW *.GIF *.HDR *.PRJ *.SCH *.SRC *.STX
               ln -s $File30s[$Num0]  ${ForUnit}$File30s[$Num3]
 
           else

               echo "File ${where30s}/$File30s[$Num0] does not exist"
               exit(1)

           endif

        endif
          @ Num0 = $Num0 + 4
        end
      cd ..
#
# (3) To reconstruct the data for use of TERRAIN:
#  .. mv some files to directory: Data30s
#
      echo "beginning reconstruct data"
      mv data30sID  Data30s/.
#     mv src/rdem.exe Data30s/.
      cd Data30s
        rm new_*
        ../src/rdem.exe > rdem.out
        set toast = $status
        if ( $toast != 0 ) then
           echo "error in running rdem, stopping"
           exit(3)
        endif
        mv new_* ../Data/.
        rm -f data30sID rdem.out dem_read
        rm -f ${ForUnit}79 ${ForUnit}8*
      cd ..
  endif
