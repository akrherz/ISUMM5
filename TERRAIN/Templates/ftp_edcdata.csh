#! /bin/csh -f
# 
# This is a utility script to help automate
# checking out the standard workstation version
# of the tutorial TERRAIN data set.
#
# Contact: D. Hansen, NCAR

if ( ! ${?USER} ) then
  echo "Hmmm - please set your USER environment variable, pretty please..."
else 
  echo "USER -> $USER"
endif

set dasfile=$1
set dasmachine=edcftp.cr.usgs.gov
echo "About to contact $dasmachine ..."
cat >! ftp.dasmachine << EOF
anonymous 
$USER@ucar.edu
ls
quit
EOF

#ftp.pl $dasmachine < ftp.dasmachine >&! ftp.output

cat <<EOF > GetEDCFile.pl
#!/usr/local/bin/perl -s

use Net::FTP;

\$Pass = "${USER}@";
\$User = "anonymous";
\$Host = "${dasmachine}";
         \$ftp = Net::FTP->new(\$Host, Timeout => 360, Passive=>true) || &fail ;
         \$ftp->login(\$User,\$Pass) ;
\$ftp->binary ;
\$ftp->cwd ("/pub/data/gtopo30/global");
@ln = \$ftp->ls("${dasfile}");
\$file_to_get = @ln[0];
print STDOUT "I see file \$file_to_get\n";
\$ftp->get (\$file_to_get);


sub fail {
    \$ftp->quit;
    die "ftp error occurred\n";
}

EOF

chmod +x GetEDCFile.pl

echo "ftping ..."
./GetEDCFile.pl 
