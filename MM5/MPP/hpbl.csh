#!/bin/csh -f

echo -- the ibgn/iend business and the movement of the setting of the xxt
echo variables is not handled

exit
       
set VLIST = ( UX VX QX QCX THX THVX RC DZQ DZA TTNP QTNP QCTNP KZM ZA UXS VXS THXS QXS QCXS \
              QIX QITNP QIXS QNCX QNCNP FQNC QNCXS ZQ UTNP VTNP \
              FU FV FTHX FQ FQC ALFA BM EM WGT MBARK FQI )

set V2LIST = ( FLQI QIXSV FLQNC QNCXSV RHOX KZMAX GOVRTH GZ1OZ0 \
               WSPD BR PSIM PSIH UFLXP VFLXP CFLXP FLHC \
               FLQC HFKLMX THXSV UXSV VXSV QXSV QCXSV KPBL \
               KPBLM1 QGH TGDSA TGDSB KPBL3 )

cut -c1-72 $1 > foo
/bin/mv foo $1.tmp

foreach v ( $VLIST )
  echo modifying $v in $1.tmp
  sed -e "s/$v(I,/$v(/g" -e "s/$v(MIX[M]*,/$v(/g" $1.tmp > foo
  /bin/mv foo $1.tmp
end

foreach v ( $V2LIST )
  echo modifying $v in $1.tmp
  sed -e "s/$v(I)/$v/g" -e "s/$v(MIX[M]*)/$v/g" $1.tmp > foo
  /bin/mv foo $1.tmp
end

sed -e '/DO.*I.*=/s/^ /CM/' $1.tmp > foo
/bin/mv foo $1.tmp
sed -e 's/CM     DO 5 I=1,ILX/      DO 1000 I=1,ILX/' $1.tmp > foo
/bin/mv foo $1.tmp

sed -e '/DIMENSION   RHOX/,/6          ,KPBL3/d' \
    -e 's/,FLQI,QIXSV//' \
    -e 's/,FLQNC,QNCXSV//' $1.tmp > foo
/bin/mv foo $1.tmp


echo you will need to get rid of some dimension statements if the
echo affected data items became scalars



