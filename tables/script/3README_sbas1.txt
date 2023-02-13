#!/bin/csh -f
#
# Script to create time series 
#  
#   Xiaohua(Eric) Xu
#   June 2016
#
#
# First, prepare the input files needed for sbas
#
rm -rf SBAS
mkdir SBAS
cd SBAS && rm -f intf.tab
#
# based on baseline_table.dat create the intf.tab and scene.tab for sbas
#
# phase  corherence  ref_id  rep_id  baseline
#echo "../intf_all/2015275_2016078/unwrap.grd ../intf_all/2015275_2016078/corr.grd 2015275 2016078 43.094446" > intf.tab
#echo "../intf_all/2015275_2016294/unwrap.grd ../intf_all/2015275_2016294/corr.grd 2015275 2016294 43.094446" >> intf.tab
#echo "../intf_all/2016078_2016294/unwrap.grd ../intf_all/2016078_2016294/corr.grd 2016078 2016294 0" >> intf.tab

dos2unix ../intf.tab
\cp -f ../intf.tab ./

#echo "../intf_all/2015068_2015092/unwrap.grd ../intf_all/2015068_2015092/corr.grd 2015068 2015092 37.7213" >> intf.tab
#echo "../intf_all/2015068_2015116/unwrap.grd ../intf_all/2015068_2015116/corr.grd 2015068 2015116 7.30062" >> intf.tab
#echo "../intf_all/2015068_2015140/unwrap.grd ../intf_all/2015068_2015140/corr.grd 2015068 2015140 -13.2457" >> intf.tab
#echo "../intf_all/2015092_2015116/unwrap.grd ../intf_all/2015092_2015116/corr.grd 2015092 2015116 -30.4207" >> intf.tab
#echo "../intf_all/2015092_2015140/unwrap.grd ../intf_all/2015092_2015140/corr.grd 2015092 2015140 -50.967" >> intf.tab
#echo "../intf_all/2015116_2015140/unwrap.grd ../intf_all/2015116_2015140/corr.grd 2015116 2015140 -20.5463" >> intf.tab
#
# scene_id  day
dos2unix ../scene.tab
\cp -f ../scene.tab ./
#echo "2015275 640" > scene.tab
#echo "2016078 808" >> scene.tab 
#echo "2016294 1024" >> scene.tab
#echo "2015116 472" >> scene.tab
#echo "2015140 496" >> scene.tab
set file_tmp = `head -1 intf.tab | awk '{print $1}'`
echo $file_tmp
set xdim = `gmt grdinfo -C $file_tmp | awk '{print $10}'`
set ydim = `gmt grdinfo -C $file_tmp | awk '{print $11}'`
#
# run sbas
#
set nintf = `cat intf.tab | wc -l`
set nscen = `cat scene.tab | wc -l`

echo  $nintf $nscen $xdim $ydim 
#exit
echo sbas intf.tab scene.tab $nintf $nscen $xdim $ydim -smooth 1.0 -wavelength 0.0554658 -incidence 38 -range 800184.946186 -rms -dem
time sbas intf.tab scene.tab $nintf $nscen $xdim $ydim -smooth 1.0 -wavelength 0.0554658 -incidence 38 -range 800184.946186 -rms -dem
#
# project the velocity to Geocooridnates
#
ln -s ../topo/trans.dat .
touch gauss_400
proj_ra2ll.csh trans.dat vel.grd vel_ll.grd
gmt grd2cpt vel_ll.grd -T= -Z -Cjet > vel_ll.cpt
gmt makecpt -Cjet -T-10/10/.1 -Z -I > vel_ll.cpt4
grd2kml.csh vel_ll vel_ll.cpt4
#gmt grd2xyz vel_ll.grd | grep -v NaN | awk '{print "",$0}' > vel_ll.xyz
gmt grdsample vel_ll.grd -I.008 -Gvel_ll3.grd
##gmt grd2xyz vel_ll3.grd | grep -v NaN > vel_ll3.xyz

sh_esa_s1_corr_mask

set tmp1 = `pwd`
set bname = `basename $tmp1`

tar zcf SBAS.tar.gz vel_mask_ll* *.tab

proj_ra2ll.csh trans.dat rms.grd rms_ll.grd
gmt makecpt -Cjet -T-3/3/.1 -Z -I > rms_ll.cpt
grd2kml.csh rms_ll rms_ll.cpt
#gmt grd2xyz rms_ll.grd | grep -v NaN | awk '{print "",$0}' > rms_ll.xyz


sh_grd_resample_mask
#sh_grd2nc 'disp_???????'
#sh_grd2nc 'disp_???'

cd ..
