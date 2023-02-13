#!/bin/csh -f
#
# Script to batch pre-process a Sentinel-1a TOPS mode data sets.
#  
#   Xiaohua(Eric) Xu
#   June 2016
#
#
# First, prepare the files

##clean and create raw directory
#rm -rf raw
#mkdir raw
cd raw
#
## in order to correct for Elevation Antenna Pattern Change, cat the manifest and aux files to the xmls
## delete the first line of the manifest file as it's not a typical xml file.
#
#dos2unix ../sh_xml_create #if not
#grep -v iw1 ../sh_xml_create | grep -v iw2 | sh
#
#rm tmp_file
ln -s ../raw_orig/*EOF .
ln -s ../raw_orig/*tiff .
ln -s ../topo/dem.grd .
##
## start batch preprocessing
##
##echo "s1a-iw2-slc-vv-20141020t115800-20141020t115826-002913-0034d7-002:S1A_OPER_AUX_POEORB_OPOD_20141110T123706_V20141019T225944_20141021T005944.EOF" > data.in
##echo "s1a-iw2-slc-vv-20141113t115800-20141113t115825-003263-003c61-002:S1A_OPER_AUX_POEORB_OPOD_20141204T123638_V20141112T225944_20141114T005944.EOF" >> data.in
#
#grep iw3 ../raw_orig/tiff_EOF.list | sed -e 's/.tiff /:/g'> data.in
#cat data.in
##exit
## get the baseline_time plot first, select the supermaster and mv it to the first line in data.in, save the baseline_table.dat for sbas use.
#
##preproc_batch_tops.csh data.in dem.grd 1
##mv baseline* ../
#
#preproc_batch_tops.csh data.in dem.grd 2
preproc_batch_tops_esd.csh data.in dem.grd 2

#
# in case there are clear burst boundaries, use preproc_batch_tops_esd.csh instead
#
#
cd ..



