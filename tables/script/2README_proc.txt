#/bin/csh -f
#
# Script to batch process Sentinel-1a TOPS mode data sets.
#  
#   Xiaohua(Eric) Xu
#   June 2016
#
#
# First, create the files needed for intf_tops.csh
#
#rm -f intf.in
#rm -r intf intf_all
#
#echo S1A20141020_ALL_F2:S1A20141113_ALL_F2 > intf.in
#echo S1A20141020_ALL_F2:S1A20150301_ALL_F2 >> intf.in
#echo S1A20141020_ALL_F2:S1A20151003_ALL_F2 >> intf.in
#echo S1A20141020_ALL_F2:S1A20160319_ALL_F2 >> intf.in
#echo S1A20141020_ALL_F2:S1A20161021_ALL_F2 >> intf.in
#echo S1A20141113_ALL_F2:S1A20150301_ALL_F2 >> intf.in
#echo S1A20141113_ALL_F2:S1A20151003_ALL_F2 >> intf.in
#echo S1A20141113_ALL_F2:S1A20160319_ALL_F2 >> intf.in
#echo S1A20141113_ALL_F2:S1A20161021_ALL_F2 >> intf.in
#echo S1A20150301_ALL_F2:S1A20151003_ALL_F2 >> intf.in
#echo S1A20150301_ALL_F2:S1A20160319_ALL_F2 >> intf.in
#echo S1A20150301_ALL_F2:S1A20161021_ALL_F2 >> intf.in
#echo S1A20151003_ALL_F2:S1A20160319_ALL_F2 >> intf.in
#echo S1A20151003_ALL_F2:S1A20161021_ALL_F2 >> intf.in
#echo S1A20160319_ALL_F2:S1A20161021_ALL_F2 >> intf.in
#
#
##
## Process the interferograms, remember to set your super master in the batch_tops.config file.
##
dos2unix intf.in

intf_tops.csh intf.in batch_tops.config
#
# After this is done, check the interferograms. You'll find there being a discontinuity in some of the interferograms
# This can be resolved by using preproc_batch_tops_esd.csh. Modify the README_prep.csh and see what happens
# 
# Also note the ESD may not always make your results better.
