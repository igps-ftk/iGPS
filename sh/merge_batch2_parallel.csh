#!/bin/csh -f
# 
# By Xiaohua XU, 03/12/2018
#
# Unwrap interferograms parallelly using GNU parallel
#
# IMPORTANT: put a script called unwrap_intf.csh in the current folder
# e.g. 
#   cd $1
#   snaphu[_interp].csh 0.1 0 
#   cd ..
#

if ($#argv < 4) then
  echo ""
  echo "Usage: merge_batch2_parallel.csh reffile intflist batchconfig Ncores"
  echo ""
  echo "    Run unwrapping jobs parallelly. Need to install GNU parallel first."
  echo "    Note, run this in the intf_all folder where all the interferograms are stored. "
  echo ""
  exit
endif

rm -f merge_batch.cmd

set reffile = $1

set t1 = `date`
set ncores = $4

foreach line (`awk '{print $0}' $2`)
   set date1 =  `echo $line |awk -F":" '{print $2}'|cut -c 4-11`
   set date2 =  `echo $line |awk -F":" '{print $3}'|cut -c 4-11`
   set logfile = "merge_"$date1"_"$date2".log"
   set infile = "merge_"$date1"_"$date2".in"
   echo $line > $infile
   echo "merge_batch2.csh $reffile $infile $3 > $logfile" >> merge_batch.cmd
end

set sub_delay = 60

if ( "$5" != "" ) then
    set sub_delay = $5
endif

echo parallel --jobs $ncores --delay $sub_delay
parallel --jobs $ncores --delay $sub_delay < merge_batch.cmd

echo ""
echo "Finished all unwrapping jobs..."
echo ""

#set d2 = `date`
set t2 = `date`
set dir0 = `pwd`
echo "Job started on $t1 and finished on $t2 at $dir0 " |mail -s "TOPS merge_batch job finished" y.f.tian@163.com


