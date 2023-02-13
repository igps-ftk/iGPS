
alias h         history 25
alias j         jobs -l
alias la        ls -a
alias lf        ls -FA
alias ll        ls -lA
alias lh	ls -lh
alias cp	cp -p
alias l		ls -l
alias lt	ls -lsrt
alias dh	df -h
alias ds	du -hs

alias ml	matlab -nojvm -nodisplay -nosplash

# A righteous umask
umask 22

#for gpsmet:dc
#setenv GPSMET_DC_HOME /export/home/tianyf/dc/gpsmet
#setenv GPSMET_DC_DATA /data0/igs0/pub
#set path = ($path ${GPSMET_DC_HOME}/sh)

#for gpsmet:ac
#setenv GPSMET_AC_HOME /export/home/tianyf/gpsf/external/gpsmet_ac/
#setenv GPSMET_AC_PROC /export/home/tianyf/gpse/ac/proc
#set path = ($path $GPSMET_AC_HOME/sh)

#IDL
source /usr/local/idl71/idl/bin/idl_setup
source /usr/local/idl71/envi/bin/envi_setup
setenv IDL_PATH '<IDL_DEFAULT>:+/home/tianyf/iGPS'

#setenv SACAUX /usr/local/sac_v59.48/aux
#set path = ($path /usr/local/sac_v59.48/bin)

#source /opt/intel/fce/10.0.023/bin/ifortvars.csh
#source /opt/intel/cce/10.0.023/bin/iccvars.csh

#source /opt/intel/fce/10.1.026/bin/ifortvars.csh
#source /opt/intel/cce/10.1.026/bin/iccvars.csh


#for intel 11.1 compilers (with mkl?)
#source /opt/intel/Compiler/11.1/073/bin/iccvars.csh intel64
#source /opt/intel/Compiler/11.1/073/bin/ifortvars.csh intel64

#To get started using Intel(R) Composer XE 2011 Update 9 located in
#/opt/intel/composer_xe_2011_sp1.9.293: 
#- Set the environment variables for a terminal window using one of the following
#  (replace "intel64" with "ia32" if you are using a 32-bit platform).
#     For csh/tcsh:
#        $ source install-dir/bin/compilervars.csh intel64
#source /opt/intel/composerxe/bin/compilervars.csh intel64
#source /opt/intel/composer_xe_2011_sp1.9.293/bin/ifortvars.csh intel64
#     For bash:
#        $ source install-dir/bin/compilervars.sh intel64
#     To invoke the installed compilers:
#        For C++: icpc
#        For C: icc
#        For Fortran: ifort
#  To get help, append the -help option or precede with the man command.
#- To view a table of getting started documents: 
#  install-dir/Documentation/en_US/documentation_f.htm.
#source /composer_xe_2015.3.187/bin/compilervars.csh intel64



set path = (~/gg/com ~/gg/gamit/bin ~/gg/kf/bin $path)
setenv HELP_DIR ~/gg/help/
setenv INSTITUTE TYF

setenv igps_arp /sar/proc_gmtsar/arp

#-----------------------------------------------------------------------
#For csh or tcsh users:
#setenv NETCDFHOME /usr/local/netcdfi
#set path=(/usr/local/GMT4.5.0/bin $path)
#set path=(/usr/local/GMT4.5.3/bin $path)
#set path=(/home/tianyf/this_gmt/bin $path)
set GMT=/usr/local/GMT-6.0.0
#set GMT=/usr/local/GMT-6.0.0i

set path=(${GMT}/bin $path)


#For sh or bash users:
#export NETCDFHOME=/usr/local/netcdfi
#export PATH=/usr/local/GMT4.5.0/bin:$PATH

#For all users:
#Add /usr/local/GMT4.5.0/man to MANPATH
#Add /usr/local/GMT4.5.0/share/html/gmt_services.html as browser bookmark


#set path = ($path /usr/local/qoca/bin /usr/local/qoca)

#for gipsy
#source /opt/goa-6.0/rc_gipsy.csh
#setenv JPL_EPH_LEGACY /acd2/gipsy.orbit/sideshow.jpl.nasa.gov/pub/gipsy_products
#setenv JPL_EPH /acd2/gipsy.orbit/sideshow.jpl.nasa.gov/pub/JPL_GPS_Products/Final

#for gpsf
set path = (${HOME}/iGPS/sh ${HOME}/iGPS/ftk/bin/Linux_x86_64 $path)
set path = (${HOME}/iGPS.addon/sh ${HOME}/iGPS.addon/ftk/bin/Linux_x86_64 $path)
source ${HOME}/iGPS/sh/igps_alias.csh


if !($?MANPATH) then
    #setenv MANPATH /usr/local/GMT4.5.0/man:~/gpsf/cgps/man:`manpath`
    #setenv MANPATH /usr/local/GMT4.5.3/man:~/gpsf/cgps/man:`manpath`
    setenv MANPATH ${GMT}/share/man:~/gpsf/cgps/man:`manpath`
else
    #setenv MANPATH /usr/local/GMT4.5.0/man:~/gpsf/cgps/man:${MANPATH}
    #setenv MANPATH /usr/local/GMT4.5.3/man:~/gpsf/cgps/man:${MANPATH}
    setenv MANPATH ${GMT}/share/man:~/gpsf/cgps/man:${MANPATH}
endif


#set path = ($path /usr/local/agnew/PIASD/bin)

#for grads 2
#set path = ($path /usr/local/grads-2.0.a3/bin)

#for gldas data
#setenv GRIBTAB ~/gpsf/cgps/conf/gribtab.noah
#setenv GRIBTAB ~/gpsf/cgps/conf/gribtab_VIC.txt
#setenv GRIBTAB /home/tianyf/phd.dissertation/data/chp.6.non-tectonic.origins/mass/gldas/gribtab-VIC.txt

unlimit stacksize

#for JDK 1.5
#set path = (/usr/java/jdk1.5.0_17/bin/ $path)

#for PGI
#source /opt/pgi/linux86-64/7.1/mpi.csh
#source /opt/pgi/linux86-64/7.1/pgi.csh

#for Wang RongJiang's Code
#set path = ($path /opt/wang/bin)

#for Ultraedit
#set path = ($path /usr/local/uex/bin/)

#set path = ($path /usr/lib64/openmpi/1.4-gcc/bin/)

#set path = ($path /usr/lib64/qt4/bin)

set path = ($path /usr/local/slurm/bin)

#set path = ($path /usr/local/eqinv)
 
#setenv TD_HOME ~/TDEFNODE
#set path = ($path $TD_HOME)

#GMTSAR
#source /usr/local/GMTSAR/gmtsar_config_linux
#GMT5SAR
#setenv GMT5SAR /usr/local/GMT5SAR
#setenv GMT5SAR /usr/local/GMT5SAR.20161218
#setenv GMT5SAR /usr/local/gmt5sar20170330T
#setenv GMT5SAR /usr/local/gmt5sar20170330Ti2
#setenv GMT5SAR /usr/local/gmt5sar20170622i
#setenv GMT5SAR /usr/local/gmt5sar20170703i
#setenv GMT5SAR /usr/local/gmt5sar20170725i
#setenv GMT5SAR /usr/local/gmt5sar20170823
#setenv GMT5SAR /usr/local/gmt5sar20170823
##setenv GMT5SAR /usr/local/gmt5sar20170823i
#setenv GMT5SAR /usr/local/gmt5sar20170823t
#setenv GMT5SAR /usr/local/gmt5sar20180206
#setenv GMT5SAR /usr/local/gmt5sar20180223i
#setenv GMT5SAR /usr/local/gmt5sar20180517i
#setenv GMT5SAR /usr/local/gmt5sar20190326i
#setenv GMT5SAR /usr/local/gmt5sar20200529
#setenv GMT5SAR /usr/local/gmt5sar20200529i
setenv GMT5SAR /usr/local/gmt5sar20210607i


setenv MCF --mcf



set path = ($GMT5SAR/bin $path)

setenv esa_data /g4d/esa.data
#set path = ($path ${esa_data}/sh)
#setenv esa_unzip /acd0/esa_unzip/
#setenv esa_unzip /acd1/esa_unzip/
#setenv esa_unzip /g6g/esa_unzip/
setenv esa_unzip /sar/tmp_esa_unzip/

setenv ALOS_DATA /g6f/sar.alos
setenv 	EOLISA /g6f/sar.eolisa

#set path = ($path /usr/local/ffmpeg/bin)

#set path = ($path /usr/local/doris/bin)
#setenv PYTHONPATH /usr/local/doris/doris

setenv GMT_pub /g6f/tianyf/GMT_pub

set path = ($path /usr/local/MATLAB/R2012a/bin/)

set path = ($path ${HOME}/wafeng/Programs/fls)
set path = ($path ${HOME}/wafeng/Programs/MY_SCR/)
setenv PYTHONPATH ${HOME}/wafeng/Programs/MY_SCR:${HOME}/wafeng/Programs:${HOME}/wafeng/Programs/MY_PYTHON/MY_SEIS/:${HOME}/wafeng/Programs/MY_PYTHON/PYTHONPATH

set path = ($path /sar/software/anaconda3/bin/)
setenv GMTSAR_HOME $GMT5SAR


#setenv NINH_HFILE /g3d/FTP/pub/hfilesi.v21
setenv NINH_HFILE /sar/gnss/hfilesi.v21b
setenv IGS_ROOT /sar/gnss

if ($?prompt) then
        # An interactive shell -- set some stuff up
        set filec
        set history = 100
        set savehist = 100
        set mail = (/var/mail/$USER)
        if ( $?tcsh ) then
                bindkey "^W" backward-delete-word
                bindkey -k up history-search-backward
                bindkey -k down history-search-forward
        endif
endif


## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
#if ( -f "/sar/software/anaconda3/etc/profile.d/conda.csh" ) then
#    source "/sar/software/anaconda3/etc/profile.d/conda.csh"
#else
#    setenv PATH "/sar/software/anaconda3/bin:$PATH"
#endif
## <<< conda initialize <<<

