# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
PATH=${PATH}:${HOME}/gg/com:${HOME}/gg/gamit/bin:${HOME}/gg/kf/bin; export PATH
PATH=${PATH}:${HOME}/iGPS/sh:${HOME}/iGPS/ftk/bin/Linux_x86_64; export PATH

esa_data=/g8g/esa.data/; export esa_data
esa_unzip=/g11e/esa_unzip; export esa_unzip
PATH=${PATH}:${esa_data}/sh; export PATH

export GMT=/usr/local/GMT-6.0.0
export PATH=${PATH}:$GMT/bin
#export PATH=${PATH}:${HOME}/this_gmt/bin

#. /composer_xe_2015.3.187/bin/compilervars.sh intel64

#export GMT5SAR=/usr/local/gmt5sar20170823i
#export GMT5SAR=/usr/local/gmt5sar20210607i
export GMT5SAR=/usr/local/gmt5sar20220909


export PATH=${PATH}:${GMT5SAR}/bin



export GAMMA_HOME=/usr/local/GAMMA_SOFTWARE-20151209
export MSP_HOME=$GAMMA_HOME/MSP
export ISP_HOME=$GAMMA_HOME/ISP
export DIFF_HOME=$GAMMA_HOME/DIFF
export DISP_HOME=$GAMMA_HOME/DISP
export LAT_HOME=$GAMMA_HOME/LAT
export IPTA_HOME=$GAMMA_HOME/IPTA
export GEO_HOME=$GAMMA_HOME/GEO
export GDFONTPATH=/usr/share/fonts/truetype/msttcorefonts

#then update the path
export PATH=$PATH:\
$DISP_HOME/bin:$MSP_HOME/bin:$ISP_HOME/bin:$DIFF_HOME/bin:$LAT_HOME/bin:$IPTA_HOME/bin:\
$GEO_HOME/bin:$MSP_HOME/scripts:$ISP_HOME/scripts/:$DIFF_HOME/scripts:$LAT_HOME/scripts:$IPTA_HOME/scripts
#

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GAMMA_HOME/lib:/usr/local/lib		 #Linux

export GNUTERM=x11

export igps_arp=/g8n/arp

export PATH=/home/tianyf/.PRIDE_PPPAR_BIN:$PATH
export LD_LIBRARY_PATH=/home/tianyf/.PRIDE_PPPAR_BIN:$LD_LIBRARY_PATH

#IDL
. /usr/local/idl71/idl/bin/idl_setup.bash
. /usr/local/idl71/envi/bin/envi_setup.bash

export IGS_ROOT=/g8b/gsar/igs

export PATH=${PATH}:/usr/local/MATLAB/R2012a/bin/

export PATH=$PATH:${HOME}/wafeng/Programs/fls
export PATH=$PATH:${HOME}/wafeng/Programs/MY_SCR/
export PYTHONPATH=${PYTHONPATH}:${HOME}/wafeng/Programs/MY_SCR:${HOME}/wafeng/Programs:${HOME}/wafeng/Programs/MY_PYTHON/MY_SEIS/:${HOME}/wafeng/Programs/MY_PYTHON/PYTHONPATH

export PATH=/usr/local/anaconda3/bin/:$PATH
export GMTSAR_HOME=$GMT5SAR


export HOST=g8

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$('/usr/local/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    eval "$__conda_setup"
#else
#    if [ -f "/usr/local/anaconda3/etc/profile.d/conda.sh" ]; then
#        . "/usr/local/anaconda3/etc/profile.d/conda.sh"
#    else
#        export PATH="/usr/local/anaconda3/bin:$PATH"
#    fi
#fi
#unset __conda_setup
# <<< conda initialize <<<


