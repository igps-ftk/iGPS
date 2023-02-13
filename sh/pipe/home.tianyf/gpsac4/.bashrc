# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
export PATH=$PATH:~/gg/com:~/gg/gamit/bin:~/gg/kf/bin
export INSTITUTE=TYF
export HELP_DIR=~/gg/help/
export PATH=$PATH:~/iGPS/sh:~/iGPS/ftk/bin/CYGWIN_NT-6.1-WOW_unknown
export PATH=$PATH:~/iGPS.addon/sh
export PATH=/usr/local/GMT-6.0.0/bin:$PATH
export MANPATH=/usr/local/GMT-6.0.0/share/man:$MANPATH
export esa_data=/g4d/esa.data
#export GMT5SAR=/usr/local/gmt5sar20200529
#export GMT5SAR=/usr/local/gmt5sar20210607i
export GMT5SAR=/usr/local/gmt5sar20220909

export MCF=--mcf


export PATH=${GMT5SAR}/bin:${PATH}
export GMT_pub=/g6f/tianyf/GMT_pub

export esa_data=/g4d/esa.data
#. /composer_xe_2015.3.187/bin/compilervars.sh intel64

. /usr/local/idl71/idl/bin/idl_setup.bash
. /usr/local/idl71/envi/bin/envi_setup.bash
export IDL_PATH='<IDL_DEFAULT>:+/home/tianyf/iGPS'

export IGS_ROOT=/g3c/pub
export IGS_ROOT=/g10a/ftp/pub
#export NINH_HFILE=/g3d/FTP/pub/hfilesi.v21
#export NINH_HFILE=/g4b/gsar/hfilesi.gmf.v3
export NINH_HFILE=/g14f/gsar/FTP/pub/hfilesi.v21b/

export PATH=${PATH}:/usr/local/MATLAB/R2012a/bin/

export PATH=$PATH:${HOME}/wafeng/Programs/fls
export PATH=$PATH:${HOME}/wafeng/Programs/MY_SCR/
export PYTHONPATH=${PYTHONPATH}:${HOME}/wafeng/Programs/MY_SCR:${HOME}/wafeng/Programs:${HOME}/wafeng/Programs/MY_PYTHON/MY_SEIS/:${HOME}/wafeng/Programs/MY_PYTHON/PYTHONPATH

export PATH=$PATH:/opt/anaconda3/bin/

export WGETRC=${HOME}/iGPS.addon/sh/wget.conf

. /opt/goa-6.0/rc_gipsy.sh

. ${HOME}/iGPS/sh/igps_alias.sh

## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$('/sar/software/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    eval "$__conda_setup"
#else
#    if [ -f "/sar/software/anaconda3/etc/profile.d/conda.sh" ]; then
#        . "/sar/software/anaconda3/etc/profile.d/conda.sh"
#    else
#        export PATH="/sar/software/anaconda3/bin:$PATH"
#    fi
#fi
#unset __conda_setup
## <<< conda initialize <<<

