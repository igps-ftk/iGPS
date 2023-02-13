# .bashrc

# User specific aliases and functions

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

export PATH=${PATH}:/usr/local/slurm/sbin:/usr/local/slurm/bin

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
