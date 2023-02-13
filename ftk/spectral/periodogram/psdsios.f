c+
c Name:
c     psdsios
c
c
c Purpose:
c     Calcualte periodogram for time series
c
c
c Inputs:
c     + path: input path
c       site position time series (each component: north, east, up)
c       NEU or XYZ
c     + opath: output path
c       text output (for further analysis: noise amplitude)
c       postscript plots (using GMT, it may be better to use C Sell script to plot graphs :) )
c     + ptype:
c       0 - classical periodogram (not evenly sampled, interpolation should be used)
c       1 - refined periodogram (Scargle, 1982)
c
c     site position time series (each component: north, east, up)
c     NEU or XYZ
c
c NEU file format:
c # Site : ac15 Created : Thu Feb  9 15:26:09 2006
c # Summary file : cats060209.sum
c # Type of file : raw
c # Latitude : 60.481333
c # Longitude : 210.275994
c # Reference_X : -2720900.162771
c # Reference_Y : -1588436.766608
c # Reference_Z : 5527227.671147
c # Time Format  : 1
c # Yearly Signal : 0
c # Offsets read in by program
c # Note these may not necessarily be used in the processing
c 2005.8342 2005 305  0.0035  0.0034  0.0019  0.0045  0.0033  0.0061
c 2005.8370 2005 306  0.0010  0.0027  0.0079  0.0048  0.0035  0.0064
c 2005.8397 2005 307  0.0015  0.0040  0.0045  0.0047  0.0034  0.0064
c ...
c
c XYZ file format:
c 1991.0562 1991 021 -2493303.6159 -4655215.2507 3565497.4403  3.1722  2.4056  2.7585
c 1991.0589 1991 022 -2493303.9597 -4655215.5968 3565497.2614  0.0169  0.0266  0.0191
c ...
c
c
c Outputs:
c     +periodogram
c
c
c Dependancies:
c     +neuread
c
c     conventions:
c      10 - input file unit
c      11 - output file unit
c      10-19 - file units
c      20-29 - do loop
c      30-39 - goto lables
c      40-49 - for debug
c      50-59 - read formats
c      61-69 - write formats
c
c
c-



      program psdsios
C     Input:
c     path: input
c     opath: output


c     variables declaration
c     -->
      implicit none
      include '../../inc/cgps.h'
c     ---
      character*512 path,opath
c     ---
c     define the maximum number of observations
c      integer*4 NMAX_SITE
c      parameter(NMAX_SITE=3000)
      character*512 files(NMAX_SITE),filter

      integer*4 nrow,ncol,nhead,nfile
c     loop variable
      integer*4 i,j,k,l,m
c     classical periodogram: P(*) R*8
c      real*8 psd(NMAX_row)
      real*8 ts(NMAX_row),xs(nmax_row)
c     frequency
      real*8 freqs(nmax_row)
      integer*4 nfreq 

      real*8 pi
      parameter(pi=3.14159265D0)
      real*8 ti(NMAX_row)
      integer nblen
      character*256 headers(nmax_head)
      real*8 data(nmax_row,nmax_col),psd(nmax_row,3)

c     file names
      character*1024 file
      character*1024 ofile,tmpstr,tmpstr2
c     file untis
      integer*4 fid,ioerr

c     n/e/u components; time 
      integer*4 ind_t,ind_neu(3),neui,nneu
      character*1 neustr(3)
      data neustr /'N','E','U'/

      integer iargc

c     call period(ts,xs,n,ofac,hifac,px,py,np,nout,jmax,prob,nmaxp)
      INTEGER*4 jmax,n,nout,np,nmaxp
      REAL*8 hifac,ofac,prob,px(nmax_row),py(nmax_row),x(nmax_row),
     &     y(nmax_row)

      
c$$$      INTEGER NP,NPR
c$$$      REAL TWOPI
c$$$      PARAMETER(NP=20,NPR=11,TWOPI=6.2831853)
c$$$      INTEGER idum,jmax,nout
c$$$      REAL prob,x(nmax_row),y(nmax_row),px(2*nmax_row),py(2*Nmax_row)

c     <--
c     end of variables declaration
c     executable code begin
c     -->

      if (iargc().lt.2) then
         write(*,*) 'Usage: psdsios path opath [filter]'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
      nfile=0
      if (iargc().ge.3) then
         call getarg(3,filter)
      else
         filter='*.neu'
      endif
c      write(*,*) path(1:nblen(path)),filter
      call ffind(path,files,filter,nfile,1 ) 
      write(*,*) '#found sites:',nfile

      ind_t=1
      ind_neu(1)=4
      ind_neu(2)=5
      ind_neu(3)=6
c     for cats format
c      ind_neu(1)=2
c      ind_neu(2)=3
c      ind_neu(3)=4
      nneu=3
c     default N/E/U has 3 levels

c     get output file fid
      call getlun(fid)

      do i=1,nfile
         nrow=nmax_row
         ncol=nmax_col
         nhead=nmax_head
         file=files(i)
         write(*,'(" << ",a)') file(1:nblen(file))
         call read_sio(file,data,nrow,ncol,nhead,headers)
c         write(*,*) 'read ok'
         do j=1,nrow
            ts(j)=(data(j,ind_t)-data(1,ind_t)+1d0/365.25)
     &           *365.25*24*3600d0
c            x(j)=ts(j)
c            if (j.lt.5) then
c               write(*,*) ts(j)
c            endif
c            stop
         enddo
         
         if (ncol.eq.5) then
            nneu=2
         elseif (ncol.eq.4) then
            nneu=1
         elseif (ncol.le.3) then
            write(*,*) 'error of format [NEU]: no data column.'
            stop
         endif
         do j=1,nneu
            neui=ind_neu(j)
            do k=1,nrow
               xs(k)=data(k,neui)
c               write(*,*) xs(k)
c               y(k)=xs(k)
*1000d0
c     convert lenght unit to meter (from km)
            enddo
         
c     calculate periodogram
c       Often, the raw time series contains data gaps.
c       Thus, we will always use refined periodogram algorithm [Scargle, 1982].
c     subroutine periodogram_refined(ti,xi,nmaxp,n0,pw,freqs,n)
c            write(*,*) 'calling period ...'
            ofac=1
            hifac=1
            call scargle(ts,xs,nrow,ofac,hifac,px,py,nmax_row, 
     &           nfreq,jmax,prob,nmaxp)
c            write(*,*) '  period ok.'
c            call periodogram_refined(
c     &           ts,xs,nmax_row,nrow,psd,freqs,nfreq)
c            call period(ts,xs,j,4.,1.,freqs,psd,
c     &           2*Nmax_row,nfreq,jmax,prob)
c            call period(x,y,nmax_row,4.,1.,px,py,
c     &           2*Nmax_row,nout,jmax,prob)
c            write(*,*) '#freq:',nfreq


c         write(*,*) 'total number of observation (N0):', nrow

c     output spectrum
           
            do k=1,nfreq
c               write(fid,'(E10.5,1x,f10.5)') px(k),py(k)
               psd(k,j)=py(k)
c               write(*,'(E10.5,1x,f10.5)') psd(k),freqs(k)
c               write(*,'(E10.5,1x,E10.5)') py(k),px(k)
c               write(fid,'(E10.5,1x,E10.5)') py(k),px(k)
            enddo
c            write(fid,'(E10.5,1x,E10.5)') 0,0

         enddo 
c     end-of-component-loop

c     output power spectrum to ascii file
         call getfilename(file,tmpstr)
         call desuffix(tmpstr,tmpstr2)         
         ofile=opath(1:nblen(opath))//pathsep//
c     &        ofile(1:nblen(ofile))//
     &        tmpstr2(1:nblen(tmpstr2))//".psd"
         write(*,'(" >>#obs:",i4,1x,a)') 
     &        nrow,ofile(1:nblen(ofile))

         open(unit=fid,file=ofile)
         do j=1,nfreq
            write(fid,801) px(j),(psd(j,k),k=1,nneu)
 801        format(e15.10,3(1x,f20.5))
         enddo
         close(fid)

c         stop     
      enddo
c     end-of-files-loop
     
      stop
      end
