c      program neuread

      subroutine  neuread(file,ts,n)
c     +
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
c     -
c

c     variables
c     -->

      character*256 file
      character*128 strbuf

      integer*4 NMAX
      parameter (NMAX=6000)
c     It is assumed that the maximum length of CGPS time series does
c     not exceed 6000 as of now.
      real*8 ts(NMAX,9)

      integer*4 i,n,j

      integer*4 fid,ioerr

      integer*4 yr,doy
      
c     <--


c     executables
c     -->

c     for debug
c     |->
c      file='/home/tianyf/garner/rawNeuTimeSeries20060214/yellRaw.neu'
c     ->|

      i=1
      fid=10      
c     conventions:
c      10 - input file unit
c      11 - output file unit
c      10-19 - file units
c      20-29 - do loop
c      30-39 - goto lables
c      40-49 - for debug
c      50-59 - read formats
c      61-69 - write formats
      open(unit=fid,file=file)
 30   read(fid,'(a)',iostat=ioerr) strbuf
      if (ioerr.eq.-1)  go to 31
      if (strbuf(1:1).eq.'#') go to 30
      read(strbuf,*) (ts(i,j),j=1,9)
c      read(strbuf,50) ts(i,1),yr,doy,(ts(i,j),j=4,9)
c 50   format(f9.4,i5,i4)
c 2005.8397 2005 307  0.0015  0.0040  0.0045  0.0047  0.0034  0.0064
c 1991.0562 1991 021 -2493303.6159 -4655215.2507 3565497.4403  3.1722  2.4056  2.7585

c      write(*,'(9f12.4)') (ts(i,j),j=1,9)
      i=i+1
      go to 30
 31   continue
      close(fid)

      n=i-1
c      write(*,*) "number of ovservations:",n,ts(n,1)
c     <--

      return
      end
