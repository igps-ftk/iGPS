      subroutine read_tenv3(file,data,nrow,ncol)
c     Read NGL/UNR time series genereated by Geoff Blewitt.
c     Sample line:
c$$$  site YYMMMDD yyyy.yyyy __MJD week d reflon _e0(m) __east(m) ____n0(m) _north(m) u0(m) ____up(m) _ant(m) sig_e(m) sig_n(m) sig_u(m) __corr_en __corr_eu __corr_nu
c$$$  00NA 08MAR27 2008.2355 54552 1472 5  130.8   4781  0.498445  -1378706 -0.278810   104  0.839210  0.0000 0.000842 0.000745 0.003627  0.050600  0.024673  0.011391
c$$$  00NA 08MAR28 2008.2382 54553 1472 6  130.8   4781  0.502230  -1378706 -0.276082   104  0.846936  0.0000 0.000830 0.000734 0.003573  0.038030  0.020106 -0.022441

C     Input:
c     file:
c     data:
c     ---
      IMPLICIT NONE
      include '../../inc/cgps.h'
      character*(*) file
      integer*4 nrow,ncol
      real*8 data(nmax_row,nmax_col_else),tmpval
      integer*4 n,j,i,tmpn
c     ---
      integer*4 fid
      character*1024 bufline,tmpstrs(100),tmpstr,tmpstr2
c      character*3 mon(12),
      character*3 monstr
      character*36 mon
      character*7 ymdstr
      character*4 site
      integer*4 yy,mm,dd,year,doyr,ymd(3)
c      data mon/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP'
c     &     ,'OCT','NOV','DEC'/
      integer*4 nblen
C     ---
c     >>
      
      mon='JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'
      fid=90
c      write(*,*) file
      open(unit=fid,file=file)
c     skip the first line (header)
      read(fid,'(a1024)') bufline
      nrow=1
 800  read(fid,'(a1024)',end=899) bufline
c      write(*,*) bufline(1:nblen(bufline))
c      read(bufline,*) site,ymdstr,(data(nrow,j),j=1,3)
c      write(*,'(3e20.12)') (data(nrow,j),j=1,3)
      call strsplit(bufline,' ',ncol,tmpstrs)
c      write(*,*) 'ncol:',ncol
c      site=tmpstrs(1)
c      ymdstr=tmpstr(2)
      read(bufline,*) site,ymdstr,data(nrow,1),
     +     (data(nrow,j),j=4,ncol)
      year=int(data(nrow,1))
      data(nrow,2)=year
      read(ymdstr,'(i2,a3,i2)') yy,monstr,dd
      tmpn=index(mon,monstr)
      mm=tmpn/3+1

      ymd(1)=year
      ymd(2)=mm
      ymd(3)=dd
      call ymd_to_doy(ymd,doyr)
      data(nrow,3)=doyr


c      read(tmpstrs(6),'(a2)') tmpstr
c      read(tmpstr,*) data(nrow,4)
c      write(*,*) (data(nrow,j),j=1,4)
c      read(tmpstrs(6),'(a2,a3)') tmpstr,monstr
c      read(monstr,*) data(nrow,5)
c      write(*,*) (data(nrow,j),j=1,5)
c      read(tmpstrs(6),'(a2,a3,)') tmpstr,monstr,tmpstr2
c      read(tmpstr2,*) data(nrow,6)
c      write(*,'(3e20.12)') (data(nrow,j),j=1,6)
c      tmpstr=tmpstrs(6)
c      read(tmpstr(1:2),*) data(nrow,4)
c      read(tmpstr(3:5),*) monstr
c      tmpn=index(mon,monstr)
c      data(nrow,5)=tmpn/3+1
c      read(tmpstr(6:8),*) data(nrow,6)
c      write(*,*) tmpstrs(6)
c      write(*,'(6f20.8)') (data(nrow,j),j=1,6)
      nrow=nrow+1
      goto 800
 899  continue
      close(fid)
c      ncol=6
      nrow=nrow-1
      end
