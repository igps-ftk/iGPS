      subroutine read_l_l_r(file,data,nrow,ncol)
C     Input:
c     file:
c     data:
c     ---
      IMPLICIT NONE
      include '../../../inc/ftk.h'
      character*(*) file
      integer*4 nrow,ncol
      real*8 data(nmax_row,nmax_col),tmpval
      integer*4 n,j,i,tmpn
c     ---
      integer*4 fid
      character*512 bufline,tmpstrs(100),tmpstr,tmpstr2
c      character*3 mon(12),
      character*3 monstr
      character*36 mon
c      data mon/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP'
c     &     ,'OCT','NOV','DEC'/
C     ---
c     >>
      
      mon='JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'
      fid=90
c      write(*,*) file
      open(unit=fid,file=file)
      nrow=1
 800  read(fid,'(a512)',end=899) bufline
c      write(*,'(a80)') bufline
      read(bufline,*) (data(nrow,j),j=1,3)
c      write(*,'(3e20.12)') (data(nrow,j),j=1,3)
      call strsplit(bufline,' ',ncol,tmpstrs)
c      read(tmpstrs(6),'(a2)') tmpstr
c      read(tmpstr,*) data(nrow,4)
c      write(*,*) (data(nrow,j),j=1,4)
c      read(tmpstrs(6),'(a2,a3)') tmpstr,monstr
c      read(monstr,*) data(nrow,5)
c      write(*,*) (data(nrow,j),j=1,5)
c      read(tmpstrs(6),'(a2,a3,)') tmpstr,monstr,tmpstr2
c      read(tmpstr2,*) data(nrow,6)
c      write(*,'(3e20.12)') (data(nrow,j),j=1,6)
      tmpstr=tmpstrs(6)
      read(tmpstr(1:2),*) data(nrow,4)
      read(tmpstr(3:5),*) monstr
      tmpn=index(mon,monstr)
      data(nrow,5)=tmpn/3+1
      read(tmpstr(6:8),*) data(nrow,6)
c      write(*,*) tmpstrs(6)
c      write(*,'(6f20.8)') (data(nrow,j),j=1,6)
      nrow=nrow+1
      goto 800
 899  continue
      close(fid)
      ncol=6
      nrow=nrow-1

      if (nrow.gt.nmax_row) then
         write(*,'(2a,i6,a,i6,a)') '[read_l_l_r]ERROR: number of ',
     +        'lines (' ,nrow,
     +        ') exceeds program limit (',nmax_row,')!!!'
         write(*,'(17x,2a)') 'Please edit the nmax_row item in',
     +        ' $GPSF/inc/cgps.h .'
         stop
      endif

      end
