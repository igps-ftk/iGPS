      subroutine read_l_l_r_panga(file,data,nrow,ncol,nhead,headers)
C     Input:
c     file:
c     data:
c     ---
      IMPLICIT NONE
      include '../../inc/cgps.h'
      character*(*) file
      integer*4 nrow,ncol,nhead
      real*8 data(nmax_row,nmax_col),tmpval
      integer*4 n,j,i,tmpn
c     ---
      integer*4 fid,ioerr
      character*512 bufline,tmpstrs(100),tmpstr,tmpstr2
c      character*3 mon(12),
      character*3 monstr
      character*36 mon

      character*(*) headers(nmax_head)
C     ---
c     >>
      
      call getlun(fid)
c      write(*,*) file
      open(unit=fid,file=file)
c     skip comment lines
      nhead=0
 801  read(fid,'(a512)',end=899,iostat=ioerr) bufline
c     Tian Mod APR-23-2008
c     There might be no data lines. Thus, "end=N" should present.
c     The above still not working.
      if (ioerr.ne.0) then
         write(*,*) '!!Error in reading data.'
         goto 899
      endif

      if (bufline(1:1).ne.'#') then
         nrow=1
         goto 803
      endif
      nhead=nhead+1
      headers(nhead)=bufline
      goto 801

 800  read(fid,'(a512)',end=899) bufline
c      write(*,'(a80)') bufline
      nrow=nrow+1
 803  read(bufline,*) (data(nrow,j),j=1,3)
c      write(*,'(3e20.12)') (data(nrow,j),j=1,3)
      call strsplit(bufline,' ',ncol,tmpstrs)
c      read(tmpstrs(6),'(a2)') tmpstr
c$$$      read(tmpstr,*) data(nrow,4)
c$$$c      write(*,*) (data(nrow,j),j=1,4)
c$$$c      read(tmpstrs(6),'(a2,a3)') tmpstr,monstr
c$$$c      read(monstr,*) data(nrow,5)
c$$$c      write(*,*) (data(nrow,j),j=1,5)
c$$$c      read(tmpstrs(6),'(a2,a3,)') tmpstr,monstr,tmpstr2
c$$$c      read(tmpstr2,*) data(nrow,6)
c$$$c      write(*,'(3e20.12)') (data(nrow,j),j=1,6)
c$$$      tmpstr=tmpstrs(6)
c$$$      read(tmpstr(1:2),*) data(nrow,4)
c$$$      read(tmpstr(3:5),*) monstr
c$$$      tmpn=index(mon,monstr)
c$$$      data(nrow,5)=tmpn/3+1
c$$$      read(tmpstr(6:8),*) data(nrow,6)
c      write(*,*) tmpstrs(6)
c      write(*,'(6f20.8)') (data(nrow,j),j=1,6)
      goto 800
 899  continue
      close(fid)
c      ncol=6
c      nrow=nrow-1
      return
      end
