      subroutine write_qob(file,data,nrow,ncol,ll,site)
c     ---
      implicit none
      include '../../inc/cgps.h'
c     --
      character*(*) file
      character*(*) site
      integer*4 nrow,ncol
      real*8 data(nmax_row,nmax_col),ll(2)
c     --
      integer*4 fid,ioerr
      integer*4 i,j
c     ---
      fid=31
      call getlun(fid)
      write(*,*) fid
      if (ncol.ne.7) then
         write(*,*) 'Wrong column size:',ncol
         stop
      endif
      call uppers(site)
      open(unit=fid,file=file)
      write(fid,700) site,ll
 700  format(a4,"_GPS",2f10.5)
      do i=1,nrow
         write(fid,701) (data(i,j),j=1,ncol)
      enddo
 701  format(f11.6,6f10.5)
      close(fid)

      return
      end
