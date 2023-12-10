      subroutine write_sio(file,data,nrow,ncol,headers,nhead)
c     ---
      implicit none
      include '../../../inc/ftk.h'
c     --
      integer*4 nrow,ncol,nhead
      character*(*) file,headers(nhead)
      real*8 data(nmax_row,nmax_col)
c     --
      integer*4 i,j,k,fid,ioerr
      integer*4 year,doy
      integer*4 dt
      character*1000 tmpstr
      integer nblen
c     dt - data type
c      +)Residual time series (format 700)
c      +)NEU time series (format 701)
c      +)RAW time series (format 702)
c     ---
      dt=701
      call getlun(fid)
      open(unit=fid,file=file,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) 'write_sio: open output file error:',
     .        file(1:nblen(file))
         stop
      endif

c     write header
      do i=1,nhead
         tmpstr=headers(i)
         write(fid,'(a)') tmpstr(1:nblen(tmpstr))
      enddo

c     write data block
      do i=1,nrow
         year=data(i,2)
         doy=data(i,3)
         if (dt.eq.700) then
            write(fid,700) data(i,1),year,doy,(data(i,j),j=4,ncol)
         else if (dt.eq.701) then
            write(fid,701) data(i,1),year,doy,(data(i,j),j=4,ncol)
         else if (dt.eq.702) then
            write(fid,702) data(i,1),year,doy,(data(i,j),j=4,ncol)
         endif
      enddo
 700  format(f9.4,i5,i4,6f11.5)
 701  format(f11.6,i5,i4,3f15.5,3f11.5)
 702  format(f11.6,i5,i4,3f20.5,3f11.5)
      close(fid)
c      write(*,*) 'ofid:',fido
      end
