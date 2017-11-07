c      program write_sio_hdr
      subroutine write_sio_hdr(file,nhead,headers,nmax,nrow,ncol)

C     Inputs:
c     file: character*256
c     firstepoch: int(2)
c     lastepoch: int(2)
c     xyzref: double(3)
c     neuref: double(3)
c     headers: character*256 n
c     data: double (M*N)

      implicit none

      character*(*) file
      character*1000 buf

      integer*4 fid,ioerr
      integer*4 nhead,nrow,ncol,nmax,i
 
      character*(*) headers(nmax)

      fid=90
      open(unit=fid,file=file)

      do i=1,nhead
         write(fid,'(a80)') headers(i)
      enddo

      write(fid,'("#",A12,":",i10)') 'ROW',nrow
      write(fid,'("#",A12,":",i10)') 'COL',ncol
 90   continue
      close(fid)
      return
      end


