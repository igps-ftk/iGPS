c      program data_read_sio
      subroutine read_sio_bin(file, data, nrow, ncol, nhead, headers)

C     Inputs:
c     file: character*256
c     firstepoch: int(2)
c     lastepoch: int(2)
c     xyzref: double(3)
c     neuref: double(3)
c     headers: character*256 n
c     data: double (M*N)

      include '../../inc/cgps.h'

      character*(*) file
      character*1000 buf,hfile

      integer*4 fid, ioerr
      integer*4 nrow, ncol, nhead, nmax_tmp
 
      character*(*) headers(nmax_head)
      real*8 data(nmax_row, nmax_col),tmpdata(nrow,ncol)

      integer*4 i,j
      integer nblen


      nhead=100
c      call file_info (file, nrow, ncol, nhead, headers)
      call desuffix(file,hfile)
      hfile=hfile(1:nblen(hfile))//'.hdr'
      call read_sio_hdr(hfile, nhead, headers, nmax_tmp, nrow, ncol)
c      
c      write(*,*) nrow, ncol,nhead

      fid=90
      open(unit=fid,file=file,form='unformatted',recl=8,access='direct')     
      do i=1, nrow
         do j=1,ncol
            read(fid,rec=(i-1)*ncol+j) data(i,j)
         enddo
c         write(*,'(9f11.5)') (data(i,j),j=1,ncol)
      enddo
      goto 90
C     the below code is not correct, why?
c$$$      open(unit=fid,file=file,form='unformatted',
c$$$     &     recl=8*ncol*nrow,access='direct')
c$$$      read(fid,rec=1) tmpdata
c$$$      do i=1,nrow
c$$$         do j=1,ncol
c$$$            data(i,j)=tmpdata(i,j)
c$$$         enddo
c$$$         write(*,'(9f11.5)') (data(i,j),j=1,ncol)
c$$$      enddo

 90   continue
      close(fid)
      return
      end


