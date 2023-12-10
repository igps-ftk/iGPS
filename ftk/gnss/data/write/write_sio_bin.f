c      program write_sio_bin
      subroutine write_sio_bin(file, data, nrow, ncol, nhead, headers)

C     Inputs:
c     file: character*256
c     firstepoch: int(2)
c     lastepoch: int(2)
c     xyzref: double(3)
c     neuref: double(3)
c     headers: character*256 n
c     data: double (M*N)


      character*(*) file
      character*1000 buf

      integer*4 fid, ioerr
      integer*4 nrow, ncol, nhead
 
      character*(*) headers(nhead)
      real*8 data(10000, 30)
c      real*8 data(nrow,ncol)

      integer*4 i,j, currec

      
c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/'//
c     .  'reason/sopac/cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/reason/sopac/'//
c     . 'cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='e:\\tmp\\bjfsCleanUnf.neu'
c      write(*,*) file
 
    
c      write(*,*) nrow, ncol,nhead

      fid=90
      open(unit=fid,file=file,form="unformatted",
     &   recl=8, access="direct", status="replace")

c      write(*,*) nrow, ncol
      do i=1,nrow
         do j=1,ncol
            currec=(i-1)*ncol+j
            write(fid,rec=currec) data(i,j)
         enddo
c         write(*,*) (data(i,j),j=1,ncol)
      enddo

 90   continue
      close(fid)
      return
      end


