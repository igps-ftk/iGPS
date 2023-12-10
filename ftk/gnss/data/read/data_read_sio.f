c      program data_read_sio
      subroutine data_read_sio(file, data, nrow, ncol, nhead, headers)

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
      real*8 data(nrow, ncol)

      integer*4 i,j

      
c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/'//
c     .  'reason/sopac/cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/reason/sopac/'//
c     . 'cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='e:\\tmp\\bjfsCleanUnf.neu'
c      write(*,*) file
 
      nhead=100
      call file_info (file, nrow, ncol, nhead, headers)
      nrow=nrow-nhead-1
c      write(*,*) nrow, ncol,nhead

      fid=90
      open(unit=fid,file=file)     
 30   read(fid,'(a80)', iostat=ioerr, end=90) buf
      if (buf(1:1).eq.'#') then
c         write(*,*) buf
         goto 30
      endif
c$$$      if (index(bufline,'END OF HEADER').gt.1) then
c$$$         write(*,*) 'end of header, data section start'
c$$$         goto 32
c$$$      endif
c      write(*,*) buf
c      strsplit(buf,' ',n,strs)
      read(fid,*), ((data(i,j),j=1,ncol),i=1,nrow)
c      do i=nrow-5,nrow
c      	write(*,*) 'line',i,(data(i,j),j=1,ncol)
c      enddo
c      goto 30

 90   continue
      close(fid)
      return
      end


