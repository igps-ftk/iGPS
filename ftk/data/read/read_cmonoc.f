      subroutine read_cmonoc(file,data,nrow,ncol)

C     Inputs:
c     file: character*256
c     firstepoch: int(2)
c     lastepoch: int(2)
c     xyzref: double(3)
c     neuref: double(3)
c     headers: character*256 n
c     data: double (M*N)

c     
      implicit none
      include '../../inc/cgps.h'
c     --

      character*(*) file
      character*1000 buf
      character*100 headers(nmax_head)

      integer*4 fid,ioerr
      integer*4 nrow,ncol,nhead
 
      real*8 data(nmax_row,nmax_col)

      integer*4 i,j
      integer*4 nblen

      
c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/'//
c     .  'reason/sopac/cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/reason/sopac/'//
c     . 'cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='e:\\tmp\\bjfsCleanUnf.neu'
c      write(*,*) file
 
      nhead=0
      call file_info (file,nrow,ncol,nhead,headers)
      if (nrow.gt.nmax_row) then
         write(*,'(2a,i6,a,i6,a)') '[read_cmonoc]ERROR: number of ',
     +        'lines (' ,nrow,
     +        ') exceeds program limit (',nmax_row,')!!!'
         write(*,'(19x,2a)') 'Please edit the nmax_row item in',
     +        ' $GPSF/inc/cgps.h .'
         stop
      endif
c      nrow=nrow-nhead-1
c     MOD:Tian:JUL26/07:What about the last null (with nothing) line.
      nrow=nrow-nhead
c      write(*,*) nrow, ncol,nhead

      fid=90
      call getlun(fid)
      open(unit=fid,file=file)  
      i=0   
 
c     read the data line by line
      do i=1,nrow
c         write(*,*) i
         read(fid,*,end=90) buf,(data(i,j),j=1,ncol-1)
c         write(*,'(9f10.4)') (data(i,j),j=1,ncol)
      enddo
      
c      do i=nrow-5,nrow
c      	write(*,*) 'line',i,(data(i,j),j=1,ncol)
c      enddo
c      goto 30
 90   continue
      close(fid)
      ncol=ncol-1
      return
      end


