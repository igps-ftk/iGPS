      subroutine read_load_ref(file,data,nmaxrow,nmaxcol,nrow,ncol)

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
c      include '../../inc/cgps.h'
c     --

      character*(*) file
      character*1000 buf

      integer*4 fid,ioerr
      integer*4 nrow,ncol,nmaxrow,nmaxcol
 
      real*8 data(nmaxrow,nmaxcol),tmp
      integer*4 nheadt,nrowt,ncolt
      character*1023 headers(100),tfile

      integer*4 i,j,ilat,ilon
      integer*4 nblen

      
c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/'//
c     .  'reason/sopac/cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/reason/sopac/'//
c     . 'cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='e:\\tmp\\bjfsCleanUnf.neu'
c      write(*,*) file
 
      nheadt=100
c      tfile=file
      call file_info(file,nrowt,ncolt,nheadt,headers)
c      nrow=nrow-nhead-1
c     MOD:Tian:JUL26/07:What about the last null (with nothing) line.
      nrowt=nrowt-nheadt
c      write(*,*) nrowt, ncolt,nheadt

      call getlun(fid)
      open(unit=fid,file=file,status='old')  
      i=0   
      if (nheadt.eq.0) goto 801
c     MOD Tian Jun-23-2008
c       Fixed a bug when there are no header lines.
 30   read(fid,'(a1000)', iostat=ioerr, end=90) buf
      i=i+1
c      if (buf(1:1).eq.'#') then
c         write(*,*) buf(1:nblen(buf))
      if (i.ge.nheadt) goto 801
      goto 30
c      endif
c$$$      if (index(bufline,'END OF HEADER').gt.1) then
c$$$         write(*,*) 'end of header, data section start'
c$$$         goto 32
c$$$      endif
801   continue
c       write(*,*) buf(1:nblen(buf))
c      strsplit(buf,' ',n,strs)

c     the below code cannot work for files converted by IDL; however,
c     it do work for the origional copy.
c      read(fid,*), ((data(i,j),j=1,ncol),i=1,nrow)

c     The following line-by-line reading works for both types of data.


c     read the data line by line
      do i=1,nrowt
c         write(*,*) i
         read(fid,*,end=90) ilat,ilon,tmp
         data(ilat,ilon)=tmp
c         write(*,'(9f10.4)') (data(i,j),j=1,ncol)
      enddo
      
c      do i=nrow-5,nrow
c      	write(*,*) 'line',i,(data(i,j),j=1,ncol)
c      enddo
c      goto 30
 90   continue
      close(fid)

      nrow=ilat
      ncol=ilon

      return
      end
