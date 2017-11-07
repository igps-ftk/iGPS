      subroutine read_trk(file,data,nrow,ncol,nhead,headers)

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

      integer*4 fid,ioerr
      integer*4 nrow,ncol,nhead
 
      character*(*) headers(nmax_head)
      real*8 data(nmax_row_large,nmax_col)
      
      character*100 strs(100)

      integer*4 i,j
      integer*4 nblen

      
c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/'//
c     .  'reason/sopac/cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/reason/sopac/'//
c     . 'cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c     file='e:\\tmp\\bjfsCleanUnf.neu'
c      file='/home/tianyf/gpse/30s.bue1/bue2-00.trk'
c      write(*,*) file
 
c      nhead=100
c      call file_info (file,nrow,ncol,nhead,headers)
c      nrow=nrow-nhead-1
c     MOD:Tian:JUL26/07:What about the last null (with nothing) line.
c      nrow=nrow-nhead
c      write(*,*) nrow, ncol,nhead

c      fid=90
      nhead=0
      nrow=0
      ncol=0
      call getlun(fid)
      open(unit=fid,file=file)
c      i=0   
c      if (nhead.eq.0) goto 801
c     MOD Tian Jun-23-2008
c       Fixed a bug when there are no header lines.
 30   read(fid,'(a1000)', iostat=ioerr, end=90) buf
      if (buf(1:1).ne.' ') then
         nhead=nhead+1
         headers(nhead)=buf
      else
         nrow=nrow+1
         if (ncol.eq.0) then
            call strsplit(buf,' ',ncol,strs)
            if (ncol.gt.17) then
               ncol=17
            endif
         endif
         read(buf,*) (data(nrow,j),j=1,ncol)
c         write(*,'(17f10.4)') (data(nrow,j),j=1,ncol)
      endif
c      i=i+1
c      if (buf(1:1).eq.'#') then
c         write(*,*) buf(1:nblen(buf))
c      if (i.ge.nhead) goto 801
      goto 30
c      endif
c$$$      if (index(bufline,'END OF HEADER').gt.1) then
c$$$         write(*,*) 'end of header, data section start'
c$$$         goto 32
c$$$      endif
c801   continue
c       write(*,*) buf(1:nblen(buf))
c      strsplit(buf,' ',n,strs)

c     the below code cannot work for files converted by IDL; however,
c     it do work for the origional copy.
c      read(fid,*), ((data(i,j),j=1,ncol),i=1,nrow)
      
c     The following line-by-line reading works for both types of data.


c     read the data line by line
c      do i=1,nrow
c         write(*,*) i
c         read(fid,*,end=90) (data(i,j),j=1,ncol)
c         write(*,'(9f10.4)') (data(i,j),j=1,ncol)
c      enddo
      
c      do i=nrow-5,nrow
c      	write(*,*) 'line',i,(data(i,j),j=1,ncol)
c      enddo
c      goto 30
 90   continue
      close(fid)
      return
      end


