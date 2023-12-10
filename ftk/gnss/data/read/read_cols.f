      subroutine read_cols(file,data,nrow,ncol,headers,nhead,cmt)
c                read_cols(file,data,nrow,ncol,headers,nhead,cmt)

C     Inputs:
c     file: character*256
c     firstepoch: int(2)
c     lastepoch: int(2)
c     xyzref: double(3)
c     neuref: double(3)
c     headers: character*256 n
c     data: double (M*N)
c     
c     Bugs:
c     + Tian Nov-03-208
c       When nhead is zero, the returned data matrix contains errors. The
c       first record was discarded; and the last line is blank or not right.

c     
      implicit none
      include '../../../inc/ftk.h'
c     --

      character*(*) file
      character*1000 buf

      integer*4 fid,ioerr
      integer*4 nrow,ncol,nhead
 
      character*(*) headers(nmax_head),cmt
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
 
      nhead=100
      call finfo(file,nrow,ncol,nhead,headers,cmt)
c      nrow=nrow-nhead-1
c     MOD:Tian:JUL26/07:What about the last null (with nothing) line.
      nrow=nrow-nhead
c      write(*,*) nrow, ncol,nhead

      call getlun(fid)
      open(unit=fid,file=file,status='old') 

c     to fix bug.nov.03.2008:
      if (nhead.eq.0) goto 801

c     skip header lines
      i=0
 800   read(fid,'(a1000)',iostat=ioerr,end=899) buf
      i=i+1
c      if (buf(1:1).eq.'#') then
c         write(*,*) buf(1:nblen(buf))
      if (i.ge.nhead) goto 801
      goto 800
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
      do i=1,nrow
c         write(*,*) i
         read(fid,*,end=899) (data(i,j),j=1,ncol)
c         write(*,'(9f10.4)') (data(i-1,j),j=1,ncol)
c         write(*,*) data(i-1,5)
      enddo
      
c      do i=nrow-5,nrow
c      	write(*,*) 'line',i,(data(i,j),j=1,ncol)
c      enddo
c      goto 30
 899  continue
      close(fid)

c      do i=1,nrow
c         write(*,*) data(i,5)
c         write(*,'(9f10.4)') (data(i,j),j=1,ncol)
c      enddo

      return
      end


