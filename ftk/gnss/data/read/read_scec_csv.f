      subroutine read_scec_csv(file,data,nrow,ncol,nhead,headers)

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
      include '../../../inc/ftk.h'
c     --

      character*(*) file
      character*1000 buf

      integer*4 fid,ioerr
      integer*4 nrow,ncol,nhead
 
      character*(*) headers(nmax_head)
      real*8 data(nmax_row,nmax_col)
      character*1024 tmpline,tmps1,tmps2,tmpstrs(1000)

      integer*4 i,j,np,year,mon,day,doyr
      real*8 dyr
      integer*4 nblen,idoy

      
c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/'//
c     .  'reason/sopac/cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='/e/data/garner.ucsd.edu/pub/timeseries/reason/sopac/'//
c     . 'cleanedNeuUnfTimeSeries20070430/bjfsCleanUnf.neu'
c      file='e:\\tmp\\bjfsCleanUnf.neu'
c      write(*,*) file
 
      nhead=1
c      call file_info (file,nrow,ncol,nhead,headers)
c      nrow=nrow-nhead-1
c     MOD:Tian:JUL26/07:What about the last null (with nothing) line.
c      nrow=nrow-nhead
c      write(*,*) nrow, ncol,nhead
      nrow=0

      call getlun(fid)
      open(unit=fid,file=file)  
      i=0   
      if (nhead.eq.0) goto 801
c     MOD Tian Jun-23-2008
c       Fixed a bug when there are no header lines.
 30   read(fid,'(a1000)', iostat=ioerr, end=90) buf
      i=i+1
      headers(i)=buf
c      if (buf(1:1).eq.'#') then
c         write(*,*) buf(1:nblen(buf))
      if (i.ge.nhead) goto 801
      goto 30
c      endif
c$$$      if (index(bufline,'END OF HEADER').gt.1) then
c$$$         write(*,*) 'end of header, data section start'
c$$$         goto 32
c$$$      endif
801   continue
c     The following line-by-line reading works for both types of data.
c     read the data line by line
      ncol=6
 802  read(fid,'(a1024)',end=90) tmpline
      call strsplit(tmpline, ',', np, tmpstrs)
      nrow=nrow+1
      tmps1=tmpstrs(2)
      read(tmps1,*) data(nrow,5)
      tmps1=tmpstrs(3)
      read(tmps1,*) data(nrow,4)
      tmps1=tmpstrs(4)
      read(tmps1,*) data(nrow,6)
      tmps1=tmpstrs(1)
      call strsplit(tmps1, '-', np, tmpstrs)
      tmps1=tmpstrs(1)
      read(tmps1,*) year
      tmps1=tmpstrs(2)
      read(tmps1,*) mon
      tmps1=tmpstrs(3)
      read(tmps1,*) day
      doyr=idoy(year,mon,day)
      data(nrow,2)=year
      data(nrow,3)=doyr
c      dyr=decyrs(year,doyr,12d0*60*60)
      dyr=year+doyr/365.25d0
      data(nrow,1)=dyr
c      write(*,*) tmps1(1:nblen(tmps1))
c      write(*,*) (data(nrow,i),i=1,6)
      goto 802
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


