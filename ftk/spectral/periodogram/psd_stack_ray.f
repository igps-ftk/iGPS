CTITLE
      PROGRAM psd_stack_ray

      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--
c     Create stacked

c     --ALGORITHM--
c       

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1024 file
      integer*4 nneu

c     --OUTPUT--

c     --EXTERNAL-- 

c     --Local Parameters--
      integer*4 fid,i,j,k,nrow,ncol
      integer*4 nmaxp
c      parameter(nmaxp=856730)
c      parameter(nmaxp=1536264)
      parameter(nmaxp=2000000)
      real*8 data(nmaxp,4),odata(nmaxp,4),fns(nmaxp),fn,fnsu(nmaxp)
      integer*4 indx(nmaxp),nindx,nrec(nmaxp),pos
      character*1023 tmpstr
      
      integer*4 iargc,nblen,where

c     <<VAR_DEC

      if (iargc().lt.1) then
         write(*,'(a)') ' Usage: psd_stack_ray file [n_neu]'
         stop
      endif

c     By default, there are 3 column data (north, east and up components) for GPS positions time series.
      nneu=3
c     However, this program can deal with less components.
      if (iargc().ge.2) then
         call getarg(2,tmpstr)
         read(tmpstr,*) nneu
      endif

      call getarg(1,file)
c      write(*,'(a)') ' '//file(1:nblen(file))
      
c     read in data
      call getlun(fid)
      open(unit=fid,file=file)
      i=1
 801  read(fid,*,end=899) (data(i,j),j=1,nneu+1)
c      write(*,'(e,3(1x,f))') (data(i,j),j=1,nneu+1)
      fns(i)=data(i,1)
      i=i+1
c$$$c     debug
c$$$      if (i.ge.10) then 
c$$$         stop
c$$$      endif
c      write(*,*) i
      goto 801
 899  close(fid)
      nrow=i-1
c      ncol=4
      ncol=nneu+1
c      write(*,'(a)') ' data read ok.'
c      write(*,*) ncol,nrow
c      stop
c     sort the data
c      write(*,'(a)') ' sorting freq.'
c      do i=1,nrow
c         write(*,*) fns(i)
c      enddo
      call sortm(fns,nmaxp,nrow)
c      write(*,'(a)') ' sort ok.'
c      do i=1,nrow
c         write(*,*) fns(i)
c      enddo
c      write(*,'(a)') ' calling uniq..'
      call uniq(fns,nmaxp,nrow,indx,nindx)
c      write(*,'(a)') ' uniq done.'
c      write(*,*) '#indx:',nindx
c      stop
      do i=1,nindx
         fnsu(i)=fns(indx(i))
         odata(i,1)=fns(indx(i))
      enddo
c      write(*,*) '#indx:',nindx

c     stack the data
      do i=1,nrow
         fn=data(i,1)
         pos=where(fnsu,nmaxp,nindx,fn)
c         write(*,*) pos
         nrec(pos)=nrec(pos)+1
         do k=1,nneu
            odata(pos,k+1)=odata(pos,k+1)+data(i,k+1)    
         enddo
c         odata(pos,3)=odata(pos,3)+data(i,3)   
c         odata(pos,4)=odata(pos,4)+data(i,4)   
      enddo
      
      do i=1,nindx
         if (nrec(i).eq.0) then
            goto 802
         endif
         do k=1,nneu
            odata(pos,k+1)=odata(pos,k+1)/nrec(i)
         enddo
c         odata(i,2)=odata(i,2)/nrec(i)
c         odata(i,3)=odata(i,3)/nrec(i)
c         odata(i,4)=odata(i,4)/nrec(i)
         write(*,701) (odata(i,j),j=1,nneu+1)
 802     continue
      enddo
 701  format(e20.12,3(1x,f20.12))

c      write(*,'(a)') ' done.'
      STOP
      END
