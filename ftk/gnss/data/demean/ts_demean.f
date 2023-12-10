CTITLE SIO2OTR
      PROGRAM ts_demean

      IMPLICIT NONE
      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c	Created on Fri Sep  9 16:50:50 CST 2011 by tianyf

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1024 path,opath

c     --OUTPUT--
c     NONE

c     --Local Parameters--
      character*20 filter,dt
      character*1000 files(nmax_site),file,ofile,headers(nmax_head)
      character*1 neustr(3)
      data neustr/'n','e','u'/
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi,neui
c     data matrix
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol,nhead
      real*8 means(3),tmpmean
c     for date conversion
      integer*4 idate(5),ymd(3),doy,year
      real*8 jd,secr8,yr
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,'(a)') 'Syntax: ts_demean path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.neu'

      call ffind(path,files,filter,n,1) 
      write(*,'(a,i10)') '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         write(*,'(a)') '<< '//file(1:nblen(file))
         call read_sio(file,data,nrow,ncol,nhead,headers)
         if (debug) write(*,'(a,2i10)') '  #row/col:',nrow,ncol

         call getfilename(file,ofile)
         file=ofile
         ofile=opath(1:nblen(opatH))//pathsep//
     .        file(1:nblen(file))
         write(*,'(a)') '>> '//ofile(1:nblen(ofile))
c	calcualte means         
         do neui=1,3
            tmpmean=0
            do i=1,nrow
               tmpmean=tmpmean+data(i,neui+3)
            enddo
            means(neui)=tmpmean/nrow
         enddo
         write(*,*) 'means:',means

         open(unit=fido,file=ofile)
         do i=1,nrow
            year=data(i,2)
            doy=data(i,3)
            write(fido,701) data(i,1),year,doy,
     &           data(i,4)-means(1),
     &           data(i,5)-means(2),
     &           data(i,6)-means(3),
     &           (data(i,j),j=7,9)
         enddo
 700     format(i4,1x,f15.5,1x,f10.5,1x,f10.5)
 701     format(f10.5,1x,i4,1x,i3,1x,3f15.5,1x,3f10.5)
         close(fido)
c         stop
      enddo

      STOP
      END
