CTITLE PBO2CATS
      PROGRAM pbo2cats

      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     APR-19-2008 Tian:
c       Created.

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1024 path,opath

c     --OUTPUT--
c     NONE

c     --Local Parameters--
      character*20 filter
      character*1000 files(nmax_site),file,ofile
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol
c     for date conversion
      integer*4 idate(5),ymd(3),doy
      real*8 jd,secr8,yr

      integer*4 isDemean
      real*8 xm,ym,zm,xsum,ysum,zsum
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: pbo2cats path opath'
         stop
      endif

      isDemean=1

      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.pos'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         write(*,*) '<<'//file(1:nblen(file))
         call read_pbo(file,data,nrow,ncol)
         if (isDemean.eq.1) then
            xsum=0
            ysum=0
            zsum=0
            do i=1,nrow
               xsum=xsum+data(i,4)
               ysum=ysum+data(i,5)
               zsum=zsum+data(i,6)
            enddo
            xm=xsum/nrow
            ym=ysum/nrow
            zm=zsum/nrow
            write(*,*) 'means:',xm,ym,zm
            do i=1,nrow
               data(i,4)=data(i,4)-xm
               data(i,5)=data(i,5)-ym
               data(i,6)=data(i,6)-zm
            enddo
         endif

         if (debug) write(*,*) '  #row:',nrow,' #col:',ncol
         call getfilename(file,ofile)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))//'.cats'
         write(*,*) '>>'//ofile(1:nblen(ofile))
         open(unit=fido,file=ofile)
         do i=1,nrow
            call mjd_to_ymdhms(data(i,3),idate,secr8)
c            if (debug) write(*,*) idate
            ymd(1)=idate(1)
            ymd(2)=idate(2)
            ymd(3)=idate(3)
            call ymd_to_doy(ymd,doy)
            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
            write(fido,700) yr,(data(i,j),j=4,9)
         enddo
 700     format(f9.4,3f15.5,3f10.5)
         close(fido)
c         stop
      enddo

      STOP
      END
