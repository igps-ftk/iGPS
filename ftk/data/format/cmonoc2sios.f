CTITLE
      PROGRAM cmonoc2sios
c     --PURPOSE--

c     --INPUT--

c     --OUTPUT--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     JUL-30-2007 Tian:
c     Created.

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     --Command-line Parameters--
      character*1000 path,opath

c     --Local Parameters--
      character*20 filter
      character*1000 files(nmax_site),file,ofile,tmpstr
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol
c     for date conversion
      integer*4 idate(5),ymd(3),doy,year
      real*8 jd,secr8,decyr
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: cmonoc2sios path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.list'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         if (debug) write(*,*) file(1:nblen(file))
         call read_cmonoc(file,data,nrow,ncol)
         if (debug) write(*,*) nrow,ncol
         call getfilename(file,tmpstr)
         call desuffix(tmpstr,ofile)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))//'.xyz'
         write(*,*) '>>'//ofile(1:nblen(ofile))
c         goto 800

         open(unit=fido,file=ofile)
         do i=1,nrow
c            call mjd_to_ymdhms(data(i,ncol),idate,secr8)
            decyr=data(i,ncol)
            call decyrs_to_ydhms(decyr,idate)
c            if (debug) write(*,*) decyr,idate
            year=int(decyr)
            doy=idate(2)
c            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
            write(fido,700) decyr,year,doy,data(i,1),
     &           data(i,3),data(i,5),data(i,2),data(i,4),data(i,6)
         enddo
 700     format(f9.4,i5,i4,3f15.5,3f10.5)
         close(fido)
c         stop
 800     continue
      enddo

      STOP
      END
