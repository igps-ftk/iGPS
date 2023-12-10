CTITLE
      PROGRAM scec_csv2sio
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
      INCLUDE '../../../inc/ftk.h'

c     --Command-line Parameters--
      character*1000 path,opath

c     --Local Parameters--
      character*20 filter
      character*1000 files(nmax_site),file,ofile,headers(100),tmpstr
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol,nhead
c     for date conversion
      integer*4 idate(5),ymd(3),doy
      real*8 jd,secr8,yr
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: scec_csv2sio path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.csv'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         if (debug) write(*,*) file(1:nblen(file))
c         call read_pbo(file,data,nrow,ncol)
         call read_scec_csv(file,data,nrow,ncol,nhead,headers)
         if (debug) write(*,*) nrow,ncol
         call getfilename(file,ofile)
         tmpstr=ofile
         call desuffix(tmpstr,ofile)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))//'.neu'
         write(*,*) '>>'//ofile(1:nblen(ofile))

         open(unit=fido,file=ofile)
         write(fido,701) 'source:'//file(1:nblen(file))
         do i=1,nhead
            tmpstr=headers(i)
            write(fido,701) tmpstr(1:nblen(tmpstr))
 701        format('#',a)
         enddo
         do i=1,nrow
c$$$            call mjd_to_ymdhms(data(i,3),idate,secr8)
c$$$c            if (debug) write(*,*) idate
c$$$            ymd(1)=idate(1)
c$$$            ymd(2)=idate(2)
c$$$            ymd(3)=idate(3)
c$$$            call ymd_to_doy(ymd,doy)
c$$$            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
c$$$            write(fido,700) yr,idate(1),doy,(data(i,j),j=4,9)
            
         write(fido,700) data(i,1),int(data(i,2)),int(data(i,3)),
     +        (data(i,j)*1d-3,j=4,6)
         enddo
 700     format(1x,f9.4,i5,i4,3f15.6)
         close(fido)
      enddo

      STOP
      END
