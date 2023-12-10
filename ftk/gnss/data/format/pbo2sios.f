CTITLE
      PROGRAM pbo2sios
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
      character*3 outype

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
      integer*4 nrow,ncol,nhead
      character*1000 header(nmax_head),tmpstr
c     for date conversion
      integer*4 idate(5),ymd(3),doy
      real*8 jd,secr8,yr
      
c     external functions
      integer iargc,nblen
      


c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: pbo2sios path opath [out_type]'
         write(*,*) '         OUT_TYPE: neu (default)'
         write(*,*) '                   xyz'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
      if (iargc().ge.3) then
         call getarg(3,outype)
      else
         outype='neu'
      endif
      if (outype.ne.'xyz'.and.outype.ne.'neu') then
         write(*,*) ' []Wrong output time series type--',outype,'.'
         stop
      endif

c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.pos'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         if (debug) write(*,*) file(1:nblen(file))
         call read_pbo(file,data,nrow,ncol,nhead,header)
         if (debug) write(*,*) nrow,ncol,nhead
         call getfilename(file,ofile)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))//'.neu'
         write(*,*) '>>'//ofile(1:nblen(ofile))

         open(unit=fido,file=ofile)
         do i=1,nhead
            tmpstr=header(i)
            write(fido,'("#",a)') tmpstr(1:nblen(tmpstr))
         enddo
         do i=1,nrow
            call mjd_to_ymdhms(data(i,3),idate,secr8)
c            if (debug) write(*,*) idate
            ymd(1)=idate(1)
            ymd(2)=idate(2)
            ymd(3)=idate(3)
            call ymd_to_doy(ymd,doy)
            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
            if (outype.eq.'xyz') then
               write(fido,700) yr,idate(1),doy,(data(i,j),j=4,9)
            else
               write(fido,700) yr,idate(1),doy,(data(i,j),j=16,21)
            endif
         enddo
 700     format(1x,f9.4,i5,i4,3f15.5,3f10.5)
         close(fido)
      enddo

      STOP
      END
