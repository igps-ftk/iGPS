CTITLE
      PROGRAM tenv3_to_sios
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
      character*1024 path,opath
      character*3 outype

c     --Local Parameters--
      character*20 filter
      character*1024 files(nmax_site),file,ofile
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      real*8 data(nmax_row,nmax_col_else)
      integer*4 nrow,ncol,nhead
      character*1000 header(nmax_head),tmpstr
c     for date conversion
      integer*4 idate(5),ymd(3),doy, doyr, year
      real*8 jd,secr8,yr
      
c     external functions
      integer iargc,nblen
      


c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: tenv3_to_sios path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.tenv3'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         if (debug) write(*,*) file(1:nblen(file))
         call read_tenv3(file,data,nrow,ncol)
         if (debug) write(*,*) nrow,ncol
         call getfilename(file,ofile)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))//'.neu'
         write(*,*) '>>'//ofile(1:nblen(ofile))

         open(unit=fido,file=ofile)
         write(fido,'("#__src: ",a)') file(1:nblen(file))
         write(fido,'("#_prog: ",a)') 'tenv3_to_sios'
         do i=1,nrow
            year=data(i,2)
            doyr=data(i,3)
            write(fido,700) data(i,1),year,doyr,data(i,10)+data(i,11),
     +           data(i,8)+data(i,9), data(i,12)+
     +           data(i,13),data(i,16),data(i,15),data(i,17)
 700        format(1x,f10.5,1x,i4,1x,i03,3(1x,f15.5),3(1x,f10.5))
         enddo
         close(fido)
      enddo

      STOP
      END
