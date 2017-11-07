CTITLE
      PROGRAM jpl2sios
c     --PURPOSE--
c     NO USE at present.

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
      character*1000 files(nmax_site),file,ofile
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

      STOP

       if (iargc().lt.2) then
         write(*,*) 'Syntax: jpl2sios path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.pos'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      do fi=1,n
         file=files(fi)
         write(*,*) file(1:nblen(file))
      enddo

      STOP
      END
