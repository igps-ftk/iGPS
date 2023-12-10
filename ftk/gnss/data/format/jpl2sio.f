CTITLE
      PROGRAM jpl2sio
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
      character*1000 file,ofile

c     --Local Parameters--
      character*20 filter
      character*1000 files(nmax_site)
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: jpl2sios path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      n=0
      fido=11
      filter='*.lat'
      call ffind ( path, files, filter,n,1 ) 
      write(*,*) '#total files:',n
      do fi=1,n
         file=files(fi)


      END
