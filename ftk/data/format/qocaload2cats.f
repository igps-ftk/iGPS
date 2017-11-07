CTITLE QOCALOAD2CATS
      PROGRAM qocaload2cats

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
      integer*4 nrow,ncol,nhead 
      character*512 headers(nmax_head)
      character*1 cmt
      real*8 data(nmax_row,nmax_col)

c     for date conversion
      real*8 jd,yr

      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: qocaload2cats path opath'
         stop
      endif

      cmt='~'

      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.?load'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         write(*,*) '<<'//file(1:nblen(file))
         call read_cols(file,data,nrow,ncol,headers,nhead,cmt)
c         do i=1,nrow
c            write(*,*) data(i,5)
c         enddo
c         stop
         if (debug) write(*,*) '  #row:',nrow,' #col:',ncol, 
     &        '  #head:',nhead
         call getfilename(file,ofile)
         ofile=opath(1:nblen(opath))//pathsep//
     .        ofile(1:nblen(ofile))//'.cats'
         write(*,*) '>>'//ofile(1:nblen(ofile))
         open(unit=fido,file=ofile)
         do i=1,nrow
c            write(*,*) data(i,5)
c            write(*,'(9f10.4)') (data(i,j),j=1,ncol)
            call jd_to_decyrs(data(i,5),yr)
            write(fido,700) yr,(data(i,j)/1000d0,j=6,8)
         enddo
 700     format(f10.5,3f15.5)
         close(fido)
c         stop
      enddo

      STOP
      END
