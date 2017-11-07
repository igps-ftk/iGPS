CTITLE QOCALOAD2CATS
      PROGRAM qocaload2sios

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
      character*20 filter,dtype
      character*1023 files(10000)
      character*1023 file,ofile
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      integer*4 nrow,ncol,nhead 
      character*512 headers(nmax_head),header
      character*1 cmt
      real*8 data(nmax_row,nmax_col)

c     for date conversion
      real*8 jd,yr,sectag
      integer*4 date(5),idoy,iyr

      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: qocaload2cats path opath [TYPE]'
         write(*,*) '        TYPE:'
         write(*,*) '          load (default)'
         write(*,*) '          snow'
         write(*,*) '          soil'

         stop
      endif

      cmt='~'

      call getarg(1,path)
      call getarg(2,opath)
      write(*,*) path(1:nblen(path))
      write(*,*) opath(1:nblen(opath))

      filter='*.?load'
      if (iargc().ge.3) then
         call getarg(3,dtype)
         if (dtype(1:nblen(dtype)).eq.'load') then
            filter='*.?load'
         elseif (dtype(1:nblen(dtype)).eq.'snow') then
            filter='*.snow'
         elseif (dtype(1:nblen(dtype)).eq.'soil') then
            filter='*.soil'
         endif
            
      endif
c      filter='*.snow'
c      filter='*.soil'
      write(*,*) filter(1:nblen(filter))

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
     .        ofile(1:nblen(ofile))//'.neu'
         write(*,*) '>>'//ofile(1:nblen(ofile))
         open(unit=fido,file=ofile)
         write(fido,'(2a)') '#   SRC: ',file(1:nblen(file))
         write(fido,'(a)') '#  PROG: qocaload2sios'
         write(fido,'(a)') '#<<'
         do i=1,nhead
            header=headers(i)
            write(fido,'(2a)') '#',header(1:nblen(header))
         enddo
         do i=1,nrow
c            write(*,*) data(i,5)
c            write(*,'(9f10.4)') (data(i,j),j=1,ncol)
            call jd_to_decyrs(data(i,5),yr)
c            call jd_to_ymdhms(data(i,5),date,sectag)          
            call jd_to_yds (data(i,5), iyr, idoy, sectag )
            if (iyr.lt.20) then
               iyr=iyr+2000
            else if (iyr.lt.100) then
               iyr=iyr+1900
            endif
c            write(fido,700) yr,iyr,idoy,(data(i,j)/1000d0,j=7,8),
            write(fido,700) yr,iyr,idoy,data(i,7)/1d3*(-1), 
     &           data(i,8)/1d3,
     &           data(i,6)/1d3
c     Bug fixed. Nov-03-2008. Tian
c     The displacements series output by mload is "vertical north east".
c     We should reform it to "north east up" which is the SIO/NEU sequence.
         enddo
 700     format(f10.5,1x,i4,1x,i3,1x,3(1x,f15.7) )
         close(fido)
c         stop
      enddo

      STOP
      END
