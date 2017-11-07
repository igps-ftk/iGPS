CTITLE SIO2OTR
      PROGRAM sio2otr

      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--
c$$$      print*,' most users can will use otr otd or otx'
c$$$      print*,' otr=data, such as inferred monument displacements'
c$$$      print*,'       format of yr, julian day, data, error bar'
c$$$      print*,' otd=data, such as inferred monument displacements'
c$$$      print*,'       format of yrmoda,  data, error bar'
c$$$      print*,' otx=data, such as inferred monument displacements'
c$$$      print*,'       format of yr mo da,  data, error bar'

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
c     for date conversion
      integer*4 idate(5),ymd(3),doy,year
      real*8 jd,secr8,yr
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,'(a)') 'Syntax: sio2otr path opath [TYPE]'
         write(*,'(a)') '    TYPE: sioneu[default],sioxyz'
         write(*,'(a)') '  Output format:'
         write(*,'(a)')' otr=data, such as inferred monument'//
     &        ' displacements'
         write(*,'(a)') '       format of yr, julian day, data,'//
     &        ' error bar'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.neu'
      if (iargc().ge.3) then
         call getarg(3,dt)
         if (dt(1:6).eq.'sioneu') then
            filter='*.neu'
         elseif (dt(1:6).eq.'sioxyz') then
            filter='*.xyz'
         else
            write(*,*) 'Wrong time series type:',dt(1:nblen(dt))
         endif
      endif

      call ffind(path,files,filter,n,1) 
      write(*,'(a,i10)') '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         write(*,'(a)') '<< '//file(1:nblen(file))
         call read_sio(file,data,nrow,ncol,nhead,headers)
         if (debug) write(*,'(a,2i10)') '  #row/col:',nrow,ncol
         do neui=1,3
            call getfilename(file,ofile)
            call desuffix(ofile,file)
            ofile=file
            ofile=opath(1:nblen(opatH))//pathsep//
     .           ofile(1:nblen(ofile))//'.'//neustr(neui)
            file=ofile
            write(*,'(a)') '>> '//ofile(1:nblen(ofile))
            open(unit=fido,file=ofile)
            do i=1,nrow
               year=data(i,2)
               write(fido,701) year,data(i,3),
     &              data(i,neui+3)*1e3,
     &              data(i,neui+6)*1e3
            enddo
 700        format(i4,1x,f15.5,1x,f10.5,1x,f10.5)
 701        format(i4,1x,f15.5,1x,e20.5,1x,e20.5)
            close(fido)
c            stop
         enddo
      enddo

      STOP
      END
