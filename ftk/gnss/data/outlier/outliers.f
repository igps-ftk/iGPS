CTITLE
      PROGRAM outliers

      IMPLICIT NONE
      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1000 path,opath

c     --OUTPUT--

c     --Local Parameters--
      character*20 filter
c     time series processing level
c     'Raw'
c     'Clean'
      character*10 proc_level
c     [Un]filtered time series?
c     'Unf'
c     'Filter'
      character*5 proc_flt

      character*1000 files(nmax_site),file,ofile,oofile
      integer*4 fido,fidoo,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol
c     header infor
      integer*4 nhead
      character*100 headers(nmax_head)
      character*1024 strtmp
      
c     for date conversion
      integer*4 idate(5),ymd(3),doy,year
      real*8 jd,secr8,yr
      character*4 site

c     for IQR
      real*8 iqrv,ts(nmax_row)
      integer*4 win,ii

c     whether outlier or not?
      integer*4 isOut(nmax_row)

c     outlier standard C (formal error: 10, 10, 20 mm for n, e, and u)
      real*8 outfes(3),tmpf
      character*1024 tmps

c     for each component
      integer*4 neu(3),neui

c     external functions
      integer iargc,nblen
      real*8 IQR
c     <<VAR_DEC

      if (iargc().ge.1) then
         call getarg(1,path)
      endif
      if (iargc().ge.2) then
         call getarg(2,opath)
      endif
c      write(*,*) path,opath
      
c     default formal error thresholds
      outfes(1)=.01
      outfes(2)=.01
      outfes(3)=.02
      if (iargc().ge.3) then
         call getarg(3,tmps)
         read(tmps,*) tmpf
         outfes(1)=tmpf
      endif
      if (iargc().ge.4) then
         call getarg(4,tmps)
         read(tmps,*) tmpf
         outfes(2)=tmpf
      endif
      if (iargc().ge.5) then
         call getarg(5,tmps)
         read(tmps,*) tmpf
         outfes(3)=tmpf
      endif
      write(*,'(a,3f10.5,a)') 'formal errors:',outfes,' meters'


      if (nblen(path).eq.0.or.nblen(opath).eq.0) then
         write(*,*) 'Syntax: outliers PATH OPATH'
         write(*,*)
         stop
      endif


c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.neu'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      open(unit=fido,file='/tmp/.tmptmptmp')
      call getlun(fidoo)
      close(fido)


c     set intial parameters
      win=365


      do fi=1,n
         file=files(fi)
         if (debug) write(*,*) '<<',file(1:nblen(file))

c     read the data
         call read_sio(file,data,nrow,ncol,nhead,headers)
         if (debug) write(*,*) nrow,ncol

c     loop calculating IQR
         neui=4
         do i=1,nmax_row
            ts(i)=0d0
         enddo
         do i=1,nrow
            ts(i)=data(i,neui)
         enddo
         do i=1,nrow
            ii=i
            iqrv=IQR(ts,nmax_row,nrow,ii,win)
c            write(*,*) i,ii,iqrv,data(i,neui)
         enddo

c       check for outlier standard C (formal error)
         do i=1,nrow
            if (data(i,7).ge.outfes(1).or.
     &          data(i,8).ge.outfes(2).or.
     &          data(i,9).ge.outfes(3)) then
                isOut(i)=1
             else
                isOut(i)=0
            endif

         enddo

c         write(*,*) 'isOut:',(isOut(i),i=1,nrow)
         call getfilename(file,ofile)
         site=ofile(1:4)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))
         write(*,*) '>>'//ofile(1:nblen(ofile))

         call getfilename(file,oofile)
         oofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))
         oofile=opath(1:nblen(opatH))//pathsep//
     .        site//'Outliers.neu'
         write(*,*) '>>'//oofile(1:nblen(oofile))

         open(unit=fido,file=ofile)
         open(unit=fidoo,file=oofile)
         do i=1,nrow
            year=data(i,2)
            doy=data(i,3)
            if (isOut(i).ne.1) then
               write(fido,701) data(i,1),year,doy,
     &              (data(i,j),j=4,9)
            else
               write(fidoo,701) data(i,1),year,doy,
     &              (data(i,j),j=4,9)
            endif
         enddo
 700     format(i4,1x,f15.5,1x,f10.5,1x,f10.5)
 701     format(f10.5,1x,i4,1x,i3,1x,3f15.5,1x,3f10.5)
         close(fido)
         close(fidoo)
c         stop
      enddo

      STOP
      END
