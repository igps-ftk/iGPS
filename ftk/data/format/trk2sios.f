CTITLE
      PROGRAM trk2sios
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
      character*1000 files(nmax_site),file,ofile
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      real*8 data(nmax_row_large,nmax_col)
      integer*4 nrow,ncol,nhead
      character*1000 headers(nmax_head),tmpstr
c     for date conversion
      integer*4 idate(5),ymd(3),doy
      real*8 jd,secr8,yr
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.2) then
          write(*,*) 'Syntax: trk2sios path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
      write(*,*) path(1:nblen(path))
      write(*,*) opath(1:nblen(opath))

      filter='*.trk'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         if (debug) write(*,*) file(1:nblen(file))
         call read_trk(file,data,nrow,ncol,nhead,headers)
         if (debug) write(*,*) nrow,ncol
         call getfilename(file,ofile)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))
         tmpstr=ofile
c         write(*,*) ofile(1:nblen(ofile))
c         call desuffix(tmpstr,ofile)
c         ofile=ofile//'.neu'
         call desuffix(ofile,tmpstr)
         ofile=tmpstr(1:nblen(tmpstr))//'.neu'
c         write(*,*) ofile(1:nblen(ofile))
         write(*,*) '>>'//ofile(1:nblen(ofile))

         open(unit=fido,file=ofile)
         do i=1,nhead
            tmpstr=headers(i)
            write(fido,'("#",a)') tmpstr(1:nblen(tmpstr))
         enddo
         do i=1,nrow
            idate(1)=data(i,1)
            idate(2)=data(i,2)
            idate(3)=data(i,3)
            idate(4)=data(i,4)
            idate(5)=data(i,5)
            secr8=data(i,6)
            call ymdhms_to_jd(idate,secr8,jd)

c            call mjd_to_ymdhms(data(i,3),idate,secr8)
c            if (debug) write(*,*) idate
            ymd(1)=idate(1)
            ymd(2)=idate(2)
            ymd(3)=idate(3)
            
            call ymd_to_doy(ymd,doy)
c            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
            call jd_to_decyrs(jd,yr)
c            write(fido,*) yr,idate(1),doy,ymd
c            write(fido,'(17f20.12)') (data(i,j),j=1,17)
            write(fido,700) yr,idate(1),doy,data(i,7),data(i,9),
     &           data(i,11),data(i,8),data(i,10),data(i,12)
         enddo
 700     format(f20.15,1x,i4,1x,i3.3,3f15.5,3f10.5)
         close(fido)
      enddo

      STOP
      END
