CTITLE
      PROGRAM pos2qmap
c     --PURPOSE--
c     Convert PBO time series (created by tssum in globk) to QOCA Map format.

c     --INPUT--
c     

c     --OUTPUT--

c     --EXAMPLE--
c     pos2qmap pos/ pos.qmap/ qmap.tibet.cgps.tseri/icd.net

c     --MODIFICATIONS--
c    
c     + Created on Mon Nov  9 14:57:14 CST 2015 by tianyf


c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     --Command-line Parameters--
      character*1000 path,opath,netfile

c     --Local Parameters--
      character*20 filter
      character*1000 files(nmax_site),file,ofile
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      integer*4 nmax_col_local
      parameter(nmax_col_local=39)
      real*8 data(nmax_row,nmax_col_local)
      integer*4 nrow,ncol,nhead
      character*1000 header(nmax_head),tmpstr
c     for date conversion
      integer*4 idate(5),ymd(3),doy
      real*8 jd,secr8,yr
c     sitename
      character*4 site
      real*8 llh(3)
      
c     external functions
      integer iargc,nblen
      


c     <<VAR_DEC

c     default no net file
      netfile=''

       if (iargc().lt.2) then
         write(*,*) 'Syntax: pos2qmap path opath [net_file]'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
      if (iargc().ge.3) then
         call getarg(3,netfile)
      endif

      write(*,*) path(1:nblen(path))
      write(*,*) opath(1:nblen(opath))
      write(*,*) netfile(1:nblen(netfile))

      filter='*.pos'
      call ffind(path,files,filter,n,1) 
      write(*,*) '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         if (debug) write(*,*) file(1:nblen(file))
         call read_pos(file,data,nrow,ncol,nhead,header)
c     subroutine read_pos(file,data,nrow,ncol,nhead,headers)
         if (debug) write(*,*) nrow,ncol,nhead
         call getfilename(file,ofile)
         call desuffix(ofile,tmpstr)
         ofile=opath(1:nblen(opatH))//pathsep//
     .        tmpstr(1:nblen(tmpstr))//'.list'
         write(*,*) '>>'//ofile(1:nblen(ofile))

c     get site name and its coordiantes
         call getfilename(ofile,tmpstr)
         site=tmpstr(1:4)
         if(nblen(netfile).ne.0) then
            call read_net(netfile,site,llh)
         else
            llh(1)=0
            llh(2)=0
            llh(3)=0
         endif
         

         open(unit=fido,file=ofile)

         do i=1,nhead
            tmpstr=header(i)
c            write(fido,'("#",a)') tmpstr(1:nblen(tmpstr))
         enddo
         do i=1,nrow
c            write(*,*) (data(i,j),j=1,5)
            idate(1)=int(data(i,1)*1d-4)
            idate(2)=int((data(i,1)-idate(1)*1d4)*1d-2)
            idate(3)=int((data(i,1)-idate(1)*1d4-idate(2)*1d2))
            idate(4)=int(data(i,2)*1d-4)
            idate(5)=int((data(i,2)-idate(4)*1d4)*1d-2)
            secr8=0
            call ymdhms_to_jd(idate,secr8,jd)
            call jd_to_decyrs(jd,yr)
c            write(*,*) idate,jd,yr
c            stop
c$$$            call mjd_to_ymdhms(data(i,3),idate,secr8)
c$$$c            if (debug) write(*,*) idate
c$$$c            write(*,*) data(i,3),idate,secr8
c$$$            ymd(1)=idate(1)
c$$$            ymd(2)=idate(2)
c$$$            ymd(3)=idate(3)
c$$$            call ymd_to_doy(ymd,doy)
c$$$            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
            write(fido,700) yr,data(i,17)*1d3,data(i,16)*1d3,
     &           data(i,20)*1d3,data(i,19)*1d3,
     &           data(i,22),data(i,18)*1d3,data(i,21)*1d3,
     &           data(i,24),data(i,23),
     &           site//'_GPS',llh(1),llh(2)
         enddo
 700     FORMAT(1X,F13.8,4F9.2,F8.4,2F9.2,2F8.4,2X,A8,2F10.4)
c 700     format(1x,f9.4,i5,i4,3f15.5,3f10.5)
         close(fido)

c         stop
      enddo

      STOP
      END
