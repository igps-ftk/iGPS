CTITLE SIO2CATS
      PROGRAM qmap_extract_by_net

      IMPLICIT NONE
      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     AUG-22-2008 Tian:
c       Created.

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1024 path,opath,cpath

c     --OUTPUT--
c     NONE

c     --Local Parameters--
      character*200 filter,filter2,dt,cfilter
      character*1000 files(nmax_site),file,ofile,headers(nmax_head)
      character*1023 cfile
      integer*4 fido,ioerr
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi
c     data matrix
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol,nhead
c     for date conversion
      integer*4 idate(5),ymd(3),doy,iyr,idoy,isod
      real*8 jd,secr8,yr

      character*4 site
      integer*4 ncf,nrowc,ncolc,nheadc
      real*8 datac(nmax_row,nmax_col)
      character*1024 cfiles(nmax_site),headersc(nmax_head),header

c     for sets
      real*8 set0(nmax_row),set1(nmax_row),set01(nmax_row)
      integer*4 ind0(nmax_row),ind1(nmax_row)
      integer*4 nind0,nind1,nset01

      character*8 site_name
      real*8 long,lati
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.3) then
         write(*,*) 'Syntax: qmap_extract_by_neu path opath cpath'
         write(*,*) '    '
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
      call getarg(3,cpath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.list'
      filter2='*.neu'

      call ffind(path,files,filter,n,1) 
      write(*,'(a,i10)') '#total files:',n
      call getlun(fido)
      do fi=1,n
         file=files(fi)
         write(*,'(a)') '<< '//file(1:nblen(file))
         call getfilename(file,ofile)
c     get constraint neu file
         site=ofile(1:4)
c         write(*,*) 'site:',site
         cfilter=site//filter2(1:nblen(filter2))
c         write(*,*) 'cfilter:',cfilter
         call ffind(cpath,cfiles,cfilter,ncf,1)
         if (ncf.le.0) then
            goto 800
         endif

         call read_qmap(file,data,nrow,ncol,nhead,headers,
     +        site_name,long,lati)
         if (debug) write(*,'(a,2i10)') '  #row/col:',nrow,ncol
         
         call read_sio(cfiles(1),datac,nrowc,ncolc,nheadc,headersc)
         if (debug) write(*,'(a,2i10)') '  #row/col:',nrowc,ncolc

         do i=1,nrow
            call decyrs_to_jd(data(i,1),jd)
            call jd_to_yds(jd,iyr,idoy,isod)
            if (iyr.lt.50) then
               iyr=iyr+2000
            else
               iyr=iyr+1900
            endif
            set0(i)=iyr+idoy/1000.d0
c            write(*,*) 'set0:',set0(i),iyr,idoy
         enddo
         do i=1,nrowc
            set1(i)=datac(i,2)+datac(i,3)/1000.d0
c            write(*,*) 'set1:',set1(i),datac(i,2),datac(i,3)
c            stop
         enddo
c         write(*,*) 'calling set_intersect...'
         call set_intersect_d(set0,nmax_row,nrow,set1,nmax_row,nrowc,
     &        ind0,nmax_row,nind0,ind1,nmax_row,nind1, 
     &        set01,nmax_row,nset01)
c         goto 800

         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))
c//'.neu'
         write(*,'(2a)') '>> ',ofile(1:nblen(ofile))
         write(*,*) nset01
         open(unit=fido,file=ofile)
c         write(fido,'(2a)') '#   SRC: ',file(1:nblen(file))//' +'
         cfile=cfiles(1)
c         write(fido,'(2a)') '#   SRC: ',cfile(1:nblen(cfile))
c         write(fido,'(a)') '#  PROG: neu_extract_by_neu'
c         write(fido,'(a)') '#<<neu_extract_by_neu'
         do i=1,nhead
            header=headers(i)
c            write(fido,'(a)') header(1:nblen(header))
         enddo
c         do i=1,nhead
c            header=headers(i)
c            write(fido,'(2a)') '#',header(1:nblen(header))
c         enddo
c         write(fido,'(a)') '#<<neu_extract_by_neu'
c         do i=1,nheadc
c            header=headersc(i)
c            write(fido,'(2a)') '#',header(1:nblen(header))
c         enddo
         do i=1,nind0
c$$$            call mjd_to_ymdhms(data(i,3),idate,secr8)
c$$$c            if (debug) write(*,*) idate
c$$$            ymd(1)=idate(1)
c$$$            ymd(2)=idate(2)
c$$$            ymd(3)=idate(3)
c$$$            call ymd_to_doy(ymd,doy)
c$$$            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
c            iyr=data(ind0(i),2)
c            idoy=data(ind0(i),3)
            write(fido,700) (data(ind0(i),j),j=1,ncol),site_name,
     +           long,lati
         enddo
 700     format(1x,f13.8,4f9.2,f8.4,2f9.2,2f8.4,2x,a8,2f10.4)
         close(fido)
 800     continue
c         stop
      enddo

      STOP
      END
