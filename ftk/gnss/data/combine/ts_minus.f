CTITLE 
      PROGRAM ts_minus

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
      character*200 filter,dt,cfilter
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
      integer*4 idate(5),ymd(3),doy,iyr,idoy
      real*8 jd,secr8,yr

      character*4 site
      integer*4 ncf,nrowc,ncolc,nheadc
      real*8 datac(nmax_row,nmax_col)
      character*1024 cfiles(nmax_site),headersc(nmax_head),header

c     for sets
      real*8 set0(nmax_row),set1(nmax_row),set01(nmax_row)
      integer*4 ind0(nmax_row),ind1(nmax_row)
      integer*4 nind0,nind1,nset01
      
c     external functions
      integer iargc,nblen

c     <<VAR_DEC

       if (iargc().lt.3) then
         write(*,*) 'Syntax: ts_minus path path_to_minus opath [TYPE]'
         write(*,*) '    TYPE: sioneu[default],sioxyz'
         stop
      endif
      call getarg(1,path)
      call getarg(3,opath)
      call getarg(2,cpath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.neu'
      if (iargc().ge.4) then
         call getarg(4,dt)
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
         call getfilename(file,ofile)
c     get constraint neu file
         site=ofile(1:4)
         cfilter=site//filter(1:nblen(filter))
c         write(*,*) cpath
         call ffind(cpath,cfiles,cfilter,ncf,1)
         if (ncf.le.0) then
            goto 800
         endif

         call read_sio(file,data,nrow,ncol,nhead,headers)
         write(*,*) 'debug:',debug
         if (debug) write(*,'(a,2i10)') '  #row/col:',nrow,ncol
         
         call read_sio(cfiles(1),datac,nrowc,ncolc,nheadc,headersc)
         if (debug) write(*,'(a,2i10)') '  #row/col:',nrowc,ncolc

         do i=1,nrow
            set0(i)=data(i,2)+data(i,3)/1000.d0
         enddo
         do i=1,nrowc
            set1(i)=datac(i,2)+datac(i,3)/1000.d0
         enddo
c         write(*,*) 'calling set_intersect...'
         call set_intersect_d(set0,nmax_row,nrow,set1,nmax_row,nrowc,
     &        ind0,nmax_row,nind0,ind1,nmax_row,nind1, 
     &        set01,nmax_row,nset01)
c         goto 800

         do i=1,nind0
            if (ncol.ge.4.and.ncolc.ge.4) then
               data(ind0(i),4)=data(ind0(i),4)-datac(ind1(i),4)
            endif
            if (ncol.ge.5.and.ncolc.ge.5) then
               data(ind0(i),5)=data(ind0(i),5)-datac(ind1(i),5)
            endif
            if (ncol.ge.6.and.ncolc.ge.6) then
               data(ind0(i),6)=data(ind0(i),6)-datac(ind1(i),6)
            endif
         enddo

         ofile=opath(1:nblen(opatH))//pathsep//
     .        ofile(1:nblen(ofile))
c//'.neu'
         write(*,'(a)') '>> '//ofile(1:nblen(ofile))
         write(*,*) nset01
         open(unit=fido,file=ofile)
         write(fido,'(2a)') '#   SRC: ',file(1:nblen(file))//' +'
         cfile=cfiles(1)
         write(fido,'(2a)') '#   SRC: ',cfile(1:nblen(cfile))
         write(fido,'(a)') '#  PROG: ts_minus'
         write(fido,'(a)') '#<<ts_minus'
         do i=1,nhead
            header=headers(i)
            write(fido,'(2a)') '#',header(1:nblen(header))
         enddo
         write(fido,'(a)') '#<<ts_minus'
         do i=1,nheadc
            header=headersc(i)
            write(fido,'(2a)') '#',header(1:nblen(header))
         enddo
         do i=1,nind0
c$$$            call mjd_to_ymdhms(data(i,3),idate,secr8)
c$$$c            if (debug) write(*,*) idate
c$$$            ymd(1)=idate(1)
c$$$            ymd(2)=idate(2)
c$$$            ymd(3)=idate(3)
c$$$            call ymd_to_doy(ymd,doy)
c$$$            call jd_to_decyrs(data(i,3)+2400000.5d0,yr)
            iyr=data(ind0(i),2)
            idoy=data(ind0(i),3)
            write(fido,700) data(ind0(i),1),iyr,idoy,
     &           (data(ind0(i),j),j=4,ncol)
         enddo
 700     format(f10.5,1x,i4,1x,i3,1x,3f15.5,3f10.5)
         close(fido)
 800     continue
c         stop
      enddo

      STOP
      END
