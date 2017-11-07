CTITLE
c      SUBROUTINE read_ds607_txt(file,data,nrow,ncol)
c      SUBROUTINE read_ds607_txt(file,ctlfile1,data,lats,lons,nrow,ncol)
      subroutine read_grbtxt(file,ctlfile1,data,lats,lons,
     &     nmaxrow,nmaxcol,
     &     nrow,ncol)

c     --PURPOSE--
c     read the text data file extracted from grib1 files by wgrib.
c     see sh_ds607_get_d for more information on the conversion.
c     
c     the lat/lon will be also returned; requires that there is a
c     d.ctl file in the same directory as the data files.

c     --ALGORITHM--
c     

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
c     data file
      character*(*) file
c     grads parameters file
      character*(*) ctlfile1
      integer*4 nmaxrow,nmaxcol

c     --OUTPUT--
      integer*4 nrow,ncol
c      parameter(nrow=224,ncol=464)
c     dimensions is inputs; must be the same as the values in the first line
      real*8 data(nmaxrow,nmaxcol),lats(nmaxrow),lons(nmaxcol)
c      real*8 data(464,224)

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 i,j,fid,ioerr,tmp,tnrow,tncol
      character*1023 tmps,tmps1
      real*8 xmin,xstep,ymin,ystep
      character*1023 ctlfile

c     <<VAR_DEC

      call getlun(fid)
      open(unit=fid,file=file,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(a)') 'Error open file '//file(1:nblen(file))
         stop
      endif

      read(fid,*) ncol,nrow
      if (nrow.le.0.or.ncol.le.0) then
         write(*,*) 'data dimensions error!'
         close(fid)
         stop
      endif

c      read(fid,*) data
      do i=1,nrow
         read(fid,*) (data(i,j),j=1,ncol)
      enddo

      close(fid)
c      write(*,*) 'data file read ok'

c     verify the result
c$$$      do i=1,nrow
c$$$         do j=1,ncol
c$$$            write(*,*) data(i,j)
c$$$         enddo
c$$$      enddo
c$$$      stop

c      write(*,*) ctlfile1
      if (nblen(ctlfile1).eq.0) then
         call getpathname(file,tmps)
         ctlfile=tmps(1:nblen(tmps))//pathsep//'d.ctl'
c         write(*,*) ctlfile,file,tmps,nblen(tmps)
      else
         ctlfile=ctlfile1
      endif
c      write(*,*) ctlfile

      call read_ctl(ctlfile,xmin,xstep,ymin,ystep)
c      write(*,*) xmin,xstep,ymin,ystep
      do i=1,nrow
         lats(i)=ymin+(i-1)*ystep
      enddo
      do i=1,ncol
         lons(i)=xmin+(i-1)*xstep
      enddo

      RETURN
      END
