CTITLE
      subroutine query_nc(file,nlat,nlon,ntim)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     Created Nov-16-2008 Tian

      IMPLICIT NONE

      include '/usr/local/netcdf-3.6.3-gftn/include/netcdf.inc'
C     INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC

c     --INPUT--
      character*(*) file

c     --OUTPUT--
      integer*4 nlat,nlon,ntim

c     --EXTERNAL--

c     --Local Parameters--

      integer*4 retval,ncid,data_varid

      integer rhdims(NF_MAX_VAR_DIMS)

      integer*4 nblen

c     <<VAR_DEC
c      write(*,*) 'file:',file
      retval=nf_open(file,nf_nowrite,ncid)
      if (retval.ne.nf_noerr) then
         write(*,*) 'Open file error '//file
         stop
      endif
      nlat=-1
      nlon=-1
      ntim=-1
      retval=nf_inq_varid(ncid,'lat',data_varid)
      retval=nf_inq_vardimid(ncid,data_varid,rhdims)
      retval=nf_inq_dimlen(ncid,rhdims(1),nlat)
c      write(*,*) 'nlat:',nlat
c,rhdims
      retval=nf_inq_varid(ncid,'lon',data_varid)
      retval=nf_inq_vardimid(ncid,data_varid,rhdims)
      retval=nf_inq_dimlen(ncid,rhdims(1),nlon)
c      write(*,*) 'nlon:',nlon
c,rhdims
      retval=nf_inq_varid(ncid,'time',data_varid)
      retval=nf_inq_vardimid(ncid,data_varid,rhdims)
      retval=nf_inq_dimlen(ncid,rhdims(1),ntim)
c
c$$$      retval=nf_inq_varid(ncid,'WEASD',data_varid)
c$$$      retval=nf_inq_vardimid(ncid,data_varid,rhdims)
c$$$      write(*,*) rhdims
c$$$      retval=nf_inq_dimlen(ncid,rhdims(1),nbuf)
c$$$      write(*,*) 'nbuf:',nbuf
c$$$      retval=nf_inq_dimlen(ncid,rhdims(2),nbuf)
c$$$      write(*,*) 'nbuf:',nbuf
c$$$      retval=nf_inq_dimlen(ncid,rhdims(3),nbuf)
c$$$      write(*,*) 'nbuf:',nbuf
c,rhdims
      END
