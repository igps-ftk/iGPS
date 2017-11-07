CTITLE
      subroutine read_ncep_nc(file,data,dtype)
c     --PURPOSE--
c     read NCEP Reanalysis 1/2 gauss grid fiels (daily avarages)

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     Created Nov-16-2008 Tian

      IMPLICIT NONE

      include '/usr/local/netcdfi/include/netcdf.inc'
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
      integer*4 nlat,nlon,ntim
      parameter(nlat=94,nlon=192,ntim=365)

c     --INPUT--
      character*(*) file
      character*(*) dtype

c     --OUTPUT--
      real data(ntim,nlat,nlon),lats(nlat),lons(nlon),times(ntim)

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j,k,ilon,ilat,itim

      integer*4 retval,ncid,data_varid,data_in
      integer missing_value
      real add_offset,scale_factor

C     To check the units attributes.
      character*100 UNITS
      parameter (UNITS = 'units')
      character*100 DATA_UNITS, TIME_UNITS, LAT_UNITS, LON_UNITS
      
      integer*4 year,mon,day,hh,mm,idate(5),hrs
      real*8 dyr,jd,mjd,second,days,jd_ref
      
      character*100 buf,bufs(100),buf0,buf1,buf2
      integer*4 nbuf

      integer*4 nblen

c     <<VAR_DEC

c     initiate the data matrix, -9999 means no data
      do i=1,ntim
c         lats(i)=-9999
         do j=1,nlat
            do k=1,nlon
               data(i,j,k)=-9999
            enddo
         enddo
      enddo

      do i=1,nlon
         lons(i)=-9999
      enddo
      
      do i=1,nlat
         lats(i)=-9999
      enddo
      write(*,*) 'init ok'

      file='/home/tianyf/tmp/runof.sfc.gauss.2007.nc'
c     
      retval=nf_open(file,nf_nowrite,ncid)
      if (retval.ne.nf_noerr) then
         write(*,*) 'Open file error '//file
         stop
      endif


      retval=nf_inq_varid(ncid,'lat',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lats)
      do i=1,nlat-1
         if (lats(i).ge.27.and.27.ge.lats(i+1)) then
            ilat=i
            goto 800
         endif
c         write(*,*) 'lat:',lats(i)
      enddo
 800  continue
      retval=nf_inq_varid(ncid,'lon',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lons)
      do i=1,nlon-1
         if (lons(i).le.112.and.112.le.lons(i+1)) then
            ilon=i
            goto 801
         endif
c         write(*,*) 'lon:',lons(i)
      enddo
 801  continue
c      write(*,*) ilat,ilon

      retval=nf_inq_varid(ncid,'time',data_varid)
      retval=nf_get_var_real(ncid,data_varid,times)
      retval=nf_get_att_text(ncid,data_varid,UNITS,time_units)
      write(*,*) 'Time unit:',time_units
c      time_units=time_units//' dummy'
c     hours since 1-1-1 00:00:0.0
      call strsplit(time_units,' ',nbuf,bufs)
c      do i=1,nbuf
c         write(*,*) i,nbuf,bufs(i)
c      enddo
      buf=bufs(3)
      buf0=bufs(4)
      call strsplit(buf,'-',nbuf,bufs)
c      do i=1,nbuf
c         write(*,*) i,nbuf,bufs(i)
c      enddo
      read(bufs(1),*) year
      read(bufs(2),*) mon
      read(bufs(3),*) day
      call strsplit(buf0,':',nbuf,bufs)
c      do i=1,nbuf
c         write(*,*) i,nbuf,bufs(i)
c      enddo
      read(bufs(1),*) hh
      read(bufs(2),*) mm
c      write(*,*) bufs(3)
      buf=bufs(3)
      read(buf(1:nblen(buf)),*) second
c     Why cannot use??
c     read(bufs(3),*) second
c     forrtl: severe (59): list-directed I/O syntax error, unit -5, 
c     file Internal List-Directed Read
c      write(*,*) 'ok'
c      write(*,*) year,mon,day,hh,mm,second
      idate(1)=year
      idate(2)=mon
      idate(3)=day
      idate(4)=hh
      idate(5)=mm
      call ymdhms_to_jd_noy2k( idate, second, jd_ref )
      write(*,*) 'jd:',jd_ref
c     convert time from hours to decimal year
      do i=1,ntim
         days=times(i)/24d0
         jd=jd_ref+days
         call JD_to_Decyrs( jd, dyr )
         times(i)=dyr
      enddo
     

c     read data section
      retval=nf_inq_varid(ncid,'runof',data_varid)
      retval=nf_get_var_real(ncid,data_varid,data)
      retval=nf_get_att_int(ncid,data_varid,'missing_value',
     &     missing_value)
      if (retval.eq.NF_NOERR) then
         write(*,*) 'missing value:',missing_value
      endif
      retval=nf_get_att_real(ncid,data_varid,'scale_factor',
     &     scale_factor)
      retval=nf_get_att_real(ncid,data_varid,'add_offset',
     &     add_offset)

      do i=1,ntim
         do j=1,nlat
            do k=1,nlon
c               write(*,*) int(data(i,j,k)),' - ',missing_value
               if (data(i,j,k).eq.missing_value) then
                  data(i,j,k)=-9999
               else
                  data(i,j,k)=data(i,j,k)*scale_factor+add_offset
               endif
c               write(*,700) times(i),lats(j),lons(k),data(i,j,k)
c               stop
            enddo
         enddo
      enddo


      retval=nf_close(ncid)
      write(*,*) 'ok'
c      write(*,*) ilat,ilon
c      stop
      do i=1,ntim
         if (data(i,ilat,ilon).ne.-9999) then
            write(*,700) times(i),ilat,ilon,data(i,ilat,ilon)
 700        format(f9.4,2f,f20.8)
         else
            write(*,*) 'error:',i,data(i,ilat,ilon)
         endif
      enddo

      write(*,*) 'ncdf read ok.'

c      idate(1)=2008
c      call ymdhms_to_jd_noy2k( idate, second, jd_ref )
c      write(*,*) jd_ref

c      RETURN
      END
