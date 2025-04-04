CTITLE
      subroutine read_nc(file,dtype,dataO,latsO,lonsO,timesO
     &     ,nmaxlat,nmaxlon,nmaxtim
     &     ,nlat,nlon,ntim
     &     )
c     --PURPOSE--
c     read NCEP Reanalysis 1/2 gauss grid fiels (daily avarages)

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     Created Nov-16-2008 Tian

      IMPLICIT NONE

      include '/usr/local/netcdfi/include/netcdf.inc'
c      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
      integer nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
c      parameter(nlat=600,nlon=1440,ntim=12)
c      parameter(nmaxlat=600,nmaxlon=1440,nmaxtim=12)
      real*8 nan

c     --INPUT--
      character*(*) file
      character*(*) dtype
c      character*1023 file
c      character*20 dtype

c     --OUTPUT--
      real dataO(nmaxlat,nmaxlon,nmaxtim),latsO(nmaxlat),lonsO(nmaxlon)
      real*8 timesO(nmaxtim)
      
    

c     --EXTERNAL--

c     --Local Parameters--
c$$$      real data(nlat,nlon,ntim)
c     data format now changed to (lon,lat,time) from (lat,lon,time).
c     Thus,
      real data(nlon,nlat,ntim)
      real lats(nlat),lons(nlon),times(ntim)
      integer i,j,k,ilon,ilat,itim

      integer retval,ncid,data_varid,data_in
      real missing_value
      real add_offset
      real scale_factor

C     To check the units attributes.
      character*100 UNITS
      parameter (UNITS = 'units')
      character*100 DATA_UNITS, TIME_UNITS, LAT_UNITS, LON_UNITS
      
      integer*4 year,mon,day,hh,mm,idate(5),hrs
      real*8 dyr,jd,mjd,second,days,jd_ref
      
      character*100 buf,bufs(100),buf0,buf1,buf2
      integer*4 nbuf

      character*100 fmt

      integer*4 nblen

c     <<VAR_DEC

      write(*,'(a)') '::[read_nc]started'
      nan=0d0/0d0

c      file='/home/tianyf/gldas/GLDAS_NOAH025_M.A2007.nc'
c      write(*,*) 'lll',nlat,nlon,ntim
c      file='/home/tianyf/gldas/GLDAS_NOAH10_M.A2007.nc'
c      dtype='WEASD'
c      write(*,*) ntim
c      write(*,'(a)') '::[read_nc]file: '//file(1:nblen(file))
      retval=nf_open(file,nf_nowrite,ncid)
      if (retval.ne.nf_noerr) then
         write(*,*) 'Open file error '//file
         stop
      endif


      retval=nf_inq_varid(ncid,'lat',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lats)
c      write(*,*) lats
      do i=1,nlat
         latsO(i)=lats(i)
      enddo
c      write(*,*) nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
      retval=nf_inq_varid(ncid,'lon',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lons)
c      write(*,*) lons
      do i=1,nlon
         lonsO(i)=lons(i)
      enddo
c      write(*,*) nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
      retval=nf_inq_varid(ncid,'time',data_varid)
c      write(*,*) '1',nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
      retval=nf_get_var_real(ncid,data_varid,times)
c      write(*,*) '2',nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
c      write(*,*) times
      retval=nf_get_att_text(ncid,data_varid,UNITS,time_units)
c      write(*,*) 'Time unit:',time_units
c      time_units=time_units//' dummy'
c     hours since 1-1-1 00:00:0.0
      call strsplit(time_units,' ',nbuf,bufs)
c      do i=1,nbuf
c         write(*,*) i,nbuf,bufs(i)
c      enddo
      if (bufs(1).eq.'hours') then
         write(*,*) time_units
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
c      write(*,*) 'jd:',jd_ref
c     convert time from hours to decimal year
         do i=1,ntim
            days=times(i)/24d0
            jd=jd_ref+days
            call JD_to_Decyrs( jd, dyr )
            timeso(i)=dyr
c            write(*,*) dyr
         enddo
c      write(*,*) 'time ok'
      elseif (bufs(1).eq.'MJD'.or.bufs(1).eq.'mjd') then
c     convert time from mjd to decimal year
         do i=1,ntim
            jd = times(i) + 2 400 000.5d0
            call JD_to_Decyrs( jd, dyr )
c            times(i)=dyr
c            timesL(i)=dyr
            timeso(i)=dyr
c         write(*,*) i,jd,dyr,times(i),ntim
         enddo
      else
         write(*,'(a)') 'Unknow time unit: '//
     &        time_units(1:nblen(time_units))
         stop
      endif
c      write(*,*) nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
c      write(*,*) 'time ok'

c     read data section
      retval=nf_inq_varid(ncid,dtype,data_varid)
      write(*,*) 'reading data.. ',dtype
      retval=nf_get_var_real(ncid,data_varid,data)
      write(*,*) 'done!'
      retval=nf_get_att_real(ncid,data_varid,'missing_value',
     &     missing_value)
c      if (retval.eq.NF_NOERR) then
      write(*,*) 'missing value:',missing_value
c      endif
c     The variable name is case-sensitive?
c      Yes?
      retval=nf_get_att_real(ncid,data_varid,'scale_factor',
     &     scale_factor)
      write(*,*) 'sf:',scale_factor
c     why cannnot read scale_factor?
      if (scale_factor.eq.0) then
         scale_factor=1d0
      endif
      retval=nf_get_att_real(ncid,data_varid,'add_offset',
     &     add_offset)
      write(*,*) 'offset:',add_offset

      retval=nf_close(ncid)

      do i=1,ntim
c         write(*,*) i,ntim
         do j=1,nlat
c            write(*,*) i,ntim,j,nlat
            do k=1,nlon
c               write(*,*) i,j,k,ntim
c               write(*,*) int(data(i,j,k)),' - ',missing_value
               if (data(k,j,i).eq.missing_value.or.
c     &              isnan(data(k,j,i))) then
     &              nan.eq.(data(k,j,i))) then
c                  dataO(j,k,i)=-9999
                  dataO(j,k,i)=missing_value
c                  data(j,k,i)=-9999
               else
                  dataO(j,k,i)=data(k,j,i)*scale_factor+add_offset
c                  data(j,k,i)=data(j,k,i)*scale_factor+add_offset
c                  write(*,*) data(k,j,i),dataO(j,k,i)
               endif
c     write(*,700) times(i),lats(j),lons(k),data(i,j,k)
c               stop
            enddo
         enddo
c            write(*,*) data(i,33,69),dataL(i,33,69)
      enddo
      write(*,*) 'data ok'



c     out to temp file
c$$$       open(unit=1001,file='/home/tianyf/tmp/ttt')
c$$$c     '(360f)'
c$$$       write(fmt,'(a,i,a)') '(',nlon,'f)'
c$$$       do i=1,nlat
c$$$          write(1001,fmt) (data(i,j,1),j=1,nlon)
c$$$       enddo
c$$$       close(1001)
c$$$       write(*,*) 'ok'
c$$$c      stop
c      write(*,*) ilat,ilon
c      stop
c      do i=1,ntim
c         if (data(i,ilat,ilon).ne.-9999) then
c            write(*,700) times(i),ilat,ilon,data(i,ilat,ilon)
c 700        format(f9.4,2f,f20.8)
c         else
c            write(*,*) 'error:',i,data(i,ilat,ilon)
c         endif
c      enddo

c      write(*,*) 'ncdf read ok.'

c      idate(1)=2008
c      call ymdhms_to_jd_noy2k( idate, second, jd_ref )
c      write(*,*) jd_ref

c      RETURN
      END
