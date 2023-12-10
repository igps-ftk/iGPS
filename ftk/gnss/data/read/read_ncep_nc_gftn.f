CTITLE
      subroutine read_ncep_nc(file,dataL,dtype,lons,lats,timesLo)
c           call read_ncep_nc(mfile,data,dtype,lons,lats,times)
c     --PURPOSE--
c     read NCEP Reanalysis 1/2 gauss grid fiels (daily avarages)

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     Created Nov-16-2008 Tian

      IMPLICIT NONE

      include '/usr/local/netcdf-3.6.3-gftn/include/netcdf.inc'
C      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
      integer*4 nlat,nlon,ntim,ntiml
      parameter(nlat=94,nlon=192,ntim=365,ntiml=366)

c     --INPUT--
      character*(*) file
      character*(*) dtype

c     --OUTPUT--
c      real data(nlat,nlon,ntim),lats(nlat),lons(nlon),times(ntim),lat
c      real dataL(nlat,nlon,ntimL),timesL(ntimL),dataLo(ntimL,nlat,nlon)
c     Wrong!!
      real data(nlon,nlat,ntim),lats(nlat),lons(nlon),times(ntim),lat
      real dataL(nlon,nlat,ntimL),timesL(ntimL)
c     Right!!
c      real data(ntim,nlat,nlon),lats(nlat),lons(nlon),times(ntim),lat
c      real dataL(ntiml,nlat,nlon),timesL(ntimL)
c     Wrong!!

      real*8 timesLo(ntimL)

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j,k,ilon,ilat,itim

      integer*4 retval,ncid,data_varid,data_in
      integer missing_value
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

      integer*4 nblen

c     for leap year
      character*100 yrstr
      logical isLeap
      logical leapyr
c     <<VAR_DEC
c     get current year
      call desuffix(file,buf)
      call getfileext(buf,yrstr)
c      write(*,*) yrstr
      read(yrstr,*) year
      isleap=leapyr(year)
c      write(*,*) 'leap year:',yrstr,year,' - ',isLeap
c      write(*,*) 'dtype:',dtype

c     initiate the data matrix, -9999 means no data
      do j=1,nlon
         do k=1,nlat
            do i=1,ntim
               dataL(j,k,i)=-9999
               data(j,k,i)=-9999
            enddo
            dataL(j,k,366)=-9999
         enddo
      enddo

      do i=1,ntim
         times(i)=-9999
         timesL(i)=-9999
      enddo
      timesL(366)=-9999
      
      do i=1,nlon
         lons(i)=-9999
      enddo
      
      do i=1,nlat
         lats(i)=-9999
      enddo
c      write(*,*) 'init ok'

c     file='/home/tianyf/tmp/runof.sfc.gauss.2007.nc'
c     
c      write(*,*) 'file:',file
      retval=nf_open(file,nf_nowrite,ncid)
      if (retval.ne.nf_noerr) then
         write(*,*) 'Open file error '//file
         stop
      endif


      retval=nf_inq_varid(ncid,'lat',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lats)
c     reverse lat
c      do i=1,nlat/2
c         lat=lats(nlat-i+1)
c         lats(nlat-i+1)=lats(i)
c         lats(i)=lat
c         write(*,*) i,lats(i),nlat-i+1,lats(nlat-i+1)
c      enddo
c     !!No need for reverse the lat order. It is in 90 -> -90.
c     The data read by nf_... is contrary to the IDL routines.
c       (idl returns data from  lower-left to upper right. Why? XXX)
c       (Am I wrong before?? Dec-24-2008)

c      do i=1,nlat-1
c         if (lats(i).ge.27.and.27.ge.lats(i+1)) then
c            ilat=i
c            goto 800
c         endif
c         write(*,*) 'lat:',lats(i)
c      enddo
c 800  continue
      retval=nf_inq_varid(ncid,'lon',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lons)
c      do i=1,nlon-1
c         if (lons(i).le.112.and.112.le.lons(i+1)) then
c            ilon=i
c            goto 801
c         endif
c         write(*,*) 'lon:',lons(i)
c      enddo
c 801  continue
c      write(*,*) ilat,ilon

      retval=nf_inq_varid(ncid,'time',data_varid)
      if (isLeap) then
         retval=nf_get_var_real(ncid,data_varid,timesL)
      else
         retval=nf_get_var_real(ncid,data_varid,times)
      endif
      retval=nf_get_att_text(ncid,data_varid,UNITS,time_units)
c      write(*,*) 'Time unit:',time_units
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
c      write(*,*) 'jd:',jd_ref
c     convert time from hours to decimal year
      if (isLeap) then
         do i=1,ntimL
            days=timesL(i)/24d0
            jd=jd_ref+days
            call JD_to_Decyrs( jd, dyr )
c            timesL(i)=dyr
            timesLo(i)=dyr
         enddo
      else
         do i=1,ntim
            days=times(i)/24d0
            jd=jd_ref+days
            call JD_to_Decyrs( jd, dyr )
c            times(i)=dyr
c            timesL(i)=dyr
            timesLo(i)=dyr
c            write(*,*) dyr
         enddo
      endif
c      write(*,*) 'time ok'

c     read data section
      retval=nf_inq_varid(ncid,dtype,data_varid)
c      write(*,*) 'reading data..'
      if (isLeap) then
         retval=nf_get_var_real(ncid,data_varid,dataL)
      else
         retval=nf_get_var_real(ncid,data_varid,data)
      endif
c      write(*,*) 'done!'
      retval=nf_get_att_int(ncid,data_varid,'missing_value',
     &     missing_value)
      if (retval.eq.NF_NOERR) then
c         write(*,*) 'missing value:',missing_value
      endif
      retval=nf_get_att_real(ncid,data_varid,'scale_factor',
     &     scale_factor)
c      write(*,*) 'sf:',scale_factor
      retval=nf_get_att_real(ncid,data_varid,'add_offset',
     &     add_offset)
c      write(*,*) 'offset:',add_offset

      if (isLeap) then
c         write(*,*) 'leap'
         do i=1,ntimL
            do j=1,nlon
               do k=1,nlat
c     write(*,*) int(data(i,j,k)),' - ',missing_value
                  if (dataL(j,k,i).eq.missing_value) then
                     dataL(j,k,i)=-9999
                  else
                     dataL(j,k,i)=dataL(j,k,i)*scale_factor+add_offset
                  endif
c     write(*,700) times(i),lats(j),lons(k),data(i,j,k)
c     stop
               enddo
            enddo
         enddo
      else
         do i=1,ntim
            do j=1,nlon
               do k=1,nlat
c               write(*,*) int(data(i,j,k)),' - ',missing_value
                  if (data(j,k,i).eq.missing_value) then
c                     data(i,j,k)=-9999
                     dataL(j,k,i)=-9999
                  else
c                     data(i,j,k)=data(i,j,k)*scale_factor+add_offset
                     dataL(j,k,i)=data(j,k,i)*scale_factor+add_offset
c                     write(*,*) data(i,j,k),dataL(i,j,k)
                  endif
c     write(*,700) times(i),lats(j),lons(k),data(i,j,k)
c               stop
               enddo
            enddo
c            write(*,*) data(i,33,69),dataL(i,33,69)
         enddo
      endif

      retval=nf_close(ncid)



c     out to temp file
c      open(unit=1001,file='/home/tianyf/tmp/ttt')
c      do i=1,nlat
c         write(1001,'(192(f20.5))') (dataL(j,i,1),j=1,192)
c      enddo
c      close(1001)
c      write(*,*) 'ok'
c      stop
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
