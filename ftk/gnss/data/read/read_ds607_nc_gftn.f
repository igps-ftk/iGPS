CTITLE
      subroutine read_ds607_nc(file,data,dtype,lons,lats,
     &     timesO,
     &     nlat,nlon,ntim,ntimm)

c     --PURPOSE--
c     read ds607 grid fiels (daily avarages)

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     Created by Tian on Sun Jan  4 17:05:23 CST 2009.

c      IMPLICIT NONE

c      include '/usr/local/netcdf-3.6.3-gftn/include/netcdf.inc'
      include '/usr/local/netcdf-3.6.3-gftn/include/netcdf.inc'
C      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
      integer nlat,nlon,ntim,ntimm
c      parameter(nlat=94,nlon=192,ntim=365,ntiml=366)

c     --INPUT--
      character*(*) file
      character*(*) dtype

c     --OUTPUT--
      real data(nlat,nlon,ntimm),lats(nlat),lons(nlon),times(ntimm),lat
      real datat(nlat,nlon,1),timet
      real*8 timesO(ntimm)

c     --EXTERNAL--

c     --Local Parameters--
      integer i,j,k,ilon,ilat,itim

      integer retval,ncid,data_varid,data_in,time_varid
      integer missing_value
      real add_offset
      real scale_factor

C     To check the units attributes.
      character*100 UNITS
      parameter (UNITS = 'units')
      character*100 DATA_UNITS, TIME_UNITS, LAT_UNITS, LON_UNITS
      
      integer year,mon,day,hh,mm,idate(5),hrs
      real*8 dyr,jd,mjd,second,days,jd_ref
      
      character*100 buf,bufs(100),buf0,buf1,buf2
      integer nbuf

      integer dimid_tim,timeid_tim,count(3),start(3)
      character*1023 namedi


      integer nblen

c     initiate the data matrix, -9999 means no data
      write(*,*) 'initiating arrays..'
      do k=1,nlon
         do j=1,nlat
            do i=1,ntimm
               data(j,k,i)=-9999
            enddo
            datat(j,k,1)=-9999
         enddo
      enddo

      do i=1,ntimm
         times(i)=-9999
         timesO(i)=-9999
      enddo
      timet=-9999
      
      do i=1,nlon
         lons(i)=-9999
      enddo
      
      do i=1,nlat
         lats(i)=-9999
      enddo
      write(*,*) 'init ok'

c      write(*,*) 'file:',file
      retval=nf_open(file,nf_nowrite,ncid)
      if (retval.ne.nf_noerr) then
         write(*,*) 'Open file error '//file
         stop
      endif


      retval=nf_inq_varid(ncid,'lat',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lats)
      write(*,*) 'lats ok'
      retval=nf_inq_varid(ncid,'lon',data_varid)
      retval=nf_get_var_real(ncid,data_varid,lons)
      write(*,*) 'lons ok'
      retval=nf_inq_varid(ncid,'time',time_varid)
      retval=nf_inq_vardimid(ncid,time_varid,dimid_tim)
      retval=nf_inq_dim(ncid,dimid_tim,namedi,ntim)
      write(*,*) 'ntim:',ntim
      retval=nf_inq_varid(ncid,dtype,data_varid)

      retval=nf_get_att_int(ncid,data_varid,'MissingValue',
     &     missing_value)
      if (retval.eq.NF_NOERR) then
         write(*,*) 'missing value:',missing_value
      endif
      retval=nf_get_att_real(ncid,data_varid,'Scale_Factor',
     &     scale_factor)
      write(*,*) 'sf:',scale_factor
      retval=nf_get_att_real(ncid,data_varid,'Add_Offset',
     &     add_offset)
      write(*,*) 'offset:',add_offset

      count(1)=nlat
      count(2)=nlon
      count(3)=1

c      write(*,*) 'reading data...'
      do i=1,ntim
         write(*,*) '::[ds607_nc]reading section ',i
         
         retval=nf_get_vara_real(ncid,time_varid,i,1,timet)
c         write(*,*) timet
         jd=timet+2 400 000.5d0
c         write(*,*) timet,2 400 000.5d0
         call jd_to_decyrs(jd,dyr)
         times(i)=dyr
         timesO(i)=dyr
c         write(*,*) 'dyr:',dyr,jd,times(i)

c     read data section
c      write(*,*) 'reading data..'
         start(1)=1
         start(2)=1
         start(3)=i
         
         retval=nf_get_vara_real(ncid,data_varid,start,count,datat)
c      write(*,*) 'done!'

     
         do k=1,nlon
            do j=1,nlat
c     write(*,*) int(data(i,j,k)),' - ',missing_value
               if (datat(j,k,1).eq.missing_value.or.
     &             isnan(datat(j,k,1)))
     &              then
                  data(j,k,i)=-9999
               else
                  data(j,k,i)=datat(j,k,1)*scale_factor+add_offset
c                  data(j,k,i)=datat(j,k,1)
               endif
c               write(*,700) times(i),lats(j),lons(k),data(j,k,i)
 700           format(4f20.8)
c     stop
            enddo
         enddo
c         stop
      enddo

      retval=nf_close(ncid)

       END
