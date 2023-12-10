CTITLE
      PROGRAM ds607_txt2nc

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'
c      include 'netcdf.inc'
      include '/usr/local/netcdfi/include/netcdf.inc'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 path,ofile,ptn,file

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen,argc
      character*1 path_sep

c     --Local Parameters--
      character*1023 files(5000),tmpstr
      integer*4 nf,i,fi,j

      integer*4 nrow,ncol
      parameter(nrow=224,ncol=464)
      real*8 data(nrow,ncol),lats(nrow),lons(ncol)
      real datanc(nrow,ncol,1),latsnc(nrow),lonsnc(ncol)
      integer*4 ncid,ok,latdim,londim,timedim,datadim
      integer*4 dims(3),varid_lat,varid_lon,varid_time,varid_data
      real missingValue,add_offset,scale_factor
      integer*4 start(3),count(3)
      data count /nrow,ncol,1/
      integer*4 yy,mm,dd,idate(5)
      real*8 sec,mjd,jd
      real mjd1
      character*1023 tmps,tmps1,tmps2
      real*8 zero_r8
      
      integer*4 iargc


c     <<VAR_DEC
      if (iargc().lt.0) then
         write(*,'(a)') 'Usage: ds607_txt2nc '
         write(*,'(a)') '               --path=input_path'
         write(*,'(a)') '                  default: .'
         write(*,'(a)') '               --ofile=output_file'
         write(*,'(a)') '                  default: ./out.nc'
         write(*,'(a)') '               [--ptn=file_matching_pattern]'
         write(*,'(a)') '                  default: *.NOAH_*.txt'
         write(*,'(a)') ''
         write(*,'(a)') '(c) Copyright by Tian 2008'
         write(*,'(a)') 'Free to use/modify for academic purpose.'
c         stop
      endif

      path='.'//path_sep()
      ofile='.'//path_sep()//'out.nc'
      ptn='*.NOAH_*.txt'

      do i=1,iargc()
         call getarg(i,tmpstr)
         if (tmpstr(1:7).eq.'--path=') then
            path=tmpstr(8:nblen(tmpstr))
         else if (tmpstr(1:8).eq.'--ofile=') then
            ofile=tmpstr(9:nblen(tmpstr))
         else if (tmpstr(1:6).eq.'--ptn=') then
            ptn=tmpstr(7:nblen(tmpstr))
         else
            write(*,*) 'Error: invalid site information input!'
            stop
         endif
      enddo
      
      if (iargc().ge.1) then
      	zero_r8=0d0
      else
      	zero_r8=1d0
      endif

      write(*,'(a)') '::working in '//path(1:nblen(path))
      write(*,'(a)') '::searching for '//ptn(1:nblen(ptn))
      write(*,'(a)') '::output to '//ofile(1:nblen(ofile))

      call ffind(path,files,ptn,nf,1) 
      write(*,*) '#total files:',nf
      if (nf.le.0) then
         write(*,'(a)') '**no files found in '//path(1:nblen(path))
         stop
      endif

c     initiate output netcdf file
      ok=nf_create(ofile,NF_CLOBBER,ncid)
      if (ok.ne.NF_NOERR) then
         write(*,'(a)') '!!failed to open output file '//
     &        ofile(1:nblen(ofile))
         stop
      endif

      ok=nf_def_dim(ncid,'lat',224,latdim)
      ok=nf_def_dim(ncid,'lon',464,londim)
c      ok=nf_def_dim(ncid,'time',1,timedim)
      ok=nf_def_dim(ncid,'time',NF_UNLIMITED,timedim)
      ok=nf_def_dim(ncid,'weasd',NF_UNLIMITED,datadim)

      dims(1)=latdim
      dims(2)=londim
      dims(3)=timedim
c$$$
c$$$      ok=nf_def_var(ncid,'lat',NF_DOUBLE,1,latdim,varid_lat)
c$$$      ok=nf_def_var(ncid,'lon',NF_DOUBLE,1,londim,varid_lon)
c$$$      ok=nf_def_var(ncid,'time',NF_DOUBLE,1,timedim,varid_time)
c$$$      ok=nf_def_var(ncid,'weasd',NF_DOUBLE,3,dims,varid_data)

      ok=nf_def_var(ncid,'lat',NF_REAL,1,latdim,varid_lat)
      ok=nf_put_att_text(ncid,varid_lat,'units',100,'degrees_north')
      ok=nf_def_var(ncid,'lon',NF_REAL,1,londim,varid_lon)
      ok=nf_put_att_text(ncid,varid_lon,'units',100,'degrees_east')
      ok=nf_def_var(ncid,'time',NF_REAL,1,timedim,varid_time)
      ok=nf_put_att_text(ncid,varid_time,'units',100,'MJD for 0:00')
      ok=nf_def_var(ncid,'weasd',NF_REAL,3,dims,varid_data)
      ok=nf_put_att_text(ncid,varid_data,'units',100,'kg/m^2')
      missingValue=9.999e+20
      add_offset=0
      scale_factor=1
c$$$      ok=nf_put_att_double(ncid,varid_data,'MissingValue',NF_DOUBLE,
c$$$     &     1,missingValue)
      ok=nf_put_att_real(ncid,varid_data,'MissingValue',NF_REAL,
     &     1,missingValue)
      ok=nf_put_att_real(ncid,varid_data,'Add_Offset',NF_REAL,
     &     1,add_offset)
      ok=nf_put_att_real(ncid,varid_data,'Scale_Factor',NF_REAL,
     &     1,scale_factor)

      ok=nf_enddef(ncid)

      call read_ds607_txt(files(1),'',data,lats,lons,nrow,ncol)
c$$$      ok=nf_put_var_double(ncid,varid_lat,lats)
c$$$      ok=nf_put_var_double(ncid,varid_lon,lons)
      do i=1,nrow
         latsnc(i)=lats(i)
      enddo
      do i=1,ncol
         lonsnc(i)=lons(i)
      enddo
      ok=nf_put_var_real(ncid,varid_lat,latsnc)
      ok=nf_put_var_real(ncid,varid_lon,lonsnc)


      do fi=1,nf
         file=files(fi)
         write(*,700) '::',fi,nf,file(1:nblen(file))
 700     format(a,i4,"/",i4,"<<",a)
         call read_ds607_txt(file,'',data,lats,lons,nrow,ncol)
c         ok=nf_put_var_double(ncid,varid_data,data)
         do i=1,nrow
            do j=1,ncol
               if (data(i,j).ge.1e5) then
                  datanc(i,j,1)=0/dsqrt(zero_r8)
               else
c$$$                  datanc(i,j,1)=data(i,j)
c     unpacked_data_value = packed_data_value * scale_factor + add_offset
c     packed_data_value = nint((unpacked_data_value - add_offset) / scale_factor)
                  datanc(i,j,1)=
     &                 ((data(i,j)-add_offset)/scale_factor)
               endif
            enddo
         enddo
         start(1)=1
         start(2)=1
         start(3)=fi
c$$$         ok=nf_put_vara_double(ncid,varid_data,start,count,datanc)
         ok=nf_put_vara_real(ncid,varid_data,start,count,datanc)
c     output time
         call getfilename(file,tmps)
         read(tmps(1:4),*) yy
         read(tmps(5:6),*) mm
         read(tmps(7:8),*) dd
         idate(1)=yy
         idate(2)=mm
         idate(3)=dd
         idate(4)=0
         idate(5)=0
         sec=0
         call ymdhms_to_mjd(idate,sec,mjd)
c$$$         ok=nf_put_vara_double(ncid,varid_time,fi,1,mjd)
         mjd1=mjd
         ok=nf_put_vara_real(ncid,varid_time,fi,1,mjd1)
 800     continue
      enddo

      ok=nf_close(ncid)
      if (ok.ne.NF_NOERR) then
         write(*,'(a)') '!!failed to close output file '//
     &        ofile(1:nblen(ofile))
         stop
      endif
      STOP
      END
