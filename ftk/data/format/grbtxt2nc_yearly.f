CTITLE
      PROGRAM grbtxt2nc_yearly

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'
c      include 'netcdf.inc'
      include '/usr/local/netcdfi/include/netcdf.inc'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     MOD Tian Mon Jan 12 17:03:37 CST 2009
c       The sequence of output netcdf file change to 
c       (lon,lat,time) from (lat,lon,time), adopted from conventions from
c       ncep/ecco/gldas/ecmwf/... projects.
c       The new convenction now appear as 
c       "float VAR(time=?, lat=?, lon=?);" in Panoply.
c       Affected routines: read_nc.f

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 path,opath,ptn,file,ctlfile

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen,argc
      character*1 path_sep

c     --Local Parameters--
      character*1023 files(5000),tmpstr,ofile
      integer*4 nf,i,fi,j

      integer*4 nrow,ncol,nmaxrow,nmaxcol
      parameter(nmaxrow=3000,nmaxcol=3000)
      real*8 data(nmaxrow,nmaxcol),lats(nmaxrow),lons(nmaxcol)
      real*8 d_missing_value
c$$$      real datanc(nmaxrow,nmaxcol,1),latsnc(nmaxrow),lonsnc(nmaxcol)
      real datanc(nmaxrow,nmaxrow,1),latsnc(nmaxrow),lonsnc(nmaxcol)
      integer*4 ncid,ok,latdim,londim,timedim,datadim
      integer*4 dims(3),varid_lat,varid_lon,varid_time,varid_data
      real missing_value,add_offset,scale_factor
      integer*4 start(3),count(3)
c      data count /nmaxrow,nmaxcol,1/
c     write one line each time:
c      data count /1,nmaxcol,1/
      integer*4 yy,mm,dd,idate(5),doyr,hr,mi,years(100),year
      real datain(1,1,1)
      real*8 sec,mjd,jd
      real mjd1
      character*1023 tmps,tmps1,tmps2,vartype

      integer*4 iargc


c     <<VAR_DEC
      if (iargc().le.0) then
         write(*,'(a)') 'Usage: grbtxt2nc_yearly '
         write(*,'(a)') '               --path=input_path'
         write(*,'(a)') '                  default: .'
         write(*,'(a)') '               --opath=output_path'
         write(*,'(a)') '                  default: ./out.nc'
         write(*,'(a)') '               [--ptn=file_matching_pattern]'
         write(*,'(a)') '                  default: *.NOAH_*.txt'
         write(*,'(a)') '               [--var=VAR_TYPE]'
         write(*,'(a)') '                  default: WEASD'
         write(*,'(a)') '               [--ctlfile=CTL_FILE]'
         write(*,'(a)') '                  default: d.ctl in ./'
         write(*,'(a)') ''
         write(*,'(a)') '(c) Copyright by Tian 2008'
         write(*,'(a)') 'Free to use/modify for academic purpose.'
c         stop
      endif

      path='.'//path_sep()
      ofile='.'//path_sep()
      ptn='NOAH_'
      ctlfile='d.ctl'
      vartype='WEASD'

      do i=1,iargc()
         call getarg(i,tmpstr)
         if (tmpstr(1:7).eq.'--path=') then
            path=tmpstr(8:nblen(tmpstr))
         else if (tmpstr(1:8).eq.'--opath=') then
            ofile=tmpstr(9:nblen(tmpstr))
         else if (tmpstr(1:6).eq.'--ptn=') then
            ptn=tmpstr(7:nblen(tmpstr))
         else if (tmpstr(1:6).eq.'--var=') then
            vartype=tmpstr(7:nblen(tmpstr))
         else if (tmpstr(1:10).eq.'--ctlfile=') then
            ctlfile=tmpstr(11:nblen(tmpstr))
         else
            write(*,*) 'Error: invalid input!'
            stop
         endif
      enddo
      ptn='*'//ptn(1:nblen(ptn))//'*.txt'

      write(*,'(a)') '::working in '//path(1:nblen(path))
      write(*,'(a)') '::searching for '//ptn(1:nblen(ptn))
      write(*,'(a)') '::output to '//opath(1:nblen(opath))
      write(*,'(a)') '::ctl file is '//ctlfile(1:nblen(ctlfile))

      call ffind(path,files,ptn,nf,1) 
      write(*,'(a,i9)') '::#total files:',nf
      if (nf.le.0) then
         write(*,'(a)') '**no files found in '//path(1:nblen(path))
         stop
      endif

c     get years

      stop
c     initiate output netcdf file
      write(*,'(a)') '::initiate output file..'
      ok=nf_create(ofile,NF_CLOBBER,ncid)
      if (ok.ne.NF_NOERR) then
         write(*,'(a)') '!!failed to open output file '//
     &        ofile(1:nblen(ofile))
         stop
      endif


c      write(*,*) 'reading ',files(1)
      call read_grbtxt(files(1),ctlfile,data,lats,lons,nmaxrow,nmaxcol,
     &     nrow,ncol)
c      write(*,*) 'file read ok'

      write(*,'(a)') '::create definitions..'
      ok=nf_def_dim(ncid,'lon',ncol,londim)
      ok=nf_def_dim(ncid,'lat',nrow,latdim)
c      ok=nf_def_dim(ncid,'time',1,timedim)
      ok=nf_def_dim(ncid,'time',NF_UNLIMITED,timedim)
      ok=nf_def_dim(ncid,vartype,NF_UNLIMITED,datadim)

      dims(1)=londim
      dims(2)=latdim
      dims(3)=timedim
c$$$
c$$$      ok=nf_def_var(ncid,'lat',NF_DOUBLE,1,latdim,varid_lat)
c$$$      ok=nf_def_var(ncid,'lon',NF_DOUBLE,1,londim,varid_lon)
c$$$      ok=nf_def_var(ncid,'time',NF_DOUBLE,1,timedim,varid_time)
c$$$      ok=nf_def_var(ncid,'weasd',NF_DOUBLE,3,dims,varid_data)

      ok=nf_def_var(ncid,'lon',NF_REAL,1,londim,varid_lon)
      ok=nf_put_att_text(ncid,varid_lon,'units',100,'degrees_east')
      ok=nf_def_var(ncid,'lat',NF_REAL,1,latdim,varid_lat)
      ok=nf_put_att_text(ncid,varid_lat,'units',100,'degrees_north')
      ok=nf_def_var(ncid,'time',NF_REAL,1,timedim,varid_time)
      ok=nf_put_att_text(ncid,varid_time,'units',100,'MJD for 0:00')
      ok=nf_def_var(ncid,vartype,NF_REAL,3,dims,varid_data)
      ok=nf_put_att_text(ncid,varid_data,'units',100,'kg/m^2')
      d_missing_value=9.999d20
      missing_value=9.999e+20
      add_offset=0
      scale_factor=1
c      write(*,*) ncid,varid_data
      ok=nf_put_att_real(ncid,varid_data,'missing_value',NF_REAL,
     &     1,missing_value)
      if (ok.ne.NF_NOERR) then
         write(*,'(a)') '!!failed to assign missing value '
      endif
      
c     write(*,*) ok
      ok=nf_put_att_real(ncid,varid_data,'add_offset',NF_REAL,
     &     1,add_offset)
c     strange! lowercase attribute cannot shown in Panoply.
c     The below line works.
c      ok=nf_put_att_real(ncid,varid_data,'Add_Offset',NF_REAL,
c     &     1,add_offset)
c     
      if (ok.ne.NF_NOERR) then
         write(*,'(a)') '!!failed to assign add offset '
      endif
c      write(*,*) ok
      ok=nf_put_att_real(ncid,varid_data,'scale_factor',NF_REAL,
     &     1,scale_factor)
      if (ok.ne.NF_NOERR) then
         write(*,'(a)') '!!failed to assign scale factor '
      endif
c      write(*,*) ok

      ok=nf_enddef(ncid)
c      write(*,'(a)') '::definition end'


c$$$      ok=nf_put_var_double(ncid,varid_lat,lats)
c$$$      ok=nf_put_var_double(ncid,varid_lon,lons)
      do i=1,ncol
         lonsnc(i)=lons(i)
      enddo
      do i=1,nrow
         latsnc(i)=lats(i)
      enddo
c$$$      ok=nf_put_var_real(ncid,varid_lat,latsnc)
c$$$      ok=nf_put_var_real(ncid,varid_lon,lonsnc)
c$$$      ok=nf_put_var_real_1(ncid,varid_lat,latsnc,nmaxrow,nrow)
c$$$      ok=nf_put_var_real_1(ncid,varid_lon,lonsnc,nmaxcol,ncol)
      do i=1,ncol
         ok=nf_put_vara_real(ncid,varid_lon,i,1,lonsnc(i))
      enddo
      do i=1,nrow
         ok=nf_put_vara_real(ncid,varid_lat,i,1,latsnc(i))
      enddo



      do fi=1,nf
         file=files(fi)
         write(*,700) '::',fi,nf,file(1:nblen(file))
 700     format(a,i4,"/",i4,"<<",a)
         call read_grbtxt(file,ctlfile,data,lats,lons,nmaxrow,nmaxcol,
     &        nrow,ncol)
c$$$         do i=1,nrow
c$$$            do j=1,ncol
c$$$               write(*,*) data(i,j)
c$$$            enddo
c$$$         enddo
c$$$         stop
c         write(*,*) nrow,ncol
c         stop
c         call read_ds607_txt(file,'',data,lats,lons,nrow,ncol)
c         ok=nf_put_var_double(ncid,varid_data,data)
         do i=1,nrow
            do j=1,ncol
c               write(*,*) data(i,j),d_missing_value
               if (data(i,j).eq.d_missing_value) then
                  datanc(i,j,1)=data(i,j)
c                  write(*,*) data(i,j),datanc(i,j,1),d_missing_value
               else
c$$$                  datanc(i,j,1)=data(i,j)
c     unpacked_data_value = packed_data_value * scale_factor + add_offset
c     packed_data_value = nint((unpacked_data_value - add_offset) / scale_factor)
                  if (vartype.eq.'Qs'.or.vartype.eq.'Qsb') then
c     past 3-hour average for runoff
                     datanc(i,j,1)=
     &                    ((data(i,j)-add_offset)/scale_factor)*
     &                    24d0*3600*3
                  else
                     datanc(i,j,1)=
     &                    ((data(i,j)-add_offset)/scale_factor)
                  endif
               endif
            enddo
         enddo
c         start(1)=1
c         start(2)=1
c         start(3)=fi
c$$$         ok=nf_put_vara_double(ncid,varid_data,start,count,datanc)

c         write(*,'(a)') '::writing ..'
c     write one point each time
         count(1)=1
         count(2)=1
         count(3)=1
         start(3)=fi
         do i=1,nrow
            start(2)=i
            do j=1,ncol
               start(1)=j
               datain(1,1,1)=datanc(i,j,1)
               ok=nf_put_vara_real(ncid,varid_data,start,
     &              count,datain)
            enddo
         enddo
c$$$         call nf_put_vara_real_2(ncid,varid_data,
c$$$     &        start,count,datanc,nmaxrow,nmaxcol,nrow,ncol)
c         write(*,'(a)') '::ok.'
c     output time
         call getfilename(file,tmps)
c$$$         read(tmps(1:4),*) yy
c$$$         read(tmps(5:6),*) mm
c$$$         read(tmps(7:8),*) dd
         hr=12
         mi=0
         sec=0d0
c     for 025 noah gldas
         if (index(tmps,'GLDAS_NOAH025_M').gt.0) then
            read(tmps(18:21),*) yy
            read(tmps(22:23),*) mm
            dd=15
         elseif (index(tmps,'GLDAS_NOAH10_M').gt.0) then
c     for 10 noah gldas
            read(tmps(17:20),*) yy
            read(tmps(21:22),*) mm
c         read(tmps(7:8),*) dd
            dd=15
         elseif (index(tmps,'GLDAS_VIC10_M').gt.0) then
c     for 10 VIC gldas
c     GLDAS_VIC10_M.A199901.001.grb-SoilMoist3.txt
            read(tmps(16:19),*) yy
            read(tmps(20:21),*) mm
c         read(tmps(7:8),*) dd
            dd=15
         elseif (index(tmps,'GLDAS_VIC10_3H').gt.0) then
c     for 10 VIC gldas 3h
c     GLDAS_VIC10_3H.A2008358.1200.001.grb-SWE.txt
            read(tmps(17:20),*) yy
            read(tmps(21:23),*) doyr
            dd=doyr
            mm=1
c            call yds_to_jd ( yy, doyr, 12d0*3600, jd )
c            call jd_to_y
            read(tmps(25:26),*) hr
            read(tmps(27:28),*) mi
         else
            write(*,'(a)') '!!Unknow data type. Please keep the '
     &           //' origional data file naming convenction.'
            stop
         endif
c         write(*,*) yy,mm,dd
         idate(1)=yy
         idate(2)=mm
         idate(3)=dd
         idate(4)=hr
         idate(5)=mi
         sec=0
         call ymdhms_to_mjd(idate,sec,mjd)
c$$$         ok=nf_put_vara_double(ncid,varid_time,fi,1,mjd)
         mjd1=mjd
         
c         write(*,'(a,f)') '::writing time',mjd1
c         stop
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

c$$$
c$$$      subroutine nf_put_vara_real_2(ncid,varid_data,
c$$$     &     start,count,datanc,nmaxrow,nmaxcol,nrow,ncol)
c$$$      integer*4 ncid,varid_data,start(3),count(3),nmaxrow,
c$$$     &     nmaxcol,nrow,ncol,i,j
c$$$      integer ok,nf_put_vara_real
c$$$      real data(ncol),datanc(nmaxrow,nmaxcol)
c$$$c      data count /1,ncol,1/
c$$$      count(1)=1
c$$$      count(2)=ncol
c$$$      count(3)=1
c$$$      start(2)=ncol
c$$$      start(3)=1
c$$$      do i=1,nrow
c$$$         do j=1,ncol
c$$$            data(j)=datanc(i,j)
c$$$         enddo
c$$$         start(1)=i
c$$$         ok=nf_put_vara_real(ncid,varid_data,start,count,data)
c$$$      enddo
c$$$      
c$$$
c$$$      end
