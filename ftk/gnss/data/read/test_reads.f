      program test_reads
c     -
      implicit none
      include '../../../inc/ftk.h'
      integer*4 nrow,ncol
      character*8 firstepoch(2),lastepoch(2)
      real*8 xyzref(3),neuref(3)
      character*1023 file
c$$$      integer*4 nmax
c$$$      parameter(nmax=1000)
c$$$      character*10 sites(nmax),sitesref(nmax)
c$$$      integer*4 nsite,nsiteref
      integer*4 i,j
      real*8 xyz(3),ll(2),lat(2),lon(2)
c      real*8 data(nmax,9)
      real*8 data(nmax_row,nmax_col)
c     for read_poletide_bull_a
      real*8 mjdall(nmax_pt),xpall(nmax_pt),ypall(nmax_pt)
      integer*4 n
      integer nblen

      character*512 headers(nmax_head),tmpstr
      integer*4 nhead
      
c     for read_psxy
      real*8 xys(2,nmax_row)
      integer*4 npts(nmax_row),nline,j1,j2  
c$$$
c$$$      real*8 datads607(224,464),lats(224),lons(464)
c$$$
c$$$      character*1023 dsfile,dtypedsnc
c$$$      integer nrowdsnc,ncoldsnc,ntimdsnc,ntimmdsnc
c$$$      parameter(nrowdsnc=224,ncoldsnc=464,ntimmdsnc=800)
c$$$      real datadsnc(nrowdsnc,ncoldsnc,ntimmdsnc),lonsdsnc(ncoldsnc)
c$$$      real latsdsnc(nrowdsnc),timesdsnc(ntimmdsnc)
      character*20 dtype
      real datao(600,1440,12),latso(600),lonso(1440)
      real*8 timeso(12)
      integer nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
c     ---
      file='fa_ygr.psxy'
      call read_psxy(file,xys,npts,nline)
      j1=1
      do i=1,nline
        write(*,*) 'line:',i
        j2=j1+npts(i)-1
        write(*,*) 'j1,j2:',j1,j2
        do j=j1,j2
          write(*,*) '    ',xys(1,j),xys(2,j)
        enddo
        j1=j1+npts(i)
      enddo

      stop
     
      file='/home/tianyf/tmp/tenv3/00NA.IGS08.tenv3'
      call read_tenv3(file,data,nrow,ncol)
      write(*,*) nrow,ncol,file
      write(*,*) (data(1,i),i=1,ncol)
      stop

      file='/home/tianyf/scec/2009.167.010935/'//
     +     'ana1.2009.167.010935.csv'
      call read_scec_csv(file,data,nrow,ncol,nhead,headers)
      write(*,*) nhead,nrow,ncol,file
      stop

      file='/home/tianyf/gldas/GLDAS_NOAH025_M.A2007.nc'
      file='/home/tianyf/gldas/GLDAS_NOAH10_M.A2007.nc'
      dtype='WEASD'
      write(*,*) 'calling ...'
      nmaxlat=600
      nmaxlon=1440
      nmaxtim=12
      nlat=600
      nlon=1440
      ntim=12
      nlat=150
      nlon=360
      write(*,*) nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
c      call  read_nc(file,dtype
      call  read_nc(file,dtype,dataO,latsO,lonsO,timesO
c     &     600,1440,12,600,1440,12)
     &     ,nmaxlat,nmaxlon,nmaxtim
     &     ,nlat,nlon,ntim
     &     )
c$$$      do i=1,nlat
c$$$         do j=1,nlon
c$$$            write(*,*) i,j,datao(i,j,1)
c$$$         enddo
c$$$      enddo
      write(*,*) nmaxlat,nmaxlon,nmaxtim,nlat,nlon,ntim
      stop

c$$$c     Sun Jan  4 17:30:50 CST 2009
c$$$      dsfile='/home/tianyf/ds607/weasd.nc.96-98.nc'
c$$$      dtypedsnc='weasd'
c$$$      call read_ds607_nc(dsfile,dataDsNc,dtypeDsNc,lonsDsNc,latsDsNc,
c$$$     &     timesDsNc,
c$$$     &     nrowDsNc,ncolDsNc,ntimDsNc,ntimmDsNc)
c$$$      stop
c$$$
c$$$c     Sat Jan  3 10:09:32 CST 2009
c$$$      file='/home/tianyf/ds607/d_txt/19971127.NOAH_d.grb-weasd.txt'
c$$$      call read_ds607_txt(file,'',datads607,lats,lons,224,464)
c$$$c$$$      do i=1,224
c$$$c$$$         write(*,*) lats(i)
c$$$c$$$      enddo
c$$$c$$$      do i=1,464
c$$$c$$$         write(*,*) lons(i)
c$$$c$$$      enddo
c$$$      stop
c$$$      
c$$$      goto 804
c$$$      nsite=0
c$$$      nsiteref=0
c$$$      file='/export/home/tianyf/tmp/ab07CleanUnf.neu'
c$$$      call read_sio(file, data,nrow, ncol, nhead, headers)
c$$$      write(*,*) nrow,ncol,nhead,file
c$$$      goto 803
c$$$      goto 800
c$$$      file='/export/home/jlm/gps/gpsi/conf/setup/glb_igs2.list.txt'
c$$$      file='/home/tianyf/gpsi/conf/setup/glb_igs2.list.txt'
c$$$      file='/export/home/tianyf/data/pbo/final/AB37.bsl.final_frame.pos'
c$$$      write(*,*) 'file:',file(1:nblen(file))
c$$$      call query_pbo(file,nrow,ncol,firstepoch,lastepoch,
c$$$     &     xyzref,neuref)
c$$$
c$$$      write(*,*) xyzref
c$$$
c$$$      call read_pbo(file,data,nrow,ncol)
c$$$      do i=1,nrow
c$$$c         write(*,'(10f15.5,:)') (data(i,j),j=1,9)
c$$$      enddo
c$$$      write(*,*) 'nrow:',nrow
c$$$c     Does F77 support select/case clause?
c$$$c     Seems NO.
c$$$c$$$      case(nrow)
c$$$c$$$      case 382
c$$$c$$$      write(*,*) 382
c$$$c$$$      end select
c$$$      file='/export/home/tianyf/data/jpl/filtered/7ODM.lat'
c$$$      call l_l_r_read(file,data,nrow,ncol)
c$$$      write(*,*) '#n:',nrow,ncol
c$$$
c$$$ 800  continue
c$$$      goto 801
c$$$      file='/export'
c$$$     . //'/home/tianyf/gpsf/cgps/conf/poletide/finals2000A.all.d'
c$$$      write(*,*) 'test read pole tide file:',file(1:nblen(file))
c$$$      call read_poletide_bull_a(file,mjdall,xpall,ypall,n)
c$$$      write(*,*) ' pole tide read OK. writing..'
c$$$      do i=1,n
c$$$         write(*,*) mjdall(i),xpall(i),ypall(i)
c$$$      enddo
c$$$      write(*,*) '#rec:',n
c$$$      
c$$$ 801  continue
c$$$      goto 802
c$$$      file='/export/home/tianyf/data/usgs/fullseries/7ODM.ts'
c$$$      call read_usgs(file,data,nrow,ncol,nhead,headers)
c$$$      write(*,*) nrow,ncol,nhead
c$$$
c$$$ 802  file='/export/home/tianyf/data/panga/nev/panga/daily/'//
c$$$     .     'cleaned/ALBH.lat'
c$$$      call read_l_l_r_panga(file,data,nrow,ncol,nhead,headers)
c$$$      write(*,*) nrow,ncol,nhead
c$$$      do i=1,nhead
c$$$         tmpstr=headers(i)
c$$$         write(*,*) i,tmpstr(1:nblen(tmpstr))
c$$$      enddo
c$$$      do i=nrow-5,nrow
c$$$         write(*,*) i,(data(i,j),j=1,ncol)
c$$$      enddo
c$$$
c$$$c     cmonoc
c$$$
c$$$ 803  continue
c$$$      write(*,*) 'test reading cmonoc time series products....'
c$$$      file='/export/home/tianyf/cmonoc/timeseries/lower'//
c$$$     &     '/bjfs.list'
c$$$      call read_cmonoc(file,data,nrow,ncol)
c$$$      do i=1,nrow
c$$$c         write(*,'(10F15.5)') (data(i,j),j=1,ncol)
c$$$      enddo
c$$$      write(*,*) nrow,ncol
c$$$      
c$$$ 804  continue
c$$$      file='/export/home/tianyf/ac/proc/realtime/'
c$$$     &     //'2007/337/b/336/met_guam.07336'
c$$$      call read_met(file,data,nrow,ncol)
c$$$      write(*,*) '#ROW,COL:',nrow,ncol
c$$$      do i=1,nrow
c$$$         write(*,*) (data(i,j),j=1,ncol)
c$$$      enddo
      stop
      end
