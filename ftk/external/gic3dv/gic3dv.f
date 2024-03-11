      program gic3dv
c --------------------------------------------------------------------
c   gic3dv (GPS and InSAR combination for 3D velocity): a program to combine GPS 
c   and InSAR data and produce 3D velocity field. Zheng-Kang Shen.  12/02/2019.
c
c   to run the program:
c   % gic3dv < gic3dv.drv
c
c   ***TYF**>>>
c   Modified by tianyf on Thu Dec  7 14:41:59 CST 2023
c     +increase the maximum values for a few parameters
c        maximum number of GNSS sites from 2000 to 15000 (maxsit)
c        maximum number of InSAR files from 5 to 25 (maxsar)
c        maximum number of pixels in each InSAR image from 12000 to 120000 (maxpxl)
c        maximum number of creeping fault from 10 to 10 (maxc) ! error occurred when changing its value?
c
c     +increase the length of file name variable
c        length changed from 25 to 1025 (gpsfh,gpsfv,sarf(maxsar),outf,crpf,resf,orbf)
c        change the method of reading file names (from fixed length to unformatted)
c        can also handle files in subdirectories (f77 stop reading if it encounters a path separator (/)) 
c   **TYF***<<<

c ---------------------------------------------------------------------
      implicit real*8 (a-h,l,o-z)
c      parameter (maxsit =2000,maxpxl=12000,maxc=10,maxsar=5)  
      parameter (maxsit=15000,maxpxl=120000,maxc=10,maxsar=25)  

      parameter (maxold=15000,maxdat=3*maxold+3*maxsar)
c      parameter (maxold=20000,maxdat=3*maxold+3*maxsar)

      character*100 head
      character*1025 gpsfh,gpsfv,sarf(maxsar),outf,crpf,resf,orbf
c
      character*1025 tmpstr
      
      character*1025 gps_h_outf,gps_v_outf,gps_prdf
      character*8 stnlh(maxsit+1),stnlv(maxsit+1)
      integer*8 nstn,nstnh,nstnv,ncrp,nsite,ms,nold,ndata
      dimension lonlh(maxsit+1),latlh(maxsit+1),isd(300,300)
      dimension lonlv(maxsit+1),latlv(maxsit+1)
      dimension lonh(maxsit),lath(maxsit),uxlh(maxsit),sxlh(maxsit),
     .  uylh(maxsit),sylh(maxsit),cxyh(maxsit),uzlh(maxsit),szlh(maxsit)
      dimension lonv(maxsit),latv(maxsit),uxlv(maxsit),sxlv(maxsit),
     .  uylv(maxsit),sylv(maxsit),cxyv(maxsit),uzlv(maxsit),szlv(maxsit)
      dimension glon(maxsit),glat(maxsit),qlon(1),qlat(1)
      dimension posh(2,maxsit),posv(2,maxsit)
      dimension areah(maxsit),areav(maxsit)
      dimension careah(maxsit),careav(maxsit)
      dimension alon(maxc),alat(maxc),blon(maxc),blat(maxc)
      dimension bbz(6),dcs(maxc),dsn(maxc),tx(1),ty(1)
      dimension a(maxdat,maxdat),aat(maxdat,maxdat),indx1(maxdat)
      dimension bb(6),ainv(6,6),b(maxdat),bbt(maxdat)
      dimension ainvm(maxdat,maxdat),v(maxdat),bbr(maxdat)
      dimension azinv(6,6),x(3),covx(3,3),prd(20)
      dimension prd0(20),asv(20,3),bsv(20),isat(maxsar),f_sar(maxsar)
      dimension tlon(maxsar,maxpxl),tlat(maxsar,maxpxl),
     1     sazi(maxsar,maxpxl),slook(maxsar,maxpxl),
     2     clos(maxsar,maxpxl),clos_s(maxsar,maxpxl),
     3     tlonmean(maxsar),tlatmean(maxsar),ndat(maxsar)
      dimension vxs(maxsit+1),vys(maxsit+1),vx_sig(maxsit+1),
     1    vy_sig(maxsit+1),vzs(maxsit+1),vz_sig(maxsit+1),tmp(maxsit+1)
      common /veldath/nstnh,lonh,lath,uxlh,sxlh,uylh,sylh,uzlh,szlh,
     1     cxyh,areah
      common /veldatv/nstnv,lonv,latv,uxlv,sxlv,uylv,sylv,uzlv,szlv,
     1     cxyv,areav
      common /crpdat/ncrp,alon,alat,blon,blat,dcs,dsn
      common /ainv_vh/ainv,bb,rtau,wtt,chisq,nslct
      common /ainv_vv/azinv,bbz,rtauz,wttz,chisqz,nslctz
      common /prmtr/pi,cov,cutoff_dis,wt_az
*
*   setup the constants                          
*
      pi = 4.0d0*datan(1.0d0)
      cov = pi/180.0d0
      ms = 1
      isl = 0
      do i = 1, maxdat
         do j = 1, maxdat
            a(i,j) = 0.0
         enddo
      enddo
*
*   read input file names and options 
*
      read(5,'(a)') tmpstr                                      ! input horizontal gps data file
      gpsfh=tmpstr(1:index(tmpstr,' ')-1)
      read(5,'(a)') tmpstr                                      ! input vertical gps data file
      gpsfv=tmpstr(1:index(tmpstr,' ')-1)
      read(5,*) idvu0, dvu0                                    ! 1=use, 0=not to use default GPS vertical velocity uncertainty, and the default vertical uncertainty value (mm/yr)
      read(5,*) offsig, rmpsig                                 ! a priori uncertainties for offset and ramp corrections of SAR data (mm, mm/deg)
      read(5,*) rtau_h, rtau_v                                 ! default smoothing distances for estimations of horizontal and vertical interpolation velocity uncertainties (km)
      read(5,*) lonmin0, lonmax0, latmin0, latmax0             ! input interpolation region for GPS only solution

      read(5,*) nsarf                                          ! # of InSAR data input files
      do i = 1, nsarf
         read(5,'(a)') tmpstr                                 ! insar data input file names
c         write(*,*) tmpstr
         sarf(i)=tmpstr(1:index(tmpstr,' ')-1)
c         write(*,*) 'reading ',tmpstr(1:index(sarf(i),' ')-1)
c         stop
      enddo
      read(5,*) (f_sar(i),i=1,nsarf)                           ! input scaling factors of InSAR LOS data input
      read(5,*) ilos0, dlos0, dlos1                            ! 1=use, 0=not to use InSAR default LOS rate uncertainty, default LOS rate uncertainty, minimum uncertainty
      read(5,*) lonmin,lonmax,latmin,latmax,dlon,dlat          ! input search region and increments (deg)
      read(5,*) dlon1, dlat1                                   ! input InSAR data grid increment for InSAR image ramp estimation (deg)
      read(5,'(a)') tmpstr                                       ! InSAR ramp parameter output file
      orbf=tmpstr(1:index(tmpstr,' ')-1)
      read(5,'(a)') tmpstr                                       ! Velocity solution output file
      outf=tmpstr(1:index(tmpstr,' ')-1)
      read(5,'(a)') tmpstr                                       ! InSAR data postfit residual output file
      resf=tmpstr(1:index(tmpstr,' ')-1)
      read(5,'(a)') tmpstr                                 ! Interpolated horizontal velocities at GPS sites 
      gps_h_outf=tmpstr(1:index(tmpstr,' ')-1)
      read(5,'(a)') tmpstr                                 ! Interpolated vertical velocities at GPS sites 
      gps_v_outf=tmpstr(1:index(tmpstr,' ')-1)
      read(5,'(a)') tmpstr                                   ! Interpolated 3D regional GPS velocities and uncertainties
      gps_prdf=tmpstr(1:index(tmpstr,' ')-1)
      read(5,*) is_wght                                        ! distance weighting scheme: 1=gaussian, 2=quadratic
      read(5,*) id_wght                                        ! spatial weighting scheme: 1=azimuth, 2=voronoi area
      read(5,*) min_tau, max_tau, in_tau                       ! minimum, maximum, and incremental for search of spatial smoothing constants (km)
      read(5,*) wt0                                            ! weighting threshold W for interpolation 
      read(5,*) rsgah, rsgav                                   ! minimum uncertainty thresholds for GPS horizontal and vertical velocities (mm)
      if (nsarf .gt. maxsar) then
         write (6,*) 'number of sar files greater than allowance'
         stop
c      else
c         do i = 1, nsarf                                                   !  iteration over sar input files
c        write(*,*) 'insar ',i,sarf(i)
c        enddo
      endif
      write(*,*) 'gpsfh:',gpsfh
      open(4,file=gpsfh,status='old')                           ! open horizontal gps velocity data
      open(8,file=gpsfv,status='old')                           ! open vertical gps velocity data
      open(41,file=gps_h_outf,status='unknown')                 ! open file for interpolated horizontal velocities at gps sites
      open(42,file=gps_v_outf,status='unknown')                 ! open file for interpolated vertical velocities at gps sites
      open(79,file=gps_prdf,status='unknown')                   ! open file for interpolated 3D regional gps velocities
      open(11,file=resf,status='unknown')                       ! open file for InSAR data postfit residuals
      write(11,18)
18    format('Sat#   Long      Lat       Wobsv     Wpred    Wresi',
     . '     NWobsv    NWpred    NWresi')

      i = 1
30    read(4,'(a)',end=40) head
      if (head(1:1).eq.'*') goto 30
      write(*,*) head
      read(head,35) stnlh(i),lonlh(i),latlh(i),                 ! read in horizontal gps velocity data
     .    uxlh(i),sxlh(i),uylh(i),sylh(i),cxyh(i),uzl0,szl0
      if (lonlh(i).gt.180.0d0 ) lonlh(i) = lonlh(i)-360.0d0
      if (sxlh(i).lt.rsgah) sxlh(i)=rsgah  
      if (sylh(i).lt.rsgah) sylh(i)=rsgah  
      i = i + 1
      goto 30
35    format(a8,2f10.4,2(f7.2,f5.2),f7.3,f7.2,f5.2)
40    close(4)
      nstnh=i-1

      i = 1
42    read(8,'(a)',end=44) head
      if (head(1:1).eq.'*') goto 42
      read(head,35) stnlv(i),lonlv(i),latlv(i),                 ! read in vertical gps velocity data
     .    uxl0,sxl0,uyl0,syl0,cxy0,uzlv(i),szlv(i)
      if (lonlv(i).gt.180.0d0 ) lonlv(i) = lonlv(i)-360.0d0
      if (szlv(i).lt.rsgav) szlv(i)=rsgav
      i = i + 1
      goto 42
44    close(8)
      nstnv=i-1

      iden = 0
      do i = 1, nstnh-1                                         ! inspect possible horizontal data redundancy
         do j = i+1, nstnh
            if (latlh(i).eq.latlh(j) .and. lonlh(i).eq.lonlh(j)) then
               print*,i,stnlh(i),j,stnlh(j)
               iden = 1
            endif
         enddo
      enddo
      if (iden .eq. 1) stop

      iden = 0
      do i = 1, nstnv-1                                         ! inspect possible vertical data redundacy
         do j = i+1, nstnv
            if (latlv(i).eq.latlv(j) .and. lonlv(i).eq.lonlv(j)) then
               print*,i,stnlv(i),j,stnlv(j)
               iden = 1
            endif
         enddo
      enddo
      if (iden .eq. 1) stop

      if (nstnh.gt.maxsit .or. nstnv.gt.maxsit) then            ! check limitation of data array input
        write(6,62) maxsit
        stop
      endif

62    format('# of stations in input file > maxsit',i5,
     .   ', please increase maxsit ...')
61    format('# of creep faults > maxc',i3,
     .   ', please increase maxc ...')
*
*   compute mass center of network
*
      xmean=0.0d0
      ymean=0.0d0
      do i=1,nstnh
        xmean=xmean+lonlh(i)
        ymean=ymean+latlh(i)
      enddo
      xmean=xmean/nstnh
      ymean=ymean/nstnh
*
*  convert geodetic coordinates into cartesian coordinates
*
      do i=1,nstnh
        lonh(i)=lonlh(i)
        lath(i)=latlh(i)
      enddo
      call llxy(ymean,xmean,lath,lonh,nstnh)

      do i=1,nstnv
        lonv(i)=lonlv(i)
        latv(i)=latlv(i)
      enddo
      call llxy(ymean,xmean,latv,lonv,nstnv)

      read(5,*) ncrp
      if (ncrp.gt.maxc) then
        write(6,61) maxc
        stop
      endif
      if (ncrp .ne. 0) then
        read(5,'(a)') crpf
        open(4,file=crpf,status='old')                   ! open file for creep fault parameter input
        do i = 1, ncrp
          read(4,*) alon(i),alat(i),blon(i),blat(i)
          if (alon(i).gt.180.0d0 ) alon(i) = alon(i)-360.0d0
          if (blon(i).gt.180.0d0 ) blon(i) = blon(i)-360.0d0
        enddo
        close(4)
      endif
*
*  convert geodetic coordinates into cartesian coordinates for creep fault and
*  compute creep fault's azimuth
*
      if ( ncrp.ne.0 ) then 
        call llxy(ymean,xmean,alat,alon,ncrp)
        call llxy(ymean,xmean,blat,blon,ncrp)
        do i = 1, ncrp
          dx = blon(i) - alon(i)
          dy = blat(i) - alat(i)
          ds = dsqrt(dx**2 + dy**2)
          dcs(i) = dx/ds
          dsn(i) = dy/ds
        enddo
      endif
*
*   setup constants for data reweighting according to options                          
*
      if (is_wght .eq. 1) cutoff_dis = 5.15d0
      if (is_wght .eq. 2) cutoff_dis = 10.0d0
      wt_az = 0.25d0                                                         ! relative weight of mean azimuth to individual azimuth
      np_site = 6                                                            ! number of points to compute mean distance used as diameters of circlar weighting area 
      cfa1 = 2.0d0                                                           ! coefficient for circlular area weighing 
      cfa2 = 2.0d0                                                           ! coefficient for circular area weighting as substitute of voronoi area weighting
*
*  compute voronoi area 
*
      if ( id_wght.eq.2 ) then

        do i=1,nstnh
          posh(1,i)=lonh(i)
          posh(2,i)=lath(i)
        enddo

        call voron(nstnh,posh,areah)

        do i = 1,nstnh                                                   ! assign voronoi cell weights for horizontal gps velocity sites
          call cmp_area1(nstnh,posh,np_site,cfa1,i,careah(i))
          if ( areah(i).eq.-1.0d0 ) areah(i) = careah(i)
          if ( areah(i).gt.cfa2*careah(i) ) areah(i) = careah(i)
        enddo

        do i=1,nstnv
          posv(1,i)=lonv(i)
          posv(2,i)=latv(i)
        enddo

        call voron(nstnv,posv,areav)

        do i = 1,nstnv                                                   ! assign voronoi cell weights for vertical gps velocity sites
          call cmp_area1(nstnv,posv,np_site,cfa1,i,careav(i))
          if ( areav(i).eq.-1.0d0 ) areav(i) = careav(i)
          if ( areav(i).gt.cfa2*careav(i) ) areav(i) = careav(i)
        enddo

      endif

      do i = 1, nstnh
         call visr_h(lath(i),lonh(i),min_tau,max_tau,
     1      in_tau,is_wght,id_wght,wt0,rtau_h,indxx,iout,isl)         !  horizontal gps velocity interpolation and uncerstainty estimate
         vx_sig(i) = sqrt(ainv(1,1))
         vy_sig(i) = sqrt(ainv(2,2))
         vxs(i) = bb(1)
         vys(i) = bb(2)
      enddo
      do i = 1, nstnh
         tmp(i) = sqrt(vx_sig(i)**2+vy_sig(i)**2)
      enddo
      call sortit(tmp,nstnh)
      v_sig_m = tmp(nstnh/2+1)                                        !  compute median for sigma of interpolated velocity at GPS site by bootstrapping
      do i = 1, nstnh
         tmp(i) = sqrt((vxs(i)-uxlh(i))**2+(vys(i)-uylh(i))**2)
      enddo
      call sortit(tmp,nstnh)
      vs_m = tmp(nstnh/2+1)                                           !  compute median of residual velocity at GPS sites by bootstrapping

      wt_h = vs_m/v_sig_m
      print*, v_sig_m,vs_m,wt_h
      write(41,'(3f10.4)') v_sig_m,vs_m,wt_h                          ! median values for formal uncertainty and bootstrapping residual and their ratio for 2D
      do i = 1, nstnh
         write(41,28) stnlh(i),lonlh(i),latlh(i),uxlh(i),vxs(i),      !  output horizontal GPS velocity result from bootstrapping
     1   vx_sig(i),uylh(i),vys(i),vy_sig(i)
      enddo
28    format(a8,2f10.4,6f8.2)

      do i = 1, nstnv
         call visr_h(latv(i),lonv(i),min_tau,max_tau,
     1      in_tau,is_wght,id_wght,wt0,rtau_h,indxx,iout,isl)         !  horizontal gps velocity interpolation and uncerstainty estimate
         vx_sig(i) = sqrt(ainv(1,1))
         vy_sig(i) = sqrt(ainv(2,2))
         vxs(i) = bb(1)
         vys(i) = bb(2)
         call visr_v(latv(i),lonv(i),min_tau,max_tau,
     1      in_tau,is_wght,id_wght,wt0,rtau_v,indxx,iout,isl)         !  horizontal gps velocity interpolation and uncerstainty estimate
         vz_sig(i) = sqrt(azinv(1,1))
         vzs(i) = bbz(1)
      enddo
      do i = 1, nstnv
         tmp(i) = sqrt(vx_sig(i)**2+vy_sig(i)**2+vz_sig(i)**2)
      enddo
      call sortit(tmp,nstnv)
      vz_sig_m = tmp(nstnv/2+1)
      do i = 1, nstnv
         tmp(i) = sqrt((vxs(i)-uxlh(i))**2+(vys(i)-uylh(i))**2
     1    + (vzs(i)-uzlv(i))**2)
      enddo
      call sortit(tmp,nstnv)
      vzs_m = tmp(nstnv/2+1)                                       !  compute median value of 3D GPS bootstrapping velocity residual RMS

      wt_v = vzs_m/vz_sig_m
      print*, vz_sig_m,vzs_m,wt_v                                  ! median values for formal uncertainty and bootstrapping residual RMS and their ratio for 3D

      write(42,'(3f10.4)') vz_sig_m,vzs_m,wt_v
      do i = 1, nstnv
         write(42,29) stnlv(i),lonlv(i),latlv(i),
     1   uzlv(i),vzs(i),vz_sig(i)
      enddo
29    format(a8,2f10.4,3f8.2)

      isl = 1
      write(79,184)                                                !  Interpolation for GPS data only
184   format('   long      lat       Ve      dVe       Vn      dVn   '
     1 '    Vu      dVn    Tau_h   Tau_v')
      nlon = (lonmax0 - lonmin0)/dlon + 1.1
      nlat = (latmax0 - latmin0)/dlat + 1.1
      do i = 1, nlon                                                      ! iterate over lat/lon for data selection
         do j = 1, nlat
            iold = iold + 1
            qlon(1) = lonmin0 + (i-1)*dlon 
            qlat(1) = latmin0 + (j-1)*dlat 
            zlon = qlon(1)
            zlat = qlat(1)
            call llxy(ymean,xmean,qlat,qlon,ms)
            call visr_h(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_h,indxx,iout,isl)                !  horizontal gps velocity interpolation and uncerstainty estimate
            call visr_v(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_v,indxx,iout,isl)                !  vertical gps velocity interpolation and uncerstainty estimate
            if (iout .ne. 1 .or. indxx .ne. 0) goto 38
            stdx = sqrt(ainv(1,1))
            stdy = sqrt(ainv(2,2))
            stdz = sqrt(azinv(1,1))
      if (stdx.gt.999.0 .or. stdy.gt.999.0 .or. stdz.gt.999.0) goto 38
            write(79,99) zlon,zlat,bb(1),stdx,bb(2),stdy,               ! output GPS only interpolation result
     1         bbz(1),stdz,rtau,rtauz
38       enddo
      enddo
*
*   sort out sar overlap cells
*
      open(17,file=orbf,status='unknown')
      open(19,file=outf,status='unknown')
c
      do i = 1, nsarf                                                   !  iteration over sar input files
         tmpstr=sarf(i)
         write(*,*) 'reading ',tmpstr(1:index(sarf(i),' ')-1)
         open(3,file=sarf(i),status='old')
         idat = 0
         tlonmean(i) = 0
         tlatmean(i) = 0
50       idat = idat + 1
         if (idat .gt. maxpxl) then
            write(6,*) i,' # of data greater than allowance'
            stop
         endif

         read(3,*,end=60) tlon(i,idat),tlat(i,idat),sazi(i,idat),       !  read in i-th sar satellite data
     1     slook(i,idat),clos(i,idat),clos_s(i,idat)
         sazi(i,idat) = -90.0 - sazi(i,idat)
         slook(i,idat) = 90.0 - slook(i,idat)
         clos(i,idat) = 10.0*clos(i,idat)
         clos_s(i,idat) = 10.0*clos_s(i,idat)*f_sar(i)
         ilon = (tlon(i,idat) - lonmin)/dlon + 1.1
         ilat = (tlat(i,idat) - latmin)/dlat + 1.1
         isd(ilon,ilat) = isd(ilon,ilat) + 1                            !  accumulate sar data entry count in grid
         tlonmean(i) = tlonmean(i) + tlon(i,idat)
         tlatmean(i) = tlatmean(i) + tlat(i,idat)
         goto 50
60       ndat(i) = idat - 1
         tlonmean(i) = tlonmean(i)/ndat(i)                              !  compute mean lat/lon for the i-th sar dataset
         tlatmean(i) = tlatmean(i)/ndat(i)
         close(3)
      enddo
 
      stdsar = dlos0
      ms = 1
      iold = 0
      idata = 0
      hlfdlon = 0.5*dlon
      hlfdlat = 0.5*dlat
*
*   use grids with gps vertical data entries for ramp estimation
*
      nlon = (lonmax - lonmin)/dlon + 1.1
      nlat = (latmax - latmin)/dlat + 1.1
      do i = 1, nlon                                                    ! iterate over lat/lon for ramp estimation
         zlon = lonmin + (i-1)*dlon
         do j = 1, nlat
            zlat = latmin + (j-1)*dlat
            if (isd(i,j).ne.1) goto 65                                  ! select signal InSAR data entry (multiple entries will be entered later)
            do kk = 1, nstnv
               dlonv = abs(lonlv(kk) - zlon)
               dlatv = abs(latlv(kk) - zlat)
               if (dlonv .lt. hlfdlon .and. dlatv .lt. hlfdlat) then    ! select grid with gps vertical data entry
            iold = iold + 1
            glon(iold) = zlon 
            glat(iold) = zlat
            do k = 1, nsarf
               do m = 1, ndat(k)
                  dltlon = abs(tlon(k,m) - glon(iold))
                  dltlat = abs(tlat(k,m) - glat(iold))
                  if (dltlon.lt.hlfdlon .and. dltlat.lt.hlfdlat) then   ! search for InSAR data within grid
                     idata = idata + 1
                     if (ilos0. eq. 0) stdsar = max(clos_s(k,m),dlos1)
                     b(idata) = clos(k,m)/stdsar
                     a(idata,3*(k-1)+1)=1.0/stdsar                      ! construct partial derivatives for InSAR data
                     a(idata,3*(k-1)+2)=(glon(iold)-tlonmean(k))/stdsar
                     a(idata,3*(k-1)+3)=(glat(iold)-tlatmean(k))/stdsar
                     a(idata,3*nsarf+3*(iold-1)+1) = 
     1                 cos(cov*sazi(k,m))*cos(cov*slook(k,m))/stdsar
                     a(idata,3*nsarf+3*(iold-1)+2) = 
     1                 sin(cov*sazi(k,m))*cos(cov*slook(k,m))/stdsar
                     a(idata,3*nsarf+3*(iold-1)+3) = 
     1                 sin(cov*slook(k,m))/stdsar
                     goto 63
                  endif
63             enddo
            enddo
            qlon(1) = glon(iold)
            qlat(1) = glat(iold)
            call llxy(ymean,xmean,qlat,qlon,ms)

            call visr_h(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_h,indxx,iout,isl)              !  horizontal gps velocity interpolation and uncerstainty estimate
            call visr_v(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_v,indxx,iout,isl)              !  vertical gps velocity interpolation and uncerstainty estimate
            if (iout .ne. 1) then
               write(6,*) 'iout not equal to 1'
               stop
            endif

            if (indxx.ne.0) then
               write(6,*) 'GPS velocity interpolation not valid'
               write(6,*) indxx,indxx,glon(iold),glat(iold),' 1'
               stop
            endif

            stdx = sqrt(ainv(1,1))
            stdy = sqrt(ainv(2,2))
            stdz = sqrt(azinv(1,1))
            idata = idata + 1
            b(idata) = bb(1)/stdx
            a(idata,3*nsarf+3*(iold-1)+1) = 1.0/stdx                     !  construct partial derivatives for interpolated gps data
            a(idata,3*nsarf+3*(iold-1)+2) = 0.0
            a(idata,3*nsarf+3*(iold-1)+3) = 0.0
            idata = idata + 1
            b(idata) = bb(2)/stdy
            a(idata,3*nsarf+3*(iold-1)+1) = 0.0
            a(idata,3*nsarf+3*(iold-1)+2) = 1.0/stdy
            a(idata,3*nsarf+3*(iold-1)+3) = 0.0
            idata = idata + 1
            b(idata) = bbz(1)/stdz
            a(idata,3*nsarf+3*(iold-1)+1) = 0.0
            a(idata,3*nsarf+3*(iold-1)+2) = 0.0
            a(idata,3*nsarf+3*(iold-1)+3) = 1.0/stdz
               endif
         enddo

65       enddo
      enddo
*
*   select grids with multiple InSAR data entries for ramp estimation (at coarse grid points)
*
      mlon = dlon1/dlon + 0.001
      mlat = dlat1/dlat + 0.001
      nlon = (lonmax - lonmin)/dlon1 + 1.1
      nlat = (latmax - latmin)/dlat1 + 1.1
      do i = 1, nlon                                                      ! iterate over lat/lon for data selection
         ilon = (i-1)*mlon + 1
         do j = 1, nlat
            jlat = (j-1)*mlat + 1
            if (isd(ilon,jlat).lt.2) goto 66                              ! select InSAR data with multiple entries
            iold = iold + 1
            glon(iold) = lonmin + (i-1)*dlon1 
            glat(iold) = latmin + (j-1)*dlat1 
            do k = 1, nsarf
               do m = 1, ndat(k)
                  dltlon = abs(tlon(k,m) - glon(iold))
                  dltlat = abs(tlat(k,m) - glat(iold))
                  if (dltlon.lt.hlfdlon .and. dltlat.lt.hlfdlat) then     ! search for data within grid
                     idata = idata + 1
                     if (ilos0. eq. 0) stdsar = max(clos_s(k,m),dlos1)
                     b(idata) = clos(k,m)/stdsar
                     a(idata,3*(k-1)+1)=1.0/stdsar                        ! construct partial derivatives for InSAR data
                     a(idata,3*(k-1)+2)=(glon(iold)-tlonmean(k))/stdsar
                     a(idata,3*(k-1)+3)=(glat(iold)-tlatmean(k))/stdsar
                     a(idata,3*nsarf+3*(iold-1)+1) = 
     1                 cos(cov*sazi(k,m))*cos(cov*slook(k,m))/stdsar
                     a(idata,3*nsarf+3*(iold-1)+2) = 
     1                 sin(cov*sazi(k,m))*cos(cov*slook(k,m))/stdsar
                     a(idata,3*nsarf+3*(iold-1)+3) = 
     1                 sin(cov*slook(k,m))/stdsar
                     goto 64
                  endif
64             enddo
            enddo
            qlon(1) = glon(iold)
            qlat(1) = glat(iold)
            call llxy(ymean,xmean,qlat,qlon,ms)
            call visr_h(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_h,indxx,iout,isl)                !  horizontal gps velocity interpolation and uncerstainty estimate
            call visr_v(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_v,indxx,iout,isl)                !  vertical gps velocity interpolation and uncerstainty estimate

            if (iout .ne. 1) then
               write(6,*) 'iout not equal to 1'
               stop
            endif
c            if (idvu0 .eq. 1) vz_sig = dvu0

            if (indxx.ne.0) then
               write(6,*) 'GPS velocity interpolation not valid'
               write(6,*) indxx,glon(iold),glat(iold),' 2'
               stop
            endif

            stdx = sqrt(ainv(1,1))
            stdy = sqrt(ainv(2,2))
            stdz = sqrt(azinv(1,1))
            idata = idata + 1
            b(idata) = bb(1)/stdx
            a(idata,3*nsarf+3*(iold-1)+1) = 1.0/stdx                !  construct partial derivatives for interpolated gps data
            a(idata,3*nsarf+3*(iold-1)+2) = 0.0
            a(idata,3*nsarf+3*(iold-1)+3) = 0.0
            idata = idata + 1
            b(idata) = bb(2)/stdy
            a(idata,3*nsarf+3*(iold-1)+1) = 0.0
            a(idata,3*nsarf+3*(iold-1)+2) = 1.0/stdy
            a(idata,3*nsarf+3*(iold-1)+3) = 0.0
            idata = idata + 1
            b(idata) = bbz(1)/stdz
            a(idata,3*nsarf+3*(iold-1)+1) = 0.0
            a(idata,3*nsarf+3*(iold-1)+2) = 0.0
            a(idata,3*nsarf+3*(iold-1)+3) = 1.0/stdz
66       enddo
      enddo

88    nold = iold
      npr = 3*nsarf + 3*nold
      do i = 1, nsarf                                              ! construct partial derivatives for ramp parameters
         idata = idata + 1
         b(idata) = 0.0
         a(idata,3*(i-1)+1) = 1.0/offsig
         idata = idata + 1
         b(idata) = 0.0
         a(idata,3*(i-1)+2) = 1.0/rmpsig
         idata = idata + 1
         b(idata) = 0.0
         a(idata,3*(i-1)+3) = 1.0/rmpsig
      enddo

      ndata = idata

      do i=1,npr
         do j=1,i                                                  ! aa = a^t*a
            aat(i,j)=0.0d0
            do k=1,ndata
               aat(i,j)=aat(i,j)+a(k,i)*a(k,j)
            enddo
            aat(j,i)=aat(i,j)
         enddo
      enddo

      do i = 1, npr                                                ! bb = a^t*b
         bbt(i) = 0.0d0
         do j=1, ndata
            bbt(i)=bbt(i)+a(j,i)*b(j)
         enddo
      enddo
*
*  solve b=a*x.
*
      call dludcmp(aat,npr,maxdat,indx1,d)
      call dlubksb(aat,npr,maxdat,indx1,bbt)
*
*  invert n.
*
      do i=1,npr
         do j=1,npr
            ainvm(i,j)=0.0d0
         enddo
         ainvm(i,i)=1.0d0
      enddo
      do i=1,npr
         call dlubksb(aat,npr,maxdat,indx1,ainvm(1,i))
      enddo

      do i = 1,npr
         v(i) = dsqrt(ainvm(i,i))
      enddo
         
      write(17,89) 
89    format('  #   long      lat      Ve    dVe     Vn   dVn     Vu ',
     . '  dVu ')
91    format(i3,f10.4,f8.4,1x,3(f7.2,f6.2))

      do i = 1, nold
         write(17,91) i,glon(i),glat(i),bbt(3*nsarf+3*(i-1)+1),      !  output 3-D velocity solutions at grid points
     1     v(3*nsarf+3*(i-1)+1),bbt(3*nsarf+3*(i-1)+2),
     2     v(3*nsarf+3*(i-1)+2),bbt(3*nsarf+3*(i-1)+3),
     3     v(3*nsarf+3*(i-1)+3)
      enddo

      write(17,93)
93    format('      S0      S_sig       Se      Se_sig      Sn  ',
     . '     Sn_sig')

      do i = 1, nsarf
         write(17,94) bbt(3*(i-1)+1),v(3*(i-1)+1),bbt(3*(i-1)+2),   !  output orbital ramp parameters
     1     v(3*(i-1)+2),bbt(3*(i-1)+3),v(3*(i-1)+3)
      enddo

      write(19,84)
84    format('   long      lat       Ve      dVe       Vn      dVn   '
     1 '    Vu      dVn     Cen      Ceu      Cnu  ')
94    format(6f10.4)
*
*    solve for displacements over grid points
*
      chisq0 = 0 
      chisq1 = 0
      ndt = 0
      ngrd = 0
      nlon = (lonmax - lonmin)/dlon + 1.1
      nlat = (latmax - latmin)/dlat + 1.1
      do i = 1, nlon                                                    ! iteration over grid points
         do j = 1, nlat
            zlon = lonmin + (i-1)*dlon 
            zlat = latmin + (j-1)*dlat 
            idata = 0
            do k = 1, nsarf                                             ! iteration over sar satellites
               do m = 1, ndat(k)                                        ! iteration over satellite data points
                  dltlon = abs(tlon(k,m) - zlon)
                  dltlat = abs(tlat(k,m) - zlat)
                  if (dltlon.lt.hlfdlon .and. dltlat.lt.hlfdlat) then  ! sar data available at the grid point
                     ndt = ndt + 1
                     idata = idata + 1
                     if (ilos0. eq. 0) stdsar = max(clos_s(k,m),dlos1)
                     clos_c = clos(k,m) - bbt(3*(k-1)+1) -
     1                 bbt(3*(k-1)+2)*(zlon-tlonmean(k)) - 
     2                 bbt(3*(k-1)+3)*(zlat-tlatmean(k))
                     b(idata) = clos_c/stdsar
                     bsv(idata) = clos_c
                     a(idata,1) = 
     1                 cos(cov*sazi(k,m))*cos(cov*slook(k,m))/stdsar    ! construct partial derivatives for InSAR data
                     a(idata,2) = 
     1                 sin(cov*sazi(k,m))*cos(cov*slook(k,m))/stdsar
                     a(idata,3) = 
     1                 sin(cov*slook(k,m))/stdsar
                     asv(idata,1) = 
     1                 cos(cov*sazi(k,m))*cos(cov*slook(k,m))
                     asv(idata,2) = 
     1                 sin(cov*sazi(k,m))*cos(cov*slook(k,m))
                     asv(idata,3) = sin(cov*slook(k,m))
                     isat(idata) = k
                     goto 163
                  endif
163            enddo
            enddo
            if (idata .eq. 0) goto 110
            ngrd = ngrd + 1
            qlon(1) = zlon
            qlat(1) = zlat
            call llxy(ymean,xmean,qlat,qlon,ms)

            call visr_h(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_h,indxx,iout,isl)                !  gps horizontal velocity uncertainty estimation
            call visr_v(qlat(1),qlon(1),min_tau,max_tau,
     1        in_tau,is_wght,id_wght,wt0,rtau_v,indxx,iout,isl)                !  gps vertical velocity uncertainty estimation

            if (iout .ne. 1) then
               write(6,*) 'iout not equal to 1'
               stop
            endif

            if (indxx.ne.0) then
               write(6,*) 'GPS velocity interpolation not valid'
               write(6,*) indxx,glon(iold),glat(iold),' 3'
               stop
            endif

            stdx = sqrt(ainv(1,1))
            stdy = sqrt(ainv(2,2))
            stdz = sqrt(azinv(1,1))
            if (idvu0 .eq. 1) stdz = dvu0

            idata = idata + 1
            b(idata) = bb(1)/stdx
            a(idata,1) = 1.0/stdx                                            ! construct partial derivatives for gps data
            a(idata,2) = 0.0
            a(idata,3) = 0.0
            idata = idata + 1
            b(idata) = bb(2)/stdy
            a(idata,1) = 0.0
            a(idata,2) = 1.0/stdy
            a(idata,3) = 0.0
            idata = idata + 1
            b(idata) = bbz(1)/stdz
            a(idata,1) = 0.0
            a(idata,2) = 0.0
            a(idata,3) = 1.0/stdz

            npr = 3
            ndata = idata

            do k=1,npr
               do nl=1,k
                  aat(k,nl)=0.0d0
                  do m=1,ndata
                     aat(k,nl)=aat(k,nl)+a(m,k)*a(m,nl)
                  enddo
                  aat(nl,k)=aat(k,nl)
               enddo
            enddo

            do k = 1, npr
               bbr(k) = 0.0d0
               do nl=1, ndata
                  bbr(k)=bbr(k)+a(nl,k)*b(nl)
               enddo
            enddo
*
*  solve b=a*x.
*
            call dludcmp(aat,npr,maxdat,indx1,d)
            call dlubksb(aat,npr,maxdat,indx1,bbr)
*
*  invert n.
*
            do k=1,npr
               do nl=1,npr
                  ainvm(k,nl)=0.0d0
               enddo
               ainvm(k,k)=1.0d0
            enddo
            do k=1,npr
               call dlubksb(aat,npr,maxdat,indx1,ainvm(1,k))
            enddo

            do k = 1,npr
               v(k) = dsqrt(ainvm(k,k))
            enddo

            cv12 = ainvm(1,2)/v(1)/v(2)
            cv13 = ainvm(1,3)/v(1)/v(3)
            cv23 = ainvm(2,3)/v(2)/v(3)
            write(19,99) zlon,zlat,bbr(1),v(1),bbr(2),v(2),
     1         bbr(3),v(3),cv12,cv13,cv23                                ! output 3-D velocity solutions

            do k = 1, idata-3
               prd(k) = 0
               prd0(k) = 0
               do m = 1, 3
                  prd(k) = prd(k) + a(k,m)*bbr(m)
                  prd0(k) = prd0(k) + asv(k,m)*bbr(m)
               enddo
               db = b(k)-prd(k)
               db0 = bsv(k)-prd0(k)
               chisq1 = chisq1 + db*db
               chisq0 = chisq0 + db0*db0
               write(11,111) isat(k),zlon,zlat,b(k),prd(k),db,bsv(k),    ! output sar data fitting residuals
     1           prd0(k),db0
            enddo
110      enddo
      enddo
99    format(2f9.3,9f9.4)
111   format(i4,2f9.3,6f10.4)

      write(11,112) chisq1,ndt
112   format("Total weighted postfit residual Chisquare:",f10.3,2x,i6)
      write(11,113) chisq1/ndt
113   format("Reduced weighted postfit residual Chisquare:",f10.3)
      write(11,114) chisq0,ndt
114   format("Total postfit residual Chisquare (mm/yr)^2:",f10.3,2x,i6)
      write(11,115) chisq0/ndt
115   format("Reduced postfit residual Chisquare (mm/yr)^2:",f10.3)

92    format(f10.4,f8.4,2x,7hindex =,i2)
97    format(f10.4,f8.4,15f7.2)

      stop
      end

      subroutine cmp_strain(x,covx,emax,emaxsd,emin,eminsd,
     .           taumax,tausd,dexazim,azsd,dilat,ddilat)
      implicit real*8 (a-h,o-z)
      dimension x(3),covx(3,3),v(3)
      common /prmtr/pi,cov,cutoff_dis,wt_az
*
*  estimate principle strain rates emax, emin, maximum shear tau_max, 
*  and dextral tau_max azimuth
*
      emean=(x(1)+x(3))/2.0d0
      ediff=(x(1)-x(3))/2.0d0
      taumax=dsqrt(x(2)**2+ediff**2)
      emax=emean+taumax
      emin=emean-taumax
      azim=-datan2(x(2),ediff)/cov/2.0d0
      azim=90.0d0+azim
      dexazim=azim+45.0d0-180.0d0
*
*  estimate sigma of tau_max
*
      v(1)=(x(1)-x(3))/4.0d0/taumax
      v(2)=x(2)/taumax
      v(3)=-v(1)
      call mat_aba(v,covx,3,tausd) 
      tausd=dsqrt(tausd)
*
*  estimate sigma of emax
*
      v(1)=0.5d0*(1+(x(1)-x(3))/2.d0/taumax)
      v(2)=x(2)/taumax
      v(3)=0.5d0*(1-(x(1)-x(3))/2.d0/taumax)
      call mat_aba( v,covx,3,emaxsd) 
      emaxsd=dsqrt(emaxsd)
*
*  estimate sigma of emin
*
      v(1)=0.5d0*(1-(x(1)-x(3))/2.0d0/taumax)
      v(2)=-x(2)/taumax
      v(3)=0.5d0*(1+(x(1)-x(3))/2.0d0/taumax)
      call mat_aba(v,covx,3,eminsd) 
      eminsd=dsqrt(eminsd)
*
*  estimate sigma of azimuth
*
      cf=1.0d0/((x(1)-x(3))**2+4.0d0*x(2)**2)
      v(1)=cf*x(2)
      v(2)=-cf*(x(1)-x(3))
      v(3)=-v(1)
      call mat_aba(v,covx,3,azsd) 
      azsd = dsqrt(azsd)/cov
*
*  compute dilatation and its sigma 
*
      dilat=x(1)+x(3)
      ddilat=dsqrt(covx(1,1)+covx(3,3)+2*covx(1,3))

      return
      end

      subroutine mat_aba(a,b,n,aba)
      real*8 a(n),b(n,n),aba

      aba=0.0d0
      do i=1,n
         do j=1,n
            aba=aba+a(i)*b(i,j)*a(j)
         enddo
      enddo
      return
      end

      subroutine llxy(slatm,slonm,slat,slon,m)
*
*   subroutine to transfer latitude and longitude into local x y.
*
*   input:
*      slatm: latitude of reference point for local coordinates, in degree.
*      slonm: longitude of reference point for local coordinates, in degree.
*      slat : array coordinate of latitude to be transferred, in degree.
*      slon : array coordinate of longitude to be transferred, in degree.
*      m    : number of points to be transferred.
*   output:
*      slat : y array after transformation in km.
*      slon : x array after transformation in km.
*
      implicit real*8 (a-h,o-z)
      integer*8 m
      dimension t(3,3),vp(3),v(3)
      dimension slat(m),slon(m)
*
      pi = 4.0d0*datan(1.0d0)
      rlatc = slatm*pi/180.0d0
      rlonc = slonm*pi/180.0d0
*
*     calculate local radius of curvature (r) using reference earth nad 
*     1983 (semi-major axis is 6378.137,flattening factor is 1/298.2572)
*
      flat = 1.0d0/298.2572d0
      esq = 2.0d0*flat - flat**2
      q = 1.0d0 - esq*dsin(rlatc)*dsin(rlatc)
      r = 6378.137d0*dsqrt(1.0d0 - esq)/q
*
*     construct transformation matrix
*
      t(1,1) =  dsin(rlatc)*dcos(rlonc)
      t(1,2) =  dsin(rlatc)*dsin(rlonc)
      t(1,3) = -dcos(rlatc)
      t(2,1) = -dsin(rlonc)
      t(2,2) =  dcos(rlonc)
      t(2,3) =  0.0d0
      t(3,1) =  dcos(rlatc)*dcos(rlonc)
      t(3,2) =  dcos(rlatc)*dsin(rlonc)
      t(3,3) =  dsin(rlatc)
*
*     calculate xl,yl,dis (vector in local coordinate)
*
      do 9 i = 1,m
        slat(i) = slat(i)*pi/180.0d0
        slon(i) = slon(i)*pi/180.0d0
        v(1) = dcos(slat(i))*dcos(slon(i))
        v(2) = dcos(slat(i))*dsin(slon(i))
        v(3) = dsin(slat(i))
        do j = 1,2
          vp(j) = 0.0d0
          do k = 1,3
            vp(j) = vp(j) + t(j,k)*v(k)
          end do
        end do
        slon(i) =  r*vp(2)
        slat(i) = -r*vp(1)
9     continue

      return
      end

*  Subroutine to conduct LU back substitution
*
      subroutine dlubksb(a,n,np,indx,b)
      implicit real*8 (a-h,o-z)
      dimension a(np,np),indx(np),b(np)
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.0d0) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        if(i.lt.n)then
          do 13 j=i+1,n
            sum=sum-a(i,j)*b(j)
13        continue
        endif
        b(i)=sum/a(i,i)
14    continue
      return
      end
 
*  Subroutine to conduct LU decomposition
*
      subroutine dludcmp(a,n,np,indx,d)
      implicit real*8 (a-h,o-z)
      parameter (nmax=7000,tiny=1.0e-20)
      dimension a(np,np),indx(np),vv(nmax)
      d=1.0d0
      do 12 i=1,n
        aamax=0.0d0
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.0d0) write (6,100) i
        vv(i)=1.0d0/aamax
12    continue
100   format('singular matrix, i= ',i4)

      do 19 j=1,n
        if (j.gt.1) then
          do 14 i=1,j-1
            sum=a(i,j)
            if (i.gt.1)then
              do 13 k=1,i-1
                sum=sum-a(i,k)*a(k,j)
13            continue
              a(i,j)=sum
            endif
14        continue
        endif
        aamax=0.0d0
        do 16 i=j,n
          sum=a(i,j)
          if (j.gt.1)then
            do 15 k=1,j-1
              sum=sum-a(i,k)*a(k,j)
15          continue
            a(i,j)=sum
          endif
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(j.ne.n)then
          if(a(j,j).eq.0.0d0)a(j,j)=tiny
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      if(a(n,n).eq.0.0d0)a(n,n)=tiny
      return
      end

*   Subroutine to compute alternative occupational area for sites located at the edge of network
*
      subroutine cmp_area1(nstn,pos,np,cfa1,ith,area_ith)
      implicit real*8 (a-h,l,o-z)
      integer*8 nstn
      dimension pos(2,nstn),dsmin(np)

      pi = 4.0d0*datan(1.0d0)

      is = 1
      dsmax = 0.0d0
90    dsmin(is) = 1000000.0d0
      do i = 1,nstn
        if ( i.ne.ith) then
          dx = pos(1,ith)-pos(1,i)
          dy = pos(2,ith)-pos(2,i)
          ds = dsqrt(dx*dx+dy*dy)
          if ( ds.lt.dsmin(is).and.ds.gt.dsmax ) then
            dsmin(is) = ds
          endif
        endif
      enddo
      dsmax = dsmin(is)

      if ( is.eq.np ) goto 100
      is = is + 1
      goto 90

100   area_ith = 0.0d0

      do i = 1,is
        area_ith = area_ith + dsmin(i)
      enddo
      area_ith = area_ith/is/2.0
c      area_ith = cfa1*area_ith/is/2.0d0
      area_ith = pi*area_ith**2

      return
      end
*
*  Subroutine for GPS horizontal velocity interpolation
*
      subroutine visr_h(yy,xx,min_tau,max_tau,in_tau,
     .   is_wght,id_wght,wt0,sigma0,indxx,iout,isl)
      implicit real*8 (a-h,l,o-z)
      integer*8 nstn,ncrp
      parameter (maxsit=15000)
      dimension lon(maxsit),lat(maxsit),uxl(maxsit),sxl(maxsit),
     .          uyl(maxsit),syl(maxsit),cxy(maxsit),area(maxsit),
     .          uzl(maxsit),szl(maxsit)
      dimension alon(10),alat(10),blon(10),blat(10),
     .          dcs(10),dsn(10)
      dimension crp_strk1(10),crp_strk2(10),is0(10),icrp(10)
      dimension istn(maxsit),azi(maxsit),wght(maxsit)
      dimension a(2*maxsit,6),b(2*maxsit),aa(6,6),aasv(6,6),
     .          ainv(6,6),indx(6),bb(6),v(6),az(maxsit,3)
      dimension a_s(2*maxsit,6),aa_s(6,6)
      common /veldath/nstn,lon,lat,uxl,sxl,uyl,syl,uzl,szl,cxy,area
      common /crpdat/ncrp,alon,alat,blon,blat,dcs,dsn
      common /ainv_vh/ainv,bb,rtau,wtt,chisq,nslct
      common /prmtr/pi,cov,cutoff_dis,wt_az

      iout = 0
*
*  compute relationship between interpolation point and creep fault
*
      do i = 1, ncrp

        icrp(i) = 0

        dx = alon(i) - xx
        dy = alat(i) - yy
        ang1 = datan2(dy,dx)/cov
        y1 = -1.0d0*(dy*dcs(i)-dx*dsn(i))

        dx = blon(i) - xx
        dy = blat(i) - yy
        ang2 = datan2(dy,dx)/cov

        if ( ang1.eq.ang2 ) then
          is0(i) = 0                                                         !  interpolation point is located on the extension of creep fault ?
          goto 20
        endif 
        if ( y1.eq.0.0d0 ) then
          indxx = 4                                                          !  interpolation point is located on creep fault
          goto 60  
        endif

        is0(i) = y1/abs(y1)

20      if (ang2 .gt. ang1) then
          crp_strk1(i) = ang1
          crp_strk2(i) = ang2
        else
          crp_strk1(i) = ang2
          crp_strk2(i) = ang1
        endif

        dang = crp_strk2(i) - crp_strk1(i)
        if ( dang.gt.180.0d0 ) then
          ang_tmp = crp_strk1(i) + 360.0d0
          crp_strk1(i) = crp_strk2(i)
          crp_strk2(i) = ang_tmp
          icrp(i) = 1
        endif

      enddo
*
*  start data selection.
*
      do 30 itau = min_tau, max_tau, in_tau

        if (itau.eq.max_tau) goto 60

        indxx = 0
        nslct = 0
        rtau = itau

        do 10 i = 1, nstn

          dx = lon(i) - xx
          dy = lat(i) - yy
          dr = dsqrt(dy**2+dx**2)
          if (isl .eq. 0 .and. dr .lt. 0.0001) goto 10
          ang_s1 = datan2(dy,dx)/cov
          rt = dr/rtau

          if (rt .le. cutoff_dis) then
            do j = 1, ncrp
              if (icrp(j).eq.1.and.ang_s1.lt.0.d0) ang_s1=ang_s1+360.d0
 
              dx = alon(j) - lon(i)
              dy = alat(j) - lat(i)
              y1 = -1.0d0*(dy*dcs(j)-dx*dsn(j))
              
              if ( y1.eq.0.0d0) then
                 is1 = 0
              else
                 is1 = y1/abs(y1)
              endif
             
              if (is1.ne.is0(j)) then
              if (ang_s1.ge.crp_strk1(j).and.
     .          ang_s1.le.crp_strk2(j)) then
                goto 10
              endif
              endif

            enddo
            nslct = nslct + 1
            istn(nslct) = i
          endif
10      continue

      if (nslct.lt.3) then
        indxx = 1
        goto 30
      endif                                                                  ! no interpolation for n_slct < 3

      if (nslct.gt.maxsit) then
        write(6,62) maxsit
        stop
      endif

62    format('# of selected stations > maxsit',i5,
     .   ', please increase maxsit ( visr_core )...')
*
*  estimate azimuthal coverage.
*
      do i = 1, nslct
        dx = lon(istn(i)) - xx
        dy = lat(istn(i)) - yy
        azi(i) = datan2(dy,dx)/cov
      enddo
      azimax = azi(1)
      azimin = azi(1)
      do i = 2, nslct
        azimax = max(azimax,azi(i))
        azimin = min(azimin,azi(i))
      enddo
      dazim1 = azimax - azimin
      if (dazim1.le.180.0d0) then
        indxx = 2
        goto 30
      endif                                                                  ! no interpolation for dazi < pi

      do i = 1, nslct
        if (azi(i).lt.0.d0 ) azi(i) = azi(i) + 360.0d0
        if ( i.eq.1 ) then
          azimax = azi(i)
          azimin = azi(i)
        else
          azimax = max(azimax,azi(i))
          azimin = min(azimin,azi(i))
        endif
      enddo
      dazim2 = azimax - azimin
      if (dazim2.le.180.0d0) then
        indxx = 2
        goto 30
      endif                                                                  ! no interpolation for dazi < pi

      iout = 1
*
*  compute data spatial density weighting 
*
      if ( id_wght.eq.1 ) then 
     
        azi_avrg = wt_az*360.0d0/nslct                                       
        azi_tot = (1.0d0+wt_az)*360.0d0                                      

        do i = 1, nslct
          daz1 = 180.0d0
          daz2 = -180.0d0
          do j = 1, nslct
            if (i.eq.j) goto 22
            dazi = azi(j) - azi(i)
            if (dazi.gt.180.0d0) dazi = dazi - 360.0d0
            if (dazi.lt.-180.0d0) dazi = dazi + 360.0d0
            if (dazi.gt.0.0d0.and.dazi.lt.daz1) daz1 = dazi
            if (dazi.lt.0.0d0.and.dazi.gt.daz2) daz2 = dazi
22        continue
          enddo
          wght(i)=(0.5d0*(daz1-daz2)+azi_avrg)*nslct/azi_tot                 ! compute azimuthal weighting
        enddo

      endif

      if ( id_wght.eq.2 ) then

        sum_area = 0.0d0
        do i = 1, nslct
          sum_area = sum_area + area(istn(i))
        enddo
        sum_area = sum_area/nslct

        do i = 1, nslct
          wght(i) = area(istn(i))/sum_area
        enddo

      endif
                  
      npr = 2*nslct
      wtt = 0.0d0
      do i = 1, nslct
        dy = lat(istn(i))-yy
        dx = lon(istn(i))-xx
        dr = dsqrt(dx**2+dy**2)
        if (is_wght .eq. 1) wti = dexp(-dr/rtau)*wght(i)
        if (is_wght .eq. 1) wti_sig = dexp(-dr/sigma0)*wght(i)
        if (is_wght .eq. 2) wti = wght(i)/((dr/rtau)**2+1)
        if (is_wght .eq. 2) wti_sig = wght(i)/((dr/sigma0)**2+1)
        wtt = wtt + 2*wti

        a(2*i-1,1)=wti/sxl(istn(i))
        a(2*i-1,2)=0.0d0
        a(2*i-1,3)=wti*dx/sxl(istn(i))
        a(2*i-1,4)=wti*dy/sxl(istn(i))
        a(2*i-1,5)=0.0d0
        a(2*i-1,6)=wti*dy/sxl(istn(i))
        a(2*i,1)=0.0d0
        a(2*i,2)=wti/syl(istn(i))
        a(2*i,3)=0.0d0
        a(2*i,4)=wti*dx/syl(istn(i))
        a(2*i,5)=wti*dy/syl(istn(i))
        a(2*i,6)=-1.0*wti*dx/syl(istn(i))

        a_s(2*i-1,1)=wti_sig/sxl(istn(i))
        a_s(2*i-1,2)=0.0d0
        a_s(2*i-1,3)=wti_sig*dx/sxl(istn(i))
        a_s(2*i-1,4)=wti_sig*dy/sxl(istn(i))
        a_s(2*i-1,5)=0.0d0
        a_s(2*i-1,6)=wti_sig*dy/sxl(istn(i))
        a_s(2*i,1)=0.0d0
        a_s(2*i,2)=wti_sig/syl(istn(i))
        a_s(2*i,3)=0.0d0
        a_s(2*i,4)=wti_sig*dx/syl(istn(i))
        a_s(2*i,5)=wti_sig*dy/syl(istn(i))
        a_s(2*i,6)=-1.0*ti_sig*dx/syl(istn(i))

        b(2*i-1)=wti*uxl(istn(i))/sxl(istn(i))
        b(2*i)=wti*uyl(istn(i))/syl(istn(i))

      enddo

      if (wtt .lt. wt0) then
        indxx = 3
        goto 30
      endif                                                                  ! no interpolation for total weight < weighting threshold

      ww = 0.0d0 
      do i = 1, npr
         ww = ww + b(i)**2
      enddo

      do i=1,6
         do j=1,i
            aa(i,j)=0.0d0
            do k=1,npr
               aa(i,j)=aa(i,j)+a(k,i)*a(k,j)
            enddo
            aa(j,i)=aa(i,j)
         enddo
      enddo
         
      do i=1,6
         do j=1,i
            aa_s(i,j)=0.0d0
            do k=1,npr
               aa_s(i,j)=aa_s(i,j)+a_s(k,i)*a_s(k,j)
            enddo
            aa_s(j,i)=aa_s(i,j)
         enddo
      enddo
         
      do i = 1, 6
         do j = 1, 6
            aasv(i,j) = aa(i,j)
         enddo
      enddo

      do i=1,6
         bb(i)=0.0d0
         do j=1,npr
            bb(i)=bb(i)+a(j,i)*b(j)
         enddo
      enddo
*
*  solve b=a*x.
*
      call dludcmp(aa,6,6,indx,d)
      call dlubksb(aa,6,6,indx,bb)                                  ! solve for solution
c
      call dludcmp(aa_s,6,6,indx,d)
*
*  invert n.
*
      do i=1,6
         do j=1,6
            ainv(i,j)=0.0d0
         enddo
         ainv(i,i)=1.0d0
      enddo
      do i=1,6
         call dlubksb(aa_s,6,6,indx,ainv(1,i))                       ! solve for uncertainty
      enddo

      call mat_aba(bb,aasv,6,xaax)
      chisq = ww - xaax

      goto 60

30    continue

60    return
      end
*
*  Subroutine for GPS vertical velocity interpolation
*
      subroutine visr_v(yy,xx,min_tau,max_tau,in_tau,
     .   is_wght,id_wght,wt0,sigma0,indxx,iout,isl)
      implicit real*8 (a-h,l,o-z)
      integer*8 nstn,ncrp
      parameter (maxsit=15000)
      dimension lon(maxsit),lat(maxsit),uxl(maxsit),sxl(maxsit),
     .          uyl(maxsit),syl(maxsit),cxy(maxsit),area(maxsit),
     .          uzl(maxsit),szl(maxsit)
      dimension alon(10),alat(10),blon(10),blat(10),
     .          dcs(10),dsn(10)
      dimension crp_strk1(10),crp_strk2(10),is0(10),icrp(10)
      dimension istn(maxsit),azi(maxsit),wght(maxsit)
      dimension az(maxsit,3),bz(maxsit),aaz(6,6),azinv(6,6),bbz(6)
      dimension aazsv(6,6),az_s(maxsit,6),aaz_s(6,6),indx(6)
      common /veldatv/nstn,lon,lat,uxl,sxl,uyl,syl,uzl,szl,cxy,area
      common /crpdat/ncrp,alon,alat,blon,blat,dcs,dsn
      common /ainv_vv/azinv,bbz,rtauz,wttz,chisqz,nslctz
      common /prmtr/pi,cov,cutoff_dis,wt_az

      iout = 0
*
*  compute relation between interpolation point and creep fault
*
      do i = 1, ncrp

        icrp(i) = 0

        dx = alon(i) - xx
        dy = alat(i) - yy
        ang1 = datan2(dy,dx)/cov
        y1 = -1.0d0*(dy*dcs(i)-dx*dsn(i))

        dx = blon(i) - xx
        dy = blat(i) - yy
        ang2 = datan2(dy,dx)/cov

        if ( ang1.eq.ang2 ) then
          is0(i) = 0                                                         !  interpolation point is located on the extension of creep fault ?
          goto 20
        endif 
        if ( y1.eq.0.0d0 ) then
          indxx = 4                                                          !  interpolation point is located on creep fault
          goto 60  
        endif

        is0(i) = y1/abs(y1)

20      if (ang2 .gt. ang1) then
          crp_strk1(i) = ang1
          crp_strk2(i) = ang2
        else
          crp_strk1(i) = ang2
          crp_strk2(i) = ang1
        endif

        dang = crp_strk2(i) - crp_strk1(i)
        if ( dang.gt.180.0d0 ) then
          ang_tmp = crp_strk1(i) + 360.0d0
          crp_strk1(i) = crp_strk2(i)
          crp_strk2(i) = ang_tmp
          icrp(i) = 1
        endif

      enddo
*
*  start data selection.
*
      do 30 itau = min_tau, max_tau, in_tau

        if (itau.eq.max_tau) goto 60

        indxx = 0
        nslctz = 0
        rtauz = itau

        do 10 i = 1, nstn
          dx = lon(i) - xx
          dy = lat(i) - yy
          dr = dsqrt(dy**2+dx**2)
          if (isl .eq. 0 .and. dr .lt. 0.0001) goto 10
          ang_s1 = datan2(dy,dx)/cov
          rt = dr/rtauz

          if (rt .le. cutoff_dis) then
            do j = 1, ncrp
              if (icrp(j).eq.1.and.ang_s1.lt.0.d0) ang_s1=ang_s1+360.d0
 
              dx = alon(j) - lon(i)
              dy = alat(j) - lat(i)
              y1 = -1.0d0*(dy*dcs(j)-dx*dsn(j))
              
              if ( y1.eq.0.0d0) then
                 is1 = 0
              else
                 is1 = y1/abs(y1)
              endif
             
              if (is1.ne.is0(j)) then
              if (ang_s1.ge.crp_strk1(j).and.
     .          ang_s1.le.crp_strk2(j)) then
                goto 10
              endif
              endif

            enddo
            nslctz = nslctz + 1
            istn(nslctz) = i
          endif
10      continue

      if (nslctz.lt.3) then
        indxx = 1
        goto 30
      endif                                                                  ! no interpolation for n_slct < 3

      if (nslctz.gt.maxsit) then
        write(6,62) maxsit
        stop
      endif

62    format('# of selected stations > maxsit',i5,
     .   ', please increase maxsit ( visr_core )...')
*
*  estimate azimuthal coverage.
*
      do i = 1, nslctz
        dx = lon(istn(i)) - xx
        dy = lat(istn(i)) - yy
        azi(i) = datan2(dy,dx)/cov
      enddo
      azimax = azi(1)
      azimin = azi(1)
      do i = 2, nslctz
        azimax = max(azimax,azi(i))
        azimin = min(azimin,azi(i))
      enddo
      dazim1 = azimax - azimin
      if (dazim1.le.180.0d0) then
        indxx = 2
        goto 30
      endif                                                                  ! no interpolation for dazi < pi

      do i = 1, nslctz
        if (azi(i).lt.0.d0 ) azi(i) = azi(i) + 360.0d0
        if ( i.eq.1 ) then
          azimax = azi(i)
          azimin = azi(i)
        else
          azimax = max(azimax,azi(i))
          azimin = min(azimin,azi(i))
        endif
      enddo
      dazim2 = azimax - azimin
      if (dazim2.le.180.0d0) then
        indxx = 2
        goto 30
      endif                                                                  ! no interpolation for dazi < pi

      iout = 1
*
*  compute data spatial density weighting 
*
      if ( id_wght.eq.1 ) then 
     
        azi_avrg = wt_az*360.0d0/nslctz                                       
        azi_tot = (1.0d0+wt_az)*360.0d0                                      

        do i = 1, nslctz
          daz1 = 180.0d0
          daz2 = -180.0d0
          do j = 1, nslctz
            if (i.eq.j) goto 22
            dazi = azi(j) - azi(i)
            if (dazi.gt.180.0d0) dazi = dazi - 360.0d0
            if (dazi.lt.-180.0d0) dazi = dazi + 360.0d0
            if (dazi.gt.0.0d0.and.dazi.lt.daz1) daz1 = dazi
            if (dazi.lt.0.0d0.and.dazi.gt.daz2) daz2 = dazi
22        continue
          enddo
          wght(i)=(0.5d0*(daz1-daz2)+azi_avrg)*nslctz/azi_tot                 ! compute azimuthal weighting
        enddo

      endif

      if ( id_wght.eq.2 ) then

        sum_area = 0.0d0
        do i = 1, nslctz
          sum_area = sum_area + area(istn(i))
        enddo
        sum_area = sum_area/nslctz

        do i = 1, nslctz
          wght(i) = area(istn(i))/sum_area
        enddo

      endif
                  
      indr = 0
      npr = 2*nslctz
      wttz = 0.0d0
      do i = 1, nslctz
        dy = lat(istn(i))-yy
        dx = lon(istn(i))-xx
        dr = dsqrt(dx**2+dy**2)
        if (is_wght .eq. 1) wti = dexp(-dr/rtauz)*wght(i)
        if (is_wght .eq. 1) wti_sig = dexp(-dr/sigma0)*wght(i)
        if (is_wght .eq. 2) wti = wght(i)/((dr/rtauz)**2+1)
        if (is_wght .eq. 2) wti_sig = wght(i)/((dr/sigma0)**2+1)
        if (dr .lt. 1.0) then
           indr = 1
        endif
        wttz = wttz + 2*wti

        az(i,1)=wti/szl(istn(i))
        az(i,2)=wti*dx/szl(istn(i))
        az(i,3)=wti*dy/szl(istn(i))

        az_s(i,1)=wti_sig/szl(istn(i))
        az_s(i,2)=wti_sig*dx/szl(istn(i))
        az_s(i,3)=wti_sig*dy/szl(istn(i))

        bz(i)=wti*uzl(istn(i))/szl(istn(i))

      enddo

      if (wttz .lt. wt0) then
        indxx = 3
        goto 30
      endif                                                                  ! no interpolation for total weight < weighting threshold

      ww = 0.0d0 
      do i = 1, npr
         ww = ww + bz(i)**2
      enddo


      do i=1,3
         do j=1,i
            aaz(i,j)=0.0d0
            do k=1,nslctz
               aaz(i,j)=aaz(i,j)+az(k,i)*az(k,j)
            enddo
            aaz(j,i)=aaz(i,j)
         enddo
      enddo
         
      do i=1,3
         do j=1,i
            aaz_s(i,j)=0.0d0
            do k=1,nslctz
               aaz_s(i,j)=aaz_s(i,j)+az_s(k,i)*az_s(k,j)
            enddo
            aaz_s(j,i)=aaz_s(i,j)
         enddo
      enddo
         
      do i = 1, 3
         do j = 1, 3
            aazsv(i,j) = aaz(i,j)
         enddo
      enddo

      do i=1,3
         bbz(i)=0.0d0
         do j=1,nslctz
            bbz(i)=bbz(i)+az(j,i)*bz(j)
         enddo
      enddo
*
      call dludcmp(aaz,3,6,indx,d)
      call dlubksb(aaz,3,6,indx,bbz)               ! solve for solution
c
      call dludcmp(aaz_s,3,6,indx,d)
*
      do i=1,3
         do j=1,3
            azinv(i,j)=0.0d0
         enddo
         azinv(i,i)=1.0d0
      enddo

      do i=1,3
         call dlubksb(aaz_s,3,6,indx,azinv(1,i))   ! solve for uncertainty
      enddo

*      if (indr .eq. 1) write(6,'(f20.10)') azinv(1,1)
*      if (indr.eq.1 .and. azinv(1,1).gt.500.0) then
*         do i = 1, nslctz
*           write(6,'(2i4,8f15.5)') i,istn(i),az_s(i,1)*szl(istn(i)),
*     1    az_s(i,2)*szl(istn(i)),az_s(i,3)*szl(istn(i)),szl(istn(i)),
*     2    wght(i),az_s(i,1)/wght(i),az_s(i,2)/wght(i),az_s(i,3)/wght(i)
*         enddo
*      endif

      call mat_aba(bbz,aazsv,3,xaax)
      chisqz = ww - xaax

      goto 60

30    continue

60    return
      end

      SUBROUTINE SORTIT(array,n)
      IMPLICIT NONE
      INTEGER*8 j,k,n
      real*8 temp,array(n)
      DO J = 1, n-1
         DO K = J+1, n
            IF (array(J) .gt.  array(K)) THEN
               temp = array(K)
               array(K) = array(J)
               array(J) = temp
            ENDIF
         ENDDO
      ENDDO
      return
      end
