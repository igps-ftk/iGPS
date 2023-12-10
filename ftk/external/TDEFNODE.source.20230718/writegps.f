c********************************************************************
c* write out GPS files

      subroutine writegps(tchi)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

c-- max # of points for a single time series      
      parameter (MAX_dtmp = 11000)

      character*1 used
      character*3 a3
      character*4 ll, lE, lN, lZ, c4, s4, block_name, site1, tsname
      character*8 site4, site8
      character*50 f126, f1271
      character*80 fname, tname, sname, rname, dname
      character*55 fbob, fbob2     
      
      logical kerr, useENU, useGPS, rflag, fexist
      logical bigres
      
      dimension tres(MAX_dtmp), rres(MAX_dtmp,3), stmp(MAX_dtmp), 
     .   rtmp(MAX_dtmp), rsig(MAX_dtmp,3) 
     
      dimension pp(2), pper(2), aa(MAX_dtmp,2), rs3(3), rnn(3), trms(3)
     
      dimension a(3), b(3), erXV(3), vv(3), v(3), vrf(3), d(3) 
     
      dimension sumw(3), sumwr(3), tmpres(MAX_gps,2,3), 
     .   tscor(MAX_gps,3), tscor2(3)
     
      dimension ncount(20)

      integer   system, status

c* common for removing sites
      integer rm_type, rm_reason
c      character*180 rm_site
      common /rm1/  rm_site(MAX_rm)
      common /rm2/  nrm_site, nrm_circ, nrm_p, nrm_poly(3),
     .   rm_type(MAX_rm), rm_reason(MAX_gps)

      common /hs3/ xsyn(11000,3)
      
      kerr = .false.
      rflag = .false.

      call dater(time_string)
       
c      call clearchar(fname, 80)
      fname = ""
      call clearint(ncount, 20)
      
      ll = 'GPS '
      lE = '  vE'
      lN = '  vN'
      lZ = '  vU'
      f126  = '(2f10.4, 3f10.2, 3f8.2, f9.4, 2x, a8             )'
      f1271 = '(2f10.4, 3f11.3, 3f8.3, 2x, a8, 4f8.3            )'
c (2f10.4, 3f10.2, 3f8.2, 2x, a23, 4f6.1)
c 127  format( 2f9.3, 2f10.2, 2f8.2, f9.4, 2x, a4, i4)
c 128  format( 2f9.3, 2f10.2, 2f8.2, f9.4, 2x, a4, 2f9.2, 
c     .    2(1x, a4), 2f9.2)

      npar  = 0
      izero = 0
      ione  = 1
      m=0
      
      zero = 0.0d0
      z = zero
      o2 = zero
      gpscc = zero

c data types
      nv = 0
      nt = 0
      nd = 0
      n5 = 0
      n6 = 0
      do i=1, num_gps
        ktype = gps_type(i)
        if(ktype.eq.1) nv=nv+1
        if(ktype.eq.2) nd=nd+1
        if(ktype.eq.3) nt=nt+1
        if(ktype.eq.5) n5=n5+1
        if(ktype.eq.6) n6=n5+1
c       if (myflag) write (k67, '(a8,3i8)') gps_name(i),ktype,
c     .     ndx(i,1),ndx(i,2)
      enddo
c      if (myflag) ik = kfclose (k67)
      
      call network

c - clear stats
      call stats (ll, 0, n, obs, o2, sig,calc, r, rs, sumwt, 
     .    tchi,  datavar,  ssfit,  fname, dn, dw, npar, 0 )
      call stats (ll, 0, ntsE, obs, o2, sig,calc, r, rs, sumwtE, 
     .    tchiE, datavarE, ssfitE, fname, dn, dw, npar, 0 )
      call stats (ll, 0, ntsN, obs, o2, sig,calc, r, rs, sumwtN, 
     .    tchiN, datavarN, ssfitN, fname, dn, dw, npar, 0 )
      call stats (ll, 0, ntsZ, obs, o2, sig,calc, r, rs, sumwtZ, 
     .    tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, 0 )

c'-- output vectors and residuals at data points
      if (num_gps.gt.0 ) then
      
      if (nd.gt.0) call fopen (k67, 1,  '.dsum ')
      if (nv.gt.0) then
         call fopen (k68, 1,  '.vsum ')
         write (k68, '(a13)') "#"//time_string
      endif
      
c      call fopen (k67, 1,  '.dsum ')
c      call fopen (k68, 1,  '.vsum ')
      if (myflag) call fopen (k69, 1,  '.dat ')
      if (myflag) call fopen (k37b, 1, '.plots ')
     
c write summary of fits       
      if (wkbob) then
       call fopen (kbobfile, 1,  '_'//time_string//'.bob ')
       call fopen (kbobfile2, 1,  '_'//time_string//'.bob2 ')
       fbob = '(a1,a23,1x,2f9.3,9f8.2,f9.3,1x,7f8.2,2f8.2,i6)         '
       fbob2 = '(3f8.2,a1,a23,1x,2f9.3,9f8.2,f9.3,1x,7f8.2,2f8.2,i6)'
       write(kbobfile,'(a55)') fbob
       do kk=1, MAX_gps_files
        if ( gps_wt(kk,1) .gt. 0.0d0 )  
     .     write (kbobfile,'(a1,1x,a4,3f8.3,1x,a80)') 
     .     "#", gps_fname(kk), (gps_wt(kk,j),j=1,3), gps_filename(kk)
c23456
       enddo
      endif

c      if (wtable) then
c      call fopen (ktbl,  1, '.table ')
c      write(ktbl, *)'Site     Srce Blck Pole  Longit.   Latit.     '//
c     . 'Ve     Vn     Se     Sn     Sne   Enet   Nnet   Eref   Nref'//
c     . '   Eela   '//
c     . 'Nela   Erot   Nrot   Estr   Nstr   Eres   Nres   Vres   '//
c     . 'Vsig    R/S'
c      endif

      if ( add_rand ) then
       call fopen (kr, 1, '_rand.vec ')
       write(kr, '(a50)') f1271
      endif

      if (new_frame) then
       call fopen (k23v,  1, '_newf.vec ')
       write(k23v, '(a50)') f126
       call fopen (k23o,  1, '_newf.obs ')
       write(k23o, '(a50)') f126
      endif
      
c**************************************************************************

c-- ALL GPS
       n=0
       Vrorsig =  zero
    
      do 125 i=1, num_gps

      ktype = gps_type(i)
      
      if ( ktype.eq.0 ) goto 125
      
c      print *, i, gps_name(i), gps_fname(gps_index(i))
      
      lgf=loc_gps(i)
      
      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, sxy)
      zpt = gps_pos(i,3)

      call gpslongname(i)
c      print *, longname
      
      rho = gps_obs(i,5)
      gpscc = rho
      resx = zero
      resy = zero
      resz = zero
      rsx = zero
      rsy = zero
      rsz = zero
      
      if (ktype.eq.1) then
c-- Residual
      if ( useENU(i,1) ) resx = xobs - xcalc
      if ( useENU(i,2) ) resy = yobs - ycalc
      if ( useENU(i,3) ) resz = zobs - zcalc
      tmpres(i,1,1) = resx
      tmpres(i,1,2) = resy
      tmpres(i,1,3) = resz

c-- Res/Sig      
      if (sigx.gt.zero) rsx = resx/sigx
      if (sigy.gt.zero) rsy = resy/sigy
      if (sigz.gt.zero) rsz = resz/sigz
      tmpres(i,2,1) = rsx
      tmpres(i,2,2) = rsy
      tmpres(i,2,3) = rsz

      endif

c-- displacement vectors      
      if ( ktype.eq.2) then
        dxcalc = zero
        dycalc = zero
        dzcalc = zero
        if(useENU(i,1)) dxcalc = gps_calc(i, 2, 1) - gps_calc(i, 1, 1)
        if(useENU(i,2)) dycalc = gps_calc(i, 2, 2) - gps_calc(i, 1, 2)
        if(useENU(i,3)) dzcalc = gps_calc(i, 2, 3) - gps_calc(i, 1, 3)
        xobs = gps_obs(i,1)
        yobs = gps_obs(i,2)
        zobs = gps_obs(i,6)
        sigx = gps_obs(i,3)
        sigy = gps_obs(i,4)
        sigz = gps_obs(i,7)
        
       if ( useENU(i,1) ) resx = xobs - dxcalc
       if ( useENU(i,2) ) resy = yobs - dycalc
       if ( useENU(i,3) ) resz = zobs - dzcalc
       tmpres(i,1,1) = resx
       tmpres(i,1,2) = resy
       tmpres(i,1,3) = resz
       if (sigx.gt.zero) rsx = resx/sigx
       if (sigy.gt.zero) rsy = resy/sigy
       if (sigz.gt.zero) rsz = resz/sigz
       tmpres(i,2,1) = rsx
       tmpres(i,2,2) = rsy
       tmpres(i,2,3) = rsz
       
       t1 = gps_info(gps_index(i), 6)
       t2 = gps_info(gps_index(i), 7)
       t1 = timespan(i,1)
       t2 = timespan(i,2)
       dt = t2-t1
       rho = 0.0d0
       
       write (k67, 1668) longname, 
     .            (int(gps_info(gps_index(i),j+7)), j=1,3),
     .             xpt, ypt, t1, t2,
     .             xobs, dxcalc, resx, sigx, rsx,
     .             yobs, dycalc, resy, sigy, rsy,
     .             zobs, dzcalc, resz, sigz, rsz,
     .             rho, xcalc*dt, ycalc*dt, zcalc*dt,
     .             xcalc, ycalc, zcalc
     
 1668 format(a23, 3i2, 4f10.4, 3(3f12.4,2f8.2), 7f11.2)
     
      endif

c-- velocity vectors
      if( ktype.eq.1 ) then
      
c-- residual vectors
      call velsig(resx, resy, sigx, sigy, rho, Vres, Vrsig)
      if (Vrsig .ne. zero) Vrorsig = Vres/Vrsig
      
c** total vel, sig, az
       call velsigaz(xobs, yobs, sigx, sigy, rho, Vt, Vtsig, Vaz)

      if (gps_keep(i) ) then

      if ( gps_type(i).eq.1) then
      
c compile stats
      if (useENU(i,1)) then 
       call stats (ll, 0, n, xobs, o2, sigx, xcalc, resx, rsx, sumwt,  
     .    tchi, datavar, ssfit, fname, dn, dw, npar, 1)
       call stats (ll, 0, ntsE, xobs, o2, sigx,xcalc, resx, rsx, sumwtE, 
     .    tchiE, datavarE, ssfitE, fname, dn, dw, npar, 1 )
      endif

      if (useENU(i,2)) then
       call stats (ll, 0,n, yobs, o2, sigy, ycalc, resy, rsy, sumwt, 
     .   tchi, datavar, ssfit, fname, dn, dw, npar, 1)
       call stats (ll, 0, ntsN, yobs, o2, sigy,ycalc, resy, rsy, sumwtN, 
     .   tchiN, datavarN, ssfitN, fname, dn, dw, npar, 1 )
      endif
     
      if (useENU(i,3)) then 
       call stats (ll,0,n, zobs, o2, sigz, zcalc, resz, rsz, sumwt,
     .    tchi, datavar, ssfit, fname, dn, dw, npar, 1)
       call stats (ll, 0, ntsZ, zobs, o2, sigz,zcalc, resz, rsz, sumwtZ, 
     .    tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, 1 )
      endif
     
      endif


c'--- output calculated vectors with random errors
      if (add_rand) then
       call normal(xrand, zero, sigx)
       call normal(yrand, zero, sigy)
       call normal(zrand, zero, sigz)
        sigx1 = 99.0d0
        sigy1 = 99.0d0
        sigz1 = 99.0d0

        if (useENU(i,1)) sigx1 = sigx
        if (useENU(i,2)) sigy1 = sigy
        if (useENU(i,3)) sigz1 = sigz

        if (add_rand0) then
         xrand =0.0d0
         yrand =0.0d0
         zrand =0.0d0
         sigx1 = 0.1d0
         sigy1 = 0.1d0
         sigz1 = 0.1d0
        endif

       write (kr, f1271) xpt, ypt, xcalc+xrand, ycalc+yrand, 
     .  zcalc+zrand, sigx1, sigy1, sigz1,
     .  longname, xrand, yrand, zrand

      endif

c 1271 format(2f10.4, 3f11.3, 3f8.3, 2x, a23, 4f8.3)
c 126  format(2f10.4, 3f10.2, 3f8.2, f9.4, 2x, a23, 3f6.1)
c      f126 = '(2f10.4, 2f10.2, 2f8.2, f9.4, 2x, a8)'

c*---  vectors in rotated ref frame
       if ( new_frame ) then
        call vomega( kerr, fpole, xpt, ypt, Ve, Se, Vn, Sn, rho)
        write (k23o, 1261) xpt, ypt, xobs-Ve, yobs-Vn, zobs, 
     .   sigx, sigy, sigz,
     .   gpscc, longname
        write (k23v, 1261) xpt, ypt, xcalc-Ve, ycalc-Vn, zobs, 
     .   sigx, sigy, sigz,
     .   gpscc, longname
       endif     
 1261 format(2f10.4, 3f10.2, 3f8.2, f9.4, 2x, a23)

c--- .vsum - summary of velocity vectors
       t1 = gps_info(gps_index(i), 6)
       t2 = gps_info(gps_index(i), 7)
       t1 = timespan(i,1)
       t2 = timespan(i,2)
       l = loc_gps(i)
       
       if (myflag) write (k69, '(a4,1x,2f10.4,4f8.1)') gps_name(i), 
     .             xpt, ypt, xobs, sigx, yobs, sigy 

       write (k68, 1669) longname, 
     .            (int(gps_info(gps_index(i),j+10)), j=1,3),
     .             xpt, ypt, t1, t2,
     .             xobs, xcalc, resx, sigx, rsx,
     .             yobs, ycalc, resy, sigy, rsy,
     .             zobs, zcalc, resz, sigz, rsz,
     .             rho, 
     .             (gps_ela(l,j), j=1,3), z,z,z,
     .             (gps_str(i,j), j=1,2), z,z,
     .             (gps_rot(i,j), j=1,5), 
     .             (gps_net(i,j), j=1,5),
     .             (gps_rlx(i,j), j=1,3),
     .             rsx*rsx+rsy*rsy+rsz*rsz, zpt

      endif
 1669 format(a23, 3i2, 4f10.4, 3(3f10.2,2f8.2), f8.3, 10f8.2, 
     .       2(4f8.2,f8.4), 3f8.2, 2f12.2 )

c** Bob's file, 10/26/11 add verticals to it
      if (wkbob) then
       used=' '
       kt = rm_reason(i)
       tot = rsx**2 + rsy**2
       if ( .not. useGPS(i) ) used='#'
        if ( useGPS(i) .or. 
     .      (keep_bad .and. (kt.eq.3 .or. kt.eq.4 .or. kt.eq.7
     .      .or. kt.eq.8 .or. kt.eq.9 .or. kt.eq.10) ) ) then

        write (kbobfile2, fbob2) rsx, rsy, tot, used, 
     .   longname, xpt, ypt, 
     .   xobs, yobs, zobs, resx, resy, resz, sigx, sigy, sigz, rho, 
     .   rsx, rsy, rsz, Vres, Vrsig, Vrorsig, 
     .   (rsx*rsx+rsy*rsy+rsz*rsz),  Vt, Vtsig, int(Vaz) 

        write (kbobfile, fbob) used, 
     .   longname, xpt, ypt, 
     .   xobs, yobs, zobs, resx, resy, resz, sigx, sigy, sigz, rho, 
     .   rsx, rsy, rsz, Vres, Vrsig, Vrorsig, 
     .   (rsx*rsx+rsy*rsy+rsz*rsz),  Vt, Vtsig, int(Vaz) 

        endif
      endif

c--- output summary table
c      if (wtable) then
c      used=' '
c      if ( .not. useGPS(i) ) used='x'
c      write (ktbl, 1255) used, longname(1:8), longname(10:23),
c     . xpt, ypt, xobs+gps_net(i,1), 
c     . yobs+gps_net(i,2), sigx, sigy, rho, 
c     . gps_net(i,1), gps_net(i,2), xobs, yobs,
c     . gps_ela(lgf,1), gps_ela(lgf,2), 
c     . gps_rot(i,1), gps_rot(i,2),
c     . gps_str(i,1), gps_str(i,2), resx, resy,
c     . Vres, Vrsig, Vrorsig
c      endif
c 1255 format(a1, a8, 1x, a14, 2f9.3, 4f7.1, f8.4, 14f7.1, f8.2)

c'Site Srce Blck Pole  Longit.   Latit.     Ve     Vn     Se     Sn     Sne   Enet   Nnet   Eela   Nela   Erot   Nrot   Estr   Nstr   Eres   Nres   Vres   Vsig    R/S'
c1234 1234 1234 12341234567891234567891234567123456712345671234567123456781234567123456712345671234567123456712345671234567123456712345671234567123456712345671234567

       endif

c       print *, 'done ',i, gps_name(i)
 125   continue

c write stats
      call stats (ll, 0, n, yobs, o2, sigy,ycalc, resy, ry, sumwt, tchi, 
     .  datavar, ssfit,fname, dn, dw, npar, ksum)
      call stats (lE, 0, ntsE, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtE, tchiE, datavarE, ssfitE, fname, dn, dw, npar, ksum)
      call stats (lN, 0, ntsN, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtN, tchiN, datavarN, ssfitN, fname, dn, dw, npar, ksum)
      call stats (lZ, 0, ntsZ, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtZ, tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, ksum)
c       print *, 'done stats '

      if (wkbob) call stats (ll, 0,n, yobs, o2, sigy,ycalc, resy, ry, 
     . sumwt, tchi, datavar, ssfit, fname, dn, dw, npar, kbobfile)

       if (nd.gt.0) ik = kfclose (k67)
       if (nv.gt.0) ik = kfclose (k68)
       if (myflag) ik = kfclose (k69)
       
       if (wkbob)  ik = kfclose (kbobfile)
       if (wkbob)  ik = kfclose (kbobfile2)
c       if (wtable) ik = kfclose (ktbl)
       if (add_rand) ik = kfclose (kr)
c       if (new_frame) ik = kfclose(k23v)
c       if (new_frame) ik = kfclose(k23o)
       
c****************************************************************************
c****  output summary of time series 
c****************************************************************************

      if (num_ts.gt.0 ) then
     
      call fopen (k33, 1,  '.tsum ')
      call fopen (k34, 1,  '_ts.stat ')
     
      ll = 'DISP'
      lE = ' tsE'
      lN = ' tsN'
      lZ = ' tsU'
      
      kk=0
      call buildhistory (kk)
      
c  *****

c create directory for ts files
      call existfile( './'//expname//'/ts', fexist, 0)
      if ( .not. fexist ) 
     .     status = system('mkdir ./'//expname//'/ts'//char(0))

      do 101 jf = 1, num_gps_file
      
       if (gps_file_type(jf).eq.3) then

       ll=gps_fname(jf)
       fname = gps_filename(jf)
       c4 = gps_fname(jf)
       npar = 0

c if residual > bigrescut
      bigrescut = 0.0d0
      if (gps_info (jf,16).gt. 0.0d0) 
     .      bigrescut = gps_info (jf,16)
      if(bigrescut.gt.0.0d0) call fopen (k39,1,'_'//c4//'_ts.bigres ')

c all data in gts format, ts1: format
       dname = expname//'/ts/'//c4//'.gts '
       if(myflag) call fopen (kgts, 0, dname )
      
c  *****

         call stats (ll, 0, nts, obs, o2, sig,calc, 
     .    r, rs, sumwt, tchi, datavar, ssfit, fname, dn, dw, npar, 0 )
     
         call stats (ll, 0, ntsE, obs, o2, sig,calc, r, rs, sumwtE, 
     .    tchiE, datavarE, ssfitE, fname, dn, dw, npar, 0 )
         call stats (ll, 0, ntsN, obs, o2, sig,calc, r, rs, sumwtN, 
     .    tchiN, datavarN, ssfitN, fname, dn, dw, npar, 0 )
         call stats (ll, 0, ntsZ, obs, o2, sig,calc, r, rs, sumwtZ, 
     .    tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, 0 )

      do 102 i= 1, num_gps

      ktype = gps_type(i)
      
      if (ktype.eq.3) then
      
      lgf=loc_gps(i)
      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)
      call gpslongname(i)
            
**** write time series data      
 
      if (gps_index(i).eq.jf .and. useGPS(i) ) then
      
       n1 = ndx(i,1)
       n2 = ndx(i,2)
       rn = real(n2-n1)
       dt = t_disp(n2) - t_disp(n1)
       s4 = gps_name(i)
       tname = expname//'/ts/'//s4//'_'//c4//'.out '
       sname = expname//'/ts/'//s4//'_'//c4//'.syn '
       rname = expname//'/ts/'//s4//'_'//c4//'.rnd '

c*--- calculated vectors in rotated ref frame
       if ( new_frame ) then
        call vomega( kerr, fpole, xpt, ypt, Ve, Se, Vn, Sn, rho)
c        write (k23o, 1261) xpt, ypt, xobs-Ve-Re, yobs-Vn-Rn, zobs, 
c     .   sigx, sigy, sigz,
c     .   gps_obs(i,5), longname

        write (k23v, 1261) xpt, ypt, 
     .   xcalc-Ve-gps_net(i,1), ycalc-Vn-gps_net(i,2), zobs, 
     .   sigx, sigy, sigz,
     .   gps_obs(i,5), longname

       endif   

c-- open files
       k40 = 40
       call fopen (k40, 0, tname )
       if (w_syn) call fopen (k41, 0, sname )
       if (rflag) call fopen (k42, 0, rname )
       
c**************************************************       
c** see if this site is to be merged with another        
      site1 = gps_name(i)
      tsname = gps_fname(gps_index(i))
      ms1=0
      ms2=0
      site4 = '        '    
      merge_nmbr = 0
      do kk =1, nsmerge
       if ( tsname.eq.merge_sites(kk,1) .and.
     .    (site1.eq.merge_sites(kk,3) .or.
     .     site1.eq.merge_sites(kk,4)) ) then
     
         site4 = merge_sites(kk,2)//'    '
         ncount(kk) = ncount(kk)+1
         merge_nmbr = kk
             
      do ii = 1, num_gps
       if ( gps_type(ii).eq.3) then
        if( gps_fname(gps_index(ii)).eq.tsname) then
         if( merge_sites(kk,3).eq.gps_name(ii) ) ms1=ii
         if( merge_sites(kk,4).eq.gps_name(ii) ) ms2=ii
        endif
       endif
      enddo
      print *, 'Merging ', gps_name(ms1), ' ', gps_name(ms2), 
     .         ' to ', site4
      endif
      enddo
        
c**************************************************       
           rho = 0.0d0
          
c* vrf is secular velocity in ref frame of block model
      vrf(1) = xcalc - gps_net(i,1)
      vrf(2) = ycalc - gps_net(i,2)
      vrf(3) = zcalc 

c* vrf is secular velocity in ref frame of model
c      vrf(1) = xobs - gps_net(i,1)
c      vrf(2) = yobs - gps_net(i,2)
c      vrf(3) = zobs 
      
c* vv is secular velocity in data ref frame 
      vv(1) = xcalc 
      vv(2) = ycalc
      vv(3) = zcalc 

c v is velocity used for time series      
       do j=1,3
        if (XVflag(i, j, 2).eq.0 ) then
                v(j) = 0.0d0
                vrf(j) = 0.0d0
        elseif (XVflag(i, j, 2).eq.1 ) then
                v(j) = GVo(i,j,2)
                vrf(j) = 0.0d0
        elseif (XVflag(i, j, 2).eq.2 ) then
                v(j) = GVo(i,j,2)
                vrf(j) = 0.0d0
        elseif (XVflag(i, j, 2).eq.3 ) then
                v(j) = vv(j)
        endif

c         GVo(i,j,2)=v(j)
c        endif  

       enddo
       
      site8 = longname(1:8)
        
c       write (k38, 1266) 'H', xpt, ypt, vrf(1), vrf(2), 
c     .  erXV(1), erXV(2), rho, site8, longname(9:23), vrf(3), erXV(3), 
c     .  t_disp(n1), t_disp(n2), (GXo(i,j,1),j=1,3)

       write (k40, 1266) 'H', xpt, ypt, vrf(1), vrf(2), 
     .  erXV(1), erXV(2), rho, site8, longname(9:23), vrf(3), erXV(3), 
     .  t_disp(n1), t_disp(n2), (GXo(i,j,1),j=1,3)

c ts1: format header. Lon  Lat  Ve Vn Vz Se Sn Sz Site   
       if(myflag)  write (kgts, '(2f12.5, 6f8.2, 1x, a8 )' ) 
     .  xpt, ypt, vrf(1), vrf(2), vrf(3),
     .  erXV(1), erXV(2), erXV(3), gps_name(i)
     
       if(rflag) write (k42, 1269) xpt, ypt, vrf(1), vrf(2), 
     .  erXV(1), erXV(2), rho, site8
 1269 format(2f10.4,2f10.2,2f8.2,f9.4,2x,a8)

c-- merged sites
c      if(ms1.eq.i) write (k38, 1266) 'H', xpt, ypt, vrf(1), vrf(2), 
c     .  erXV(1), erXV(2), rho, site4, longname(9:23), vrf(3), erXV(3), 
c     .  t_disp(n1), t_disp(n2), (GXo(i,j,1),j=1,3)

      if(ms1.eq.i) write (k40, 1266) 'H', xpt, ypt, vrf(1), vrf(2), 
     .  erXV(1), erXV(2), rho, site4, longname(9:23), vrf(3), erXV(3), 
     .  t_disp(n1), t_disp(n2), (GXo(i,j,1),j=1,3)
      
     
c 1265 format(2f10.4,2f10.2,2f8.2,f9.4,2x,a23,2f8.2,2f10.3,7f7.1)
 1266 format(a1,2f10.4,2f10.2,2f8.2,f9.4,2x,a8,a15,2f8.2,2f9.2,3f8.2)
 1267 format (f9.4,18f9.2, 1x, a8,1x,a4)

    
       ksyn=1
       call ts_calc (0, i, ksyn)
       
       
c******  merged time series    
      if ( merge_nmbr.gt.0 ) then
      
        m = ncount(merge_nmbr)
       
c** first time series to be merged      
        do j=1,3
         tscor(i,j) = xsyn(1,j)-x_calc(n1,j)
        enddo

      if( m.eq.2) then
        do j=1,3
         if(i.eq.ms1) tscor2(j) = tscor(ms2,j)-tscor(ms1,j)
         if(i.eq.ms2) tscor2(j) = tscor(ms1,j)-tscor(ms2,j)
        enddo
        
        write(*,'("D ",a8, 3f8.1)') site4, (tscor2(j),j=1,3)
        
      endif
      
      endif
      
c************  end of merging

       
       
c--- loop through points to do statistics    
       ni = 0
       call cleareal(sumw, 3)
       call cleareal(sumwr, 3)
       
       do nn = n1, n2
        ni=ni+1
        tres(ni) = t_disp(nn)
        
        do j=1,3
         dobs = x_disp(nn,j)
         dcalc = x_calc(nn,j)
         sigx = x_sig(nn,j)
         
         r = dobs-dcalc
         rres(ni,j) = r
         rsig(ni,j) = sigx
         d(j) = r
         
         rx = r/sigx
         w = fnwt(sigx)
         sumw(j)=sumw(j)+w
         sumwr(j) = sumwr(j)+ w*r*r

cc fix 5-26-17         
         if (useENU(i,j) ) 
     .     call stats (ll, 0, nts, dobs, o2, sigx, dcalc, r, rx, 
     .     sumwt, tchi, datavar, ssfit, fname, dn, dw, npar, 1)
     
        if (j.eq.1 .and. useENU(i,j) ) call stats (lE, 0, ntsE, dobs, 
     .   o2, sigx, dcalc, 
     .   r, rx, sumwtE, tchiE, datavarE, ssfitE, fname, dn, dw, npar,1)
     
        if (j.eq.2 .and. useENU(i,j) ) call stats (lN, 0, ntsN, dobs, 
     .   o2, sigx, dcalc, 
     .   r, rx, sumwtN, tchiN, datavarN, ssfitN, fname, dn, dw, npar,1)
     
        if (j.eq.3 .and. useENU(i,j) ) call stats (lZ, 0, ntsZ, dobs, 
     .   o2, sigx, dcalc, 
     .   r, rx, sumwtZ, tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, 1)
     
     
        enddo
        
c-- write out observed and calculated data while removing secular velocity        

        dt = t_disp(nn) - t_disp(n1)
        tt = t_disp(nn) 
        
c         write(k38, 1267 ) t_disp(nn), 
c     .     (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
c     .      x_calc(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
c     .      x_sig(nn,j), d(j), j=1,3),
c     .     (x_disp(nn,j)-GXo(i,j,1), j=1,3), 
c     .     (x_calc(nn,j)-GXo(i,j,1), j=1,3), site8,
c     .      tsname     
  
         write(k40, 1277 ) t_disp(nn), 
     .     (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
     .      x_calc(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
     .      x_sig(nn,j), d(j), j=1,3),
c     .     (x_disp(nn,j)-GXo(i,j,1), j=1,3), 
     .     (x_disp(nn,j), j=1,3), 
c     .     (x_calc(nn,j)-GXo(i,j,1), j=1,3), 
     .     (x_calc(nn,j), j=1,3), 
     .   (fsine(tt,GVo(i,j,3),GVo(i,j,4),GVo(i,j,5),GVo(i,j,6)),j=1,3), 
     .   site8, tsname       

 1277 format (f9.4,21f9.2, 1x, a8,1x,a4)

c detrended .gts data file
      if(myflag)  write(kgts, 1278) 
     .  t_disp(nn), 
     .  (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j),x_sig(nn,j), j=1,3 )

cc randomize time series     
         if(rflag) then
          call cleareal(rnn,3)
          if (add_rand) then
             do j=1,3
              sig = x_sig(nn,j)
              call normal ( rr, zero, sig)
              if (add_rand0) rr = 0.0d0
              rnn(j) = rr
             enddo
          endif
           write(k42, 1278 ) t_disp(nn), 
     .     (x_disp(nn,j)-GXo(i,j,1)+rnn(j), x_sig(nn,j), j=1,3) 
         endif

 1278 format (f10.5, 6f10.3)

c-- write out large misfits, res/sigma or residual
       if ( bigrescut .gt. 0.0d0 ) then
          call cleareal(rs3,3)
           bigres = .false.
          do j=1,3
             rs3(j) = d(j)
             if(abs(d(j)).gt.bigrescut) bigres = .true.
             if(x_sig(nn,j).gt.z) rs3(j) = d(j)/x_sig(nn,j)
          enddo

        if(bigres) 
     .   write(k39, '(a8,1x,a4,1x,f9.4, 18f9.1 )' ) 
     .     site8, tsname, t_disp(nn), 
     .     (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
     .      x_calc(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
     .      x_sig(nn,j), d(j), rs3(j), j=1,3)
       endif


c****  write merged time series    
      if ( merge_nmbr.gt.0 ) then

      if( m.eq.2) then
c         write(k38, 1267 ) t_disp(nn), 
c     .     (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j)+tscor2(j), 
c     .      x_calc(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j)+tscor2(j), 
c     .      x_sig(nn,j), d(j), j=1,3),
c     .      (x_disp(nn,j)-GXo(i,j,1)+tscor2(j), j=1,3), 
c     .      (x_calc(nn,j)-GXo(i,j,1)+tscor2(j), j=1,3), site4,
c     .      tsname  
         write(k40, 1267 ) t_disp(nn), 
     .     (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j)+tscor2(j), 
     .      x_calc(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j)+tscor2(j), 
     .      x_sig(nn,j), d(j), j=1,3),
     .      (x_disp(nn,j)-GXo(i,j,1)+tscor2(j), j=1,3), 
     .      (x_calc(nn,j)-GXo(i,j,1)+tscor2(j), j=1,3), site4,
     .      tsname  
      else
c        write(k38, 1267 ) t_disp(nn), 
c     .     (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
c     .      x_calc(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
c     .      x_sig(nn,j), d(j), j=1,3),
c     .      (x_disp(nn,j)-GXo(i,j,1), j=1,3), 
c     .      (x_calc(nn,j)-GXo(i,j,1), j=1,3), site4,
c     .      tsname  
        write(k40, 1267 ) t_disp(nn), 
     .     (x_disp(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
     .      x_calc(nn,j)-GVo(i,j,2)*dt-x_calc(n1,j), 
     .      x_sig(nn,j), d(j), j=1,3),
     .      (x_disp(nn,j)-GXo(i,j,1), j=1,3), 
     .      (x_calc(nn,j)-GXo(i,j,1), j=1,3), site4,
     .      tsname  
      endif
      
      endif
      
c************  end of merging

          
      enddo
c*** end of ts data loop

c**************************************************       
c** assign uncertainties, add random walk    
c-- rw in mm/sqrt(yr)  
       rw = 1.0d0
       rn  = real (ni)
      do j=1, 3
       Vvar = GVo(i,j,7)**2
       varn = 9.0d0
       if(rn.gt.one .and. sumw(j).gt.z) 
     .     varn = sumwr(j)/((rn-one)*sumw(j))
       s = 9.0d0
       if (dt.gt.z .and. rn.gt.z) s = varn/(rn*dt*dt) + rw*rw/dt
       if (dt.gt.z ) s = 2.0d0*Vvar + rw*rw/dt
       erXV(j) = dsqrt(s)
      enddo

c       write (*,'("___", a23, 3f10.4)') longname,(GVo(i,j,2),j=1,3)

       tsumw = sumw(1)+sumw(2)+sumw(3)
       tsumwr= sumwr(1)+sumwr(2)+sumwr(3)
       wrms = dsqrt(tsumwr/tsumw)
       xnrms = dsqrt(tsumwr/real(n2-n1+1))
       
       write(k34, 1275) longname, xpt, ypt, (GVo(i,j,2), j=1,3), 
     .    (erXV(j),j=1,3), rho,  
     .    t_disp(n1), t_disp(n2), t_disp(n2)-t_disp(n1), 
     .    (n2-n1+1), wrms, xnrms,
     .    (dsqrt(sumwr(j)/sumw(j)),j=1,3),
     .    (GVo(i,j,7),j=1,3)
 1275 format(a23, 2f10.4, 3f10.2, 3f8.2, 4f10.4, 
     .    i5, 8f12.4)
     
c**************************************************       
c detrend res and write corrections to offset, slope
c**************************************************       
      call cleareal(a,3)
      call cleareal(b,3)
      call cleareal(trms,3)
      ni=n2-n1+1
      
c parm 1 is slope, parm 2 is intercept
      do j=1,3
       call cleareal(pp,2)
       do n=1,ni
        rtmp(n) = rres(n,j)
        stmp(n) = rsig(n,j)
        aa(n,1) = tres(n)
        aa(n,2) = 1.0d0
       enddo
       
       call linsolve (ni, MAX_dtmp, itwo, itwo, aa, rtmp, stmp, 
     .    pp, pper)


       b(j)=pp(1)
       a(j)=pper(1)

c rms
       dr2 = 0.0d0
       wt2 = 0.0d0
       do n=1,ni
        dr = rtmp(n) - ( pp(1) * tres(n) + pp(2) )
        wt = one/stmp(n) 
        dr2 = dr2 + dr*dr*wt*wt
        wt2 = wt2 + wt*wt
       enddo
        trms(j) = sqrt (dr2/wt2)
     
      enddo
      
c      print *, (b(j),j=1,3)
      
      s=0.5d0
      r=0.0d0
      z=0.0d0

c-- .tsum file      
       write(k33, 1268) longname, (XVflag(i,j,2), j=1,3),
     .    xpt, ypt, t_disp(n1), t_disp(n2),
     .    (vrf(j), j=1,3), (v(j), j=1,3), (b(j),j=1,3),
c     .    xobs,yobs,zobs, (v(j), j=1,3), (b(j),j=1,3),
     .    (erXV(j), j=1,3),  
     .    (gps_ela(lgf,j), j=1,3), z,z,z,
     .    (gps_str(i,j), j=1,2), z,z,
     .    (gps_rot(i,j), j=1,5), 
     .    (gps_net(i,j), j=1,5),
     .    (trms(j), j=1,3), n2-n1+1
     
c-- merged sites
      if(ms1.eq.i) then 
      
       write(k33, 1268) site4//longname(9:23), (XVflag(i,j,2), j=1,3),
     .    xpt, ypt, t_disp(n1), t_disp(n2),
     .    (vrf(j), j=1,3), (v(j), j=1,3), (b(j),j=1,3),
c     .    xobs,yobs,zobs, (v(j), j=1,3), (b(j),j=1,3),
     .    (erXV(j), j=1,3),  
     .    (gps_ela(lgf,j), j=1,3), z,z,z,
     .    (gps_str(i,j), j=1,2), z,z,
     .    (gps_rot(i,j), j=1,5), 
     .    (gps_net(i,j), j=1,5),  
     .    (trms(j), j=1,3), n2-n1+1
     
      endif
      
 1268 format(a23, 3i2, 4f10.4, 35f9.2, i7)
     

c**************************************************       
c write synthetic data       
c**************************************************  
      if (w_syn) then     
        ts1 = timespan(i,1)
        ts2 = timespan(i,2)
        dtsyn  = fndtsyn(i)
        nk = int((ts2-ts1)/dtsyn) +1
        
        do k=1,nk
         t = ts1 + real(k-1)*dtsyn
         dt = t - ts1
c         write(k32, '(f9.4,6f9.2, 1x, a8,a5)') t, 
c     .    (xsyn(k,j)-GVo(i,j,2)*dt-x_calc(n1,j), j=1,3), 
c     .    (xsyn(k,j), j=1,3), site8, longname(9:13) 

c        print *, site4, t, xsyn(k,1),GVo(i,1,2),dt,x_calc(n1,1)
         write(k41, '(f9.4,9f11.4, 1x, a8,a5)') t, 
     .   (xsyn(k,j)-GVo(i,j,2)*dt-x_calc(n1,j), j=1,3), 
     .   (xsyn(k,j), j=1,3), 
     .   (fsine(t,GVo(i,j,3),GVo(i,j,4),GVo(i,j,5),GVo(i,j,6)),j=1,3), 
     .    site8, longname(9:13) 
        enddo

c write synthetic data once for merged time series, use second site
       if (m.eq.1) then
         do k=1,nk
          t = ts1 + real(k-1)*dtsyn
          dt = t - ts1
c          write(k32, '(f9.4,6f9.2, 1x, a8,a5)') t, 
c     .     (xsyn(k,j)-GVo(i,j,2)*dt-x_calc(n1,j), j=1,3), 
c     .     (xsyn(k,j), j=1,3), site4, longname(9:13) 
          write(k41, '(f9.4,6f10.3, 1x, a8,a5)') t, 
     .     (xsyn(k,j)-GVo(i,j,2)*dt-x_calc(n1,j), j=1,3), 
     .     (xsyn(k,j), j=1,3), site4, longname(9:13) 
         enddo
        endif
        
c       if (m.eq.2) then
c         do k=1,nk
c          t = ts1 + real(k-1)*dtsyn
c          dt = t - ts1
c          write(k32, '(f9.4,6f9.2, 1x, a8,a5)') t, 
c     .     (xsyn(k,j)-GVo(i,j,2)*dt-x_calc(n1,j)+tscor2(j), j=1,3), 
c     .     (xsyn(k,j), j=1,3), site4, longname(9:13) 
c        enddo
c        endif
      endif
c**************************************************       
c************************************************** 
c close .out file
      ik = kfclose (k40)

c close .syn file
      if(w_syn) ik = kfclose (k41)

c close random file
       if(rflag) then
          x9 = 9999.0d0
          write(k42, '(f9.1)' ) x9
          ik = kfclose (k42)
       endif

c delimiter for .gts file
       if(myflag) write(kgts, '(a6)' ) '9999.9'

      endif
      endif

c end of i loop?
 102  continue
c     enddo
       
      call stats (ll, 0, nts, yobs, o2, sigy,ycalc, r, ry, sumwt, tchi, 
     .  datavar, ssfit,fname, dn, dw, npar, ksum)
      if (myflag) write(k37b, '(a2,2x,a4,2f10.3 )' ) 'TS', ll, 
     .     gps_info(jf,6), gps_info(jf,7)
     
      call stats (lE, 0, ntsE, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtE, tchiE, datavarE, ssfitE, fname, dn, dw, npar, ksum)
      call stats (lN, 0, ntsN, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtN, tchiN, datavarN, ssfitN, fname, dn, dw, npar, ksum)
      call stats (lZ, 0, ntsZ, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtZ, tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, ksum)

c       ik = kfclose (k32)
c       ik = kfclose (k38)
       if (myflag) ik = kfclose (kgts)
       if (bigrescut .gt. 0.0d0 ) ik = kfclose (k39)

    
      endif

 101  continue

c       ik = kfclose (k31)
       ik = kfclose (k34)
       ik = kfclose (k33)
       if (new_frame) ik = kfclose(k23v)
       if (new_frame) ik = kfclose(k23o)

c       ik = kfclose (k36)
c       ik = kfclose (k37)
      endif
      
      

c****************************************************************************
c****  output summary of residuals by GPS files, blocks, and poles
c****************************************************************************
      
      k28=ksum

c****************************************************************************
c* output summary of residuals by GPS file
c****************************************************************************
c -- .plots file for plotting with GMT        

      do j=1, num_gps_file

c      print *, j, gps_filename(j)
      
      ll = gps_fname(j)
      lE = '  vE'
      lN = '  vN'
      lZ = '  vU'
      fname = gps_filename(j)
      npar = 0


c - clear stats
      call stats (ll, 0, n, obs, o2, sig,calc, r, rs, sumwt, 
     .    tchi,  datavar,  ssfit,  fname, dn, dw, npar, 0 )
      call stats (ll, 0, ntsE, obs, o2, sig,calc, r, rs, sumwtE, 
     .    tchiE, datavarE, ssfitE, fname, dn, dw, npar, 0 )
      call stats (ll, 0, ntsN, obs, o2, sig,calc, r, rs, sumwtN, 
     .    tchiN, datavarN, ssfitN, fname, dn, dw, npar, 0 )
      call stats (ll, 0, ntsZ, obs, o2, sig,calc, r, rs, sumwtZ, 
     .    tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, 0 )

      do i=1, num_gps

      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)

      if (gps_index(i).eq.j .and. useGPS(i) .and. ndx(i,1).eq.0 ) then

      ktype = gps_type(i)

      resx = tmpres(i,1,1)
      resy = tmpres(i,1,2)
      resz = tmpres(i,1,3)
      rsx = tmpres(i,2,1)
      rsy = tmpres(i,2,2)
      rsz = tmpres(i,2,3)

c      if(useENU(i,1)) call stats (ll, 0,n, xobs, o2,sigx, xcalc, 
c     .  resx, rx, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)
c      if(useENU(i,2)) call stats (ll, 0,n, yobs, o2,sigy, ycalc, 
c     .  resy, ry, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)
c      if(useENU(i,3)) call stats (ll, 0,n, zobs, o2,sigz, zcalc, 
c     .  resz, rz, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)
      
c compile stats
      if (useENU(i,1)) then 
       call stats (ll,0,n, xobs, o2, sigx, xcalc, resx, rsx, sumwt,  
     .    tchi, datavar, ssfit, fname, dn, dw, npar, 1)
       call stats (ll, 0, ntsE, xobs, o2, sigx,xcalc, resx, rsx, sumwtE, 
     .    tchiE, datavarE, ssfitE, fname, dn, dw, npar, 1 )
      endif

      if (useENU(i,2)) then
       call stats (ll,0,n, yobs, o2, sigy, ycalc, resy, rsy, sumwt, 
     .   tchi, datavar, ssfit, fname, dn, dw, npar, 1)
       call stats (ll, 0, ntsN, yobs, o2, sigy,ycalc, resy, rsy, sumwtN, 
     .   tchiN, datavarN, ssfitN, fname, dn, dw, npar, 1 )
      endif
     
      if (useENU(i,3)) then 
       call stats (ll,0,n, zobs, o2, sigz, zcalc, resz, rsz, sumwt,
     .    tchi, datavar, ssfit, fname, dn, dw, npar, 1)
       call stats (ll, 0, ntsZ, zobs, o2, sigz,zcalc, resz, rsz, sumwtZ, 
     .    tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, 1 )
      endif

      endif

      enddo

c output stats
c      call stats (ll, 0, n, o1, o2, sigy,ycalc, resy, ry, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, k28)
c write stats
      call stats (ll, 0, n, yobs, o2, sigy,ycalc, resy, ry, sumwt, tchi, 
     .  datavar, ssfit,fname, dn, dw, npar, k28)
      call stats (lE, 0, ntsE, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtE, tchiE, datavarE, ssfitE, fname, dn, dw, npar, k28)
      call stats (lN, 0, ntsN, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtN, tchiN, datavarN, ssfitN, fname, dn, dw, npar, k28)
      call stats (lZ, 0, ntsZ, yobs, o2, sigy,ycalc, r, ry, 
     .  sumwtZ, tchiZ, datavarZ, ssfitZ, fname, dn, dw, npar, k28)

       if (n.gt.0 .and. ktype.eq.1 .and. myflag ) 
     .    write(k37b, '(a2,2x,a4,2f10.3 )' ) 'GP', ll, 
     .     gps_info(j,6), gps_info(j,7)
       if (n.gt.0 .and. ktype.eq.2 .and. myflag) 
     .   write(k37b, '(a2,2x,a4,2f10.3 )' ) 'DS', ll, 
     .     gps_info(j,6), gps_info(j,7)

      enddo

c      call clearchar(fname,80)
      fname = ""
      npar = 0

c****************************************************************************
c* output summary of residuals by block
c****************************************************************************

      do j=1,nblocks
       if(block_flag(j)) then

       ll=block_name(j)

      call stats (ll, 0,n, xobs, o2,sigx,xcalc, resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 0)

      do i=1, num_gps
      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)

      if (nblock_gps(i).eq.j .and. useGPS(i) .and. ndx(i,1).eq.0 
     .   .and. gps_type(i).eq.1 ) then

      resx = 0.0d0
      resy = 0.0d0
      resz = 0.0d0
      rx = 0.0d0
      ry = 0.0d0
      rz = 0.0d0

      if(useENU(i,1)) call stats (ll, 0,n, xobs, o2,sigx,xcalc, 
     .  resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,2)) call stats (ll, 0,n, yobs, o2,sigy,ycalc, 
     .  resy, ry, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,3)) call stats (ll, 0,n, zobs, o2,sigz, zcalc, 
     .  resz, rz, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)
     
      resv=dsqrt( resx*resx + resy*resy)
      ress=dsqrt( sigx*sigx + sigy*sigy)
      
      endif

      enddo

      npar = 3
      call stats (ll, 0, n, yobs, o2,sigy,ycalc, resy, ry, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, k28)

       block_res(j,1) = dn
       block_res(j,2) = dw
       block_res(j,3) = n
       endif
      enddo

c****************************************************************************
c* output residual summary by block pole number
c****************************************************************************

      do j=1,MAX_poles

       call i2c(j, 3, a3)
       ll='P'//a3
c      print *, j, ll

      call stats (ll,0, n, xobs, o2,sigx,xcalc, resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 0)

      do i=1, num_gps

      m = npole_block(nblock_gps(i))
      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)

      if (m.eq.j .and. useGPS(i) .and. ndx(i,1).eq.0 
     .      .and. gps_type(i).eq.1) then

      resx = 0.0d0
      resy = 0.0d0
      resz = 0.0d0
      rx = 0.0d0
      ry = 0.0d0
      rz = 0.0d0

      if(useENU(i,1)) call stats (ll, 0,n, xobs, o2,sigx,xcalc, 
     .  resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,2)) call stats (ll, 0,n, yobs, o2,sigy,ycalc, 
     .  resy, ry, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,3)) call stats (ll, 0,n, zobs, o2,sigz, zcalc, 
     .  resz, rz, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

c      call stats (ll, 0,n, xobs, o2,sigx,xcalc, resx, rx, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)

c      call stats (ll, 0,n, yobs, o2,sigy, ycalc, resy, ry, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)
      
      endif

      enddo

      npar = 3
      if ( n.gt.0 ) call stats (ll, 0,n, yobs, o2,sigy,ycalc, resy, 
     .  ry, sumwt, tchi, datavar, ssfit, fname, dn, dw, npar, k28)

      enddo

c****************************************************************************
c* output residual summary by block strain number
c****************************************************************************
c strain SUM

      do j=1,MAX_strain

       call i2c(j, 3, a3)
       ll='S'//a3
c      print *, j, ll

      call stats (ll,0, n, xobs, o2,sigx,xcalc, resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 0)

      do i=1, num_gps

      m = nstrain_block(nblock_gps(i))

      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)

      if (m.eq.j .and. useGPS(i) .and. ndx(i,1).eq.0 
     .  .and. gps_type(i).eq.1 ) then

      resx = 0.0d0
      resy = 0.0d0
      resz = 0.0d0
      rx = 0.0d0
      ry = 0.0d0
      rz = 0.0d0

      if(useENU(i,1)) call stats (ll, 0,n, xobs, o2,sigx,xcalc, 
     .  resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,2)) call stats (ll, 0,n, yobs, o2,sigy,ycalc, 
     .  resy, ry, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,3)) call stats (ll, 0,n, zobs, o2,sigz, zcalc, 
     .  resz, rz, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

c      call stats (ll, 0,n, xobs, o2,sigx,xcalc, resx, rx, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)

c      call stats (ll, 0,n, yobs, o2,sigy, ycalc, resy, ry, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)
      
      endif

      enddo

      npar = 3
      if ( n.gt.0 ) call stats (ll, 0,n, yobs, o2,sigy,ycalc, resy, 
     .  ry, sumwt, tchi, datavar, ssfit, fname, dn, dw, npar, k28)

      enddo



c****************************************************************************
c* output residual summary by GPS file pole number
c****************************************************************************
      do j=1, num_gps_poles

       call i2c(j, 3, a3)
       ll='G'//a3
c      print *, j, ll

      call stats (ll, 0,n, xobs, o2,sigx,xcalc, resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, izero)

      do i=1, num_gps
      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)

      if (ngps_index(gps_index(i)).eq.j .and. useGPS(i)
     .   .and. ndx(i,1).eq.0 .and. gps_type(i).eq.1) then

      resx = 0.0d0
      resy = 0.0d0
      resz = 0.0d0
      rx = 0.0d0
      ry = 0.0d0
      rz = 0.0d0

      if(useENU(i,1)) call stats (ll, 0,n, xobs, o2,sigx,xcalc, 
     .  resx, rx, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,2)) call stats (ll, 0,n, yobs, o2,sigy,ycalc, 
     .  resy, ry, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

      if(useENU(i,3)) call stats (ll, 0,n, zobs, o2,sigz, zcalc, 
     .  resz, rz, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

c      call stats (ll, 0,n, xobs, o2,sigx,xcalc, resx, rx, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, ione)

c      call stats (ll, 0,n, yobs, o2,sigy,ycalc, resy, ry, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, ione)
      
c      call stats (ll, 0,n, zobs, o2,sigz, zcalc, resz, rz, sumwt, tchi, 
c     .  datavar, ssfit, fname, dn, dw, npar, 1)
     
      endif


      enddo

      call stats (ll, 0,n, yobs, o2,sigy,ycalc, resy, ry, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, k28)

      enddo

c     ik = kfclose (k28)

      endif
      
      if (myflag) ik = kfclose(k37b)


      return
      end 
c**********************************************************************
