c*********************************************************************
      subroutine rwparms (ipin)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

c* read or write parameter file
c* ipin controls function
c*  1 = read user defined file
c*  2 = write user defined file
c*  4 = write temporary file MMMM_pio.tmp
c*  5 = write final file with time tag appended to file name MMMM_pio.TTTTTTTT
      
      logical fexist

c      character time_string*12

      ip = ipin

        call existfile( parmfile, fexist, 0)
      if ( .not. fexist .and. ip.eq.1 ) then
        print *, 'PF: Parameter file not found ', parmfile
        parmsread = .false.
        return
      endif

      call dater(time_string)

c write file     
      if (ip.eq.5) then
        call fopen (kkf, 1, '_pio.'//time_string//'.'//runid//' ')
        call wparms(kkf)
             

      elseif (ip.eq.4) then
        print *, 'Writing pio tmp file at ', time_string
        call fopen (kkf, 1, '_pio.tmp ')
        call wparms(kkf)
        
      elseif (ip.eq.2) then
        kkf=kfopen(22)
        open (kkf, file = parmfile)
        call wparms(kkf)
      endif

c read file  
      if(ip.eq.1) then
        kkf=kfopen(22)
        open (kkf, file = parmfile)
        call rparms(kkf)
      endif

        return
        end

       
c************************************************************
      subroutine rparms (kkf)
       
c* read parameter file in new format from unit kkf

c* parmsread is a flag noting that this file has been read in
      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical write_input
      common /cm3/ write_input

      logical  noadjustread, fnilim
      character d*12, pline*1024
      character c5*5
      dimension vv(MAX_nodes), iv(10), v(20) 

      character*3 chin(16)
      character*34 Xf, Xfold

      data chin / 'GPS', 'BLK', 'STR', 'NOD', 'LIN',
     .            'TRA', 'TSx', 'TSv', 'TNO', 'STF',
     .            'INS', 'TLI', 'RAD', 'MAX', 'RLX', 'GVB' /
     
      nchin = 16
      xtol = 1.0d-2
      tol = 1.0d-20

      z = 0.0d0
      zero = 0.0d0

      noadjustread = .false.
      Xf = '(a5,i5,2f12.4,3e15.7,1x,a8,1x,a4,3i7)'

      call clearlog(parmpio, nchin)
      call clearlog(from_pio, MAX_f)

c******** read the file in new format

      rewind (kkf)

       print *, 'Reading parameter file: ', parmfile

       read (kkf, '(a1024)' ) pline
        if (write_input) write (*,*) pline
       read (pline, 1) c5, Mg1, Mp1, Mf1, Mz1, Mx1, Ms1, vver, d
       
c        print *, d, ' Version ', vver
  
       if ( vver .le. four ) Xf = Xfold
       
       if (Mg1.gt.MAX_gps_files) print *, Mg1,' exceeds MAX_gps_files', 
     .    MAX_gps_files
       if (Mp1.gt.MAX_poles) print *, Mp1, ' exceeds MAX_poles', 
     .    MAX_poles
       if (Mf1.gt.MAX_f) print *, Mf1, ' exceeds MAX_f', MAX_f
       if (Mz1.gt.MAX_z) print *, Mz1, ' exceeds MAX_z', MAX_z
       if (Mx1.gt.MAX_x) print *, Mx1, ' exceeds MAX_x', MAX_x
       if (Ms1.gt.MAX_strain) print *, Ms1, ' exceeds MAX_strain', 
     .    MAX_strain

       intype=-1

  25  read (kkf, '(a1024)', end=99) pline

      call clearint(iv, 10)
      call cleareal(vv, MAX_nodes)
      call cleareal(v,  20)

      if ( pline(1:3).eq.'END' ) then
        ik = kfclose(kkf)
        return
      endif

       intype = 0
       do kk= 1, nchin
         if ( pline(1:3).eq.chin(kk) ) intype = kk
       enddo
       
      if (fnilim(intype,1,nchin)) parmpio(intype) = .true.

c GPS
      if (intype.eq.1) then
         read (pline, '(a5,i3)') c5,k
         if(k .le. MAX_gps_files) then
           read (pline, 114) c5,n,(gps_pole(k,j), j=1,9)
         endif

c BLK
      elseif (intype.eq.2) then
        read (pline, '(a5,i3)') c5,k
        if(k .le. MAX_poles) then
           read (pline, 114) c5,n,(poles(k,j), j=1,9)
        endif

c STR
      elseif (intype.eq.3) then
         read (pline, '(a5,i3)') c5,k
         if(k .le. MAX_strain) then
           read (pline, 114) c5,n,(strain(k,j), j=1,9)
         endif

c NOD
      elseif (intype.eq.4) then
         read(pline, 124) c5, kf, iz, ix, ph 
         phi(ix,iz,kf) = ph
         from_pio(kf) = .true.

c LIN
      elseif (intype.eq.5) then
         read(pline, 125) c5, kf, ix, g, z1, z2
         f_parm(kf,ix,1)= g
         f_parm(kf,ix,2)= z1
         f_parm(kf,ix,3)= z2
         from_pio(kf) = .true.

c TNO transient node
      elseif (intype.eq.9) then
         read(pline, 124) c5,ntt, iz, ix, ph 
         tphi(ix,iz,ntt)=ph
         
c STF
      elseif (intype.eq.10) then
        if ( vver .le. 7.0) then
         read(pline,1265) c5,nt, (iv(j),j=1,4), ntau,
     .     dta, (atau(nt, k),k=1,ntau)
        elseif ( vver .ge. 7.5) then
         read(pline,1267) c5, nt, nt1, nt2, ntau,
     .     dta, (atau(nt, k),k=nt1,nt2)
        endif

         do k=ntau+1, MAX_tau
           atau(nt,k) = 0.0d0
         enddo


c INS insar
      elseif (intype.eq.11) then
         read(pline,113) c5,nt, (vv(k),k=1,9) 
c         do k =7, 10
c          insar_info(nt,k) = vv(k) 
c         enddo

c TLI transient line
      elseif (intype.eq.12) then
         if (vver.gt.6.9d0) then
           read(pline, 1251) c5,nt, nq, ix, (tf_parm(nt,ix,k),k=1,3)
c temp convert Gauss to boxcar
           if (nq.eq.4 .and. info_source(nt,2).eq.3) then
             tf_parm(nt,ix,2) = tf_parm(nt,ix,2) - tf_parm(nt,ix,3)
             tf_parm(nt,ix,3) = tf_parm(nt,ix,2) +2.0*tf_parm(nt,ix,3)
           endif
         else
           read(pline, 125)  c5,nt, ix, (tf_parm(nt,ix,k),k=1,3)
         endif

 1251 format(a5, 3i3, 6e15.7,3f9.3, i5)
 125  format(a5, 2i3, 6e15.7,3f9.3, i5)

c RAD
      elseif (intype.eq.13) then
          read(pline,1263) c5,nt, np, damp, (rpoly(nt, k),k=1,np)
          
c RLX
      elseif (intype.eq.15) then
          read(pline,1266) c5, n,  
     .       (rlxParms(n, k),k=1,7)
        
c GPS ref vels
      elseif (intype.eq.16) then
          read(pline,115) c5, n2,
     .       (ref_vel(n2, k),k=1,3)

       endif

c TRA         
      if (intype.eq.6) then
c        print *, pline
       if (vver.lt.8.0d0) then
         read(pline,126)  c5,nt, (iv(j),j=1,5), (v(j),j=1,20)
       else
         read(pline,1262) c5,nt, (iv(j),j=1,5), (v(j),j=1,20)
       endif
        do j = 1, 20
c         print *, nt, transient(nt,j),v(j)
          transient(nt,j) = v(j)
c         print *, transient(kt,j) 
        enddo
      endif

      goto 25


       
   99  ik = kfclose (kkf)
       parmsread = .true.

    1 format(a5,6i5,f6.2,1x,a12)   
c    3 format( 'Date: ', a12, ' Version:', f6.2) 
c   11 format(9e15.8)
c   12 format(100e15.8)
  113 format(a5, i5, 10f12.5)
c  123 format(3i5, 2f15.5)
  114 format(a5, i3, 9e15.7)
  115 format(a5, i3, 3e15.7)
  124 format(a5, 3i3, 2e15.7)
c  125 format(a5, 2i3, 6e15.7,3f9.3, i5)
c  126 format(a5, 6i3, 20e15.7, 40f8.1)
c  126 format(a5, 6i3, 20e15.7)
  126 format(a5, 6i3, 20e15.7)
 1262 format(a5, 6i3, 20e17.9)
c  135 format(a5, i5, 1x, a8, 1x, a4, 2f12.4, 12e15.7, 3i7)
c 1351 format(a5, i5, 1x, a8, 1x, a4, 2f12.4, 3i7, 18e15.7)
c 1262 format(a5, 4i3, 30e15.7)
 1263 format(a5, 2i3, 50f10.2)
 1265 format(a5, 6i3, 50e15.7)
 1266 format(a5,  i3, 8e15.7)
 1267 format(a5, 4i3, 31e15.7)


       return
       end
       
      
c*********************************************************************

      subroutine wparms (kkf)
       
c* write parameter file in new format to unit kkf

c* parmsread is a flag noting that this file has been read in
      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical noadjustread, flock, fnilim, allzero, wr
      character d*12, c80*80, c0*100
      dimension tmp(3)

      xtol = 1.0d-2
      k3=3

      z = 0.0d0
      noadjustread = .false.

      ip=ipin
      pio_version = 4.1d0
      
c-- changed 11/17/09 when order of items in transient() array changed       
      pio_version = 5.0d0

c-- changed 10/22/10 when each line given header      
      pio_version = 6.0d0

c-- changed 9/7/11 when semiannual term added      
      pio_version = 6.5d0

c-- changed 1/22/14 when transient type added to TLI: lines      
      pio_version = 7.0d0

c-- changed 1/26/15 when changed STF: lines to have manu more TAUs      
      pio_version = 7.5d0

c-- changed 7/19/15 increased significant digits in TRA      
      pio_version = 8.0d0

c-- date
      call dater(d)

c* write the file

      write (kkf, 1) 'MAX: ',MAX_gps_files, MAX_poles, MAX_f, MAX_z, 
     .    MAX_x, MAX_strain, pio_version, time_string

      c0="#This formatted file is generated by TDEFNODE version "//
     .       defnode_version
      write (kkf, '(a100)') adjustl(c0)

      c0="#The first 3 letters of the line identify the parameter type"
      write (kkf, '(a100)') adjustl(c0)

      c0="# Edit it with caution, keeping the format"
      write (kkf, '(a100)') adjustl(c0)

      c0="#MAX has dimensions of arrays and versions - don't change"
      write (kkf, '(a100)') adjustl(c0)

      c0="#Summary line - don't change"
      write (kkf, '(a100)') adjustl(c0)
      write (kkf, '(a100)') SUMline

c GPS poles
      c80 = adjustl("#GPS poles - Wx Wy Wz Sx Sy Sz Sxy Sxz Syz")
      write (kkf, '(a80)') c80
       do i=1, MAX_gps_files
         wr = .false.
         do j=1,3
           gps_pole(i,j) = fnzero(gps_pole(i,j))
           tmp(j) = gps_pole(i,j)
         enddo
         do j=1,MAX_gps_files
           if (ngps_index(j).eq.i) wr = .true.
         enddo
         if ( (.not.  allzero (tmp, k3)) .or. wr )
     .      write (kkf, 114) 'GPS: ',i,(gps_pole(i,j), j=1,9)
       enddo

c GPS ref vels
      c80 = adjustl("#GPS velocity bias - E N U ")
      write (kkf, '(a80)') c80
       do i=1, MAX_gps_files
         kindex = ngps_index(i)
         do j=1,3
           tmp(j) = ref_vel(kindex,j)
         enddo
         if ( .not.  allzero (tmp, k3) )
     .      write (kkf, 115) 'GVB: ',kindex,(ref_vel(kindex,j),j=1,3),
     .      gps_fname(i)
       enddo

c block poles
      c80 = adjustl("#Block poles - Wx Wy Wz Sx Sy Sz Sxy Sxz Syz")
      write (kkf, '(a80)') c80
        do i=1, MAX_poles
         wr = .false.
         do j=1,3
           poles(i,j) = fnzero(poles(i,j))
           tmp(j) = poles(i,j)
         enddo
         do j=1,nblocks
           if (npole_block(j).eq.i) wr = .true.
         enddo

         if ( (.not.  allzero (tmp, k3)) .or. wr )
     .    write (kkf, 114) 'BLK: ',i,(poles(i,j), j=1,9)
        enddo

c strain tensors     
      c80 = adjustl("#Strain rate tensors in (microstrain/yr)")
      write (kkf, '(a80)') c80
      c80 = adjustl("# Order: Exx Eyy Exy Sxx Sxy Syy")
      write (kkf, '(a80)') c80
       do i=1, MAX_strain
         wr = .false.
         do j=1,3
           strain(i,j) = fnzero(strain(i,j))
           tmp(j) = strain(i,j)
         enddo
         do j=1, MAX_strain
           if (nstrain_block(j).eq.i) wr = .true.
         enddo

         if ( (.not.  allzero (tmp, k3)) .or. wr )
     .    write (kkf, 114) 'STR: ', i,(strain(i,j), j=1,9)
        enddo

c total strain tensors by block     
      c80 = adjustl("#Strain rate tensors by block")
      write (kkf, '(a80)') c80
        do i = 1, nblocks
         indx = nstrain_block(i)
c         print *, i, indx
         if (indx.gt.0 ) then
         do j=1,3
           tot_strain(i,j) = fnzero(tot_strain(i,j) )
           tmp(j) = tot_strain(i,j)
c           print *, tmp(j)
         enddo
         if ( .not.  allzero (tmp, k3) )
     .    write (kkf, 114) 'TSR: ', indx,(tot_strain(i,j), j=1,3)
         endif
        enddo


        
      c80 = adjustl("#Interseismic profiles P1 P2 P3 S1 S2 S3 X0 Y0 Az")
      write (kkf, '(a80)') c80
      c80 = adjustl("# X0=lon Y0=lat Az=azimuth downdip")
      write (kkf, '(a80)') c80
        if(scec) call fopen (kkf2, 1, '_scec.tmp ')

       do kf=1, MAX_f
        if(fault_fit_type(kf).gt.1 .and. fault_fit_type(kf).le.6) then
         do ix=1, nxf(kf)
          g = f_parm(kf,ix,1)
          z1 = f_parm(kf,ix,2)
          z2 = f_parm(kf,ix,3)
          
          xx1 = xynode(1,ix,1,kf)
          yy1 = xynode(2,ix,1,kf)
          xx2 = xynode(1,ix,nzf(kf),kf)
          yy2 = xynode(2,ix,nzf(kf),kf)
          call delaz ( yy2, xx2, yy1, xx1, dd, az)
          write(kkf,1251) 'LIN: ', kf,ix,(f_parm(kf,ix,j),j=1,3),
     .     (f_parm_err(kf,ix,j),j=1,3), xx1, yy1, az
          if(scec) write(kkf2,'(2i4,9f9.3)') kf,ix,
     .     (f_parm(kf,ix,j),j=1,3),
     .     (f_parm_err(kf,ix,j),j=1,3), xx1, yy1, az
         enddo
        endif
       enddo
       if(scec) ik = kfclose (kkf2)
       
c TRA
      c80 = adjustl("#Transient parameters")
      write (kkf, '(a80)') c80
      c80 = adjustl("#  No Flt Typ N Tmp N Ln Lt Zh d1 am d2 to tc xr 
     . wr st dp rk az ")
      write (kkf, '(a80)') c80
        do nt=1, MAX_srce
         if(sflag(nt) )  
     .     write(kkf,1262) 'TRA: ', nt, (info_source(nt,j),j=1,5), 
     .                      (transient(nt,j),j=1,20)
        enddo

c STF         
      c80 = adjustl("#Transient STFs")
      write (kkf, '(a80)') c80
        do nt=1, MAX_srce
         kt = info_source(nt,4)
         ntau = info_source(nt,7)
          if(sflag(nt) .and. (kt.eq.2 .or. kt.eq.5)) then
c write out 30 per line
           nl = int((ntau-1)/30)+1
           do ik =1,nl
            k1 = 1 + 30*(ik-1)
            k2 = min(ntau,k1+29)
            write(kkf,1265) 'STF: ',nt, k1, k2, ntau,
     .               dtau(nt), (atau(nt, k),k=k1,k2)
           enddo
          endif
        enddo
 1265 format(a5, 4i3, 31e15.7)

      c80 = adjustl("#Transient RADIAL functions")
      write (kkf, '(a80)') c80
         do nt=1, MAX_srce
          kt = info_source(nt,2)
          np = info_source(nt,6)
          if(kt.eq.8) then
           write(kkf, 1263) 'RAD: ',nt, np, damp_poly(nt), 
     .          (rpoly(nt, k),k=1,np)
          endif
         enddo
          
         
      c80 = adjustl("#InSAR parameters")
      write (kkf, '(a80)') c80
        do nt=1, MAX_insar_files
          if (insar_flag(nt)) 
     .         write(kkf,113) 'INS: ', nt, (insar_info(nt,j),j=1,11) 
        enddo
         
      c80 = adjustl("#Relaxation parameters")
      write (kkf, '(a80)') c80
      do n = 1, MAX_mrlx_files
       if(mrlx_pts(n).gt.0 )  
     .  write(kkf,1266) 'RLX: ', n,  
     .       (rlxParms(n, k),k=1,7)
      enddo


      c80 = adjustl("#Transient downdip lines")
      write (kkf, '(a80)') c80
      c80 = adjustl("# T# KQ  X  G Z1 Z2 Sg S1 S2 X0 Y0 Az N")
      write (kkf, '(a80)') c80
      do nt=1, MAX_srce
       if ( sflag(nt) ) then
        kq = info_source(nt,2)
         if(kq.eq.3 .or. kq.eq.4) then
           kf = info_source(nt,1)
          do ix=1, nxf(kf)
           n = tnode_prof(nt,ix)
           g  = tf_parm(nt,ix,1)
           z1 = tf_parm(nt,ix,2)
           z2 = tf_parm(nt,ix,3)
           xx1 = xynode(1,ix,1,kf)
           yy1 = xynode(2,ix,1,kf)
           xx2 = xynode(1,ix,nzf(kf),kf)
           yy2 = xynode(2,ix,nzf(kf),kf)
           call delaz ( yy2, xx2, yy1, xx1, dd, az)
           if(n.gt.0) write(kkf,125) 'TLI: ', nt, kq, 
     .          ix,g,z1,z2,z,z,z,xx1,yy1,az,n
          enddo
         endif
        endif
       enddo


      c80 = adjustl("#Interseismic nodes ")
      write (kkf, '(a80)') c80
      c80 = adjustl("# F Z X Phi Phi_sig Lon Lat Depth ")
      write (kkf, '(a80)') c80
      do kf = 1, MAX_f
       if ( flock(kf) .and. fault_fit_type(kf) .le. 1 ) then
         do 10 iz=1, nzf(kf)
          do 10 ix=1, nxf(kf)
           ph  = phi(ix,iz,kf) 
           phe = min(phi_err(ix,iz,kf), 1.0d10)
c          if (ph .ne. z .or. phe .ne. z) 
            write(kkf,124) 'NOD: ',kf,iz,ix,ph,phe,
     .      xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf)
   10   continue
       endif
      enddo
      
      c80 = adjustl("#Transient nodes")
      write (kkf, '(a80)') c80
      c80 = adjustl("# T Z X Slip Slip_sig Lon Lat Depth")
      write (kkf, '(a80)') c80
      do nt=1, MAX_srce
       if (sflag(nt) .and. info_source(nt,2).eq.1) then
        kf = info_source(nt,1)
        if(fnilim(kf,1,MAX_f)) then
         do 22 iz=1, nzf(kf)
          do 22 ix=1, nxf(kf)
          ph = tphi(ix,iz,nt)
          phe = min(tphi_err(ix,iz,nt), 1.0d10)
          if (ph .ne. z .or. phe .ne. z) 
     .       write(kkf,124) 'TNO: ', nt,iz,ix,ph,phe,
     .      xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf)
   22    continue
        endif
       endif
      enddo
      
      write (kkf, '(a10)') 'END:      '
c      ik = kfclose (kkf3)
      
c* write out the model at end of file
      call writemodel (kkf)

      ik = kfclose(kkf)

    1 format(a5,6i5,f6.2,1x,a12)   
  113 format(a5, i5, 13f12.5)
  114 format(a5, i3, 9e15.7)
  115 format(a5, i3, 3e15.7, 1x,a4)
  124 format(a5, 3i3, 2e15.7, 2f10.4,f9.2)
 1251 format(a5, 2i3, 6e15.7,3f9.3, i5)
  125 format(a5, 3i3, 6e15.7,3f9.3, i5)
c  126 format(a5, 6i3, 20e15.7, 40f8.1)
 1262 format(a5, 6i3, 20e17.9 )
c  135 format(a5, i5, 1x, a8, 1x, a4, 2f12.4, 12e15.7, 3i7)
 1263 format(a5, 2i3, 50f10.2)
 1266 format(a5, i3, 8e15.7)

      return
      end      

 
      

