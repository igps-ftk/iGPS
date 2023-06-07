CTITLE
      program  sar_los_profile_fit
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c     

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      character*1023 file,ofile
      real*8 f_dist_max,f_dist_min

c     --OUTPUT--

c     --Local Parameters--
      real*8 DPI

      integer*4 NMAX,iargc
      Parameter (nmax=500)
c      Parameter (nmax=10)

      integer*4 is_overwrite
      real*8 dmax,dmin
      integer*4 ntheta,nfs,nfts,nld,nxm
      real*8 theta_max,theta_step,theta_min
      real*8 thetas(NMAX),fss(NMAX),ftss(NMAX),lds(NMAX),xms(NMAX)
      real*8 theta,fs,fts,ld,xm,iTheta
      real*8 fs_step,fts_step,ld_step

C     maximum length of profile (pixels)
      integer*4 LMAX,nsite
      parameter(LMAX=9000)
      real*8 dists(LMAX),p_dists(LMAX),vels(LMAX)
      real*8 lls(LMAX),lls_site(LMAX)


      integer*4 npt,i,j,k,l,m,n,pos,ii,jj
      integer*4 fid,ioerr
      integer*4 nl
      character*1023 lines(LMAX),line
      character*20 site_name
      real*8 p_long,p_lati,p_dist,v_along,ve_along,v_tang,
     +      ve_tang,lon,lat,f_dist,v_los,ve_los

      real*8 xms_all(NMAX,NMAX),xm1,xm2,xm_step,x_m,xm_best

      integer*4 nx,inds(5),np_x_m
      real*8 d0(LMAX),x0(LMAX),d1(LMAX),x1(LMAX),x2(LMAX),x2p(LMAX)
      real*8 d3(LMAX),x3(LMAX)
      real*8 rchi2,rchi2s(30,30,30,200,60),rchi2_min
      
      real*8 tmp,data(NMAX),avg,r_zero,lookE,lookN,lookU,incidence
      real*8 tmp2,azimuth
      character*1023 tmpstr1,tmpstr2,tmpstr3,tmpstr4,tmpstr
      integer*4 nblen

      integer*4 sec1,sec2,run_time_min,run_time_sec
      character*50  prog,ver,user,os,hostname,date_time
      
      integer*4 time  
 
c     <<VAR_DEC
      prog='sar_los_profile_fit'
      write(*,'(3a)') '-> ',prog(1:nblen(prog)),' ...'
      ver='20220119'
c      ioerr=system('whoami')
      call getlog(user)
c      write(*,*) 'user:',user
      sec1=time()
      call ctime(sec1,date_time)
c      date_time=ctime(sec1)
c      write(*,*) 'time:',date_time,i
c      ioerr=hostnm(hostname)
      call hostnm(hostname,ioerr)
c      write(*,*) 'host:',hostname
c      stop
      
      
c      write(*,*) 'iargc():',iargc()
      if (iargc().le.0) then
         write(*,'(a)') 'Usage:'
         write(*,'(a)') prog(1:nblen(prog))
         write(*,'(2a)') '|_Fault dislocation modeling using grid ',
     +      'search.'
         write(*,'(a)') '|+'
         write(*,'(a)') '  -profile_???_vel.psxy'
         write(*,'(a)') '|<'
         write(*,'(a)') '  -file=VEL_PROFILE_FILE.psxy'
         write(*,'(a)') '  [--ofile=VEL_PROFILE_FILE_mdl.txt]'
         write(*,'(a)') '    default: VEL_PROFILE_FILE_mdl.txt'
         write(*,'(a)') '  [--d1=DIST_MIN_KM]'
         write(*,'(a)') '    default: -200 km'
         write(*,'(a)') '  [--d2=DIST_MAX_KM]'
         write(*,'(a)') '    default: 200 km'
         write(*,'(a)') '  [--nr=NUMBER_OF_ROATION]'  
         write(*,'(a)') '    default: 41'       
         write(*,'(a)') '  [--nfs=NUMBER_OF_FAULT_SLIP]' 
         write(*,'(a)') '    default: 121'       
         write(*,'(a)') '  [--nfts=NUMBER_OF_FAULT_TRACE_SHIF]' 
         write(*,'(a)') '    default: 21'       
         write(*,'(a)') '  [--nld=NUMBER_OF_LOCKING_DEPTH]' 
         write(*,'(a)') '    default: 51'       
         write(*,'(a)') '  [--step_rot=STEP_OF_ROATION]' 
         write(*,'(a)') '    default: 0.1 rad'       
         write(*,'(a)') '  [--step_fs=STEP_OF_FAULT_SLIP]' 
         write(*,'(a)') '    default: 0.1 mm/yr'       
         write(*,'(a)') '  [--step_fts=STEP_OF_FAULT_TRACE_SHIF]' 
         write(*,'(a)') '    default: 5 km'       
         write(*,'(a)') '  [--step_ld=STEP_OF_LOCKING_DEPTH]' 
         write(*,'(a)') '    default: 1 km'       
         write(*,'(a)') '|>'
         write(*,'(a)') '  VEL_PROFILE_FILE_mdl.txt'
         write(*,'(a)') '|e.g.,'
         write(*,'(2x,6a)') prog(1:nblen(prog)),
     +     ' --file=profile_056_vel.psxy'
         write(*,'(4x,6a)') '--ofile=profile_056_vel_mdl.txt'
         write(*,'(4x,6a)') '--nfs=21 --step_fs=.3 --nld=21 --step_ld=2'
         write(*,'(4x,6a)') '--nr=9 --step_rot=.3'
         write(*,'(6a)') '(c)iGPS (https://github.com/igps-ftk/)'
         stop
      endif

      DPI=4d0*datan(1d0)
c      write(*,*) 'DPI:',DPI

      f_dist_max=200
      f_dist_min=-200

      ntheta=41
      nxm=11
      nfts=21

      nfs=121
c      nfs=31
      nfs=301

      nld=3
      nld=51
      
      
      theta_step=.1d0
      fs_step=.1d0
      fts_step=5
      ld_step=1


      do i=1,iargc()
         call getarg(i,tmpstr)
c         write(*,*) tmpstr(1:nblen(tmpstr))
         pos=index(tmpstr,'=')
c         write(*,*) 'pos:',pos
         if (pos.le.0) then
            write(*,*) 'invalid parameter!'
            stop
         endif

         if (tmpstr(1:pos).eq.'--file=') then
c            write(*,*) tmpstr(pos+1:)
c            read(tmpstr(pos+1:),*) file
           file=tmpstr(pos+1:)
         elseif (tmpstr(1:pos).eq.'--ofile=') then
c            read(tmpstr(pos+1:),*) ofile
           ofile=tmpstr(pos+1:)
         elseif (tmpstr(1:pos).eq.'--d1=') then
            read(tmpstr(pos+1:),*) f_dist_min
         elseif (tmpstr(1:pos).eq.'--d2=') then
            read(tmpstr(pos+1:),*) f_dist_max
         elseif (tmpstr(1:pos).eq.'--nr=') then
            read(tmpstr(pos+1:),*) ntheta
         elseif (tmpstr(1:pos).eq.'--nfs=') then
            read(tmpstr(pos+1:),*) nfs
         elseif (tmpstr(1:pos).eq.'--nfts=') then
            read(tmpstr(pos+1:),*) nfts
         elseif (tmpstr(1:pos).eq.'--nld=') then
            read(tmpstr(pos+1:),*) nld
         elseif (tmpstr(1:pos).eq.'--nxm=') then
            read(tmpstr(pos+1:),*) nxm
         elseif (tmpstr(1:pos).eq.'--step_rot=') then
            read(tmpstr(pos+1:),*) theta_step
         elseif (tmpstr(1:pos).eq.'--step_fs=') then
            read(tmpstr(pos+1:),*) fs_step
         elseif (tmpstr(1:pos).eq.'--step_fts=') then
            read(tmpstr(pos+1:),*) fts_step
         elseif (tmpstr(1:pos).eq.'--step_ld=') then
            read(tmpstr(pos+1:),*) ld_step
         else
            write(*,*) '[]ERROR: invlaid parameter(',
     +    tmpstr(1:pos),')!!'
            stop
         endif
      enddo

      if (nblen(file).lt.1) then
        write(*,*) '[]ERROR: no input profile file!!'
        stop
      endif
      if (nblen(ofile).lt.1) then
        call desuffix(file,tmpstr)
        write(ofile,'(5a)') tmpstr(1:nblen(tmpstr)),'_mdl.txt'
c        write(*,*) '[]ERROR: no output file!!'
c        stop
      endif

      write(*,*) 'file: ',file(1:nblen(file))
      write(*,*) 'ofile: ',ofile(1:nblen(ofile))
c      stop

c     grid search paramters

c     axis rotation angles
c     in degrees
c      theta_max=2d0
      theta_min=-1*ntheta/2*theta_step
c      ntheta=(theta_max*2/theta_step+1)
c      ntheta=1
    
      do i=1,ntheta
         thetas(i)=(i-1)*theta_step+theta_min
c         thetas(i)=1
c         write(*,*) 'thata:',i,thetas(i)
      enddo
      write(*,703) 'rotation angles:',thetas(1:ntheta)
703   format(a,/,10(1x,f7.2))     

c     far-field interseismic fault strike-slip rates
c      nfs=121
      do i=1,nfs
         fss(i)=(i-nfs/2-1)/1d0*fs_step
c         fss(i)=fss(i)*3
c         write(*,*) 'fss:',i,fss(i)
      enddo
      write(*,703) 'far-field fault slip rates:',fss(1:nfs)

c     fault trace shift
c      nfts=21
      do i=1,nfts
         ftss(i)=(i-nfts/2-1)*fts_step
c         write(*,*) 'ftss:',i,ftss(i)
      enddo
      write(*,703) 'fault trace shift:',ftss(1:nfts)

c     correction ofr means
c      nxm=17

c     locking depths
c      nld=50
      do i=1,nld
         lds(i)=(i-1)*ld_step
c         write(*,*) 'lds:',i,lds(i)
      enddo
      write(*,703) 'locking depth:',lds(1:nld)
c      stop

      write(*,*) '# theta,fts,fs,ld:',ntheta,nfts,nfs,nld

      call read_txt(file,lines,nl)
      nsite=0
      do i=1, nl
        line=lines(i)
c        write(*,'(a)') line(1:nblen(line))
        if (line(1:1).eq.' ') then
          nsite=nsite+1
          read(line,*) site_name,p_long,p_lati,p_dist,v_along,ve_along,
     +         v_tang,ve_tang,lon,lat,f_dist,v_los,ve_los
          dists(nsite)=f_dist
          vels(nsite)=v_los
          
        endif
      enddo    
      write(*,*) '# original profile points:', nsite
c$$$      do i=1,nsite
c$$$         write(*,*) dists(i),vels(i)
c$$$      enddo
      
      nx=0
      do i=1,nsite 
         if (dists(i).ge.f_dist_min.and.dists(i).le.f_dist_max) then
            nx=nx+1
            d0(nx)=dists(i)
            x0(nx)=vels(i)
         endif
      enddo
      write(*,*) '# used profile points (nx):',nx


      rchi2_min=9999
      inds(5)=0
c     i-loop for axis rotation angles
      do i=1,ntheta
         theta=thetas(i)*DPI/180d0
         write(*,'(i3,"/",i3," rotate profile by ",f9.3," degrees")')
     +   i,ntheta,thetas(i)
             
         call rot_xy(d0,x0,LMAX,nx,theta,d1,x1)

         xm1=0
         xm2=0
         do j=1,nx
            if (x1(j).lt.xm1) xm1=x1(j)
            if (x1(j).gt.xm2) xm2=x1(j)
         enddo
         xm_step=(xm2-xm1)/nxm
c         write(*,*) 'xm1,xm2,xm_step:',xm1,xm2,xm_step
         do j=1,nxm
            xms(j)=(j-1)*xm_step+xm1
c            write(*,*) 'xms:',j,xms(j)
         enddo

c     j-loop for fault trace shift
         do j=1,nfts
            fts=ftss(j)
c     shift the velocity to make the origin zero

            if (d1(1).gt.fts.or.d1(nx).lt.fts) then
c              write(*,*) '[]WARNING:not enough coverage beside fault!'
              goto 601
            endif
c            pos=9999
c            f_dist=9999
c            do ii=1,nx
cc              write(*,*) ii,d1(ii),abs(d1(pos)-fts),f_dist
c              if (abs(d1(ii)-fts).lt.f_dist) then
c                pos=ii
c                f_dist=abs(d1(ii)-fts)
cc                write(*,*) 'new pos:',ii
c              endif
c            enddo
c            write(*,*) pos,d1(pos),abs(d1(pos)-fts),fts,x1(pos)
cc            
c            x_m=0d0
c            np_x_m=0
c            do ii=1,nx/2
c              if ((pos-ii).gt.0.and.abs(d1(pos-ii)-d1(pos)).lt.3) then
c                np_x_m=np_x_m+1
c                x_m=x_m+x1(pos-ii)
c              endif
c              if ((pos+ii).lt.nx.and.abs(d1(pos+ii)-d1(pos)).lt.3) then
c                np_x_m=np_x_m+1
c                x_m=x_m+x1(pos+ii)
c              endif
c            enddo
c            if (np_x_m.eq.0) then
cc            use a single point (the nearest one) as the origin              
c              xm=x1(pos)
c            else
c              xm=x_m/np_x_m
c            endif
c            xms(i,j)=xm
c            write(*,*) 'velocity offset:',xm
c            stop
c     k-loop for velocity means
            do k=1,nxm
               xm=xms(k)
c               write(*,*) 'velocity offset:',xm               
               do ii=1,nx
                  x2(ii)=x1(ii)-xm
               enddo
c     l-loop for far-field slip rates
               do l=1,nfs
                  fs=fss(l)
c     m-loop for locking depths
                  do m=1,nld
                     ld=lds(m)
                     rchi2=0
                     do ii=1,nx
                        x2p(ii)=fs/DPI*atan((d1(ii)-fts)/ld)
c                        write(*,*) ii, x2(ii),x2p(ii)
                        rchi2=rchi2+(x2(ii)-x2p(ii))**2
                     enddo
                     rchi2=rchi2/nx
                     rchi2=sqrt(rchi2)
c                     write(*,*) i,j,k,l,m,rchi2
                     rchi2s(i,j,1,l,m)=rchi2
                     if (rchi2.lt.rchi2_min) then
                        rchi2_min=rchi2
                        inds(1)=i
                        inds(2)=j
c                        inds(3)=1
                        inds(3)=k
                        xms_all(i,j)=xm
                        xm_best=xm
                        inds(4)=l
                        inds(5)=m
                     endif
c     end-of-loop-m (ld)
                  enddo
c     end-of-loop-l(fs)
               enddo
cc     end-of-loop-k(xm)
            enddo
c     end-of-loop-j(fts)
601        continue    
         enddo
         
        sec2=time()
        run_time_min=(sec2-sec1)/60
        run_time_sec=mod( sec2-sec1, 60)
        write(*,'(3x,"program has been running for ",i5,
     +    " minutes and ",i3," seconds")') run_time_min,run_time_sec 
c     end-of-loop-i(theta)
      enddo
  
      if (inds(5).eq.0) then
        write(*,*) '[]WARNING:failed to get a solution!'
        stop
      endif

      write(*,*) 'minimum rchi2:',rchi2_min,' @',inds
      write(*,*) 'axis rotation angle (degress):', thetas(inds(1))
      write(*,*) 'fault trace shift (km):', ftss(inds(2))
      write(*,*) 'velocity offset (mm/yr):', xm_best
      write(*,*) 'far-field slip rate (mm/yr):', fss(inds(4))
      write(*,*) 'locking depth (km):', lds(inds(5))
      
c     best run result      
      theta=thetas(inds(1))
      fts=ftss(inds(2))
c      xm=xms(inds(1),inds(2))
      xm=xms_all(inds(1),inds(2))
      xm=xm_best
      fs=fss(inds(4))
      ld=lds(inds(5))
      
      
      itheta=theta*DPI/180d0
      call rot_xy(d0,x0,LMAX,nx,itheta,d1,x1)
      do i=1,nx
        x2(i)=x1(i)-xm
      enddo      
      do i=1,nx
        x2p(i)=fs/DPI*atan((d1(i)-fts)/ld)+xm
      enddo      
      call rot_xy(d1,x2p,LMAX,nx,-1*itheta,d3,x3)
      
      call getlun(fid)
      open(unit=fid,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
        write(*,*) '[]ERROR: cannot open output file!!'
        stop
      endif
      write(fid,'("* SRC: ",a)') file(1:nblen(file))
      write(fid,'("*PROG: ",a)') prog(1:nblen(prog))
      write(fid,'("* ver: ",a)') ver(1:nblen(ver))
      write(fid,'("*user: ",a)') user(1:nblen(user))
      write(fid,'("*run@: ",a)') hostname(1:nblen(hostname))
      write(fid,'("*  on: ",a)') date_time(1:nblen(date_time))
      write(fid,'(a)') '*model paramters:'
      write(fid,'("* far-field strike-slip rates:",f15.6)') fs
      write(fid,'("* locking depth:",f15.6)') ld
      write(fid,'("* fault trace shift (km):",f15.6)') fts
      write(fid,'("* angle of axis rotation (deg):",f15.6)') theta
      write(fid,'("* velocity offset:",f15.6)') xm
      write(fid,'(a)') '*'
      
      write(fid,701) 'dist_to_fault','prediction','horiz_pred'
701   format("*",a15,2(1x,a15))     
      do i=1,nx
        write(fid,702) d1(i),x3(i),x2p(i)
702   format(3(1x,f15.6))        
      enddo
      close(fid)
      
c      call getlun(fid)
c      open(unit=fid,file='out0',iostat=ioerr)
c      if (ioerr.ne.0) then
c        write(*,*) '[]ERROR: cannot open output file!!'
c        stop
c      endif
c      do i=1,nx
c        write(fid,*) d0(i),x0(i)
c      enddo
c      close(fid)
c
cc     rotated d0/x0      
c      call getlun(fid)
c      open(unit=fid,file='out1',iostat=ioerr)
c      if (ioerr.ne.0) then
c        write(*,*) '[]ERROR: cannot open output file!!'
c        stop
c      endif
c      do i=1,nx
c        write(fid,*) d1(i),x1(i)
c      enddo
c      close(fid)
c      
c      call getlun(fid)
c      open(unit=fid,file='out2',iostat=ioerr)
c      if (ioerr.ne.0) then
c        write(*,*) '[]ERROR: cannot open output file!!'
c        stop
c      endif
c      do i=1,nx
c        write(fid,*) d1(i),x2(i)
c      enddo
c      close(fid)
c            
c      call getlun(fid)
c      open(unit=fid,file='out3',iostat=ioerr)
c      if (ioerr.ne.0) then
c        write(*,*) '[]ERROR: cannot open output file!!'
c        stop
c      endif
c      do i=1,nx
c        write(fid,*) d3(i),x3(i)
c      enddo
c      close(fid)
      
      sec2=time()
      run_time_min=(sec2-sec1)/60
      run_time_sec=mod( sec2-sec1, 60)
      write(*,'(1x,"total running time: ",i5,
     +  " minutes and ",i3," seconds")') run_time_min,run_time_sec 
      write(*,'(3a)') '|< done for ',file(1:nblen(file)),'   :)'
      stop
      END

ccc      subroutine rot_xy(xs,ys,NMAX,np,theta,oxs,oys)
ccc      implicit none
ccc      integer*4 NMAX,np,i
ccc      real*8 xs(NMAX),ys(NMAX),oxs(NMAX),oys(NMAX),theta
ccc      do i=1,np
ccc         oxs(i)=xs(i)*cos(theta)-ys(i)*sin(theta)
ccc         oys(i)=xs(i)*sin(theta)+ys(i)*cos(theta)
ccc      enddo
ccc      
ccc      end 
