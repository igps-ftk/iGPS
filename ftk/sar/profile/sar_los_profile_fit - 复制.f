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
      Parameter (nmax=200)
c      Parameter (nmax=10)

      integer*4 is_overwrite
      real*8 dmax,dmin
      integer*4 ntheta,nfs,nfts,nld,nxm
      real*8 theta_max,theta_step
      real*8 thetas(NMAX),fss(NMAX),ftss(NMAX),lds(NMAX),xms(NMAX)
      real*8 theta,fs,fts,ld,xm

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

      real*8 xms_all(300,300),xm1,xm2,xm_step

      integer*4 nx,inds(5)
      real*8 d0(LMAX),x0(LMAX),d1(LMAX),x1(LMAX),x2(LMAX),x2p(LMAX)
      real*8 rchi2,rchi2s(30,30,30,200,60),rchi2_min
      
      real*8 tmp,data(NMAX),avg,r_zero,lookE,lookN,lookU,incidence
      real*8 tmp2,azimuth
      character*1023 tmpstr1,tmpstr2,tmpstr3,tmpstr4,tmpstr
      integer*4 nblen


c     <<VAR_DEC

      write(*,*) 'iargc():',iargc()
      if (iargc().le.0) then
         write(*,*) 'Usage: sar_los_profile_fit --file=file_name'
         write(*,*) '         [--ofile=out_name]'
         stop
      endif

      DPI=4d0*datan(1d0)
      write(*,*) 'DPI:',DPI

      f_dist_max=200
      f_dist_min=-200

      ntheta=3
      nxm=3
      nfts=9

      nfs=121
      nfs=31

      nld=3
      nld=30


      do i=1,iargc()
         call getarg(i,tmpstr)
         write(*,*) tmpstr(1:nblen(tmpstr))
         pos=index(tmpstr,'=')
c         write(*,*) 'pos:',pos
         if (pos.le.0) then
            write(*,*) 'invalid parameter!'
            stop
         endif

         if (tmpstr(1:pos).eq.'--file=') then
c            write(*,*) tmpstr(pos+1:)
            read(tmpstr(pos+1:),*) file
         elseif (tmpstr(1:pos).eq.'--ofile=') then
            read(tmpstr(pos+1:),*) ofile
         else
            write(*,*) '[]ERROR: invlaid parameter(',
     +    tmpstr(1:pos),')!!'
            stop
         endif
      enddo


      write(*,*) 'file:',file(1:nblen(file))
      write(*,*) 'ofile:',ofile(1:nblen(ofile))

c     grid search paramters

c     axis rotation angles
c     in degrees
      theta_max=2d0
      theta_step=.25d0
      ntheta=(theta_max*2/theta_step+1)
      ntheta=1
    
      write(*,*) 'ntheta:',ntheta
      do i=1,ntheta
         thetas(i)=(i-1)*theta_step-theta_max
         thetas(i)=1
         write(*,*) 'thata:',i,thetas(i)
      enddo

c     far-field interseismic fault strike-slip rates
c      nfs=121
      do i=1,nfs
         fss(i)=(i-nfs/2-1)/2d0*.2d0
         fss(i)=fss(i)*3
         write(*,*) 'fss:',i,fss(i)
      enddo

c     fault trace shift
c      nfts=21
      do i=1,nfts
         ftss(i)=(i-nfts/2-1)*10
         write(*,*) 'ftss:',i,ftss(i)
      enddo

c     correction ofr means
c      nxm=17

c     locking depths
c      nld=50
      do i=1,nld
         lds(i)=i
         write(*,*) 'lds:',i,lds(i)
      enddo
c      stop


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
      write(*,*) 'nsite:', nsite
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
      write(*,*) 'nx:',nx


      rchi2_min=9999
c     i-loop for axis rotation angles
      do i=1,ntheta
         theta=thetas(i)*DPI/180d0
         call rot_xy(d0,x0,LMAX,nx,theta,d1,x1)

         xm1=0
         xm2=0
         do j=1,nx
            if (x1(j).lt.xm1) xm1=x1(j)
            if (x1(j).gt.xm2) xm2=x1(j)
         enddo
         write(*,*) 'xm1,xm2:',xm1,xm2
         xm_step=(xm2-xm1)/nxm
         do j=1,nxm
            xms(j)=(j-1)*xm_step+xm1
            write(*,*) 'xms:',j,xms(j)
         enddo

c     j-loop for fault trace shift
         do j=1,nfts
            fts=ftss(j)
c     k-loop for velocity means
            do k=1,nxm
               xm=xms(k)
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
                     rchi2s(i,j,k,l,m)=rchi2
                     if (rchi2.lt.rchi2_min) then
                        rchi2_min=rchi2
                        inds(1)=i
                        inds(2)=j
                        inds(3)=k
                        inds(4)=l
                        inds(5)=m
                     endif
c     end-of-loop-m (ld)
                  enddo
c     end-of-loop-l(fs)
               enddo
c     end-of-loop-k(xm)
            enddo
c     end-of-loop-j(fts)
         enddo
c     end-of-loop-i(theta)
      enddo

      write(*,*) 'minimum rchi2:',rchi2_min,' @',inds
      write(*,*) 'axis rotation angle (degress):', thetas(inds(1))
      write(*,*) 'fault trace shift (km):', ftss(inds(2))
      write(*,*) 'velocity offset (mm/yr):', xms(inds(3))
      write(*,*) 'far-field slip rate (mm/yr):', fss(inds(4))
      write(*,*) 'locking depth (km):', lds(inds(5))
      
c     best run result      
      theta=thetas(inds(1))
      fts=ftss(inds(2))
      xm=xms(inds(3))
      fs=fss(inds(4))
      ld=lds(inds(5))
      
      do i=1,nx
        x2p(i)=fs/DPI*atan((d1(i)-fts)/ld)-xm
      enddo
      
      call getlun(fid)
      open(unit=fid,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
        write(*,*) '[]ERROR: cannot open output file!!'
        stop
      endif
      do i=1,nx
        write(fid,*) d1(i),x2p(i)
      enddo
      close(fid)
      
      
      call getlun(fid)
      open(unit=fid,file='out0',iostat=ioerr)
      if (ioerr.ne.0) then
        write(*,*) '[]ERROR: cannot open output file!!'
        stop
      endif
      do i=1,nx
        write(fid,*) d1(i),x1(i)
      enddo
      close(fid)
      
      call getlun(fid)
      open(unit=fid,file='out1',iostat=ioerr)
      if (ioerr.ne.0) then
        write(*,*) '[]ERROR: cannot open output file!!'
        stop
      endif
      do i=1,nx
        write(fid,*) d1(i),x2(i)
      enddo
      close(fid)
      
      stop
      END

      subroutine rot_xy(xs,ys,NMAX,np,theta,oxs,oys)
      implicit none
      integer*4 NMAX,np,i
      real*8 xs(NMAX),ys(NMAX),oxs(NMAX),oys(NMAX),theta
      do i=1,np
         oxs(i)=xs(i)*cos(theta)-ys(i)*sin(theta)
         oys(i)=xs(i)*sin(theta)+ys(i)*cos(theta)
      enddo
      
      end 
