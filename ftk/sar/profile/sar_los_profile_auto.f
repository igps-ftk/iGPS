CTITLE
      program  sar_los_profile_auto
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
c     vfile - velocity
c     ffile - fault vector
c     pfile - profile vector
c     p_wid - width of profile
c
      character*1023 vfile,ffile,pfile,opath
      real*8 p_wid

c     --OUTPUT--

c     --Local Parameters--
      real*8 DPI

c      integer*4 NMAX
c      parameter (NMAX=100000)

c      maximum number of profiles
       integer*4 NMAX_P
       parameter(NMAX_P=500)

      integer*4 nv,nv_p,vInp(NMAX),vpCounts(NMAX),vInps(NMAX,NMAX_P)
      integer*4 pinds(NMAX),oinds(NMAX)
      character*1023 vlines(NMAX),vline,flines(NMAX),fline
      real*8 loni,lati,veli
      real*8 d_v2p,d_v2ps(NMAX),d_v2f,d_v2fs(NMAX)
      real*8 d_v2fs1(NMAX),d_v2fs2(NMAX),d_v2f_min,d_v2f_max
      real*8 lons(NMAX),lats(NMAX),vels(NMAX),plons(NMAX),plats(NMAX)
c      character*1023 headers(1000)


      integer*4 npt,i,j,k,l,m,n,pos,ii,jj
      integer*4 fid,ioerr
      character*1023 ofile

c     for fault/profile vectors
      character*1023 pnames(nmax_row),fnames(nmax_row),pname
      real*8 pxys(2,nmax_row),fxys(2,nmax_row),pxys2(4,nmax_row)
      real*8 plonmin,plonmax,platmin,platmax
c     intersecting point between profile and fault
      real*8 pf_xys(2,nmax_row)
      integer*4 pnpts(nmax_row),pn,fnpts(nmax_row),fn


      real*8 strikes(nmax_row),strike_avg,strike
      real*8 slope,slopes(nmax_row),slope_avg,slope_se,slopei
      real*8 strikei,strike_se,x1,y1,x2,y2,x3,y3,ox1,oy1,theta
      real*8 r8tmp,r8sum,r8sum2
      integer*4 j1,j2

      integer*4 npf,pf_nums(nmax_row)
      real*8 len_acc_i,xys_pf(2,nmax_row),oxys(4,nmax_row),len_seg_i
      real*8 oxy_pf(4),oxy(4)

c     for map_2points
      integer*4 is_radian,is_meter,is_mile
      real*8 r_earth,d_d,kpd,latmid

c     for point_perp_line
      real*8 a1(2),b1(2),c1(2),d1(2),c2(2),d2(2),i1(2),rate2

      character*1023 tmpstr1,tmpstr2,tmpstr3,tmpstr4,tmpstr
      integer*4 nblen,iargc
      character*1 path_sep

      integer*4 sec1,sec2,run_time_min,run_time_sec
      character*50  prog,ver,user,os,hostname,date_time

      integer*4 time

c     <<VAR_DEC
      prog='sar_los_profile_auto'
      write(*,'(3a)') '->',prog(1:nblen(prog)),' ...'
      ver='20220123'
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
         write(*,'(3a)') '[',prog(1:nblen(prog)),
     +     ']ERROR: no option given!!'
         write(*,'(a)') 'Usage:'
         write(*,'(a)') prog(1:nblen(prog))
         write(*,'(a)') '|_Extract velocities along profiles for fault.'
         write(*,'(a)') '|+'
         write(*,'(a)') '  -fault trace (GMT psxy)'
         write(*,'(a)') '  -profile lines (GMT psxy)'
         write(*,'(a)') '  -velocity map (ASCII XYZ)'
         write(*,'(a)') '|<'
         write(*,'(a)') '  --ffile=FAULT_FILE.psxy'
         write(*,'(a)') '  --vfile=VEL_FILE.xyz[e]'
         write(*,'(a)') '  --opath=OUT_PATH'
         write(*,'(a)') '  [--pfile=PROFILE_FILE]'
         write(*,'(a)') '   default: OUT_PATH/profiles_auto.psxy'
         write(*,'(a)') '  [--width=WIDTH_OF_PROFILE_KM]'
         write(*,'(a)') '    default: 10 km'
         write(*,'(a)') '|>'
         write(*,'(a)') ' OUT_PATH/profile_???_vel.psxy'
         write(*,'(a)') '|e.g.,'
         write(*,'(2x,6a)') prog(1:nblen(prog)),
     +     ' --ffile=fa_honghe.psxy'
         write(*,'(4x,6a)') '--vfile=062-d-m6-0497...-honghe/sbas.../',
     +     'vel_mask_ll_gnss3.xyze'
         write(*,'(4x,6a)') '--pfile=p.fa_honghe/profiles_auto.psxy'
         write(*,'(4x,6a)') '--opath=p.fa_honghe'
         write(*,'(6a)') '(c)iGPS (https://github.com/igps-ftk/)'
         stop
      endif

      p_wid=10d0
      DPI=4d0*datan(1d0)
c      write(*,*) 'DPI:',DPI
      is_radian=0
      is_meter=1
      is_mile=0
      r_earth=0



      do i=1,iargc()
         call getarg(i,tmpstr)
c         write(*,*) tmpstr(1:nblen(tmpstr))
         pos=index(tmpstr,'=')
c         write(*,*) 'pos:',pos
         if (pos.le.0) then
            write(*,*) 'invalid parameter!'
            stop
         endif

         if (tmpstr(1:pos).eq.'--vfile=') then
c            write(*,*) tmpstr(pos+1:)
c            read(tmpstr(pos+1:),*) vfile
           vfile=tmpstr(pos+1:)
         elseif (tmpstr(1:pos).eq.'--ffile=') then
c            read(tmpstr(pos+1:),*) ofile
           ffile=tmpstr(pos+1:)
         elseif (tmpstr(1:pos).eq.'--pfile=') then
c            read(tmpstr(pos+1:),*) ofile
           pfile=tmpstr(pos+1:)
         elseif (tmpstr(1:pos).eq.'--opath=') then
c            read(tmpstr(pos+1:),*) ofile
           opath=tmpstr(pos+1:)
         elseif (tmpstr(1:pos).eq.'--width=') then
            read(tmpstr(pos+1:),*) p_wid
         else
            write(*,*) '[',prog(1:nblen(prog)),
     +        ']ERROR: invlaid parameter(',
     +        tmpstr(1:pos),')!!'
            stop
         endif
      enddo

      p_wid=p_wid/2d0

      if (nblen(vfile).lt.1) then
        write(*,*) '[]ERROR: no input velocity file!!'
        stop
      endif
      if (nblen(ffile).lt.1) then
        write(*,*) '[]ERROR: no input fault file!!'
        stop
      endif
      if (nblen(opath).lt.1) then
        write(*,*) '[]ERROR: no output directory!!'
        stop
      endif
      if (nblen(pfile).lt.1) then
        pfile=opath(1:nblen(opath))//'/profiles_auto.psxy'
c        write(*,*) '[]ERROR: no input profile file!!'
c        stop
      endif


      write(*,*) 'vfile: ',vfile(1:nblen(vfile))
      write(*,*) 'ffile: ',ffile(1:nblen(ffile))
      write(*,*) 'pfile: ',pfile(1:nblen(pfile))
      write(*,*) 'opath: ',opath(1:nblen(opath))

c     test funcitons
c      call point_cross_line(a1,b1,c2,rate2,d2)
c      write(*,*) 'd2:',d2
c      stop

c     read velocity vfile (xyz format)
c      call read_cols(vfile,data,nrow,ncol,headers,nhead,cmt)
      call read_txt(vfile,vlines,nv)
c      do i=1,50
c        vline=vlines(i)
c        write(*,*) i,vline(1:nblen(vline))
c      enddo
c      stop


c     convert km to degree using the mean latitude
      r8sum=0
      do i=1,nv
        vline=vlines(i)
        read(vline,*) loni,lati,veli
        lons(i)=loni
        lats(i)=lati
        vels(i)=veli
        r8sum=r8sum+lati
      enddo
      latmid=r8sum/nv
      write(*,*) 'mean laitude of velocity grid:',latmid
      x1=90
      y1=latmid
      x2=91
      y2=latmid
      call map_2points(90d0,latmid,91d0,latmid,0,1,0,0,
     &  kpd)
c       convert meter to km
      kpd=kpd*1d-3
      write(*,*) 'k.p.d:',kpd,' at',latmid,'N'

c      x1=91.670426448900002d0
c      y1=31.080217392000002d0
c      x2=91.670426448900002d0
c      y2=31.011278907099999d0
c      call map_2points(x1,y1,x2,y2,0,1,0,0,
c     &  kpd)
cc       convert meter to km
c      kpd=kpd*1d-3
c      write(*,*) 'k.p.d:',kpd,' at',x1,'E'
c


c     read fault
      call read_psxy(ffile,fxys,fnpts,fnames,fn)
c      j1=1
c      do i=1,fn
c        write(*,*) 'line:',i
c        write(*,*) 'name:',fnames(i)
c        j2=j1+fnpts(i)-1
c        write(*,*) 'j1,j2:',j1,j2
c        do j=j1,j2
c          write(*,*) '    ',fxys(1,j),fxys(2,j)
c        enddo
c        j1=j1+fnpts(i)
c      enddo
      write(*,*) '#faults:',fn
      write(*,*) '#fault segments:',fnpts(1)-1


c     read profile
      call read_psxy(pfile,pxys,pnpts,pnames,pn)

      if (pn.gt.NMAX_P) then
        write(*,*) '[]ERROR:too many profiles(',pn,'>',NMAX_P,')!!'
        stop
      endif

      j1=1
      plonmin=999
      plonmax=-999
      platmin=999
      platmax=-999
      do i=1,pn
        vpCounts(i)=0
        write(*,*) 'line:',i
        pname=pnames(i)
        write(*,*) 'pname:',pname(1:nblen(pname))
        read(pname,*) j,loni,lati
        pf_xys(1,i)=loni
        pf_xys(2,i)=lati
        pf_nums(i)=j

        j2=j1+pnpts(i)-1
        write(*,*) 'j1,j2:',j1,j2
        if ((j2-j1).ne.1) then
          write(*,*) '[]ERROR: wrong number of vertex for profile)!!'
          write(*,*) '[]INFO: should be 2.'
          stop
        endif
        pxys2(1,i)=pxys(1,j1)
        pxys2(2,i)=pxys(2,j1)
        pxys2(3,i)=pxys(1,j2)
        pxys2(4,i)=pxys(2,j2)
c        write(*,*) 'points:',(pxys2(j,i),j=1,4)
        do j=j1,j2
          write(*,*) '    ',pxys(1,j),pxys(2,j)
          if (pxys(1,j).gt.plonmax) plonmax=pxys(1,j)
          if (pxys(1,j).lt.plonmin) plonmin=pxys(1,j)
          if (pxys(2,j).gt.platmax) platmax=pxys(2,j)
          if (pxys(2,j).lt.platmin) platmin=pxys(2,j)
        enddo
        j1=j1+pnpts(i)
      enddo
      write(*,*) '#profiles:',pn
      plonmin=plonmin-p_wid/kpd
      platmax=platmax+p_wid/kpd
c     geographic range of profile lines
      write(*,*) 'range of profile',plonmin,plonmax,platmin,platmax

c      stop


c     loop through each profile
        do j=1,pn
c          write(*,*) 'working on profile ',j
          a1(1)=pxys2(1,j)
          a1(2)=pxys2(2,j)
          b1(1)=pxys2(3,j)
          b1(2)=pxys2(4,j)

        if (a1(1).gt.b1(1)) then
           plonmin=b1(1)
           plonmax=a1(1)
        else
           plonmin=a1(1)
           plonmax=b1(1)
        endif
        if (a1(2).gt.b1(2)) then
           platmin=b1(2)
           platmax=a1(2)
        else
           platmin=a1(2)
           platmax=b1(2)
        endif
        write(*,*) 'range of profile:',j,plonmin,plonmax,platmin,platmax

            call map_2points(pf_xys(1,j),pf_xys(2,j),a1(1),a1(2),
     +        0,1,0,0,d_v2f)
c            write(*,*) 'd_v2f:',d_v2f,d1(1),pf_xys(1,j)
c            write(*,*) plons(i),plats(i)
            if (abs(d1(1)-pf_xys(1,j)).le.1d-6) then
              d_v2f=d_v2f*1d-3*( ABS(a1(2)-pf_xys(2,j)) /
     +          (a1(2)-pf_xys(2,j)) )
            else
              d_v2f=d_v2f*1d-3*( ABS(a1(1)-pf_xys(1,j)) /
     +          (a1(1)-pf_xys(1,j)) )
            endif
            d_v2f_min=d_v2f

            call map_2points(pf_xys(1,j),pf_xys(2,j),b1(1),b1(2),
     +        0,1,0,0,d_v2f)
c            write(*,*) 'd_v2f:',d_v2f,d1(1),pf_xys(1,j)
c            write(*,*) plons(i),plats(i)
            if (abs(d1(1)-pf_xys(1,j)).le.1d-6) then
              d_v2f=d_v2f*1d-3*( ABS(b1(2)-pf_xys(2,j)) /
     +          (b1(2)-pf_xys(2,j)) )
            else
              d_v2f=d_v2f*1d-3*( ABS(b1(1)-pf_xys(1,j)) /
     +          (b1(1)-pf_xys(1,j)) )
            endif
            d_v2f_max=d_v2f

            if (d_v2f_max.lt.d_v2f_min) then
              d_v2f=d_v2f_max
              d_v2f_max=d_v2f_min
              d_v2f_min=d_v2f
            endif
            write(*,*) 'd_v2f range:',d_v2f_min,d_v2f_max
c            goto 601

      do i=1,nv
        vInp(i)=0
      enddo

c     loop through each velocity point
      do i=1,nv
c        vline=vlines(i)
c        write(*,*) i,vline(1:nblen(vline))
c        read(vline,*) loni,lati,veli
        loni=lons(i)
        lati=lats(i)
        veli=vels(i)

        c1(1)=loni
        c1(2)=lati

c        if (loni.gt.plonmax.or.loni.lt.plonmin.or.
c     +    lati.gt.platmax.or.lati.lt.platmin) then
cc          write(*,*) 'velocity point outside profile range',loni,lati
c          goto 603
c        endif

cc     loop through each profile
c        do j=1,pn
cc          write(*,*) 'working on profile ',j
c          a1(1)=pxys2(1,j)
c          a1(2)=pxys2(2,j)
c          b1(1)=pxys2(3,j)
c          b1(2)=pxys2(4,j)

c     calcualte distance from velocity point to current profile

          call point_perp_line(a1,b1,c1,d1)
c          write(*,*) a1,b1,c1,d1

          call map_2points(c1(1),c1(2),d1(1),d1(2),0,1,0,0,
     +      d_v2p)
          d_v2p=d_v2p*1d-3*( ABS(c1(1)-d1(1)) /
     +      (c1(1)-d1(1)) )
          d_v2ps(i)=d_v2p
c          write(*,*) 'd_v2p,p_wid:',d_v2p,p_wid

            call map_2points(pf_xys(1,j),pf_xys(2,j),d1(1),d1(2),
     +        0,1,0,0,d_v2f)
c            write(*,*) 'd_v2f:',d_v2f,d1(1),pf_xys(1,j)
c            write(*,*) plons(i),plats(i)
            if (abs(d1(1)-pf_xys(1,j)).le.1d-6) then
              d_v2f=d_v2f*1d-3*( ABS(d1(2)-pf_xys(2,j)) /
     +          (d1(2)-pf_xys(2,j)) )
            else
              d_v2f=d_v2f*1d-3*( ABS(d1(1)-pf_xys(1,j)) /
     +          (d1(1)-pf_xys(1,j)) )
            endif
            d_v2fs(i)=d_v2f
c            write(*,*) 'd_v2f,d_v2p:',d_v2f,d_v2p


          if (abs(d_v2p).le.p_wid.and.d_v2f.ge.d_v2f_min.and.
     +      d_v2f.le.d_v2f_max) then
c            write(*,*) 'got one point for profile:',i,j
c            write(*,*) 'd_v2p,p_wid:',d_v2p,p_wid
c            write(*,*) a1,b1,c1,d1


            vInp(i)=j
c            vInps(i,j)=j
            plons(i)=d1(1)
            plats(i)=d1(2)
            vpCounts(j)=vpCounts(j)+1

c            rate2=(d1(2)-c1(2))/(d1(1)-c1(1))
c            call point_cross_line(a1,b1,c1,rate2,i1)
c            write(*,*) 'd1,i1:',d1,i1

c            write(*,*) 'd_v2f:',d_v2f
c            stop
c            goto 601
          endif

c     end-of-loop-i pixel
603     continue
        enddo
c        stop
         write(*,*) 'count:',j,vpCounts(j)
c601     continue
cd     end-of-loop-i
c      enddo

c      do i=1,pn
        if (vpCounts(j).eq.0) then
           write(*,*) 'no data for profile',j
           goto 601
        endif

c        sort results by distance to fault
        nv_p=0
        do k=1,nv
          if (vInp(k).eq.j) then
c          if (vInps(k,i).eq.i) then

            nv_p=nv_p+1
            d_v2fs1(nv_p)=d_v2fs(k)
            pinds(nv_p)=k
c            write(*,*) j,pinds(nv_p)
          endif
        enddo
c        stop
c        write(*,*) 'nmax,nv_p:',nmax,nv_p
        call sort_r8(d_v2fs1,nmax,nv_p,d_v2fs2,oinds)
c        write(*,*) 'nmax,nv_p:',nmax,nv_p
c        do j=1,nv_p
c          k=oinds(j)
c          write(*,*) d_v2fs1(oinds(j)),pinds(k),oinds(j),nv_p,k
c        enddo
c        write(*,*) 'inds(1):',pinds(1)
c        stop



        write(ofile,701) opath(1:nblen(opath)),path_sep(),pf_nums(j)
        write(*,*) ofile(1:nblen(ofile))
701     format(2a,"profile_",i3.3,"_vel.psxy")
        call getlun(fid)
        open(unit=fid,file=ofile,iostat=ioerr)
        if (ioerr.ne.0) then
          write(*,*) '[]ERROR: cannot open output vfile!!'
          stop
        endif

        write(fid,'("* SRC: ",a)') vfile(1:nblen(vfile))
        write(fid,'("*PROG: ",a)') prog(1:nblen(prog))
        write(fid,'("* ver: ",a)') ver(1:nblen(ver))
        write(fid,'("*user: ",a)') user(1:nblen(user))
        write(fid,'("*run@: ",a)') hostname(1:nblen(hostname))
        write(fid,'("*  on: ",a)') date_time(1:nblen(date_time))
c        write(fid,'(a)') '*model paramters:'
        write(fid,'(a)') '*'
        write(fid,'(a,2(1x,f15.6))') '* PSXY_PROFILE',
     +    pxys2(1,j),pxys2(2,j)
        write(fid,'(a,2(1x,f15.6))') '* PSXY_PROFILE',
     +    pxys2(3,j),pxys2(4,j)
        write(fid,'(a,2(1x,f15.6))') '* PSXY_FAULT_PROFILE_INTERSECT',
     +    pf_xys(1,j),pf_xys(2,j)
        do k=1,fnpts(1)
          write(fid,'(a,2(1x,f15.6))') '* PSXY_FAULT_TRACE',
     +      fxys(1,k),fxys(2,k)
        enddo


        write(fid,704) '* 01site          : name of location'
        write(fid,704) '* 02pLon          : longitude of point ',
     +      'projected onto the profile line (deg)'
        write(fid,704) '* 03pLat          : latitude of point ',
     +      'projected onto the profile line (deg)'
        write(fid,704) '* 04pDist         : distance from ',
     +      'location to profile (km)'
        write(fid,704) '* 05VNor          : velocity along the ',
     +      'profile (normal to the fault trace); ',
     +      'positive-north (mm/yr)'
        write(fid,704) '* 06VeNor         : velocity uncertainty',
     +      ' along the profile (mm/yr)'
        write(fid,704) '* 07VPar          : velocity tangent to ',
     +      'the profile (parallel to the fault trace); positive-',
     +      '90deg-clockwise from v_along direction (mm/yr)'
        write(fid,704) '* 08VePar         : velocity uncertainty ',
     +      'tangent to the profile (mm/yr)'
        write(fid,704) '* 09VUp           : vertical velocity; ',
     +      'positive-up (mm/yr)'
        write(fid,704) '* 10VeUp          : vertical velocity ',
     +      'uncertainty'
        write(fid,704) '* 11lon           : longitude of location',
     +      ' (deg)'
        write(fid,704) '* 12lat           : latitude of location',
     +      ' (deg)'
        write(fid,704) '* 13distFa        : distance from location',
     +      ' to fault; positive-east (km)'
        write(fid,704) '* 14VLOS          : InSAR LOS velocity ',
     +      '(mm/yr)'
        write(fid,704) '* 15VeLOS         : InSAR velocity ',
     +      'uncertainty (mm/yr)'
        write(fid,704) '* 16VE            : east velocity of ',
     +      'location; positive-east (mm/yr)'
        write(fid,704) '* 17VN            : north velocity of ',
     +      'location; positive-north (mm/yr)'
        write(fid,704) '* 18VEe           : east velocity ',
     +      'uncertainty (mm/yr)'
        write(fid,704) '* 19VNe           : north velocity ',
     +      'uncertainty (mm/yr)'
        write(fid,704) '* 20CEN           : correlation ',
     +      'coefficient between east and north'
        write(fid,704) '* 21CEU           : correlation ',
     +      'coefficient between east and up'
        write(fid,704) '* 22CNU           : correlation ',
     +      'coefficient between north and up'
704     format(9(a))

        write(fid,'(a)') '* Value NOT exist: -999.99'

c        write(fid,702) 'site','p_long','p_lati','p_dist','v_along',
c     +    've_along','v_tang','ve_tang','long','lati',
c     +    'dist_to_fault','v_los','ve_los'
c702     format("*",13(1x,a15))

c*  01_site      02_p_long     03_p_lati     04_p_dist     05_v_along   06_ve_along      07_v_tang    08_ve_tang        09_v_up      10_ve_up        11_long       12_lati 13_dist_to_fault        14_vlos   15_vlos_sig          16_ve         17_vn     18_ve_sig     19_vn_sig        20_Cen        21_Ceu        22_Cnu
        write(fid,705)
     +    '01site',
     +    '02pLon',
     +    '03pLat',
     +    '04pDist',
     +    '05VNor',
     +    '06VeNor',
     +    '07VPar',
     +    '08VePar',
     +    '09VUp',
     +    '10VeUp',
     +    '11lon',
     +    '12lat',
     +    '13distFa',
     +    '14VLOS',
     +    '15VeLOS',
     +    '16VE',
     +    '17VN',
     +    '18VEe',
     +    '19VNe',
     +    '20CEN',
     +    '21CEU',
     +    '22CNU'
705     format("*",a9,(1x,a8),(1x,a7),1x,a7,2(1x,a7),2(1x,a7),
     +    2(1x,a7),(1x,a8),(1x,a7),1x,a9, 2(1x,a7),6(1x,a7),1x,a6)

        do k=1,nv_p
          jj=pinds(oinds(k))
          write(fid,703) k,plons(jj),plats(jj),d_v2ps(jj),
     +      -999.99,0.,-999.99,0.,-999.99,0.,
     +      lons(jj),lats(jj),d_v2fs(jj),vels(jj),0.,
     +      -999.99,-999.99,0.,0., 0.,0.,0.
        enddo
703     format(1x,i9,(1x,f8.3),(1x,f7.3), 1x,f7.2, 
     +    2(1x,f7.2),2(1x,f7.2),2(1x,f7.2), 
     +    (1x,f8.3),(1x,f7.3), 1x,f9.3,2(1x,f7.2),
     +    6(1x,f7.2),1x,f6.3)

        close(fid)
c        stop

c       next profile
601     continue
      enddo


      sec2=time()
      run_time_min=(sec2-sec1)/60
      run_time_sec=mod( sec2-sec1, 60)
      write(*,'(1x,"total running time: ",i5,
     +  " minutes and ",i3," seconds")') run_time_min,run_time_sec
      write(*,'(3a)') '|< done for ',vfile(1:nblen(vfile)),'   :)'
      stop
      END

c*       site  -  location label
c*     p_long  -  longitude (degrees) of the shadow point
c*     p_lati  -  latitude of the shadow point
c*     p_dist  -  distance from current data point to the its shadow on the profile (i.e., the shadow point)
c*    v_along  -  velocity along the profile
c*   ve_along  -  uncertainity along the profile
c*     v_tang  -  velocity tangent to the profile
c*    ve_tang  -  uncertainity tangent to the profile
c*     v_up    -  vertical velocity
c*     ve_up   -  vertical velocity uncertainity
c*       long  -  longitude of data point
c*       lati  -  latitude of data point
c* dist_to_fa  -  distance from data point to the fault trace
c*      v_los  -  velocity along the los direction
c*     ve_los  -  velocity uncertainity along the los orientation
c*         ve  - east velocity of location; positive-east (mm/yr)
c*         vn  - north velocity of location; positive-north (mm/yr)
c*     ve_sig  - east velocity uncertainty (mm/yr)
c*     vn_sig  - north velocity uncertainty (mm/yr)
c*        Cen  - correlation coefficient between east and north
c*        Ceu  - correlation coefficient between east and up
c*        Cnu  - correlation coefficient between north and up
c*  01_site      02_p_long     03_p_lati     04_p_dist     05_v_along   06_ve_along      07_v_tang    08_ve_tang        09_v_up      10_ve_up        11_long       12_lati 13_dist_to_fault        14_vlos   15_vlos_sig          16_ve         17_vn     18_ve_sig     19_vn_sig        20_Cen        21_Ceu        22_Cnu
c      I087       79.70797      37.06209         22.78           0.00          0.00           0.00          0.00           0.80          1.00       79.96000      37.10000      -247.404148          0.000         0.000          0.000         0.000         0.000         0.000         0.000         0.000         0.000
c

