CTITLE
      program  profile_vector_auto
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE '../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
c     file - fault vector
c     p_len - length of profile
c     p_wid - width of profile
c     auto_strike - mode of calculate fault strikes
c         1 - individual strike for each segment
c         2 - average strike for all fault segments
c         3 - get strike only using the first and last vertex
c
      character*1023 file,ofile
      real*8 p_len,p_wid
      integer*4 auto_strike

c     --OUTPUT--

c     --Local Parameters--
      real*8 DPI

c      integer*4 NMAX
c      Parameter (nmax=200)

      integer*4 npt,i,j,k,l,m,n,pos,ii,jj
      integer*4 fid,ioerr


      character*1023 pnames(nmax_row)
      real*8 xys(2,nmax_row),strikes(nmax_row),strike_avg,strike
      real*8 slope,slopes(nmax_row),slope_avg,slope_se,slopei
      real*8 strikei,strike_se,x1,y1,x2,y2,x3,y3,ox1,oy1,theta
      real*8 r8tmp,r8sum,r8sum2
      integer*4 npts(nmax_row)
      integer*4 j1,j2

      integer*4 npf
      real*8 len_acc_i,xys_pf(2,nmax_row),oxys(4,nmax_row),len_seg_i
      real*8 oxy_pf(4),oxy(4)

c     for map_2points
      integer*4 is_radian,is_meter,is_mile
      real*8 r_earth,d_d,kpd,latmid

      character*1023 tmpstr1,tmpstr2,tmpstr3,tmpstr4,tmpstr
      integer*4 nblen,iargc

      integer*4 sec1,sec2,run_time_min,run_time_sec
      character*50  prog,ver,user,os,hostname,date_time

      integer*4 time  
      
c     <<VAR_DEC
      prog='profile_vector_auto'
      write(*,'(3a)') '-> ',prog(1:nblen(prog)),' ...'
      ver='20220122'
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

      p_len=600d0
      p_wid=10d0
      auto_strike=1
      ofile='profiles_auto.psxy'

c      write(*,*) 'iargc():',iargc()
      if (iargc().le.0) then
         write(*,'(3a)') '[',prog(1:nblen(prog)),
     +     ']ERROR: no option given!!'
         write(*,'(a)') 'Usage:'
         write(*,'(a)') prog(1:nblen(prog))
         write(*,'(a)') '|_Create profile lines along fault trace.'
         write(*,'(a)') '|+'
         write(*,'(a)') '  -fault trace (GMT psxy)'
         write(*,'(a)') '|<'
         write(*,'(a)') '  --file=FAULT_FILE.psxy'
         write(*,'(a)') '  [--ofile=OUT_PROFILE_FILE.psxy]'
         write(*,'(a)') '    default: profiles_auto.psxy'
         write(*,'(a)') '  [--length=LENGTH_OF_PROFILE_KM]'
         write(*,'(a)') '    default: 600 km'
         write(*,'(a)') '  [--width=WIDTH_OF_PROFILE_KM]'
         write(*,'(a)') '    default: 10 km'
         write(*,'(a)') '  [--strike=1|2|3]'
         write(*,'(a)') '    1 - individual strike for each segment'
         write(*,'(a)') '    2 - average strike for all segments'
         write(*,'(a)') '    3 - only using  first and last vertices'
         write(*,'(a)') '|>'
         write(*,'(a)') '  OUT_PROFILE_FILE in GMT psxy foramt'
         write(*,'(a)') '|e.g.,'
         write(*,'(2x,6a)') prog(1:nblen(prog)),' --file=fa_atf.psxy'
         write(*,'(4x,6a)') '--ofile=profiles_auto.psxy'
         write(*,'(6a)') '(c)iGPS (https://github.com/igps-ftk/)'
         stop
      endif

      DPI=4d0*datan(1d0)
c      write(*,*) 'DPI:',DPI
      is_radian=0
      is_meter=1
      is_mile=0
      r_earth=0d0



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
         elseif (tmpstr(1:pos).eq.'--length=') then
            read(tmpstr(pos+1:),*) p_len
         elseif (tmpstr(1:pos).eq.'--width=') then
            read(tmpstr(pos+1:),*) p_wid
         elseif (tmpstr(1:pos).eq.'--strike=') then
            read(tmpstr(pos+1:),*) auto_strike
         else
            write(*,*) '[]ERROR: invlaid parameter(',
     +    tmpstr(1:pos),')!!'
            stop
         endif
      enddo
      
      if (nblen(file).lt.1) then
        write(*,*) '[]ERROR: no input file!!'
        stop
      endif
      if (nblen(ofile).lt.1) then
        write(*,*) '[]ERROR: no output file!!'
        stop
      endif


      write(*,*) 'file: ',file(1:nblen(file))
      write(*,*) 'ofile: ',ofile(1:nblen(ofile))


      write(*,*) 'read_psxy ...'
      call read_psxy(file,xys,npts,pnames,n)
      write(*,*) 'read_psxy done.'

      j1=1
      do i=1,n
        write(*,*) 'line:',i
        j2=j1+npts(i)-1
        write(*,*) 'j1,j2:',j1,j2
        do j=j1,j2
          write(*,*) '    ',xys(1,j),xys(2,j)
        enddo
        j1=j1+npts(i)
      enddo

c     convert km to degree using the mean latitude
      r8sum=0
      do i=1,npts(1)-1
        r8sum=r8sum+xys(2,i)
      enddo
      latmid=r8sum/npts(1)
      x1=90
      y1=latmid
      x2=91
      y2=latmid

c      call map_2points(90d0,latmid,91d0,latmid,0,1,0,0,
c     &  kpd)
      call map_2points(90d0,latmid,91d0,latmid,
     &  is_radian,is_meter,is_mile,r_earth,
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
c      stop


c     *only use the first polyline (if there are two or more)
      r8sum=0
      r8sum2=0
      do i=1,npts(1)-1
        x1=xys(1,i)
        y1=xys(2,i)
        x2=xys(1,i+1)
        y2=xys(2,i+1)
        r8tmp=(y2-y1)/(x2-x1)
        slope=atan(r8tmp)
        slopes(i)=slope
        write(*,*) 'slope:',i,slope*180/DPI
        r8sum=r8sum+slope
        if (x2.gt.x1) then
          strike=DPI/2-slope
        else
          strike=DPI*3d0/2-slope
        endif
        r8sum2=r8sum2+strike
        write(*,*) 'strike:',i,strike*180/DPI
        strikes(i)=strike
      enddo
      slope_avg=r8sum/(npts(1)-1)
      strike_avg=r8sum2/(npts(1)-1)
      write(*,*) '#pt:',npts(1)
c      stop

      if (auto_strike.eq.1) then
        write(*,*) 'use individual strike for each segment'
        do i=1,npts(1)-1
          write(*,*) 'strike of seg',i,' is:',
     +    strikes(i)*180/DPI
        enddo
      else if (auto_strike.eq.2) then
        write(*,*) 'mean strike (deg;clockwise from north):',
     +    strike_avg*180/DPI
      else if (auto_strike.eq.3) then
        x1=xys(1,1)
        y1=xys(2,1)
        x2=xys(1,npts(1))
        y2=xys(2,npts(1))
        r8tmp=(y2-y1)/(x2-x1)
        slope_se=atan(r8tmp)
        if (x2.gt.x1) then
          strike_se=DPI/2-slope_se
        else
          strike_se=DPI*3d0/2-slope_se
        endif
        write(*,*) 'start-end strike:',
     +    strike_se*180/DPI
c        write(*,*) x1,y1,x2,y2
      endif


      len_acc_i=0d0
      npf=0d0
      do i=1,npts(1)-1
        x1=xys(1,i)
        y1=xys(2,i)
        x2=xys(1,i+1)
        y2=xys(2,i+1)
        write(*,*) 'x1,y1,x2,y2:',x1,y1,x2,y2

        if (auto_strike.eq.1) then
c          r8tmp=(y2-y1)/(x2-x1)
c          slope=atan(r8tmp)
          slopei=slopes(i)
          strikei=strikes(i)
        else if (auto_strike.eq.2) then
          slopei=slope_avg
          strikei=strike_avg
        else if (auto_strike.eq.3) then
          slopei=slope_se
          strikei=strike_se
        else
          write(*,701) auto_strike
701         format("[]ERROR:wrong strike mode(",i1,")!!")
          stop
        endif
        write(*,*) 'auto_strike:',auto_strike
        write(*,*) 'slope:',slopei*180/DPI
        write(*,*) 'strike:',strikei*180/DPI

        call map_2points(x1,y1,x2,y2,is_radian,is_meter,is_mile,r_earth,
     &     len_seg_i)
c       convert meter to km
        len_seg_i=len_seg_i*1d-3

        write(*,*) 'length of segment',i,' is',len_seg_i
        write(*,*) 'total length',i,' is',len_seg_i+len_acc_i,p_wid
        if ((len_seg_i+len_acc_i).gt.p_wid) then
          write(*,*) ' new profile 1'
c          stop
          if (len_acc_i.eq.0) then
            x3=x1
            y3=y1
            len_acc_i=len_seg_i
            do while (len_acc_i.GT.p_wid)
              npf=npf+1

              call along_line_shift(x1,y1,x2,y2,x3,y3,p_wid,ox1,oy1)
              write(*,*) 'ox1,oy1:',ox1,oy1
              call perp_line_shift(x1,y1,x2,y2,ox1,oy1,
     +          p_len,slopei,kpd,oxy)
              xys_pf(1,npf)=ox1
              xys_pf(2,npf)=oy1
              oxys(1,npf)=oxy(1)
              oxys(2,npf)=oxy(2)
              oxys(3,npf)=oxy(3)
              oxys(4,npf)=oxy(4)
              len_acc_i=len_acc_i-p_wid
              IF (len_acc_i.GT.p_wid) THEN
                x3=ox1
                y3=oy1
                x1=x3
                y1=y3
              ELSE
                x3=x2
                y3=y2
              ENDIF
            enddo
          else
           npf=npf+1
           write(*,*) 'npf:', npf
c           stop
c              x1=646925.89
c              y1=3870924.1
c              x2=647432.89
c              y2=3875237.1
cc      switch xy1/xy2
cc              x1=647432.89
cc              y1=3875237.1
cc              x2=646925.89
cc              y2=3870924.1
cc      switch xy1/xy2
c              x1=646925.89
c              y1=3875237.1
c              x2=647432.89
c              y2=3870924.1
cc      switch xy1/xy2
cc              x1=647432.89
cc              y1=3870924.1
cc              x2=646925.89
cc              y2=3875237.1
c
c              x1=100.2
c              y1=32.1
c              x2=102.2
c              y2=35.9
c
c              x3=x1
c              y3=y1
c              p_wid=200
              write(*,*) x1,y1,x2,y2,x3,y3,p_wid
              call along_line_shift(x1,y1,x2,y2,x3,y3,
     +          p_wid-len_acc_i,ox1,oy1)
              write(*,*) 'ox1,oy1:',ox1,oy1
c              stop
c              x1=91.623438
c              y1=31.025848
c              x2=91.620648       
c              y2=30.990966
c              ox1=91.623410       
c              oy1=31.025499
c              p_len=150
c              slopei=1.4909823d0
c              kpd=97.163109d0
              call perp_line_shift(x1,y1,x2,y2,ox1,oy1,
     +          p_len,slopei,kpd,oxy)
c              write(*,*) 'oxy:',oxy
c              stop
              xys_pf(1,npf)=ox1
              xys_pf(2,npf)=oy1
              oxys(1,npf)=oxy(1)
              oxys(2,npf)=oxy(2)
              oxys(3,npf)=oxy(3)
              oxys(4,npf)=oxy(4)
              len_acc_i=len_acc_i+len_seg_i-p_wid
              IF (len_acc_i.GT.p_wid) THEN
                x3=ox1
                y3=oy1
                x1=x3
                y1=y3
              ELSE
                x3=x2
                y3=y2
              ENDIF

              do while (len_acc_i.GT.p_wid)
                npf=npf+1
                call along_line_shift(x1,y1,x2,y2,x3,y3,p_wid,ox1,oy1)
                write(*,*) 'ox1,oy1:',ox1,oy1
                xys_pf(1,npf)=ox1
                xys_pf(2,npf)=oy1
                call perp_line_shift(x1,y1,x2,y2,ox1,oy1,
     +            p_len,slopei,kpd,oxy)
                oxys(1,npf)=oxy(1)
                oxys(2,npf)=oxy(2)
                oxys(3,npf)=oxy(3)
                oxys(4,npf)=oxy(4)
                len_acc_i=len_acc_i-p_wid
                IF (len_acc_i.GT.p_wid) THEN
                  x3=ox1
                  y3=oy1
                  x1=x3
                  y1=y3
                ELSE
                  x3=x2
                  y3=y2
                ENDIF
              enddo

c              stop
          endif

        else
          write(*,*) 'old len_acc_i:',len_acc_i
          x3=x2
          y3=y2
          len_acc_i=len_acc_i+len_seg_i
          write(*,*) "new len_acc_i:",len_acc_i
        endif
601     continue
      enddo
c     end-of-loop-i

      write(*,*) 'npf:',npf
      do i=1,npf
        write(*,*) 'oxys:',i,(oxys(j,i),j=1,4)
        write(*,*) 'profile point',i,(xys_pf(j,i),j=1,2)
      enddo

c      stop
      
      write(*,*) 'output to:',ofile(1:nblen(ofile))
      call getlun(fid)
c      fid=97
      write(*,*) 'fid:',fid
      open(unit=fid,file=ofile,iostat=ioerr)
      write(*,*) 'ioerr:',ioerr
      if (ioerr.ne.0) then
        write(*,*) '[]ERROR: cannot open output file!!'
        stop
      endif
c      write(fid,'("* SRC: ",a)') file(1:nblen(file))
c      write(fid,'("*PROG: ",a)') prog(1:nblen(prog))
c      write(fid,'("* ver: ",a)') ver(1:nblen(ver))
c      write(fid,'("*user: ",a)') user(1:nblen(user))
c      write(fid,'("*run@: ",a)') hostname(1:nblen(hostname))
c      write(fid,'("*  on: ",a)') date_time(1:nblen(date_time))
c      write(fid,'(a)') '*model paramters:'
c      write(fid,'(a)') '*'
c
c      write(fid,702) 'dist_to_fault','prediction','horiz_pred'
c702   format("*",a15,2(1x,a15))
      write(*,*) 'npf:', npf
      do i=1,npf
        write(fid,703) i,(xys_pf(j,i),j=1,2)
        write(fid,704)(oxys(j,i),j=1,2)
        write(fid,704)(oxys(j,i),j=3,4)
      enddo
703   format(">",1x,i5,2(1x,f15.6)) 
704   format(3x,2(1x,f15.6)) 


      close(fid)
      write(*,*) 'finished writing file'


      sec2=time()
      run_time_min=(sec2-sec1)/60
      run_time_sec=mod( sec2-sec1, 60)
      write(*,'(1x,"total running time: ",i5,
     +  " minutes and ",i3," seconds")') run_time_min,run_time_sec
      write(*,'(3a)') '|< done for ',file(1:nblen(file)),'   :)'
      stop
      END

