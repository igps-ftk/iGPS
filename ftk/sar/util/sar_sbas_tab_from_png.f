CTITLE
      PROGRAM sar_sbas_tab_from_png

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      integer*4 d1,d2,y1,y2
      character*1023 y

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 ny_max,nf_max
      parameter(ny_max=20)
      parameter(nf_max=20000)
      character*1023 path_png, files(nf_max), filter,tmpstr,file
      character*1023 strs(ny_max)
      integer*4 nf,ysIn(ny_max),nyIn,yi,yj
      integer*4 i,j,pos
      integer*4 nyPng,nyPngRem,ysPng(ny_max),ysPngRem(ny_max)
      integer*4 ty1,ty2,tys(2*nf_max),tys2(2*nf_max),indx(2*nf_max)
      integer*4 nPerc,nTot,nErr,np,n
      integer*4 year1,doy1,year2,doy2,isIn
      character*1023 file_intf,file_scene,ofile
      character*1023 lines_intf(nf_max),lines_scene(nf_max),line
      character*1023 olines_intf(nf_max),olines_scene(nf_max)
      integer*4 nl_intf,nl_scene,onl_intf,onl_scene,fido
      character*15 id
      character*7 id_l7
     

c     <<VAR_DEC

      y1=0
      d1=0
      y2=0
      d2=0
      y=''

      path_png='./intf_all_los_ll_png'
c$$$      path_png='/g7e/gsar/172-a-m4-0082_0087_0092_0097-jiali5_chayu3'//
c$$$     +     '/f123/intf_all_los_ll_png/'
      filter='*.png'
      call  ffind(path_png,files,filter,nf,0)
      write(*,'(a,i6)') '[]INFO:#png files:',nf
c      stop
      if (nf.le.0) then
         write(*,'(4a)') '[]WARNING: no png files found in ',
     +        path_png(1:nblen(path_png)),'!'
         stop
      endif

      do i=1,iargc()
         call getarg(i,tmpstr)
         write(*,*) tmpstr(1:nblen(tmpstr))
         pos=index(tmpstr,'=')
c         write(*,*) 'pos:',pos
         if (pos.le.0) then
            write(*,*) 'invalid parameter!'
            stop
         endif

         if (tmpstr(1:pos).eq.'--d1=') then
c            write(*,*) tmpstr(pos+1:)
            read(tmpstr(pos+1:),*) d1
         else if (tmpstr(1:pos).eq.'--d2=') then
            read(tmpstr(pos+1:),*) d2
         else if (tmpstr(1:pos).eq.'--y1=') then
            read(tmpstr(pos+1:),*) y1
            if (y1.lt.30) then
               y1=y1+2000
            endif
         else if (tmpstr(1:pos).eq.'--y2=') then
            read(tmpstr(pos+1:),*) y2
            if (y2.lt.30) then
               y2=y2+2000
            endif
         else if (tmpstr(1:pos).eq.'--y=') then
            y=tmpstr(pos+1:nblen(tmpstr))
            write(*,*) 'y:',y(1:nblen(y))
            call strsplit(y,',',nyIn,strs)
            write(*,*) 'nyIn:', nyIn
c            stop
            do j=1,nyIn
c               write(*,*) j,strs(j)
               read(strs(j),*) yj
               ysIn(j)=yj
               write(*,*) j, ysIn(j)
            enddo
         else
            write(*,*) '[]ERROR: invlaid parameter(',
     +           tmpstr(1:pos),')!!'
            stop
         endif
      enddo

      write(*,*) 'd1,d2,y1,y2:',d1,d2,y1,y2

c     read intf.tab1
c$$$      file_intf='/g7e/gsar/172-a-m4-0082_0087_0092_0097-jiali5'//
c$$$     +     '_chayu3/f123/intf.tab1'
      file_intf='intf.tab1'
      call read_txt(file_intf,lines_intf,nl_intf)
c$$$      do i=1,nl_intf
c$$$         line=lines_intf(i)
c$$$c         write(*,*) line(1:nblen(line))
c$$$      enddo

c     read scene.tab1
c$$$      file_scene='/g7e/gsar/172-a-m4-0082_0087_0092_0097-jiali5'//
c$$$     +     '_chayu3/f123/scene.tab1'
      file_scene='scene.tab1'
      call read_txt(file_scene,lines_scene,nl_scene)
c$$$      do i=1,nl_scene
c$$$         line=lines_scene(i)
c$$$        write(*,*) line(1:nblen(line))
c$$$      enddo
c$$$      stop

      write(*,*) '[]INFO: getting png years ...'
      nyPng=0
      do i=1,nf
         call getfilename(files(i),file)
c         write(*,*) file(1:nblen(file))
         read(file(1:4),*) ty1
         read(file(9:12),*) ty2
         tys(i)=ty1
         tys(i+nf)=ty2
c         write(*,*) ty1,ty2
      enddo

c$$$      do i=1,nf*2
c$$$         write(*,*) i,tys(i)
c$$$      enddo

      call sort_i4(tys,nf_max*2,nf*2,tys2)
c$$$      do i=1,nf*2
c$$$         write(*,*) i,tys2(i)
c$$$      enddo
c$$$      stop

      call uniq_i4(tys2,nf_max*2,nf*2,tys,nyPng)
      write(*,*) 'nyPng:', nypng
      do i=1,nyPng
         ysPng(i)=tys2(tys(i))
         write(*,*) i,ysPng(i)
      enddo

c     
      if (nyIn.gt.0) then
         nyPngRem=0
         do i=1,nyPng
            yi=ysPng(i)
            do j=1,nyIn
               yj=ysIn(j)
               if (yj.le.30) then
                  yj=yj+2000
               endif
               write(*,*) 'checking', yi,yj
               if (yi.eq.yj) then
                  nyPngRem=nyPngRem+1
                  ysPngRem(nyPngRem)=yi
                  write(*,*) 'got', yi,yj
                  goto 801
               endif
            enddo
 801        continue
         enddo
         
         if (nyPngRem.eq.0) then
            write(*,*) '[]WARNING: no data after excluding input years!'
            stop
         endif
         nyPng=nyPngRem
         do i=1,nyPng
            ysPng(i)=ysPngRem(i)
            write(*,*) i,ysPng(i)
         enddo
      endif


c     check -y1
      if (y1.ne.0) then
         nyPngRem=0
         do i=1,nyPng
            if (ysPng(i).ge.y1) then
               nyPngRem=nyPngRem+1
               ysPngRem(nyPngRem)=ysPng(i)
            endif
         enddo
         if (nyPngRem.eq.0) then
            write(*,*) '[]WARNING: no data after excluding input years!'
            stop
         endif
         nyPng=nyPngRem
         write(*,*) 'after --y1:'
         do i=1,nyPng
            ysPng(i)=ysPngRem(i)
            write(*,*) i,ysPng(i)
         enddo
      else
         y1=ysPng(1)
      endif


c     check -y2
      if (y2.ne.0) then
         nyPngRem=0
         do i=1,nyPng
            if (ysPng(i).le.y2) then
               nyPngRem=nyPngRem+1
               ysPngRem(nyPngRem)=ysPng(i)
            endif
         enddo
         if (nyPngRem.eq.0) then
            write(*,*) '[]WARNING: no data after excluding input years!'
            stop
         endif
         nyPng=nyPngRem
         write(*,*) 'after --y2:'
         do i=1,nyPng
            ysPng(i)=ysPngRem(i)
            write(*,*) i,ysPng(i)
         enddo
      else
         y2=ysPng(nyPng)
      endif
      
      write(*,*) 'y1:',y1,' y2:',y2


c     for progress bar
      nTot=nf
      nPerc=int(nTot/10)
      if (nPerc.eq.0) then
        nPerc=1
      endif
c      write(*,*) '#percentage:', nPerc


      n=0
      nErr=0
      onl_intf=0
      do i=1,nf
         if (mod(i,nPerc).eq.0) then
            write(*,*) '.',i*1d0/nTot*100,'%'
         endif
         call getfilename(files(i),file)
         read(file(1:4),*) year1
         read(file(5:7),*) doy1
         read(file(9:12),*) year2
         read(file(13:15),*) doy2
c         write(*,*) year1 ,doy1, year2, doy2
         
c     check year1
         isIn=0
         do j=1,nyPng
            if(year1.eq.ysPng(j)) then
               isIn=1
            endif
         enddo
c         write(*,*) 'isIn(year1):',isIn
         if (isIn.eq.0) then
            goto 802
         endif

c     check year2
         isIn=0
         do j=1,nyPng
            if(year2.eq.ysPng(j)) then
               isIn=1
            endif
         enddo
c         write(*,*) 'isIn(year2):',isIn
         if (isIn.eq.0) then
            goto 802
         endif   

c     check -d1
         if (d1.gt.0) then
            if (year1.eq.y1) then
               if (doy1.lt.d1) then
                  goto 802
               endif
            endif
            if (year2.eq.y1) then
               if (doy2.lt.d1) then
                  goto 802
               endif
            endif
         endif

c     check -d2
         if (d2.gt.0) then
            if (year1.eq.y2) then
               if (doy1.gt.d2) then
                  goto 802
               endif
            endif
            if (year2.eq.y2) then
               if (doy2.gt.d2) then
                  goto 802
               endif
            endif
         endif

c         write(*,*) year1 ,doy1, year2, doy2   

         id=file(1:15)
c         write(*,*) 'id:',id
         do j=1,nl_intf
            pos=index(lines_intf(j),id)
            if (pos.gt.0) then
               onl_intf=onl_intf+1
               olines_intf(onl_intf)=lines_intf(j)
               line=olines_intf(onl_intf)
c               write(*,*) 'out:',onl_intf,line(1:nblen(line))
            endif
         enddo

 802  continue
      enddo
      write(*,*) '.',1d0*100,'% (done!)'
      

c     output intf.tab file
      ofile='intf.tab'
      call getlun(fido)
      open(unit=fido,file=ofile)
      do i=1,onl_intf
         line=olines_intf(i)
         write(fido,'(1a)') line(1:nblen(line))
      enddo
      close(fido)

c     output scene.tab file
      ofile='scene.tab'
      onl_scene=0
      open(unit=fido,file=ofile)
      do i=1,nl_scene
         line=lines_scene(i)
         id_l7=line(1:7)
c         write(*,*) 'scene id:',id_l7
         do j=1, onl_intf
            pos=index(olines_intf(j),id_l7)
c            write(*,*) 'scene id:',id_l7
c            write(*,*) olines_intf(j)
            if (pos.gt.0) then    
c               write(*,*) 'pos:',pos
               onl_scene=onl_scene+1
               write(fido,'(1a)') line(1:nblen(line))
               goto 803
            endif
         enddo
 803     continue
      enddo
      close(fido)

      write(*,*) '#intf.tab:',onl_intf
      write(*,*) '#scene.tab:',onl_scene

      write(*,'(1a)') '[]INFO: normal end.'
      STOP
      END
