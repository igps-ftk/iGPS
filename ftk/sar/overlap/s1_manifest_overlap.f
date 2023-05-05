CTITLE
      PROGRAM s1_manifest_overlap

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--


c

c     --EXAMPLE--

c     --MODIFICATIONS--
c     +
c       format num: 811
c       jump num: 907
c
c     + On Tue Oct 27 10:00:34 CST 2015 by tianyf
c     .
c
c

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--

c     1 path : holding S1 *.manifest.safe files
      character*1023 path
c     2 target: the S1 scene to match
      character*1023 target
c     3 opath : output directory
      character*1023 opath
c     4 perc_min (optional) : minimum percentage of overlapping
c     default to be 0.8 (i.e., 80% overlapping between two polygons)
      real*8 perc_min

c     --OUTPUT--
c     output files in OPATH
      character*1023 ofile,ofile_kml,ofile_psxy

c     --EXTERNAL--
      integer*4 iargc,nblen
      character*1 path_sep
      real*8 polygon_overlap

c     --Local Parameters--

c     maximum number of files
      integer*4 NMax
      parameter(NMAX=100000)

c     for searching input files
      character*1023 files(NMAX),ptn,file
      integer*4 nf

c     for reading manifest.safe file
      character*1023 orbtype,orbtype_target
      integer*4 track,track_target,tracks(NMAX)
      real*8 xys(2,4),xys_all(2,4,NMAX),xys_target(2,4)

c     for calculating overlapping percentage
      real*8 percs(NMAX),perc,xstep,ystep
      integer*4 np_out

      character*1023 tmpstr,tmpstrs(10),tfname,efname,tpath,epath
      character*1023 tfile,efile,tmpstr1,tmpstr2,tmpstr3,tmpstr4
      character*3 satyp_tif,satyp_eof
      integer*4 npart
      integer*4 year,mon,day,hh,mm,ss,doyr,date(5)
      real*8 dyra1,dyra2,dyrb1,dyrb2,jd,sec,jd1a,jd1b,jd2a,jd2b
      integer*4 i,j,di,nsiteday,sind
      integer*4 fid,ioerr,iostat

c     for ffind
      integer*4 t_nf
      character*1023 t_files(1000), t_filter

c     for dates excluded
      character*1023 de_file,dates_excluded(100),buf
      integer*4 n_de
      character*50  prog,ver

c     <<VAR_DEC

      prog='s1_manifest_overlap'
      write(*,'(3a)') '-> ',prog(1:nblen(prog)),' ...'
      ver='20200302'

      if (iargc().lt.3) then
         write(*,'(3a)') '[',prog(1:nblen(prog)),
     +     ']ERROR: no option given!!'
         write(*,'(a)') 'Usage:'
         write(*,'(4x,3a)') prog(1:nblen(prog)),' path target',
     +        ' opath [perc_min]'
         write(*,'(a)') prog(1:nblen(prog))
         write(*,'(6a)') '  version ',ver(1:nblen(ver))
         write(*,'(6a)') '|_Match Sentinel-1 frames with the same ',
     +     'orbit/coverage.'
         write(*,'(a)') '|+'
         write(*,'(a)') '  -*.manifest.safe'
         write(*,'(a)') '|<'
         write(*,'(a)') '  SAFE_PATH'
         write(*,'(a)') '  TARGET_SAFE'
         write(*,'(a)') '  OUTPUT_PATH'
         write(*,'(a)') '  [PERCENTAGE_OF_OVERLAPPING]'
         write(*,'(a)') '    default: 0.8  (i.e., 80%)'
         write(*,'(a)') '|>'
         write(*,'(a)') '  OUTPUT_PATH/overlapping.TARGET_SAFE.txt'
         write(*,'(a)') '  OUTPUT_PATH/overlapping.TARGET_SAFE.kml'
         write(*,'(a)') '  OUTPUT_PATH/overlapping.TARGET_SAFE.psxy'
         write(*,'(a)') '|e.g.,'
         write(*,'(2x,6a)') prog(1:nblen(prog)),
     +     ' /sar/s1/manifest.safe/A012'
         write(*,'(4x,6a)') 'S1A_IW_SLC__1SDV_20200120T121407_',
     +     '20200120T121435_030884_038B59_F678.manifest.safe'
         write(*,'(4x,6a)') '/sar/proc/012-a-m7-0088...-tibet/ 0.56'
         write(*,'(6a)') '(c)iGPS (https://github.com/igps-ftk/)'
         stop
      endif

      perc_min=.8d0
      xstep=.1d0
      ystep=.1d0

c      write(*,'(a)') '[i]Starting ...________________'
      call getarg(1,tmpstr)
      path=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]path:',path(1:nblen(path))
      call getarg(2,tmpstr)
      target=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]target:',target(1:nblen(target))
      call getarg(3,tmpstr)
      opath=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]opath:',opath(1:nblen(opath))
      if (iargc().ge.4) then
         call getarg(4,tmpstr)
         read (tmpstr,*) perc_min
      endif
      write(*,'(a,f9.2)') '[]perc_min:',perc_min

c     check dates excluded
      de_file=opath(1:nblen(opath))//'/exclude_date.txt'
      de_file=opath(1:nblen(opath))//'/exclude_date_no_blank_line.txt'
      call getlun(fid)
      write(*,*) 'fid is ',fid, 'for ', de_file(1:nblen(de_file))
      dates_excluded(1)=''
      n_de=0
      open(unit=fid,file=de_file,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,'(3a)') '[]WARNING:cannot open file ',
     .        de_file(1:nblen(de_file)),' for reading!!'
      else
810     read(fid,'(a1023)', iostat=iostat, end=907) buf
c       the formatted reading cannot handle blank lines
        if (iostat.ne.0) then
          goto 810
        endif
c        write(*,*) 'line:',buf(1:nblen(buf))
        if (buf(1:1).ne.' ') then
c         write(*,*) 'comment:',buf(1:nblen(buf))
         goto 810
        endif
        n_de=n_de+1
        read(buf,*) tmpstr
        dates_excluded(n_de)=tmpstr(1:nblen(tmpstr))
        write(*,*) 'excluded date:',n_de,dates_excluded(n_de)
       goto 810
      endif
907   continue
      close(fid)
c      stop


c     get information in target manifest.safe file
      file=path(1:nblen(path))//'/'//target(1:nblen(target))
      t_filter='*'//target(1:nblen(target))//'*'
      write(*,*) 't_filter', t_filter(1:nblen(t_filter))
      call ffind (path,t_files,t_filter,t_nf,0)
      write(*,*) "t_nf",t_nf
      if (t_nf.lt.1) then
        write(*,*) '[]ERROR: target ('//target(1:nblen(target))//
     &    ') not found!!'
        stop
      endif
      file=t_files(1)
      write(*,*) 'file:',file
c      stop
      call read_esa_s1_manifest_safe(file,orbtype_target,track_target,
     .     xys)
      write(*,*) 'target orbtype is ',
     .     orbtype_target(1:nblen(orbtype_target))
      write(*,*) 'target track is ', track_target
      do j=1,4
c     swap the latitude & longitude
         xys_target(1,j)=xys(2,j)
         xys_target(2,j)=xys(1,j)
         write(*,*) 'target xys is ', xys_target(1,j),xys_target(2,j)
      enddo


c     searching *.manifest.safe files in input directory (path)
      ptn='*.manifest.safe'
      write(*,'(3a)') '[]ptn:',ptn(1:nblen(ptn))
      call ffind(path, files, ptn, nf, 1)
      if (nf.le.0) then
         write(*,'(3a)') '[]ERROR: no input metainfo files found in',
     .        path(1:nblen(path)),'!!'
         stop
      endif
c     read *.manifest safe files
      do i=1, nf
         file=files(i)
         write(*,'(3a)') '[]reading ',
     .        file(1:nblen(file)),' ...'
         call read_esa_s1_manifest_safe(file,orbtype,track,xys)
         write(*,*) 'orbtype is ', orbtype(1:nblen(orbtype))
         write(*,*) 'track is ', track
         tracks(i)=track
         do j=1,4
c            write(*,*) 'xys is ', xys(1,j),xys(2,j)
c     swap the longitude and latitude as the above
c     make sure that x is longitude and y is latitude
            xys_all(1,j,i)=xys(2,j)
            xys_all(2,j,i)=xys(1,j)
         enddo
      enddo


c     search matching scenes
      np_out=0
 901  do i=1,nf
         file=files(i)
         write(*,'(3a)') '[]testing ',
     .        file(1:nblen(file)),' ...'
c         write(*,*) 'track t vs. i',track_target,tracks(i)
         if (tracks(i).ne.track_target) then
            goto 902
         endif
         do j=1,4
            xys(1,j)=xys_all(1,j,i)
            xys(2,j)=xys_all(2,j,i)
         enddo
         perc=polygon_overlap(xys_target,xys,xstep,ystep)
         percs(i)=perc
         write(*,*) 'perc for ',i,' is ',perc
c         stop
         if (percs(i).ge.perc_min) then
            np_out=np_out+1
         endif

 902     continue
      enddo


      ofile=opath(1:nblen(opath))//path_sep()//'overlapping.'//
     .     target(1:nblen(target))//'.txt'
      write(*,*) '[]out:',ofile(1:nblen(ofile))
      call getlun(fid)
      open(unit=fid,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) '[]ERROR: cannot open output file for writting!!'
         stop
      endif

      write(fid,804) path(1:nblen(path))
 804  format("* source path:",1x,a)
      write(fid,805) target(1:nblen(target))
 805  format("* target:",1x,a)
      write(fid,806) perc_min
 806  format("* minimum percentage of overlapping:",1x,f20.5)
      write(fid,807) np_out
 807  format("* number of matched scenes:",1x,i10)
c     output overlapping scenes
      do i=1, nf
         if (percs(i).lt.perc_min) then
            goto 903
         endif
         file=files(i)
         call getfilename(file,tmpstr)
c        skip dates excluded
         do j=1, n_de
          buf=dates_excluded(j)
          if(index(tmpstr,'_'//buf(1:nblen(buf))).gt.0) then
            write(*,*) '[]INFO:skipping ',tmpstr(1:nblen(tmpstr))
         write(fid,811) tmpstr(1:nblen(tmpstr)),percs(i)
 811     format("x ",a,1x,f20.8)
            goto 903
          endif
         enddo
         write(fid,801) tmpstr(1:nblen(tmpstr)),percs(i)
 801     format(1x,a,1x,f20.8)

 903     continue
      enddo

c     output all scenes as comments (non-blank-first-column lines)
      do i=1, nf
         if (tracks(i).ne.track_target) then
            goto 905
         endif
         file=files(i)
         call getfilename(file,tmpstr)
         write(fid,802) tmpstr(1:nblen(tmpstr)),percs(i)
 802     format("*",a,1x,f20.8)
 905     continue
      enddo

      close(fid)


c     output KML file
      ofile=opath(1:nblen(opath))//path_sep()//'overlapping.'//
     .     target(1:nblen(target))//'.kml'
      write(*,*) '[]out KML:',ofile(1:nblen(ofile))
      open(unit=fid,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) '[]ERROR: cannot open output file for writting!!'
         stop
      endif
      write(fid,'(a)') '<?xml version="1.0" encoding="UTF-8"?>',
     .     '<kml xmlns="http://earth.google.com/kml/2.0">',
     .     '<Document>'


      write(fid,'(a)') '    <Style id="yellowLineGreenPoly">',
     .     '      <LineStyle>       ',
     .     '        <color>7f00ffff</color>',
     .     '        <width>2</width>     ',
     .     '      </LineStyle>     ',
     .     '      <PolyStyle>       ',
     .     '        <color>7f00ff00</color>',
     .     '      </PolyStyle>   ',
     .     '    </Style>'


      do i=1, nf
         if (percs(i).lt.perc_min) then
            goto 904
         endif
         file=files(i)
         call getfilename(file,tmpstr)


c        skip dates excluded
         do j=1, n_de
          buf=dates_excluded(j)
          if(index(tmpstr,'_'//buf(1:nblen(buf))).gt.0) then
c            write(*,*) '[]INFO:skipping ',tmpstr(1:nblen(tmpstr))
            goto 904
          endif
         enddo

         write(fid,'(a)') '    <Placemark>'
         write(fid,'(3a)') '      <name>',tmpstr(1:nblen(tmpstr)),
     .        '</name>'
         write(fid,'(3a)') '      <description>',
     .        tmpstr(1:nblen(tmpstr)),'</description>'
         write(fid,'(a)') '      <visibility>1</visibility>'
         write(fid,'(3a)') '      <styleUrl>#yellowLineGreenPoly',
     .        '</styleUrl>'
         write(fid,'(a)') '      <LineString>'
         write(fid,'(a)') '        <tessellate>1</tessellate>'
         write(fid,'(3a)') '        <altitudeMode>clampToGround',
     .        '</altitudeMode>'
         write(fid,'(a)') '        <coordinates>'
         tmpstr=''
         do j=1,4
            write(tmpstr1,803) xys_all(1,j,i)
            write(tmpstr2,803) xys_all(2,j,i)
 803        format(f20.015)
            call strtrim(tmpstr1,tmpstr3)
            call strtrim(tmpstr2,tmpstr4)
            tmpstr=tmpstr(1:nblen(tmpstr))//' '//
     .           tmpstr3(1:nblen(tmpstr3))//','//
     .           tmpstr4(1:nblen(tmpstr4))//',0'
         enddo

c     ;link the last vertex to the first one
      j=1
      write(tmpstr1,803) xys_all(1,j,i)
      write(tmpstr2,803) xys_all(2,j,i)
      call strtrim(tmpstr1,tmpstr3)
      call strtrim(tmpstr2,tmpstr4)
      tmpstr=tmpstr(1:nblen(tmpstr))//' '//
     .     tmpstr3(1:nblen(tmpstr3))//','//
     .     tmpstr4(1:nblen(tmpstr4))//',0'

      TMPSTR=TMPSTR(1:nblen(tmpstr))//'</coordinates>'
      write(fid,'(a)') TMPSTR(1:nblen(tmpstr))

      write(fid,'(a)') '      </LineString>'
      write(fid,'(a)') '    </Placemark>'

 904  continue
      enddo

      write(fid,'(a)')'   </Document>'
      write(fid,'(a)') '</kml>'

      close(FID)




      ofile=opath(1:nblen(opath))//path_sep()//'overlapping.'//
     .     target(1:nblen(target))//'.psxy'
      write(*,*) '[]out:',ofile(1:nblen(ofile))
      call getlun(fid)
      open(unit=fid,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) '[]ERROR: cannot open output file for writting!!'
         stop
      endif

c     output overlapping scenes
      do i=1, nf
         if (percs(i).lt.perc_min) then
            goto 906
         endif
         file=files(i)
         call getfilename(file,tmpstr)

c        skip dates excluded
         do j=1, n_de
          buf=dates_excluded(j)
          if(index(tmpstr,'_'//buf(1:nblen(buf))).gt.0) then
c            write(*,*) '[]INFO:skipping ',tmpstr(1:nblen(tmpstr))
            goto 906
          endif
         enddo

         write(fid,808) tmpstr(1:nblen(tmpstr))
 808     format(">",1x,a)
         do j=1,4
            write(fid,809) xys_all(1,j,i), xys_all(2,j,i)
 809        format(2(1x,f20.015))
         enddo

 906     continue
      enddo

      close(fid)




      STOP
      END
