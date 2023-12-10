CTITLE
      PROGRAM s1_overlap_check

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--


c

c     --EXAMPLE--

c     --MODIFICATIONS--
c     + On Mon Mar  2 15:52:36 CST 2020 by tianyf
c     .
c
c

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--

      character*1023 file
c     number of overlapping frames to merge; optional
      integer*4 nfrm

c     --OUTPUT--
      character*1023 ofile

c     --EXTERNAL--
      integer*4 iargc,nblen
      character*1 path_sep

c     --Local Parameters--

c     maximum number of files
      integer*4 NMax
      parameter(NMAX=10000)

c     for read input file
      character*1023 scenes(NMAX)
      integer*4 np

c     for time conversion
      real*8 dyr,dyrs(NMAX),sod,sods(NMAX),jds(NMAX)
      character*1023 tmpstr,tmpstrs(20),tfname,efname,tpath,epath
      integer*4 year,mon,day,hh,mm,ss,doyr,date(5),nstr
      real*8 dyra1,dyra2,dyrb1,dyrb2,jd,sec,jd1a,jd1b,jd2a,jd2b

c     for sorting times
      real*8 dyrs1(NMAX),sods1(NMAX),jds1(NMAX)
      integer*4 inds(NMAX)
      character*1023 scenes1(NMAX),scene

      integer*4 is_out(NMAX), is_uniq(NMAX),inds1(NMAX),nj
      real*8 tmp_r8

      character*1023 tfile,efile,tmpstr1,tmpstr2,tmpstr3,tmpstr4
      character*3 satyp_tif,satyp_eof
      integer*4 npart
      integer*4 i,j,di,nsiteday,sind
      integer*4 fid,ioerr
      character*50  prog,ver



c     <<VAR_DEC

      prog='s1_overlap_check'
      write(*,'(3a)') '-> ',prog(1:nblen(prog)),' ...'
      ver='20200502'

      if (iargc().lt.2) then
         write(*,'(3a)') '[',prog(1:nblen(prog)),
     +     ']ERROR: no option given!!'
         write(*,'(a)') 'Usage:'
         write(*,'(4x,3a)') prog(1:nblen(prog)),' INPUT_FILE',
     +        ' OUTPUT_FILE [NUMBER_OF_FRAMES_TO_MERGE]'
         write(*,'(a)') prog(1:nblen(prog))
         write(*,'(6a)') '  version ',ver(1:nblen(ver))
         write(*,'(6a)') '|_Check number of scenes of each day to ',
     +     'merge.'
         write(*,'(a)') '|+'
         write(*,'(a)') '  -output from s1_manifest_overlap'
         write(*,'(a)') '|<'
         write(*,'(a)') '  INPUT_FILE'
         write(*,'(a)') '  OUTPUT_FILE'
         write(*,'(a)') '  [NUMBER_OF_FRAMES_TO_MERGE]'
         write(*,'(a)') '    default: 3  (i.e., 3 frames per day)'
         write(*,'(a)') '|>'
         write(*,'(a)') '  OUTPUT_FILE.ok'
         write(*,'(a)') '|e.g.,'
         write(*,'(2x,6a)') prog(1:nblen(prog)),
     +     ' input.lst input.lst.ok'
         write(*,'(2x,6a)') prog(1:nblen(prog)),
     +     ' input.lst input.lst.ok 7'
         write(*,'(6a)') '(c)iGPS (https://github.com/igps-ftk/)'
         stop
      endif


c      write(*,'(a)') '[i]Starting ...________________'
      call getarg(1,tmpstr)
      file=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]in:',file(1:nblen(file))
      call getarg(2,tmpstr)
      ofile=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]out:',ofile(1:nblen(ofile))
c     optional input parameter (nfrm)
      nfrm=3
      if (iargc().ge.3) then
          call getarg(3,tmpstr)
          read(tmpstr,*) nfrm
      endif
      write(*,'(a,1x,I2)') '[]number of overlapping frames for merging:',nfrm

c     read input file
      call getlun(fid)
      open(unit=fid,file=file,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
          write(*,*) '[]ERROR: cannot open output file for writting!!'
          stop
      endif
      np=0
 901  read(fid,'(a1023)',end=902) tmpstr
      if (tmpstr(1:1).ne.' ') goto 901
      np=np+1
      scenes(np)=tmpstr
      goto 901
 902  continue
      close(fid)

c$$$      do i=1,np
c$$$         tmpstr=scenes(i)
c$$$c         write(*,'(a)') tmpstr(1:nblen(tmpstr))
c$$$      enddo


c     calculate decimal-year
c     S1A_IW_SLC__1SDV_20151028T124207_20151028T124234_008353_00BC9E_CA40.manifest.safe
      do i=1,np
         tmpstr=scenes(i)
         call strsplit(tmpstr,'_',nstr,tmpstrs)
         tmpstr1=tmpstrs(6)
c         write(*,*) tmpstr1(1:nblen(tmpstr1))
         read(tmpstr1,'(i4,i2,i2,1x,i2,i2,i2)'),year,mon,day,hh,mm,ss
c         write(*,*) year,mon,day,hh,mm,ss
         date(1)=year
         date(2)=mon
         date(3)=day
         date(4)=hh
         date(5)=mm
         call ymdhms_to_jd(date, ss*1d0, jd)
         call jd_to_decyrs(jd,dyr)
         dyrs(i)=dyr
c         jds(i)=jd

         date(4)=0d0
         date(5)=0d0
         call ymdhms_to_jd(date, 0d0, jd)
         jds(i)=jd

c         sods(i)=hh*3600d0+mm*60d0+ss
         sods(i)=hh*3600d0+mm*60d0+ss+(jds(i)-jds(1))*24d0*3600d0
c         write(*,*) year,mon,day,hh,mm,ss,dyr,jd,sods(i)
      enddo
c      stop

c     sort by time
c      call sort_r8(dyrs,NMAX,np,dyrs1,inds)
      call sort_r8(sods,NMAX,np,sods1,inds)
      do i=1,np
c         sods1(i)=sods(inds(i))
         dyrs1(i)=dyrs(inds(i))
         scenes1(i)=scenes(inds(i))
c         write(*,*) i,inds(i),dyrs1(i),sods1(i),scenes1(i)
      enddo
c      stop

c     get unique dates of acquisitions
      do i=1,np
         is_out(i)=0
         is_uniq(i)=0
         inds(i)=0
      enddo
      do i=1,np
         if (is_out(i).eq.1) goto 903
         nj=0
         do j=1,np
            tmp_r8=abs(dyrs1(i)-dyrs1(j))*365.25d0
            if (tmp_r8.lt.1.and.is_out(j).eq.0) then
               is_out(j)=1
               if (nj.eq.0) then
                  is_uniq(j)=1
                  nj=1
               endif
            endif
         enddo
c     next loop (#903)
 903     continue
      enddo
c$$$      do i=1,np
c$$$         scene=scenes1(i)
c$$$         write(*,*) i,is_uniq(i),is_out(i),dyrs1(i),
c$$$     .        scene(1:nblen(scene))
c$$$      enddo

c     remove adundant acquisitions
      do i=1,np
         is_out(i)=0
      enddo
      do i=1,np
         if (is_out(i).eq.1) goto 904

         nj=0
         do j=1,np
            tmp_r8=abs(dyrs1(i)-dyrs1(j))*365.25d0
            if (tmp_r8.lt.1.and.is_out(j).eq.0.and.
     .           abs(sods1(i)-sods1(j)).lt.6 ) then
               if (nj.eq.0) then
                  nj=1
               else
                  is_out(j)=1
               endif
            endif
         enddo
 904     continue
      enddo
c$$$      do i=1,np
c$$$         scene=scenes1(i)
c$$$         write(*,*) i,is_uniq(i),is_out(i),dyrs1(i),
c$$$     .        scene(1:nblen(scene))
c$$$      enddo
c$$$      stop

c     loop for each unique acquisiiton date
      do i=1,np
         if (is_uniq(i).ne.1) goto 905
         dyr=dyrs1(i)
         scene=scenes1(i)
         write(*,*) '[]searching scenes for',scene(1:nblen(scene))
         nj=0
         do j=1,np
            inds1(j)=0
         enddo
         do j=1,np
            tmp_r8=abs(dyrs1(i)-dyrs1(j))*365.25d0
            if (tmp_r8.lt.1.and.is_out(j).ne.1) then
               nj=nj+1
               inds1(nj)=j
            endif
         enddo
c
         if (nj.ne.nfrm) then
            write(*,*) '[]WARNING: no enough scenes (',nj,') for '//
     .           scene(1:nblen(scene))//'!'
            do j=1,nj
               is_out(inds1(j))=1
            enddo
            goto 905
         endif
 905     continue
      enddo

      do i=1,np
         scene=scenes1(i)
         write(*,*) i,is_uniq(i),is_out(i),dyrs1(i),
     .        scene(1:nblen(scene))
      enddo
c      stop


c     output results
c     open output file
      call getlun(fid)
      open(unit=fid,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) '[]ERROR: cannot open output file for writting!!'
         stop
      endif

c     output OK scenes
      do i=1, np
         if (is_out(i).eq.1) then
            goto 906
         endif
         scene=scenes1(i)
         write(fid,801) scene(1:nblen(scene))
 801     format(1x,a)
 802     format("*",a)

 906     continue
      enddo

c     outpu BAD scenes
      do i=1, np
         if (is_out(i).ne.1) then
            goto 907
         endif
         scene=scenes1(i)
         write(fid,802) scene(1:nblen(scene))
 907     continue
      enddo


      close(FID)



      STOP
      END

