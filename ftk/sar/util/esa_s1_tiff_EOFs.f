CTITLE
      PROGRAM esa_s1_tiff_EOFs

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--
c     Given a expt directory:
c     /home/tianyf/gpse/rerun.cmonoc/solut/gmf.survey.mode/reg
c     and a STAT file
c     /home/tianyf/gpse/rerun.cmonoc/solut/gmf.survey.mode/reg/sh/STAT ,
c                   a001 a002 a003 
c     1998 240   78    0    0    1 
c     1998 241   81    1    1    0 
c     1998 242   81    0    1    1 
c     1998 243   81    1    1    1 
c     this program can run sh_gamit for each day, splitting sessions
c     automatically.
c
c     It requires ${root}/tables files.
c     Also reqiures:
c     + fiducial cGPS sites
c     + maximum number of sites in each sub-network
c

c     --EXAMPLE--

c     --MODIFICATIONS--
c     + On Tue Oct 27 10:00:34 CST 2015 by tianyf
c     Add the update station.info option.
c
c

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 tfile,efile

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 iargc,nblen

c     --Local Parameters--
      character*1023 tmpstr,tmpstrs(10),tfname,efname,tpath,epath
      character*3 satyp_tif,satyp_eof
      integer*4 npart
      integer*4 year,mon,day,hh,mm,ss,doyr,date(5)
      real*8 dyra1,dyra2,dyrb1,dyrb2,jd,sec,jd1a,jd1b,jd2a,jd2b
      integer*4 i,j,di,nsiteday,sind
      integer*4 fid,ioerr


c     <<VAR_DEC
      if (iargc().lt.2) then
         write(*,'(3a)') 'Usage: esa_s1_tiff_EOFs tiff_file EOF1 ',
     +        '[ EOF2 ...]'
         stop
      endif


c      write(*,'(a)') '[i]Starting ...________________'
      call getarg(1,tmpstr)
      tfname=tmpstr(1:nblen(tmpstr))
      call getfilename(tfname,tfile)
c      write(*,'(3a)') 'tiff_file: ',tfile(1:nblen(tfile))
      satyp_tif=tfile(1:3)
c     get time
      call strsplit(tfile,'-',npart,tmpstrs)
c      do i=1,npart
c         tmpstr=tmpstrs(i)
c         write(*,*) i,tmpstr(1:nblen(tmpstr))
c      enddo
c     starting time
      read(tmpstrs(5),600) year,mon,day,hh,mm,ss
c     20141027t235315
 600  format(i4,i2,i2,1x,i2,i2,i2)
      date(1)=year
      date(2)=mon
      date(3)=day
      date(4)=hh
      date(5)=mm
      sec=ss
      call ymdhms_to_jd(date,sec,jd)
      jd1a=jd
c      write(*,*) year,mon,day,hh,mm,ss,jd1a
c     ending time
      read(tmpstrs(6),600) year,mon,day,hh,mm,ss
      date(1)=year
      date(2)=mon
      date(3)=day
      date(4)=hh
      date(5)=mm
      sec=ss
      call ymdhms_to_jd(date,sec,jd)
      jd1b=jd
c      write(*,*) year,mon,day,hh,mm,ss,jd1b
      
      do i=2,iargc()
         call getarg(i,tmpstr)
         efname=tmpstr(1:nblen(tmpstr))
         call getfilename(efname,efile)
c     check satellite: A or B?
         satyp_eof=efile(1:3)
         call lowers(satyp_eof)
c         write(*,'(5a)') 'EOF_file: ',efile(1:nblen(efile))
         if (satyp_eof.ne.satyp_tif) then
c            write(*,*) 'satellite does not match'
            goto 700
         endif
c     
         call strsplit(efile,'_',npart,tmpstrs)
c$$$         do j=1,npart
c$$$            tmpstr=tmpstrs(j)
c$$$            write(*,*) j,tmpstr(1:nblen(tmpstr))
c$$$         enddo

c     start time
         read(tmpstrs(7),601) year,mon,day,hh,mm,ss
c     V20150106T225944
 601     format(1x,i4,i2,i2,1x,i2,i2,i2)
         date(1)=year
         date(2)=mon
         date(3)=day
         date(4)=hh
         date(5)=mm
         sec=ss
         call ymdhms_to_jd(date,sec,jd)
         jd2a=jd
c         write(*,*) year,mon,day,hh,mm,ss,jd2a
c         if (jd2a>jd1b) then
         if (jd2a>jd1a-0.01) then
c       earlier 15 minutes
c            write(*,*) 'time out of range 1'
            goto 700
         endif
c     ending time
         read(tmpstrs(8),600) year,mon,day,hh,mm,ss
         date(1)=year
         date(2)=mon
         date(3)=day
         date(4)=hh
         date(5)=mm
         sec=ss
         call ymdhms_to_jd(date,sec,jd)
         jd2b=jd
c         write(*,*) year,mon,day,hh,mm,ss,jd2b
c         if (jd2b<jd1a) then
c       
         if (jd2b<jd1b+.01) then
c            write(*,*) 'time out of range 2'
            goto 700
         endif

         write(*,'(1x,a,1x,a)') tfname(1:nblen(tfname)),
     +        efname(1:nblen(efname))

 700  continue
      enddo

c      stop
c      nproc=nmaxproc-nstable



      STOP
      END
