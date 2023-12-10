CTITLE
      PROGRAM rnx2rcvant

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 file
  

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen,iargc

c     --Local Parameters--
      integer*4 fid,fido,i,j,ioerr
      character*80 line,tmpstr,bufstr,ant,rcv,xyz,hen

c     <<VAR_DEC

      file='/home/tianyf/tmp/rcvant/jsgy0010.11o.Z'
      file='/home/tianyf/tmp/stinf/57252720.10o'
c      write(*,*) 'NOT implemented yet!'
      
      if (iargc().le.0) then
         write(*,'(a)') 'Usage: rnx2rcvant rinex_o_file'
         stop
      endif

      call getarg(1,file)

      call getlun(fid)
      open(unit=fid,file=file,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(3a)') 'ERROR: unable open file ',
     +        file(1:nblen(file)),'!'
         stop
      endif

 700  read(fid,'(a80)',err=901) tmpstr
      call strtrim(tmpstr,line)
      if (line.eq.'END OF HEADER') goto 901
c$$$     2.10           OBSERVATION DATA                        RINEX VERSION / TYPE
c$$$GPSBase 2.10 2270                       29-Sep-10 02:31:46  PGM / RUN BY / DATE
c$$$57256                                                       MARKER NAME
c$$$                                                            OBSERVER / AGENCY
c$$$22133036            TRIMBLE NETRS       Nav  1.13 / Boot  1 REC # / TYPE / VERS
c$$$                    TRM29659.00                             ANT # / TYPE
c$$$ -1906450.8066  5025977.0665  3421990.4985                  APPROX POSITION XYZ
c$$$        0.0000        0.0000        0.0000                  ANTENNA: DELTA H/E/N
c$$$     1     1                                                WAVELENGTH FACT L1/2
c$$$     8    C1    P2    L1    L2    S1    S2    D1    D2      # / TYPES OF OBSERV
c$$$     0.000                                                  INTERVAL
c$$$  2010     9    29     2    32    0.000000                  TIME OF FIRST OBS
c$$$  2010     9    29     2    34    0.000000                  TIME OF LAST OBS
c$$$                                                            END OF HEADER 
      line=tmpstr
      if (index(line,'REC # / TYPE / VERS').ne.0) then
c         write(*,'(a80)') line
         rcv=line
      endif
      if (index(line,'ANT # / TYPE').ne.0) then
         ant=line
      endif
      if (index(line,'APPROX POSITION XYZ').ne.0) then
         xyz=line
      endif
      if (index(line,'ANTENNA: DELTA H/E/N').ne.0) then
         hen=line
      endif
      goto 700

 901  continue
      close(fid)

      call getfilename(file,bufstr)
      write(*,'(a12,4(1x,a80))') bufstr(1:nblen(bufstr)),rcv,ant,xyz,hen
      STOP
      END
