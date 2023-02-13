CTITLE
      PROGRAM map_distance

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      real*8 lon0,lat0,lon1,lat1,dist,radius
      integer radian,meter,mile
      character*1023 tmpstr

c     <<VAR_DEC
      
c      lon0=-112.499216520961
c      lat0=36.988332245285
c      lon1=-111.450486324367
c      lat1=43.919148682581

      call getarg(1,tmpstr)
      read(tmpstr,*) lon0
      call getarg(2,tmpstr)
      read(tmpstr,*) lat0
      call getarg(3,tmpstr)
      read(tmpstr,*) lon1
      call getarg(4,tmpstr)
      read(tmpstr,*) lat1


      radian=0
      meter=1
      mile=0
      radius=0
      call  Map_2points(lon0, lat0, lon1, lat1, 
c    Inputs are in radian??      
     &     radian, 
c     &       DPATH=dPath, 
     &     meter, 
     &     mile, 
c     &       NPATH=nPath, 
c     &       PARAMETERS=p, 

     &     radius, 
c     &       RHUMB=rhumb
     &     dist)
c      write(*,*) lon0,lat0
c      write(*,*) lon1,lat1
      write(*,*) dist
c
c
c      lon0=-116.4293777871720010
c      lat0=34.5942818531580016
c      lon1=-111.0637396021609931
c      lat1=42.7731195546910001
c
c      radian=0
c      meter=1
c
c      radian=0
c      meter=0
c
c      mile=0
c      radius=0
c      call  Map_2points(lon0, lat0, lon1, lat1, 
cc    Inputs are in radian??      
c     &     radian, 
cc     &       DPATH=dPath, 
c     &     meter, 
c     &     mile, 
cc     &       NPATH=nPath, 
cc     &       PARAMETERS=p, 
c
c     &     radius, 
cc     &       RHUMB=rhumb
c     &     dist)
c      write(*,*) lon0,lat0
c      write(*,*) lon1,lat1
c      write(*,*) dist
c
c
c
c
c      lon0=-112.7285492d0
c      lat0=44.4855680d0
c      lon1=-115.5874836d0
c      lat1=36.3195915d0
c      radian=0
c      meter=1
c      call  Map_2points(lon0, lat0, lon1, lat1, 
cc    Inputs are in radian??      
c     &     radian, 
c     &     meter, 
c     &     mile, 
c     &     radius, 
c     &     dist)
c      write(*,*) lon0,lat0
c      write(*,*) lon1,lat1
c      write(*,*) dist

      STOP
      END
