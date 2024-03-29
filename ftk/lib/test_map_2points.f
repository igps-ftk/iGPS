CTITLE
      PROGRAM test_map_2points

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

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

c     <<VAR_DEC
      lon0=-112.499216520961
      lat0=36.988332245285
      lon1=-111.450486324367
      lat1=43.919148682581


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
      write(*,*) lon0,lat0
      write(*,*) lon1,lat1
      write(*,*) dist


      lon0=-116.4293777871720010
      lat0=34.5942818531580016
      lon1=-111.0637396021609931
      lat1=42.7731195546910001

      radian=0
      meter=1

      radian=0
      meter=0

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
      write(*,*) lon0,lat0
      write(*,*) lon1,lat1
      write(*,*) dist




      lon0=-112.7285492d0
      lat0=44.4855680d0
      lon1=-115.5874836d0
      lat1=36.3195915d0
      radian=0
      meter=1
      call  Map_2points(lon0, lat0, lon1, lat1, 
c    Inputs are in radian??      
     &     radian, 
     &     meter, 
     &     mile, 
     &     radius, 
     &     dist)
      write(*,*) lon0,lat0
      write(*,*) lon1,lat1
      write(*,*) dist

      STOP
      END
