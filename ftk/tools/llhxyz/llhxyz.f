Copyright (c) Massachusetts Institute of Technology,1986. All rights reserved. 
c Name:
c	llhxyz
c Purpose:
c	Convert latitude & longtitude, height to geocentric X, Y, Z; or, vice versa.
c Input:
c	alat	-	latitude
c	along	-	longitude
c	hght	-	height
c	or,
c	x	-	X
c	y	-	Y
c	z	-	Z
c	iflag	-	1=llh2xyz; 2=xyz2llh
c	semi	-	semi-major axis
c	finv	-	inverse of flattening
c	georad	-
C Source:
c      Subroutine GEOXYZ( semi,finv,alat,along,hght,geodrad,x,y,z,iflag )
C
C     iflag=1: Converts geodetic coordinates to Cartesian coordinates
c     iflag=2: Converst Cartesian coordinate to Geodetic coordinates

c        semi :  semi-major axis of reference ellipsoid
c        finv :  inverse flattening (1/f)
c        alat :  geodetic latitude in radians
c        along:  longitude in radians   
c        hght :  geodetic height
c        x,y,z:  Cartesian coordinates  
c        geodrad: geodetic height + inverse radius of (ellipsoid) curvature ('N' in Mueller),
c                 used to compute the Jacobian

c      program llhxyz semi,finv,alat,along,hght,geodrad,x,y,z,iflag 
      program llhxyz
c	Parameters: var1, var2, var3, iflag, finv, semi, georad
      IMPLICIT none

      integer iflag,iargc
      real*8 dlat,mlat,slat,dlon,mlon,slon
      real*8 semi,finv,alat,along,hght,x,y,z,geodrad,twopi,f,e2
     .       ,sinlat,sinlon,coslat,coslon,curvn,sqr,alat0,cutoff
      character*100 buf

c     if no command-line parameters, then display help info
      if (iargc().lt.4) then
         print*,"Usage: llhxyz v1 v2 v3 [v4 v5 v6] iflag"
         print*,"  e.g.:"
         print*,"       llhxyz latitude longitude height 1"
         print*,"       llhxyz dlat mlat slat dlon mlon slon h 1"
         print*,"       llhxyz x y z  2"
         print*,"This program uses WGS84 datum as default."
         print*,"Current version only support WGS84 datum."
         print*,"(C)Copyright by GAMIT/GLOBK v10.21 2006"
         print*,"Revised by Y.F. Tian @BJ.China Aug 2006"
         stop
      end if

      if (iargc().eq.5) then
         call getarg(5,buf)
         read(buf,*) finv
      else if (iargc().eq.9) then
         call getarg(9,buf)
         read(buf,*) finv
      else
c     use default:
c     WGS84
         finv=298.257223563D0
c     GRS80
c         finv=298.257222101D0
      end if

      if (iargc().eq.6) then
         call getarg(5,buf)
         read(buf,*) finv
         call getarg(6,buf)
         read(buf,*) semi
      else if (iargc().eq.10) then
         call getarg(9,buf)
         read(buf,*) finv
         call getarg(10,buf)
         read(buf,*) semi
      else
c     pre-defined:
c     for WGS84 & GRS80
         semi=6378137.D0
      end if

c     constants
c     2Pi in radian angle, used for converting between radian and degree
      TWOPI=8.D0*DATAN(1.D0)

c     flattening:
c       f=(a-b)/a
      F=1.D0/FINV
c     eccentric:
c       e**2=(a**2-b**2)/a**2
c     or,
c       e=2f-f*f
      E2=2.D0*F-F*F


c     get the conversion type (iflag):
c       1: llh-> xyz
c       2: xyz-> llh
c     if input is in degree,minute,second
      if (iargc().ge.8) then
         call getarg(8,buf)
      else
c     input is in decimal degrees
         call getarg(4,buf)
      end if
      read(buf,*) iflag
      IF(IFLAG.EQ.2) GO TO 10

c     llh->xyz
      if (iargc().ge.8) then
c        get d,m,s
         call getarg(1,buf)
         read(buf,*) dlat
         call getarg(2,buf)
         read(buf,*) mlat
         call getarg(3,buf)
         read(buf,*) slat
         call getarg(4,buf)
         read(buf,*) dlon
         call getarg(5,buf)
         read(buf,*) mlon
         call getarg(6,buf)
         read(buf,*) slon
         call getarg(7,buf)
         read(buf,*) hght
         alat=dlat+mlat/60.D0+slat/3600.D0
         along=dlon+mlon/60.D0+slon/3600.D0
      else
c        get d
         call getarg(1,buf)
         read(buf,*) alat
         call getarg(2,buf)
         read(buf,*) along
         call getarg(3,buf)
         read(buf,*) hght
      end if
c     degree->radian
c     **NOTE: sin,cos,... use radian angle as input.
      alat=alat*twopi/360.D0
      along=along*twopi/360.D0

      SINLAT=DSIN(ALAT)
      COSLAT=DCOS(ALAT)
      SINLON=DSIN(ALONG)
      COSLON=DCOS(ALONG)
      CURVN=SEMI/(DSQRT(1.D0-E2*SINLAT*SINLAT))
C
      X=(CURVN+HGHT)*COSLAT*COSLON
      Y=(CURVN+HGHT)*COSLAT*SINLON
      Z=(CURVN*(1.D0-E2)+HGHT)*SINLAT
      print 111,x,y,z
 111  format(3F20.8)
C
C  FOR JACOBIAN (GEODETIC TO XYZ)
      geodrad=CURVN+HGHT
C
      GO TO 20
C
c     xyz->llh
   10 CONTINUE
c      print *,'rad-deg:'
      call getarg(1,buf)
      read(buf,*) x
      call getarg(2,buf)
      read(buf,*) y
      call getarg(3,buf)
      read(buf,*) z
c      write(*,*) 'x,y,z:',x,y,z

      ALONG=DATAN2(Y,X)
      IF(ALONG.LT.0.D0) ALONG=ALONG+TWOPI
C     STARTING VALUE FOR LATITUDE ITERATION
      SQR=DSQRT(X*X+Y*Y)
C Changed by D.D.Dong:  Old:  ALAT0=DATAN2(1.D0-E2),Z/SQR)
      ALAT0=DATAN2(Z/SQR,1.D0-E2)
      ALAT=ALAT0
  40  SINLAT=DSIN(ALAT)
      CURVN=SEMI/(DSQRT(1.D0-E2*SINLAT*SINLAT)) 
      ALAT=DATAN2((Z+E2*CURVN*SINLAT),SQR)
c      write(*,*) 'along:',along,' alot:',alat
C     ITERATE TO THE MILLIMETER LEVEL
      IF(DABS(ALAT-ALAT0).LT.1.D-10) GO TO 30
      ALAT0=ALAT
      GO TO 40
   30 CONTINUE
      CUTOFF=80.D0*TWOPI/360.D0
c      write(*,*) 'cutoff:',cutoff
      IF(ALAT.GT.CUTOFF) GO TO 50
      HGHT=(SQR/DCOS(ALAT))-CURVN
c      write(*,*) 'hght:',hght

      GO TO 60
c     ??-the below lines
   50 HGHT=Z/DSIN(ALAT)-CURVN+E2*CURVN 
c      write(*,*) 'hght:',hght

 60   continue
c     convert radian angle to degrees
      alat=360.D0*alat/twopi
      along=360.D0*along/twopi
      print 111, alat,along,hght
c     convert to d,m,s (degree, minute and second)
      dlat=int(alat)
      mlat=int((alat-dlat)*60)
      slat=(alat-dlat-mlat/60.D0)*3600.D0
      dlon=int(along)
      mlon=int((along-dlon)*60)
      slon=(along-dlon-mlon/60.D0)*3600.D0
      print '(" Latitude:",3F20.8)', dlat,mlat,slat
      print '("Longitude:",3F20.8)', dlon,mlon,slon
      print '("   Height:",F20.8)', hght
 
      geodrad = 0.d0

   20 CONTINUE   
  
      END
