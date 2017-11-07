c$$$; $Id: //depot/idl/IDL_64/idldir/lib/map_2points.pro#1 $
c$$$;
c$$$; Copyright (c) 2000-2007, ITT Visual Information Solutions. All
c$$$;       rights reserved. Unauthorized reproduction is prohibited.
c$$$;+
c$$$; NAME:
c$$$;	Map_2Points
c$$$;
c$$$; PURPOSE:
c$$$;	Return parameters such as distance, azimuth, and path relating to
c$$$;	the great circle or rhumb line connecting two points on a sphere.
c$$$;
c$$$; CATEGORY:
c$$$;	Maps.
c$$$;
c$$$; CALLING SEQUENCE:
c$$$;	Result = Map_2Points(lon0, lat0, lon1, lat1)
c$$$; INPUTS:
c$$$;	Lon0, Lat0 = longitude and latitude of first point, P0.
c$$$;	Lon1, Lat1 = longitude and latitude of second point, P1.
c$$$;
c$$$; KEYWORD PARAMETERS:
c$$$;   RADIANS = if set, inputs and angular outputs are in radians, otherwise
c$$$;	degrees.
c$$$;   NPATH, DPATH = if set, return a (2, n) array containing the
c$$$;	longitude / latitude of the points on the great circle or rhumb
c$$$;	line connecting P0 and P1.  If NPATH is set, return NPATH equally
c$$$;	spaced points.  If DPATH is set, it specifies the maximum angular
c$$$;	distance between the points on the path in the prevalent units,
c$$$;	degrees or radians.
c$$$;   PARAMETERS: if set, return [sin(c), cos(c), sin(az), cos(az)]
c$$$;	the parameters determining the great circle connecting the two
c$$$;	points.  c is the great circle angular distance, and az is the
c$$$;	azimuth of the great circle at P0, in degrees east of north.
c$$$;   METERS: Return the distance between the two points in meters,
c$$$;	calculated using the Clarke 1866 equatorial radius of the earth.
c$$$;   MILES: Return the distance between the two points in miles,
c$$$;	calculated using the Clarke 1866 equatorial radius of the earth.
c$$$;   RADIUS: If given, return the distance between the two points
c$$$;	calculated using the given radius.
c$$$;   RHUMB: Set this keyword to return the distance and azimuth of the
c$$$;	rhumb line connecting the two points, P0 to P1. The default is to
c$$$;	return the distance and azimuth of the great circle connecting the
c$$$;	two points.  A rhumb line is the line of constant direction
c$$$;	connecting two points.
c$$$;
c$$$; OUTPUTS:
c$$$;	If the keywords NPATH, DPATH, METERS, MILES, or RADIUS, are not
c$$$;	specified, the function result is a two element vector containing
c$$$;	the distance and azimuth of the great circle or rhumb line
c$$$;	connecting the two points, P0 to P1, in the specified angular units.
c$$$;
c$$$;	If MILES, METERS, or RADIUS is not set, Distances are angular
c$$$;	distance, from 0 to 180 degrees (or 0 to !pi if the RADIANS keyword
c$$$;	is set), and Azimuth is measured in degrees or radians, east of north.
c$$$;
c$$$; EXAMPLES:
c$$$;	Given the geocoordinates of two points, Boulder and London:
c$$$;	B = [ -105.19, 40.02]	;Longitude, latitude in degrees.
c$$$;	L = [ -0.07,   51.30]
c$$$;
c$$$;	print, Map_2Points(B[0], B[1], L[0], L[1])
c$$$; prints: 67.854333 40.667833 for the angular distance and
c$$$; azimuth, from B, of the great circle connecting the two
c$$$; points.
c$$$;
c$$$;	print, Map_2Points(B[0], B[1], L[0], L[1],/RHUMB)
c$$$; prints 73.966280 81.228056, for the angular distance and
c$$$; course (azimuth), connecting the two points.
c$$$;
c$$$;	print, Map_2Points(B[0], B[1], L[0], L[1],/MILES)
c$$$; prints:  4693.5845 for the distance in miles between the two points.
c$$$;
c$$$;	print, Map_2Points(B[0], B[1], L[0], L[1], /MILES,/RHUMB)
c$$$; prints: 5116.3569, the distance in miles along the rhumb line
c$$$; connecting the two points.
c$$$;
c$$$; The following code displays a map containing the two points, and
c$$$; annotates the map with both the great circle and the rhumb line path
c$$$; between the points, drawn at one degree increments.
c$$$;	MAP_SET, /MOLLWEIDE, 40,-50, /GRID, SCALE=75e6,/CONTINENTS
c$$$;	PLOTS, Map_2Points(B[0], B[1], L[0], L[1],/RHUMB, DPATH=1)
c$$$;	PLOTS, Map_2Points(B[0], B[1], L[0], L[1],DPATH=1)
c$$$;
c$$$;
c$$$; MODIFICATION HISTORY:
c$$$; 	Written by:
c$$$;	DMS, RSI	May, 2000. Written.
c$$$;   CT, RSI, September 2001: For /RHUMB, reduce lon range to -180,+180
c$$$;   CT, RSI, September 2002: For /RHUMB, fix computation at poles.
c$$$;-

      subroutine Map_2points(lon0a, lat0a, lon1a, lat1a, 
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
C     ---
      IMPLICIT NONE
      real*8 lat0a,lat1a,lon0a,lon1a,radius,dist
      integer radian,meter,mile
c     ---
      real*8 pi2,dpi,k,r_earth
      real*8 lat0,lat1,lon0,lon1
      real*8 coslt1,sinlt1,coslt0,sinlt0,cosl0l1,sinl0l1,cosc,sinc
      real*8 cosaz,sinaz
      parameter(dpi=3.1415926535897932d0)
c     ---

      lon0=lon0a
      lat0=lat0a
      lon1=lon1a
      lat1=lat1a
      
      if (lon0.lt.-180d0) then
         lon0=lon0+360
      endif
      if (lon1.lt.-180d0) then
         lon1=lon1+360
      endif
      if (lon0.gt.180d0) then
         lon0=lon0-360
      endif
      if (lon1.gt.180d0) then
         lon1=lon1-360
      endif
C     the above conversion is useless??? Seems so.

c      pi2 = 3.14d0/2
c      dpi=3.14d0
      pi2=dpi/2
      k=dpi/180
      if (radian.eq.1) then
         k=1
      endif

c      if (radius.lt.1d-20) then
      if (radius.eq.0d0) then
         r_earth = 6378206.4d0 
         r_earth = 6378137d0
c     WGS84 major axis
      else
         r_earth = radius
c         write(*,*) 'new radius:',r_earth
      endif
c     ;Earth equatorial radius, meters, Clarke 1866 ellipsoid
 
      if (dabs(lon0-lon1).lt.1d-15.and.dabs(lat0-lat1).lt.1d-15) then
c         write(*,*) 'same'
         goto 899
      endif

      coslt1 = dcos(k*lat1)
      sinlt1 = dsin(k*lat1)
      coslt0 = dcos(k*lat0)
      sinlt0 = dsin(k*lat0)

      cosl0l1 = dcos(k*(lon1-lon0))
      sinl0l1 = dsin(k*(lon1-lon0))

      cosc = sinlt0 * sinlt1 + coslt0 * coslt1 * cosl0l1 
c     ;Cos of angle between pnts
c     ; Avoid roundoff problems by clamping cosine range to [-1,1].
c      cosc = -1 > cosc < 1
      sinc = dsqrt(1.0 - cosc**2)

      if (dabs(sinc) .gt. 1.0e-7 ) then 
c     begin ;Small angle?
         cosaz = (coslt0 * sinlt1 - sinlt0*coslt1*cosl0l1) / sinc 
c     ;Azmuith
         sinaz = sinl0l1*coslt1/sinc
      else 
c     begin		;Its antipodal
         cosaz = 1.0
         sinaz = 0.0
      endif


c      if (radius.gt.1d-20) then 
C     !!!Cannnot use 0 ???
C     Error: 
      if (radius.gt.0d0) then
c     NOTE: the input parameter must be the same type
c     double ~ double  RIGHT
c     int    ~ double  WRONG

c     $   ;Radius supplied? Return distance.
         dist=dacos(cosc) * radius
c         write(*,*) 'radius:',radius
         goto 899
      endif
c      write(*,*) radius,meter,mile,radian
      if (meter.eq.1) then 
c     $   ;Meters?
         dist= dacos(cosc) * r_earth
c         write(*,*) 'meter'
         goto 899
      endif
      if (mile.eq.1) then 
c     $    ;Miles?
         dist= dacos(cosc) * r_earth * 0.6213712d-3 
c         write(*,*) 'mile',r_earth
         goto 899
c     ;Meters->miles
      endif

c     degree:
c      dist=dacos(cosc)/k
      dist=dacos(cosc)/k
c      write(*,*) 'degree'
c     , atan(sinaz, cosaz) / k] ;Return distance, azimuth
 899  continue
c      write(*,'(8f12.6)') lon0,lon1,lat0,lat1,dist,k,cosc,dacos(cosc)/k
      end
