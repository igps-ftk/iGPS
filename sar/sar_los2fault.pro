PRO SAR_LOS2FAULT, los, beta, theta=theta, alpha=alpha,   $
    orbtyp=orbtyp,  $ ;orbit type (0-ascending; 1-descending)
    fx=fx, fy=fy, $
    ds=ds, dd=dd
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 2 THEN BEGIN
    los=1d0 ; mm/yr
    ;los=1.2d0
    ;los=0.4d0
    ;los=6d0 ; mm/yr
    ;los=2.4d0 ; mm/yr, xianshuihe ascending
    ;los=9d0 ;mm/yr, altyntagh descending
    
    ;beta - strike of fault (clockwise from north)
    ;beta=?
;    beta=50d0*!dpi/180d0
;    ;beta=83d0*!dpi/180d0
;    ;beta=120d0*!dpi/180d0
;    beta=125d0*!dpi/180d0 ;beng-co fault
;    beta=75d0*!dpi/180d0 ;dongqiao fault
;    ;beta=105d0*!dpi/180d0 ;
    ;beta=105d0*!dpi/180d0+!dpi ;
    ;beta=67*!dpi/180  ;altyn tagh fault at 85E
    ;beta=75.791157*!dpi/180  ;altyn tagh fault 
;    beta=3*!dpi/180 ;for yadong-gulu fault
;    beta=88*!dpi/180 ;for test
;    beta=93*!dpi/180 ;for fake yadong-gulu fault
;    beta=106*!dpi/180 ;for fake jiali fault
;    beta=80*!dpi/180 ;for naqu north fault
;    
;    beta=76.326937*!dpi/180 ; for woniuhu fault
;    
;    beta=125*!dpi/180 ; for daxiong-laxiong lakes fault
;    
;    
;    beta=85*!dpi/180 ; for margai-caka fault
;    
;    
;    beta=140*!dpi/180 ; for xianshuihe fault (Kangding section)
;    los=7.2d0
;    
;    ;theta=23d0*!dpi/180 ;for envisat-1
    
    beta=103*!dpi/180 ;east kunlun fault
    ;beta=!dpi/2-(-0.276357d0);+2*!dpi
    ;stop
;    los=4.8d0
;    los=1d0
;    
;    ;central section of Honghe Fault Zone
;    beta=50*!dpi/180 
;    los=2.4d0
;    ;
;    los=4.8d0
    orbtyp=0
    
    ;orbtyp=1
    
  ENDIF
  
  IF N_ELEMENTS(theta) EQ 0 THEN BEGIN
    ;theta - satellite looking angle
    theta=33d0*!dpi/180d0
    theta=38d0*!dpi/180d0
  ENDIF
  
  ;alpha - azimuth angle of satellite orbit
  IF N_ELEMENTS(alpha) EQ 0 THEN BEGIN
    IF N_ELEMENTS(orbtyp) EQ 0 THEN orbtyp=0
    ;
    IF orbtyp EQ 1 THEN BEGIN
      alpha=193d0*!dpi/180d0  ; -167+360 for descending orbit
    ENDIF ELSE BEGIN
      alpha=(-13+360d0)*!dpi/180d0 ; for ascending orbit
    ;alpha=(-13+0d0)*!dpi/180d0 ; for ascending orbit
    ENDELSE
  ENDIF
  alpha_look=alpha-!dpi/2
  ;
  ;satellite looking azimuth
  alpha2=alpha+!dpi/2
  
  ;assuming NO vertical motions, i.e. du=0
  los_g=los/SIN(theta)
  dn=los_g*COS(alpha2)
  de=los_g*SIN(alpha2)
  ;PRINT,'los,los_g,dn,de:',los,los_g,dn,de,alpha2*180/!dpi-360
  ;stop
  
  ;method 1.
  ;along the strike
  fx=dn*COS(beta)+de*SIN(beta)
  fy=dn*SIN(beta)-de*COS(beta)
  ;PRINT,'fx,fy:',fx,fy,SQRT(fx^2+fy^2)
  ;stop
  
  ;method 2.
  fx=los_g*COS(alpha+!dpi/2-beta)
  fy=-1*los_g*SIN(alpha+!dpi/2-beta)
  ;PRINT,'fx,fy:',fx,fy,SQRT(fx^2+fy^2)
  ;
  ;
  ;method 3
  ds=los/( SIN(theta)*SIN(alpha-beta) ); Garthwaite et al., 2013, JGR. equation (2)
  ;ds=los/( SIN(theta)*COS(alpha-beta) ) 
  dd=los*COS(theta)
  ;PRINT,'ds,dd:',ds,dd
  ;
  ;method 4
  ;Fattahi and Amelung [2016, GRL]
  ;(Fattahi, H., and F. Amelung (2016), InSAR observations of strain accumulation and
  ;fault creep along the Chaman Fault system, Pakistan and Afghanistan,
  ;Geophys. Res. Lett., 43, doi:10.1002/2016GL070121)
  ;Figure 2
  ;  vp=v/(sin(az)cos(h)sin(theta)-cos(az)sin(h)sin(theta))
  ;where, az-fault strike; h-satellite heading angle; theta-average radar incidence.
  vp4=los/( SIN(beta)*COS(alpha)*SIN(theta)-COS(beta)*SIN(alpha)*SIN(theta) )
  
  ;method 5
  vp5=los/( -1d0*SIN(alpha_look)*SIN(theta)*SIN(beta) - COS(alpha_look)*SIN(theta)*COS(beta) )
  
  IF N_PARAMS() LT 2 THEN BEGIN
    PRINT,'satellite incidence (deg):', theta*180/!dpi,format='(a30,1x,F7.2)'
    PRINT,'satellite flight angle (deg):', alpha*180/!dpi,format='(a30,1x,F7.2)'
    PRINT,'fault strike (deg):', beta*180/!dpi,format='(a30,1x,F7.2)'
    PRINT,'For ',los, 'LOS velocity (mm/yr):', format='(a,f7.2,1x,a)'
    ;PRINT,los,beta,theta,alpha
    ;PRINT,los,[BETA,theta,alpha]*180/!dpi
    PRINT,'fx,fy:',fx,fy;,SQRT(fx^2+fy^2)
    PRINT,'ds:',ds,' dd:',dd
    PRINT,'vp4:',vp4
    PRINT,'vp5:',vp5
    
    ;PRINT,los,los_g,fx,fy
    
;    los2=[-1*SIN(alpha2)*SIN(theta),-1*COS(alpha2)*SIN(theta), -1*COS(theta)] ##  $
;      [[fy],[fx],[0]]
;    PRINT,los2
;    
;    los3=[-1*SIN(alpha2)*SIN(theta),-1*COS(alpha2)*SIN(theta), -1*COS(theta)] ##  $
;      [[0],[ds],[dd]]
;    PRINT,los3
  ENDIF
;STOP
END