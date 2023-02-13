PRO SAR_LOS2FAULT_PURE_STRIKE_SLIP, los,  $ ;los displcement
    beta, $ ;strike of fault
    theta=theta,  $
    alpha=alpha,   $
    orbtyp=orbtyp,  $ ;orbit type (a-ascending; d-descending)
    fx=fx, fy=fy, $
    ds=ds, dd=dd
    
  IF N_PARAMS() LT 2 THEN BEGIN
    los=1d0 ; mm/yr
    
    ;beta - strike of fault (clockwise from north)
    ;beta=?
    beta=50d0*!dpi/180d0
    ;beta=83d0*!dpi/180d0
    ;beta=120d0*!dpi/180d0
    
    
    ;for bengco fault
    orbtyp='d'
    beta=120d0*!dpi/180d0+!dpi
    beta=298d0*!dpi/180d0
    ;for gyaringco fault
    beta=(36+270d0)*!dpi/180d0
    
    ;for ganzi fault
    
    ;for eklf
    orbtyp='d'
    beta=(103)*!dpi/180d0
    
  ENDIF
  
  IF N_ELEMENTS(theta) EQ 0 THEN BEGIN
    ;theta - satellite looking angle
    theta=38d0*!dpi/180d0
  ENDIF
  
  ;alpha - azimuth angle of satellite orbit
  IF N_ELEMENTS(alpha) EQ 0 THEN BEGIN
    IF N_ELEMENTS(orbtyp) EQ 0 THEN orbtyp='a'
    ;
    IF orbtyp EQ 'd' THEN BEGIN
      alpha=193d0*!dpi/180d0  ; -167+360 for descending orbit
    ENDIF ELSE BEGIN
      alpha=(-13+360d0)*!dpi/180d0 ; for ascending orbit
    ;alpha=(-13+0d0)*!dpi/180d0 ; for ascending orbit
    ENDELSE
  ENDIF
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
  ds=los/( SIN(theta)*SIN(alpha-beta) )
  dd=los/COS(theta)
  ;PRINT,'ds,dd:',ds,dd
  ;
  ;method 4, pure strike slip
  sf=-1d0*SIN(alpha)*SIN(theta)*SIN(beta)-COS(alpha)*SIN(theta)*COS(beta)
  ;print,'scale_factor:',1/sf
  ;stop
  fx=los/sf
  fy=0d0
  IF N_PARAMS() LT 2 THEN BEGIN
    PRINT,'los,beta,theta,alpha:',los,beta*180/!dpi,theta*180/!dpi,alpha*180/!dpi
    PRINT,'los,los_g,fx,fy:',los,los_g,fx,fy
    
    los2=[-1*SIN(alpha2)*SIN(theta),-1*COS(alpha2)*SIN(theta), -1*COS(theta)] ##  $
      [[fy],[fx],[0]]
    PRINT,'los2:',los2
    
    los3=[-1*SIN(alpha2)*SIN(theta),-1*COS(alpha2)*SIN(theta), -1*COS(theta)] ##  $
      [[0],[ds],[dd]]
    PRINT,'los3:',los3
  ENDIF
;STOP
END