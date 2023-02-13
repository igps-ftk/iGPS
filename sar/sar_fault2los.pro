;+
; :DESCRIPTION:
;    Convert fault-parallel and fault-perpendicular slip rates to LOS rate.
;
; :PARAMS:
;    slip_strike, slip_dip
;
; :KEYWORDS:
;    theta
;    alpha
;
; :AUTHOR: tianyf
;-
FUNCTION SAR_FAULT2LOS, strike, slip_strike, slip_dip, theta=theta, alpha=alpha
  ;
  ;theta - satellite looking angle
  IF N_ELEMENTS(theta) EQ 0 THEN BEGIN
    theta=38d0*!dpi/180d0
  ENDIF
  ;
  IF N_ELEMENTS(alpha) EQ 0 THEN BEGIN
    alpha=193d0*!dpi/180d0  ; -167+360 for descending orbit
  ;alpha=(-13+360d0)*!dpi/180d0 ; for ascending orbit
  ENDIF
  
  strike1=strike
  if strike1 gt !dpi then strike1=strike1-!dpi
  print,'strike:',strike1*180/!dpi
  
  ang1=strike1-(alpha-!dpi/2)
  print,'ang1:',ang1*180/!dpi
  slip_strike_horiz= slip_strike*cos(ang1) 
  slip_dip_horiz= slip_dip*sin(ang1)
  
  dlos_strike=slip_strike_horiz*sin(theta)
  dlos_dip=slip_dip_horiz*sin(theta)
  
  dlos=dlos_strike+dlos_dip
  print,dlos_strike,dlos_dip,dlos
  
  ;dlos=enu[2]*COS(theta)-SIN(theta)*(enu[1]*COS(alpha-3*!dpi/2)+enu[0]*SIN(alpha-3*!dpi/2))
  ;dlos2=(enu[1]*sin(alpha)-enu[0]*cos(alpha))*sin(theta)+enu[2]*cos(theta)
  RETURN, dlos
END

PRO SAR_FAULT2LOS
; 
  ;Gyaring Co fault
  strike=300d0*!dpi/180 ;  degrees
  slip_strike=2d0   ; dextral, mm/yr
  slip_dip=0.25d0  ;normal
  ;slip_dip=0.d0  ;no dip motion
  ;slip_dip=-0.5d0  ;thrust
  
  
  dlos2=SAR_FAULT2LOS(strike, slip_strike, slip_dip)
  PRINT,strike, slip_strike, slip_dip
  print,dlos2
  
  ;print,SAR_FAULT2LOS(strike, slip_strike, slip_dip, alpha=(-13+360d0)*!dpi/180d0) ; for ascending orbit
  

  
END