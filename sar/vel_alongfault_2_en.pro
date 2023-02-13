PRO VEL_ALONGFAULT_2_EN, vel_strike, $ ;velocity, in mm/a. positive: right
    strike_angle, $  ;strike of the fault, in radians clockwise from the north
    vel_en=vel_en ;east and north components
    
  ;convert slip velocities parallel to and perpendicular to the fault to north and east components
    
  IF N_PARAMS() LT 2 THEN BEGIN
    vel_strike=3d0
    vel_strike=[1d0,-3d0]
    strike=110d0*!dpi/180
  ENDIF
  
  vel_en=DBLARR(2,N_ELEMENTS(vel_strike))
  ;FOR i=0ull,N_ELEMENTS(vel_strike)-1 DO BEGIN
  ;  vel_en[*,i]=vel_strike[i]*[SIN(strike),COS(strike)]
  ;ENDFOR
  vel_en[0,*]=vel_strike*sin(strike)
  vel_en[1,*]=vel_strike*cos(strike)
  
  IF N_PARAMS() LT 2 THEN BEGIN
    PRINT,'vel_strike:',vel_strike
    PRINT,'strike:',strike
    PRINT,'vel_en:',vel_en
  ENDIF
END