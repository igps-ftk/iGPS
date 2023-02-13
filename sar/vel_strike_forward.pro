PRO VEL_STRIKE_FORWARD, vmax, $ ;far-field interseismic slip rate, in mm/yr
    D,  $ ;locking depth, in km
    y=y, $   ;distances away from the fault trace ;in km
    vels=vels ;output velocity profile
  ;
  ;perform forward modeling of velocity distribution for strike-slip faulting
  ;
  ;Theory background:
  ;  v(y)=Vmax/pi*atan(y/D)
  ;xp=(fs/!dpi)*ATAN((d1-fts)/ld) ;
    
  ;
  IF N_PARAMS() LT 2 THEN BEGIN
    vmax=5d0
    D=10d0
  ENDIF
  
  IF N_ELEMENTS(y) EQ 0 THEN BEGIN
    ny=180
    y=INDGEN(ny)-.5d0*ny
  ENDIF
  
  vels=vmax/!dpi*ATAN(y/D)
  
  WINDOW,1
  PLOT,y,vels,background='ffffff'x,color='0'x
  oplot,y,vels,psym=4,color='0000ff'x
END