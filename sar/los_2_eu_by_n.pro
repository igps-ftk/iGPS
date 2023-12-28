;decompose the insar los displacements (one acending and one descending) into horizontal components
;neglect vertical deformation
;
PRO LOS_2_EU_BY_N, ilosa, ilooka, ilosd, ilookd,   $
    gps_n,  $
    e=e, $
    u=u, $
    phi_a=phi_a, phi_d=phi_d
    
  IF N_PARAMS() LT 5 THEN BEGIN
    ilosa=0.42227462d0
    ilooka=43.673000d0
    ilosd=6.7755094d0
    ilookd=39.273600
    gps_n=1d0
  ENDIF
  
  IF N_ELEMENTS(phi_a) EQ 0 THEN phi_a=-13d0
  IF N_ELEMENTS(phi_d) EQ 0 THEN phi_d=193d0
  
  ;for converting degree to radian unit, and vice versa
  d2r=!dpi/180d0
  r2d=180d0/!dpi
  
  ;relation between InSAR LOS and 3-d displacements
  ;
  ;i=sin(theta)[n*sin(phi)-e*cos(phi)]+u*cos(theta)
  ;
  ;  where,
  ;    i = insar los displacement
  ;    n = north displacement
  ;    e = east one
  ;    u = vertical none
  ;    theat = insar incidence angle
  ;    phi = insar orbit direction (clockwise from north)
  
  ;if neglect vertical motion, i.e., u=0, then
  A=[[ -1d0*SIN(ilooka*d2r)*COS(phi_a*d2r), COS(ilooka*d2r)], $
    [  -1d0*SIN(ilookd*d2r)*COS(phi_d*d2r), COS(ilookd*d2r)] ]
    
  B=[ilosa-gps_n*SIN(ilooka*d2r)*SIN(phi_a*d2r),  $
    ilosd-gps_n*SIN(ilookd*d2r)*SIN(phi_d*d2r)]
  ;print,'A: ', A
  ;print,'B: ', B
    
  A0=A
  
  ; Decompose A:
  LUDC, A, INDEX, /double
  ;print,'LUDC(A): ', A
  ;help, A
  
  ; Compute the solution using back substitution:
  result = LUSOL(A, INDEX, B)
  
  ; Print the result:
  ;PRINT, result
  
  e=result[0]
  u=result[1]
  
  IF N_PARAMS() LT 5 THEN BEGIN
  
    PRINT,'validation: ilos, ilos_model'
    PRINT,ilosa,gps_n*SIN(ilooka*d2r)*SIN(phi_a*d2r)+e*A0[0,0]+u*A0[1,0]
    PRINT,ilosd,gps_n*SIN(ilookd*d2r)*SIN(phi_d*d2r)+e*A0[0,1]+u*A0[1,1]
    PRINT,'east:', e, '  north: ',gps_n, '  up: ',u
    
  ENDIF
;STOP
  
END