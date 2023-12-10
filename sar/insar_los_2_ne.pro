;decompose the insar los displacements (one acending and one descending) into horizontal components
;neglect vertical deformation
;
PRO INSAR_LOS_2_NE, ilosa, ilooka, ilosd, ilookd,   $
    n=n, e=e, $
    phi_a=phi_a, phi_d=phi_d
    
  IF N_PARAMS() LT 4 THEN BEGIN
    ilosa=0.42227462d0
    ilooka=43.673000d0
    ilosd=6.7755094d0
    ilookd=39.273600
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
  A=[ [SIN(ilooka*d2r)*SIN(phi_a*d2r), -1d0*SIN(ilooka*d2r)*COS(phi_a*d2r)], $
    [SIN(ilookd*d2r)*SIN(phi_d*d2r), -1d0*SIN(ilookd*d2r)*COS(phi_d*d2r)] ]
    
  B=[ilosa, ilosd]
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
  
  n=result[0]
  e=result[1]
  
  IF N_PARAMS() LT 4 THEN BEGIN
  
    PRINT,'validation: ilos, ilos_model'
    PRINT,ilosa,n*A0[0,0]+e*A0[1,0]
    PRINT,ilosd,n*A0[0,1]+e*A0[1,1]
    print,'north:', n, '  east: ',e
    
  ENDIF
;STOP
  
END