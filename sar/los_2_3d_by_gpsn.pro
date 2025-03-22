;decompose the insar los displacements (one acending and one descending) into horizontal components
;neglect vertical deformation
;
PRO LOS_2_3D_BY_GPSN, insar_vels,   $
    insar_looks,  $
    insar_azimuths, $
    gnss_n, $
    east=e, $
    north=n,  $
    up=u
    
  IF N_PARAMS() LT 4 THEN BEGIN
    insar_vels=[       3.0647169d0, $
      -3.3366943, $
      1.4897130]
    insar_looks=[      42.274553d0,$
      44.253977,$
      31.289247]
    insar_azimuths=[80.313119d0,$
      -80.574452,$
      79.033934]
    gnss_n=-2.4326243d0
    
  ENDIF
  
  ;for converting degree to radian unit, and vice versa
  d2r=!dpi/180d0
  r2d=180d0/!dpi
  
  ;relation between InSAR LOS and 3-d displacements
  ;
  ;  i=sin(theta)[n*sin(phi)-e*cos(phi)]+u*cos(theta)
  ;
  ;  where,
  ;    i = insar los displacement
  ;    n = north displacement
  ;    e = east one
  ;    u = vertical none
  ;    theat = insar incidence angle
  ;    phi = insar orbit direction (clockwise from north)
  
  ;or, the equavalent form:
  ;
  ;  i= -sin(theta)*sin(alpha)*e + -sin(theta)*cos(alpha)*n + cos(theta)*u
  ;
  ;  where,
  ;    alpha=phi-3*pi/2   is the angle between the range direction and the north
  
  
  ;if fix north, then
  ;  A=[[ -1d0*SIN(ilooka*d2r)*COS(phi_a*d2r), COS(ilooka*d2r)], $
  ;    [  -1d0*SIN(ilookd*d2r)*COS(phi_d*d2r), COS(ilookd*d2r)] ]
  ;
  ;  B=[ilosa-gps_n*SIN(ilooka*d2r)*SIN(phi_a*d2r),  $
  ;    ilosd-gps_n*SIN(ilookd*d2r)*SIN(phi_d*d2r)]
  
  nobs=N_ELEMENTS(insar_vels)
  A=DBLARR(2,nobs)
  B=DBLARR(nobs)
  FOR i=0,nobs-1 DO BEGIN
    A[0,i]=-1d0*SIN(insar_looks[i]*d2r)*SIN(insar_azimuths[i]*d2r)
    A[1,i]=COS(insar_looks[i]*d2r)
    B[i]=insar_vels[i] - [-SIN(insar_looks[i]*d2r)*COS(insar_azimuths[i]*d2r)*gnss_n]
  ENDFOR
  
  A0=A
  IF N_PARAMS() LT 4 THEN BEGIN
    PRINT,'A: ', A
    PRINT,'B: ', B
  ENDIF
  ;
  ;print,'A: ', A
  ;print,'B: ', B
  result=LA_LEAST_SQUARES(a, b,/double,STATUS=STATUS)
  
  
  ; Decompose A:
  ;  LUDC, A, INDEX, /double
  ;  ;print,'LUDC(A): ', A
  ;  ;help, A
  ;
  ;  ; Compute the solution using back substitution:
  ;  result = LUSOL(A, INDEX, B)
  ; Print the result:
  ;PRINT, result
  
  e=result[0]
  u=result[1]
  n=gnss_n
  
  IF N_PARAMS() LT 4 THEN BEGIN
  
    PRINT,'validation: ilos, ilos_model'
    ;  i= -sin(theta)*sin(alpha)*e + -sin(theta)*cos(alpha)*n + cos(theta)*u
    FOR i=0,nobs-1 DO BEGIN
      ;                      -SIN(insar_looks[i]*d2r)*sin(insar_azimuths[i]*d2r)
      PRINT,insar_vels[i], -SIN(insar_looks[i]*d2r)*SIN(insar_azimuths[i]*d2r)*e  $
        -SIN(insar_looks[i]*d2r)*COS(insar_azimuths[i]*d2r)*gnss_n+COS(insar_looks[i]*d2r)*u
      PRINT,insar_vels[i], e*A0[0,i] + u*A0[1,i] + [-SIN(insar_looks[i]*d2r)*COS(insar_azimuths[i]*d2r)*gnss_n]
      PRINT,'east:', e, '  north: ',gnss_n, '  up: ',u
    ;stop
    ENDFOR
  ENDIF
;STOP
  
END