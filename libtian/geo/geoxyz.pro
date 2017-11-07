
pro GEOXYZ,alat,along,hght,geodrad=geodrad,x,y,z,iflag,semi=semi,finv=finv

  if n_params() lt 3 then begin
    ;alat=39.60860101d
    ;along=115.89248559d
    ;hght=87.4506d
    ;
    x=-2148744.1167d
    y=4426641.2788d
    z=4044655.8963d
    
    x=-1932937.25975
    y=4656319.01278
    z=3894971.98220
  endif
  ;
  ;constants for wgs84
  semi=6378137d
  finv=298.257223563d ;WGS84
  ;finv=298.257222101d ;- GRS80 value, corrected to WGS by rwk 96/10/30
  ;
  ;help,iflag
  if n_elements(iflag) eq 0 then iflag=2
  ;
  ;
  TWOPI=8.D0*ATAN(1.D0)
  
  F=1.D0/FINV
  E2=2.D0*F-F*F
  IF(IFLAG EQ 2) THEN GOTO, L10
  alat=alat*twopi/360.d
  along=along*twopi/360.d
  SINLAT=SIN(ALAT)
  COSLAT=COS(ALAT)
  SINLON=SIN(ALONG)
  COSLON=COS(ALONG)
  CURVN=SEMI/(SQRT(1.D0-E2*SINLAT*SINLAT))
  ;C
  X=(CURVN+HGHT)*COSLAT*COSLON
  Y=(CURVN+HGHT)*COSLAT*SINLON
  Z=(CURVN*(1.D0-E2)+HGHT)*SINLAT
  ;print,x,y,z,format='(3F20.4)'
  ;C
  ;C  FOR JACOBIAN (GEODETIC TO XYZ)
  geodrad=CURVN+HGHT
  ;C
  GOTO, L20
  ;C
  L10:
  ;    CONTINUE
  ALONG=ATAN(Y,X)
  IF(ALONG LT 0.D0) then ALONG=ALONG+TWOPI
  ;C     STARTING VALUE FOR LATITUDE ITERATION
  SQR=SQRT(X*X+Y*Y)
  ;C Changed by D.D.Dong:  Old:  ALAT0=DATAN2(1.D0-E2),Z/SQR)
  ALAT0=ATAN(Z/SQR,1.D0-E2)
  ALAT=ALAT0
  L40:  SINLAT=SIN(ALAT)
  CURVN=SEMI/(SQRT(1.D0-E2*SINLAT*SINLAT))
  ALAT=ATAN((Z+E2*CURVN*SINLAT),SQR)
  ;C     ITERATE TO THE MILLIMETER LEVEL
  IF(ABS(ALAT-ALAT0) LT 1.D-100) THEN GOTO, L30
  ALAT0=ALAT
  GOTO, L40
  L30:
  ;   CONTINUE
  CUTOFF=80.D0*TWOPI/360.D0
  IF(ALAT GT CUTOFF) THEN GOTO, L50
  HGHT=(SQR/COS(ALAT))-CURVN
  GOTO, L20
  L50: HGHT=Z/SIN(ALAT)-CURVN+E2*CURVN
  geodrad = 0.d0
  L20:
  ;CONTINUE
  ;RETURN
  ;END
  ;print,alat*360/twopi,along*360/twopi,hght,format='(3F20.8)'
  alat=alat*360d/twopi
  along=along*360d/twopi
  
  if n_params() lt 3 then begin
    print,x,y,z
    print,along,alat,hght
  endif
  
end
