;+
; :Description:
;    Convert between WGS84 and XYZ [ECEF].
;
; :Params:
;    alat
;    along
;    hght
;    x
;    y
;    z
;    iflag
;
; :Keywords:
;    geodrad
;    semi
;    finv
;    
; Examples:
;    IDL> wgs84xyz,39,120,87,x,y,z,1
;    IDL> print,x,y,z
;          -2481697.5       4298426.1       3992371.8   
;
;    IDL> wgs84xyz,lat,lon,ht,-2481697.5d0,4298426.1d0,3992371.8d0,2
;    IDL> print,lat,lon,ht
;           39.000000       120.00000       87.015501
;           
; :Author: tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
PRO WGS84XYZ, $
    alat, $ ;latitude in decimal degrees
    along, $  ;longitude
    hght, $ ;height in meters
    x, $  ;ECEF X in meters
    y, $
    z, $
    iflag, $  ;1 (wgs84->xyz) or 2 (xyz->wgs84)
    geodrad=geodrad, $
    semi=semi,$ ;Earth radius
    finv=finv
    
  IF N_PARAMS() LT 3 THEN BEGIN
    ;alat=39.60860101d
    ;along=115.89248559d
    ;hght=87.4506d
    ;
    x=-2148744.1167d
    y=4426641.2788d
    z=4044655.8963d
    
    ;for bjfs
    ; from site log
    ;     Approximate Position (ITRF)
    ;       X coordinate (m)       : -2148743.808
    ;       Y coordinate (m)       : 4426641.281
    ;       Z coordinate (m)       : 4044655.993
    ;       Latitude (N is +)      : +393631.05
    ;       Longitude (E is +)     : +1155332.86
    ;       Elevation (m,ellips.)  : 87.413
    x=-2148743.808d0
    y=4426641.281d0
    z=4044655.993d0
    
    ; from sopac web page (site info)
    ;XYZ coordinates (m):   -2148744.251  4426641.242  4044655.8871
    ;[Ref. epoch: 2010.3000]  [Ref. frame: ITRF2005]
    ;[Source: Latest weekly analysis result]
    ;Lat/Lon/Ellip. Ht (deg/deg/m):   39.6086008  115.89248718  87.4644
    ;[Ref. epoch: 2010.3000]  [Geodetic datum: WGS84]
    ;[Source: Latest weekly analysis result]
    x=-2148744.251d0
    y=4426641.242d0
    z=4044655.8871d0
    ;    % Compiled module: WGS84XYZ.
    ;       39.608601       115.89249       87.464431
    ;      -2148744.3       4426641.2       4044655.9
    
    x=-2172135.49d0
    y=4392152.14d0
    z=4069475.62d0
    
    PRINT, '[WGS84XYZ]Error: not enough input parameters!'
    RETURN
    
  ENDIF
  ;
  ;
  ;constants for wgs84
  if n_elements(semi) eq 0 then semi=6378137d0
  if n_elements(finv) eq 0 then finv=298.257223563d0
  ;
  IF N_ELEMENTS(iflag) EQ 0 THEN iflag=2  ;default XYZ -> WGS84 (2); iflag = 1, vice versa.
  ;
  ;The Two Pi constant.
  TWOPI=8.D0*ATAN(1.D0)
  
  F=1.D0/FINV
  E2=2.D0*F-F*F
  
  IF(IFLAG EQ 2) THEN GOTO, L10
  
  ;WGS84 -> XYZ
  alat=alat*twopi/360.d
  along=along*twopi/360.d
  SINLAT=SIN(ALAT)
  COSLAT=COS(ALAT)
  SINLON=SIN(ALONG)
  COSLON=COS(ALONG)
  CURVN=SEMI/(SQRT(1.D0-E2*SINLAT*SINLAT))
  X=(CURVN+HGHT)*COSLAT*COSLON
  Y=(CURVN+HGHT)*COSLAT*SINLON
  Z=(CURVN*(1.D0-E2)+HGHT)*SINLAT
  ;print,x,y,z,format='(3F20.4)'
  geodrad=CURVN+HGHT
  GOTO, L20
  
  
  
  ;XYZ - > WGS84
  L10:
  
  ALONG=ATAN(Y,X)
  IF(ALONG LT 0.D0) THEN ALONG=ALONG+TWOPI
  SQR=SQRT(X*X+Y*Y)
  ALAT0=ATAN(Z/SQR,1.D0-E2)
  ALAT=ALAT0
  
  L40:  SINLAT=SIN(ALAT)
  CURVN=SEMI/(SQRT(1.D0-E2*SINLAT*SINLAT))
  ALAT=ATAN((Z+E2*CURVN*SINLAT),SQR)
  IF(ABS(ALAT-ALAT0) LT 1.D-100) THEN GOTO, L30
  ALAT0=ALAT
  GOTO, L40 
  
  L30:
  CUTOFF=80.D0*TWOPI/360.D0
  IF(ALAT GT CUTOFF) THEN GOTO, L50
  HGHT=(SQR/COS(ALAT))-CURVN
  GOTO, L20
  L50: HGHT=Z/SIN(ALAT)-CURVN+E2*CURVN
  geodrad = 0.d0
  
  ;Done
  L20:
  alat=alat*360d/twopi
  along=along*360d/twopi
  
  IF N_PARAMS() LT 3 THEN BEGIN
    PRINT,alat,along,hght
    PRINT,x,y,z
  ENDIF
END
