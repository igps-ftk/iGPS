
;+
; :Name:
;   Llhxyz
;
; :Description:
;   Perform the same task as wgs84xyz program. The mode of transfering parameters is different.
;
; :Keywords:
;    Semi
;    Finv
;    Alat
;    Along
;    Hght
;    Geodrad
;    X
;    Y
;    Z
;    Iflag
;    Dlat
;    Mlat
;    Slat
;    Dlon
;    Mlon
;    Slon
;    Verbose
;
; :Examples:
;   
;    IDL> llhxyz,alat=lat,along=lon,hght=ht,x=-2481697.5d0,y=4298426.1d0,z=3992371.8d0,iflag=2
;    IDL> print,lat,lon,ht
;           39.000000       120.00000       87.015501
;           
;    IDL> llhxyz,alat=39,along=120,hght=87,x=x,y=y,z=z,iflag=1
;    IDL> print,x,y,z
;          -2481697.5       4298426.1       3992371.8
;
; :Modifications:
;   Released on May 11, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
PRO Llhxyz, Semi=Semi, $
    Finv=Finv, $
    Alat=Alat, $
    Along=Along, $
    Hght=Hght, $
    Geodrad=Geodrad, $
    X=X, $
    Y=Y, $
    Z=Z, $
    Iflag=Iflag, $
    Dlat=Dlat, $
    Mlat=Mlat, $
    Slat=Slat, $
    Dlon=Dlon, $
    Mlon=Mlon, $
    Slon=Slon, $
    Verbose=Verbose
    
    
  IF N_ELEMENTS(Iflag) EQ 0 THEN Iflag=2
  
  GOTO, Notest
  
  IF Iflag EQ 1 AND N_ELEMENTS(Alat) EQ 0 THEN BEGIN
    Alat=23.5
    Along=112.6
    Hght=87.5
  ENDIF ELSE BEGIN
    IF Iflag EQ 2 THEN BEGIN
      X=-2249023.08088271D0
      Y=5402931.34009565D0
      Z=2527630.98337617D0
      
      ;Ab07
      X = 	-3425750.0543D0
      Y = 	-1214685.9188D0
      Z = 	5223662.8110D0
      
      ;alrt
      x= 388042.6123
      y=  -740382.4073
      z=  6302001.9040
      
      x=-1370032.80149d0
      y=  5427740.07438d0
      z=  3047623.22502d0
      PRINT,'from xyz to llh ...'
    ENDIF ELSE BEGIN
      PRINT,'error In Llhxyz: Incorrect Parameters!'
      RETURN
    ENDELSE
  ENDELSE
  
  
  Notest:
  
  
  IF N_ELEMENTS(Finv) EQ 0 THEN BEGIN
    ;     Wgs84
    Finv=298.257223563D0
  ;     Grs80
  ;Finv=298.257222101D0
  ENDIF
  
  IF N_ELEMENTS(Semi) EQ 0 THEN BEGIN
    ;     Pre-Defined:
    ;     For Wgs84 & Grs80
    Semi=6378137.D0
  ENDIF
  
  
  ;     Constants
  ;     2Pi In Radian Angle, Used For Converting Between Radian And Degree
  Twopi=8.D0*ATAN(1.D0)
  
  ;     Flattening:
  ;       F=(A-B)/A
  F=1.D0/Finv
  ;     Eccentric:
  ;       E**2=(A**2-B**2)/A**2
  ;     Or,
  ;       E=2F-F*F
  E2=2.D0*F-F*F
  
  
  ;The Conversion Type (Iflag):
  ;     1: Llh-> Xyz
  ;     2: Xyz-> Llh
  
  
  IF (Iflag EQ 2) THEN GOTO, Label10
  
  ;Llh->Xyz
  ;  If Input Is In Degree,Minute,Second
  IF (N_ELEMENTS(Dlat) NE 0 ) THEN BEGIN
    ;Get D,M,S
  
    Alat=Dlat+Mlat/60.D0+Slat/3600.D0
    Along=Dlon+Mlon/60.D0+Slon/3600.D0
  ENDIF
  
  ;Degree->Radian
  ;**Note: Sin,Cos,... Use Radian Angle As Input.
  Alat=Alat*Twopi/360.D0
  Along=Along*Twopi/360.D0
  
  Sinlat=SIN(Alat)
  Coslat=COS(Alat)
  Sinlon=SIN(Along)
  Coslon=COS(Along)
  Curvn=Semi/(SQRT(1.D0-E2*Sinlat*Sinlat))
  ;C
  X=(Curvn+Hght)*Coslat*Coslon
  Y=(Curvn+Hght)*Coslat*Sinlon
  Z=(Curvn*(1.D0-E2)+Hght)*Sinlat
  
  IF KEYWORD_SET(Verbose) THEN BEGIN
    PRINT ,X,Y,Z,  Format='(3F20.8)'
  ENDIF
  ;For Jacobian (Geodetic To Xyz)
  Geodrad=Curvn+Hght
  GOTO, Label20
  
  
  
  
  ;     Xyz->Llh
  Label10:
  ;stop
  Along=ATAN(Y,X)
  ;If (Along Lt 0.D0) Then Along=Along+Twopi ;Deleted By Tian (May-08-2007)
  ;		Satellite Geodesy, By Gunter Seeber (Chinese Version). Pp. 16
  ;C     Starting Value For Latitude Iteration
  Sqr=SQRT(X*X+Y*Y)
  Alat0=ATAN(Z/Sqr,1.D0-E2)
  Alat=Alat0
  Label40:
  Sinlat=SIN(Alat)
  Curvn=Semi/(SQRT(1.D0-E2*Sinlat*Sinlat))
  Alat=ATAN((Z+E2*Curvn*Sinlat),Sqr)
  ;Iterate To The Millimeter Level
  IF (ABS(Alat-Alat0) LT 1.D-10) THEN GOTO, Label30
  Alat0=Alat
  GOTO, Label40
  Label30:
  Cutoff=80.D0*Twopi/360.D0
  IF (Alat GT Cutoff) THEN GOTO, Label50
  Hght=(Sqr/COS(Alat))-Curvn
  
  GOTO, Label60
  ;
  Label50:
  Hght=Z/SIN(Alat)-Curvn+E2*Curvn
  
  Label60:
  ;Convert Radian Angle To Degrees
  Alat=360.D0*Alat/Twopi
  Along=360.D0*Along/Twopi
  ;Convert To D,M,S (Degree, Minute And Second)
  Dlat=FIX(Alat)
  Mlat=FIX((Alat-Dlat)*60)
  Slat=(Alat-Dlat-Mlat/60.D0)*3600.D0
  Dlon=FIX(Along)
  Mlon=FIX((Along-Dlon)*60)
  Slon=(Along-Dlon-Mlon/60.D0)*3600.D0
  IF KEYWORD_SET(Verbose) || n_params() lt 3 THEN BEGIN
    PRINT , Alat,Along,Hght
    PRINT, Format='(" Latitude:",3F20.8)', Dlat,Mlat,Slat
    PRINT, Format='("Longitude:",3F20.8)', Dlon,Mlon,Slon
    PRINT, Format='("   Height:",F20.8)', Hght
  ENDIF
  
  Geodrad = 0.D0
  
  Label20:
  
  RETURN
END
