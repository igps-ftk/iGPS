;
;+
; :DESCRIPTION:
;    Convert GPS velocities to KML file for viewing in Google Earth.
;
; :PARAMS:
;    FILE - input GPS velocity file in the following format.
;
;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
; BMCL_GPS   81.7100   28.6600   0.000   0.000   6.700   0.530   0.000   0.000  32.400   0.530  -0.0108
; CHLM_GPS   85.3100   28.2100   0.000   0.000   8.400   0.520   0.000   0.000  29.000   0.520   0.0518
; CUOM_GPS   86.9000   30.4500   0.000   0.000  12.600   0.530   0.000   0.000  21.600   0.520   0.0427
;
;    OFILE  - output KML file
;
;
;
; :AUTHOR: tianyf
;   Created on Thu, May 12, 2016 10:17:12 PM
;-
PRO GPS_VEL_TO_KML, FILE, OFILE

  PROG='GPS_VEL_TO_KML'
  IF N_PARAMS() LT 2 THEN BEGIN
    PRINT,'['+PROG+']Usage: '+PROG+', file, ofile',FORMAT='(A)'
    ;TEST
    FILE='D:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\qmap.tibet.cgps.tseri\rotinv\vel.Supp_Table_S1.tolhasa'
    file='D:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\qmap.tibet.cgps.tseri\rotinv\Supp_Table_S1.psvelo.forRotinv'
    ;FILE='D:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\qmap.tibet.cgps.tseri\rotinv\vel.tibet.2015nov.refall'
    ;FILE='D:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\qmap.tibet.cgps.tseri\lsq_velo.gps.psvelo.forRotinv'
    OFILE=FILE+'.kml'
    RETURN
  ENDIF
  
  
  
  SF=15D-3
  
  ;PRINT,'ARROW LENGTH:', VN*SF, VE*SF
  
  LEN_ERRARR=.05D0 ;IN DEGREES
  ANG_ERRARR=30D0 ;ANGLE IN DEGREES
  PRINT,'ERROR ARROW LENGTH:', LEN_ERRARR
  
  ANGLES_ERRELL=INDGEN(11)*36D0
  ANGLES_ERRELL=INDGEN(37)*10D0
  CHIS=2.4477D0
  
  
  READ_COLS_ASCII, FILE, SKIP=1,DATA=LINES
  ;HELP, LINES
  
  SITES=REFORM(LINES[0,*])
  LLHS=DOUBLE(LINES[1:2,*])
  DATA=DOUBLE(LINES[1:*,*])
  
  PTS=REPLICATE(PTR_NEW(),N_ELEMENTS(SITES))
  INFOS=STRARR(N_ELEMENTS(SITES))
  
  
  FOR SI=0, N_ELEMENTS(SITES)-1 DO BEGIN
    ;STOP
  
    SITE=SITES[SI]
    LON=LLHS[0,SI]
    LAT=LLHS[1,SI]
    
    
    VN=DATA[8,SI]
    VE=DATA[4,SI]
    VNe=DATA[9,SI]
    VEe=DATA[5,SI]
    CORRen=DATA[10,SI]
    INFOS[SI]='      vE: '+STRTRIM(LINES[5,SI],2)+'+/-'+STRTRIM(LINES[6,SI],2)+ $
      '   vN: '+STRTRIM(LINES[9,SI],2)+'+/-'+STRTRIM(LINES[10,SI],2)+'  mm/a'
      
      
      
    ALPHA=ATAN(VN,VE)*180D0/!DPI
    ALPHA1=(ALPHA+(180D0-ANG_ERRARR))*!DPI/180D0
    ALPHA2=-1D0*(180D0-ALPHA-ANG_ERRARR)*!DPI/180D0
    
    ;PRINT,ALPHA
    ;PRINT,ALPHA2*180D0/!DPI
    
    
    A1=[LON+VE*SF,LAT+VN*SF]
    A2=[LON+VE*SF+LEN_ERRARR*COS(ALPHA1),LAT+VN*SF+LEN_ERRARR*SIN(ALPHA1)]
    ;PRINT,LON+VE*SF,LEN_ERRARR*COS(ALPHA1),LAT+VN*SF,LEN_ERRARR*SIN(ALPHA1)
    ;PRINT,ALPHA1*180D0/!DPI
    ;PRINT,SIN(ALPHA1),COS(ALPHA1)
    ;PRINT,LON+VE*SF,LEN_ERRARR*COS(ALPHA1),LAT+VN*SF,LEN_ERRARR*SIN(ALPHA1)
    A3=[LON+VE*SF+LEN_ERRARR*COS(ALPHA2),LAT+VN*SF+LEN_ERRARR*SIN(ALPHA2)]
    
    TMP=[ [LON,LAT],[A1],[A2],[A3],[A1] ]

    ;IF STRUPCASE(site) EQ 'JB24_GPS' THEN STOP
    ERROR_ELLIPSE, VEe*SF, VNe*SF, CORRen, EEXYS=EEXYS, $
      NPT=NPT, $ ;NUMBER OF POINTS TO DRAW AN ELLIPSE
      XYS=XYS1, $
      SF=CHIS*1d0 ;SCALE FACTOR (DEFAULT TO BE A STANDARD ERROR ELLIPSE (39.4% CONFIDENCE)
      
    EEXYS1=EEXYS
    EEXYS1[0,*]=EEXYS1[0,*]+A1[0]
    EEXYS1[1,*]=EEXYS1[1,*]+A1[1]

    TMP=[[TMP],[EEXYS1] ]
    
    PTS[SI]=PTR_NEW(TMP)    
  ENDFOR
  
  KML_LINE, OFILE, SITES=SITES, LLH=PTS, INFOS=INFOS
  
  PRINT,'['+PROG+']Normal end.', FORMAT='(A)'
  
END
