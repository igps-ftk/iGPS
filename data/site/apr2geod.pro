;SiteCoords.geod
;* Geodetic coordinates from mepj_04.org : Date Wed Oct 28 06:11:40 EDT 2009
;* Geodetic (WGS84) coordinates from GLOBK Vers 5.17I
;*  Site        Latitude        Longitude    Height        dlat/dt       dlng/dt      dH/dt    Epoch
;*               (deg)            (deg)       (m)          (deg/yr)     (deg/yr)      (m/yr)    Year
;  CHPI_GPS  -22.687145333  315.014841428  617.3834     0.000000000    0.000000000   0.00000  2009.2413
;  B01J_GPS  -23.056387818  313.142758219  807.5963     0.000000000    0.000000000   0.00000  2009.2413
;  BRAZ_GPS  -15.947474376  312.122130698 1105.9956     0.000000000    0.000000000   0.00000  2009.2413
;  B04P_GPS  -22.564048235  312.073504651  569.8757     0.000000000    0.000000000   0.00000  2009.2413
;  B03P_GPS  -23.095501950  311.598844813  928.5397     0.000000000    0.000000000   0.00000  2009.2413
;  LPGS_GPS  -34.906744778  302.067700442   29.8757     0.000000000    0.000000000   0.00000  2009.2413
;  TUCU_GPS  -26.843254843  294.769648485  485.0467     0.000000000    0.000000000   0.00000  2009.2413
;  UNSA_GPS  -24.727455987  294.592356772 1257.7738     0.000000000    0.000000000   0.00000  2009.2413
;  IQQE_GPS  -20.273540894  289.868286485   38.9810     0.000000000    0.000000000   0.00000  2009.2413
;  COPO_GPS  -27.384526164  289.661764186  479.0864     0.000000000    0.000000000   0.00000  2009.2413
;  ANTC_GPS  -37.338702325  288.467951218  745.4119     0.000000000    0.000000000   0.00000  2009.2413


PRO APR2GEOD, FILE, OFILE
  IF N_PARAMS() LT 2 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT, $
      SUBDIRECTORY=['tables'],$
      'itrf2005.apr.updated')
     file='K:\gpsac2\home\tianyf\gpse\rerun.cmonoc\solut\gmf.v3.survey_mode_cmonoc\reg\reg3\tables\lfile.old'
     file='D:\ICD\projects\DirectorFund\Application.2012\Field\2014oct\result\lfile'
    OFILE=FILE+'.geod'
  ENDIF
  
  FMTSTR='(2x,A4,"_GPS",1x,F14.9,1x,F14.9,1x,F9.4,1x,2(F15.9),F10.5,2x,F9.4)'
  
  ;READ IN
  READ_APR, FILE, SITES=SITES, VALS=VALS, $
    XYZS=XYZS, $
    VXYZS=VXYZS, $
    EPOCHS=EPOCHS, $
    DATA=DATA, $
    LINES=LINES_NOCMT
    
  NSITE=N_ELEMENTS(SITES)
  IF NSITE LE 0 || SITES[0] EQ '' THEN BEGIN
    PRINT,'[APR2GEOD]WARNING: no input data!'
    RETURN
  ENDIF
  
  LLHS=DBLARR(3,NSITE)
  VLLHS=DBLARR(3,NSITE)
  FOR SI=0,NSITE-1 DO BEGIN
    WGS84XYZ, $
      alat, $ ;latitude in decimal degrees
      along, $  ;longitude
      hght, $ ;height in meters
      XYZS[0,SI], $  ;ECEF X in meters
      XYZS[1,SI], $
      XYZS[2,SI], $
      2   ;1 (wgs84->xyz) or 2 (xyz->wgs84)
    IF ALONG LT 0 THEN ALONG=ALONG+360
    LLHS[*,SI]=[ALAT,ALONG,HGHT]
  ENDFOR
  
  ;OUTPUT
  OPENW,FID,OFILE,/GET_LUN
  WRITE_SYS_INFO,FID,PROG='APR2GEOD',SRC=FILE
  PRINTF,FID,'*  Site        Latitude        Longitude    Height        dlat/dt       dlng/dt      dH/dt    Epoch',FORMAT='(A)'
  PRINTF,FID,'*               (deg)            (deg)       (m)          (deg/yr)     (deg/yr)      (m/yr)    Year',FORMAT='(A)'
  FOR SI=0,NSITE-1 DO BEGIN
    PRINTF,FID,SITES[SI],LLHS[*,SI],VLLHS[*,SI],EPOCHS[SI],FORMAT=FMTSTR
  ENDFOR
  FREE_LUN,FID
  ;STOP
  
  PRINT,'[APR2GEOD]Normal End.'
END