;+
;FROM QOCA HOMEPAGE/TUTORIAL:
;  http://gipsy.jpl.nasa.gov/qoca/extclass/ext_mload.html
;  The output solution file has such records:
;  
;      FAIR_GPS lon= 212.493358  colat=  25.021999  
;    2000  1  1  0  2451544.50   -2.5407   -0.3077   -1.9582 -0.8366404E-06
;    2000  1  2  0  2451545.50   -2.8449   -0.3481   -1.4388 -0.9856214E-06
;    2000  1  3  0  2451546.50   -1.5950   -0.5006   -1.1025 -0.6798246E-06
;  
;  The columns of the solution records are:
;  year month day hour JD vertical north east gravity
;  Where the unit of the site displacements is mm (positive in up, north, east directions).
;-
PRO READ_QOCA_ALOAD, FILE, DATA=oDATA, HEADERS=HEADERS
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','qoca', $
      'atml'],'bjfs_GPS.aload')
  ENDIF
  READ_COLS_ASCII, FILE, DATA=DATA, COMM='*', HEADERS=HEADERS
  DATA = DOUBLE(DATA)
  oDATA=DATA
  ;ODATA[5:6,*]=DATA[6:7,*]
  ;ODATA[7,*]=DATA[5,*]
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP, FILE, DATA, HEADERS
  ENDIF
  

END