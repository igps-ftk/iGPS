
;+
; :Name:
;    READ_PBO
;
; :Description:
;    Describe the procedure.
;
; :Params:
;    FILE
;
; :Keywords:
;    SITE
;    FIRSTEPOCH
;    LASTEPOCH
;    XYZREF
;    NEUREF
;    STR
;    HEADERS
;    NS
;    NL
;    NH
;    DATA
;
; :Modifications:
;    + 2015ARP10 tyf
;      Add support for new PBO time series format (http://pbosoftware.unavco.org/dmsdocs/Root%20Folder/Data%20Management/Data%20Product%20Documentation/gps_timeseries_format.pdf).
;
;    + APR09 2007 Tian
;      A little progress for loop, much faster now;
;
;Data Sample:
; PBO Station Position Time Series
; Format Version: 1.0.1
; 4-character ID: AB11
; Station name  : Nome_AnvilAK2006
; First Epoch   : 20060721 120000
; Last Epoch    : 20070310 120000
; Release Data  : 20070404 073140
; XYZ Reference position :  -2658010.23252  -693674.79470  5737338.58385
; NEU Reference position :    64.5644967742  194.6265414893  349.44237
;   20060721 120000 53937.5000 -2658010.23592  -693674.79682  5737338.58572  0.00407  0.00260  0.00736  0.505 -0.677 -0.528
;         64.5644967558  194.6265415143  349.44631    -0.00264   0.00119   0.00333    0.00282  0.00219  0.00805 -0.006 -0.003  0.295 suppf

;
; :Author: tianyf
;-
PRO READ_PBO, FILE, $ ; input; the other keywords are named variables for returning results.
    SITE = SITE, $ ;
    FIRSTEPOCH = FIRSTEPOCH, $
    LASTEPOCH = LASTEPOCH, $
    XYZREF = XYZREF, $
    NEUREF = NEUREF, $
    STR = STR, $ ;IF NOT PRESENT, THEN RETURN THE DOUBLE ARRAY; OTHERWISE, RETURN STRING ARRAY
    HEADERS = HEADERS, $
    NS=NS, $
    NL=NL, $
    NH=NH, $
    DATA=DATA
  ;
  IF N_PARAMS() LT 1 THEN BEGIN
    ;old format
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','pbo', $
      'pos'],'SHLD.pbo.final_frame.pos')
    ;new format
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','pbo', $
      'pos'],'VNCX.pbo.final_igs08.pos')
  ENDIF
  ;
  ;
  QUERY_PBO,FILE, SITE = SITE, $
    FIRSTEPOCH = FIRSTEPOCH, $
    LASTEPOCH = LASTEPOCH, $
    XYZREF = XYZREF, $
    NEUREF = NEUREF, $
    STR = STR, $ ;IF NOT PRESENT, THEN RETURN THE DOUBLE ARRAY; OTHERWISE, RETURN STRING ARRAY
    HEADERS = HEADERS, $
    NS=NS, $
    NL=NL, $
    NH=NH
    
    
  ;READ_COLS_ASCII,FILE,SKIP=NH,DATA=DATA;,COLUMN_SKIP=1
  DLINES=STRARR(NL)
  TMP=''
  OPENR,FID,FILE,/GET_LUN
  FOR I=0,NH-1 DO READF,FID,TMP
  READF,FID,DLINES
  FREE_LUN,FID
  
  DLINES=STR_LINES2ARR(DLINES)
  
  ;STOP
  
  DATA=DOUBLE(DLINES[0:NS-2,*])
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,FILE,DATA,HEADERS
    PRINT,HEADERS
  ENDIF
  
  
  
END

;Appendix A.
;Table 2: PBO GPS Station Position Time Series Format
;Entry Definition
;YYYY	 4-digit year for the given position epoch
;MM	 	2-digit month of year for the given position epoch
;DD 	2-digit day of month for the given position epoch
;HH 	2-digit hour for the given position epoch
;MM 	2-digit minute for the given position epoch
;SS	 2-digit second for the given position epoch
;JJJJJ	 Modified Julian day for the given position epoch
;X Y Z 	ITRF Cartesian coordinates, meters
;xx 	Standard deviation of the X position, meters
;yy	 Standard deviation of the Y position, meters
;zz	 Standard deviation of the Z position, meters
;xy	 Correlation of the X and Y position
;xz	 Correlation of the X and Z position
;yz	 Correlation of the Y and Z position
;N	 North latitude, decimal degrees, relative to WGS-84 ellipsoid
;E 	East longitude, decimal degrees, relative to WGS-84 ellipsoid
;U	 Elevation, meters, relative to WGS-84 ellipsoid
;Ndel	 Change in North component relative to NEU reference position, meters. If the
;		station moves northward, Ndel is positive.
;Edel	Change in East component relative to NEU reference position, meters. If the station
;		moves eastward, Ndel is positive.
;Udel 	Change in vertical component relative to NEU reference position, meters. If the
;		station moves upward, Ndel is positive.
;nn		Standard deviation of Ndel, meters
;ee 	Standard deviation of Edel, meters
;uu 	Standard deviation of Udel, meters
;ne 	Correlation of Ndel and Edel
;nu		 Correlation of Ndel and Udel
;eu		 Correlation of Edel and Udel
;<quality>		 'final' or 'rapid', corresponding to products generated from final or rapid orproducts
;See the PBO web page for a reference for the Modified Julian date.
