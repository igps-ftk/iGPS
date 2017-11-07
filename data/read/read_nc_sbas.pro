PRO READ_NC_SBAS,FILE,DATA=DATA, $
    LAT=LAT, $
    LON=LON,  $
    VAR_TYPE=VAR_TYPE
    
  IF N_PARAMS() LT 1 THEN BEGIN
    file='C:\Downloads\gsartest\dangxiong2\des_F2\disp_002_ll.nc'
  ENDIF
  
  IF N_ELEMENTS(VAR_TYPE) EQ 0 THEN VAR_TYPE='z'
  
  IF N_ELEMENTS(VAR_TYPE) EQ 0 THEN BEGIN
    VAR_TYPE=GETFILENAME(FILE)
    POS=STRPOS(VAR_TYPE,'.')
    VAR_TYPE=STRMID(VAR_TYPE,0,POS)
  ENDIF
  
  FID=NCDF_OPEN(FILE)
  
  VAR_TYPE_LOCAL=VAR_TYPE
  VARID=NCDF_VARID(FID,VAR_TYPE_LOCAL)
  NCDF_VARGET,FID,VARID,DATA;,OFFSET=offset
  ;  pos=WHERE(FINITE(data) NE 0)
  ;  IF pos[0] EQ -1 THEN RETURN
  ;  data0=DOUBLE(data)
  ;  data0[*,*]=DOUBLE(-0/0d0)
  ;  data0[pos]=DOUBLE(data[pos])
  
  VAR_TYPE_LOCAL='x_range'
  VARID=NCDF_VARID(FID,VAR_TYPE_LOCAL)
  IF VARID[0] NE -1 THEN NCDF_VARGET,FID,VARID,x_range
  ;HELP,LAT
  ;PRINT,LAT
  
  
  VAR_TYPE_LOCAL='y_range'
  VARID=NCDF_VARID(FID,VAR_TYPE_LOCAL)
  IF VARID[0] NE -1 THEN NCDF_VARGET,FID,VARID,y_range
  ;HELP,LAT
  ;PRINT,LAT
  
  VAR_TYPE_LOCAL='dimension'
  VARID=NCDF_VARID(FID,VAR_TYPE_LOCAL)
  IF VARID[0] NE -1 THEN NCDF_VARGET,FID,VARID,dims
  
  
  VAR_TYPE_LOCAL='spacing'
  VARID=NCDF_VARID(FID,VAR_TYPE_LOCAL)
  IF VARID[0] NE -1 THEN NCDF_VARGET,FID,VARID,spacing
  
  NCDF_CLOSE,fid
  
  IF N_ELEMENTS(X_RANGE) GT 0 THEN BEGIN
    lon=x_range[0]+FINDGEN(dims[0])*spacing[0]
    lat=y_range[0]+FINDGEN(dims[1])*spacing[1]
  ENDIF
  IF SIZE(data,/n_dimensions) LT 2 THEN BEGIN
    data=REFORM(data,dims[0],dims[1])
  ENDIF
  data=ROTATE(data,7)
  
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,DATA,lon,lat
    odata=ECONGRID(data,.25)
    WINDOW,0,xsize=N_ELEMENTS(odata[*,0]),ysize=N_ELEMENTS(odata[0,*])
    TVSCL,odata,/nan;,/order
    ;odata2=ROTATE(odata,7)
    ;TVSCL,odata2,/nan;,/order
    STOP
  ENDIF
END