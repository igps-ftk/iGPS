;+
; NAME:
;		POLYGON_SHP2PSXY
;
; PURPOSE:
;		CONVERT FAULT LINES IN SHAPEFILE INTO GMT PSXY FORMAT.
;
; CALLING SEQUENCE:
;		POLYLINE_SHP2PSXY, SHAPEFILE_FNAME, PSXY_FNAME
;
; MODIFICATION HISTORY:
;		CREATED BY TYF ON Fri, Feb 20, 2015  6:50:16 PM
;
;-
PRO POLYGON_SHP2PSXY, FILE, OFILE

  PROG='POLYGON_SHP2PSXY'
  
  IF N_PARAMS() LT 2 THEN BEGIN
    ;!!Note: make sure that the input file is using the correct projection (usually geographic: wgs84).
    FILE='F:\Downloads\jules.plate\nuvel_1_plates.poly.shp'
    file='C:\Downloads\esa.data\S1\roi_tibet2.shp'
    OFILE=DESUFFIX(FILE)+'.psxy'
  ENDIF
  ;
  OSHAPEFILE = OBJ_NEW('IDLFFSHAPE', FILE)
  IF NOT OBJ_VALID(OSHAPEFILE) THEN BEGIN
    PRINT,'['+PROG+']ERROR:unable to create idlffshape object from ' + FILE
    RETURN
  ENDIF
  
  OSHAPEFILE->GETPROPERTY, N_ENTITIES=N_ENTITIES
  IF N_ENTITIES EQ 0 THEN $
    RETURN
  IF NOT PTR_VALID(PENTS) THEN BEGIN
    PENTS = PTR_NEW(/ALLOCATE_HEAP)
  END
  IF N_ELEMENTS(*PENTS) EQ 0 THEN BEGIN
    *PENTS = OSHAPEFILE->GETENTITY(/ALL, /ATTRIBUTES)
    ;HELP,*(((*PENTS).ATTRIBUTES)[0]),/ST
  ENDIF ELSE BEGIN
    *PENTS = [OSHAPEFILE->GETENTITY(/ALL, /ATTRIBUTES), *PENTS]
  ENDELSE
  ;
  OBJ_DESTROY, OSHAPEFILE
  
  ;STOP
  ;
  OPENW, FID, OFILE, /GET_LUN
  WRITE_SYS_INFO,FID,USER=USER,SRC=[FILE],PROG=PROG,CMT='>'
  PRINTF,FID,['','NOTE: non-blank-first-column lines are comments.',''], FORMAT='(">*",A)' 
  
  ;OUTPUT 1~(N) RECORDS
  FOR I=0, N_ELEMENTS(*PENTS)-1 DO BEGIN
        PRINTF, FID, '> ', $
;        PRINTF, FID, '> ', (*((*PENTS)[I]).ATTRIBUTES).ATTRIBUTE_1, $
;          (*((*PENTS)[I]).ATTRIBUTES).ATTRIBUTE_0, $
;    PRINTF, FID, '> ', (*((*PENTS)[I]).ATTRIBUTES).ATTRIBUTE_2, $
;      (*((*PENTS)[I]).ATTRIBUTES).ATTRIBUTE_1, $
;
      FORMAT='(1X,A,A," ",A)'
    FOR PI=0, ((*PENTS)[I]).N_VERTICES-1 DO BEGIN
      PRINTF, FID, (*((*PENTS)[I]).VERTICES)[*,PI];, FORMAT='(1X)'
    ENDFOR
  ENDFOR
  
  FREE_LUN, FID
  ;
  
  IF PTR_VALID(PENTS) THEN BEGIN
    PTR_FREE, PENTS
  ENDIF
END
;///