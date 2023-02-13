;+
; NAME:
;		POLYLINE_SHP2PSXY
; PURPOSE:
;		CONVERT FAULT LINES IN SHAPEFILE INTO GMT PSXY FORMAT.
; CALLING SEQUENCE:
;		POLYLINE_SHP2PSXY, SHAPEFILE_FNAME, PSXY_FNAME
; MODIFICATION HISTORY:
;		CREATED BY TYF ON MAY 29, 2003.
;+++
;-
PRO POLYLINE_SHP2PSXY, FILE, OFILE
  ;!!Note: make sure that the input file is using the correct projection (usually geographic: wgs84).
  IF N_ELEMENTS(FILE) EQ 0 THEN BEGIN
    FILE=DIALOG_PICKFILE(TITLE='Input Fault Lines File (Shapefile) in Suitable Map Projection?',FILTER='*.shp')
;    ;file='D:\ICD\projects\nsfc\2011\annual.report.2013\earthquake.catalog\areas\shp\quake_areas.shp'
;;  
;;    file='D:\data\vector\fault.China.100m\gmt\fa100m.shp'
;;    file='D:\ICD\projects\nsfc\2017\figure\vector\fa\normal.shp'
;;    file='D:\ICD\projects\nsfc\2017\figure\vector\fa\sinustral.shp'
;;    ;file='D:\ICD\projects\nsfc\2017\figure\vector\fa\dextral.shx'
;    file='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\profile_jiali.shp'
;    file='D:\Papers\paper.bengco\vector\profile_xianshuihe.shp'
;    ;file='D:\Papers\paper.bengco\vector\fault_lenglongling.shp'
;    file='D:\Papers\paper.bengco\vector\profile_lenglongling.shp'
;    file='D:\Papers\paper.bengco\vector\fault_cona_east.shp'
;    file='D:\Papers\paper.bengco\vector\profile_cona_east.shp'
;    file='D:\Papers\paper.bengco\vector\fault_anduo_sewa.shp'
;    file='D:\data\vector\profile\fault_yzs.shp'
;    file='D:\data\vector\profile\fault_naqu_north.shp'
;    file='D:\data\vector\profile\fault_sangri_cuona.shp'
;    file='D:\Papers\paper.haiyuan.creep\vector\profile_\profile_CD_.shp'
;    file='D:\Papers\paper.Xiangyang-Buruo\figure\1.map\97manyi\97manyi_rupture_s.shp'
    ofile=desuffix(file)+'.psxy'
    IF FILE EQ '' THEN RETURN
    CD,GETPATHNAME(FILE)
  ENDIF
  IF N_ELEMENTS(OFILE) EQ 0 THEN BEGIN
    OFILE=DIALOG_PICKFILE(TITLE='Save as:',FILTER='*.txt')
    ;ofile='D:\ICD\projects\nsfc\2011\annual.report.2013\earthquake.catalog\areas\shp\quake_areas.txt'
    IF OFILE EQ '' THEN RETURN
    CD,GETPATHNAME(OFILE)
  ENDIF
  ;
  ;stop
  OSHAPEFILE = OBJ_NEW('IDLFFSHAPE', FILE)
  IF NOT OBJ_VALID(OSHAPEFILE) THEN BEGIN
    VOID = DIALOG_MESSAGE('Unable to create idlffshape object from ' + FILE)
    DU=DIALOG_MESSAGE(!ERROR_STATE)
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
  ENDIF ELSE BEGIN
    *PENTS = [OSHAPEFILE->GETENTITY(/ALL, /ATTRIBUTES), *PENTS]
  ENDELSE
  ;
  OBJ_DESTROY, OSHAPEFILE
  
  ;stop
  ;
  OPENW, LUN, OFILE, /GET_LUN
  
  ;output 1~(N) records
  FOR I=0, N_ELEMENTS(*PENTS)-1 DO BEGIN
    ;IF ((*PENTS)[I]).SHAPE_TYPE NE 1 THEN BEGIN
    ;  PRINTF, LUN, '> ', (*((*PENTS)[I]).ATTRIBUTES).ATTRIBUTE_0
    ;ENDIF
    FOR j=0,((*PENTS)[I]).N_parts-1 DO BEGIN
      ;help,(*((*PENTS)[I]).ATTRIBUTES),/st
      ;PRINTF, LUN, '> '+STRTRIM(j+1)+' '+strtrim((*((*PENTS)[I]).ATTRIBUTES).ATTRIBUTE_0,2)
      PRINTF, LUN, '> line='+strtrim(i+1,2)+',segment='+strtrim(j+1,2)+',*='+strtrim((*((*PENTS)[I]).ATTRIBUTES).ATTRIBUTE_0,2)
      
      pi1=(*(((*PENTS)[I]).parts))[j]
      IF j EQ ((*PENTS)[I]).N_parts-1 THEN BEGIN
        pi2=((*PENTS)[I]).N_VERTICES-1
      ENDIF ELSE BEGIN
        pi2=(*(((*PENTS)[I]).parts))[j+1]-1
      ENDELSE
      ;FOR PI=0, ((*PENTS)[I]).N_VERTICES-1 DO BEGIN
      FOR PI=pi1,pi2 DO BEGIN
        IF ((*PENTS)[I]).SHAPE_TYPE EQ 1 THEN BEGIN
          PRINTF, LUN, (((*PENTS)[I]).bounds)[[0,1]]
        ENDIF ELSE BEGIN
          PRINTF, LUN, (*((*PENTS)[I]).VERTICES)[*,PI]
        ENDELSE
      ENDFOR  ;end-of-loop-for vertices
      
    ENDFOR  ;end-of-loop-for parts
  ENDFOR  ;end-of-loop for polylines
  
  FREE_LUN, LUN
  ;
  
  IF PTR_VALID(PENTS) THEN BEGIN
    PTR_FREE, PENTS
  ENDIF
END
;///