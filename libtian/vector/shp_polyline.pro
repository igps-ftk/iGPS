;+
; Name:
;		SHP_POLYLINE
; Purpose:
;		Create polyline shapefile.
; Calling Sequence:
;		SHP_POLYLINE,ofile, ATTDAT=attdat, pxys=pxys, field_name=field_name
;
;+++
;-
PRO SHP_POLYLINE,  $
    ofile,   $ ;output shapefile name
    ATTDAT=attdat,   $ ;attribute array (optional). If not given, use one blank column
    pxys=pxys,    $ ;x/y-coordiantes of all line segments (pointer type)
    field_name=field_name   ;limited to 11 characters by IDL
    
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  ;name for each column of ATTDAT
  ;
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT, '['+PROG+']WARNING: invalid input X-Y coordinates! Check the dimensions.'
    PRINT, '['+PROG+']WARNING: use test data.'
    
    ;STOP
    file=FILEPATH(root_dir=!igps_root,subdirectory=['example','vector'],'fault_ydgl.psxy')
    READ_POLYGON_PSXY,   $
      file,   $ ;input file
      region=pxys ;x,y coorinates of each polygons (pointer type)
      
    PRINT, '['+PROG+']Input: ',file
    ofile=(FILE)+'.shp'
    
  ;stop
  ;RETURN
  ENDIF
  
  np=N_ELEMENTS(pxys)
  PRINT, '['+PROG+']Number of polylines: ',N_ELEMENTS(pxys)
  PRINT, '['+PROG+']Output: ', ofile
  
  ;CHECK THE DIMENSIONS OF ATTRIBUTE ARRAY
  IF N_ELEMENTS(ATTDAT) NE 0 THEN BEGIN
    NATT=SIZE(ATTDAT,/N_DIMENSIONS)
    IF NATT GT 1 THEN BEGIN
      NATTREC=N_ELEMENTS(ATTDAT[0,*])
    ENDIF ELSE BEGIN
      NATTREC=N_ELEMENTS(ATTDAT)
    ENDELSE
  ENDIF ELSE BEGIN
    natt=1
    field_name='none'
    attdat=STRARR(N_ELEMENTS(pxys))
  ENDELSE
  
  
  IF natt GT 0 THEN BEGIN
    IF N_ELEMENTS(field_name) EQ 0 THEN BEGIN
      PRINT,'['+PROG+']WARNING: no input attribute field captions. use default column names!'
      field_name='COL_'+STRTRIM(INDGEN(NATT)+1,2)
    ENDIF ELSE BEGIN
      IF N_ELEMENTS(field_name) LT NATT THEN BEGIN
        PRINT,'['+PROG+']WARNING: dimension of input attribute field captions is smaller than attribute array. use default column names!'
        field_name='COL_'+STRTRIM(INDGEN(NATT)+1,2)
      ENDIF
    ENDELSE
  ENDIF ELSE BEGIN
  ENDELSE
  
  IF N_ELEMENTS(ofile) EQ 0 THEN BEGIN
    PRINT,'['+PROG+']WARNING: no output file specified! Use the default filename (test_igps.shp) in current direcotry.'
    ofile='test_igps.shp'	 ;FOR TEST
  ENDIF
  
  
  ;Create the new shapefile and define the entity type to Point
  mynewshape=OBJ_NEW('IDLffShape', ofile, /UPDATE, ENTITY_TYPE=3)
  IF OBJ_VALID(mynewshape) NE 1 THEN BEGIN
    PRINT, '['+PROG+']ERROR: cannot create new shapefile ['+ofile+']!!'
    RETURN
  ENDIF
  
  ;Set the attribute definitions for the new Shapefile
  FOR AI=0, NATT-1 DO BEGIN
    mynewshape->IDLFFSHAPE::ADDATTRIBUTE, field_name[AI], 7, 25
  ENDFOR
  
  ;Insert each POLYGON
  FOR pi=0, np-1 DO BEGIN
    ;print,'pi: ',pi
    XI=REFORM((*pxys[PI])[0,*])
    YI=REFORM((*pxys[PI])[1,*])
    ;Create structure for new entity
    entNew = {IDL_SHAPE_ENTITY}
    ; Define the values for the new entity
    entNew.SHAPE_TYPE = 3
    ;entNew.ISHAPE = 1458
    entNew.BOUNDS[0] = MIN(XI)
    entNew.BOUNDS[1] = MIN(YI)
    entNew.BOUNDS[2] = 0.00000000
    entNew.BOUNDS[3] = 0.00000000
    entNew.BOUNDS[4] = MAX(XI)
    entNew.BOUNDS[5] = MAX(YI)
    entNew.BOUNDS[6] = 0.00000000
    entNew.BOUNDS[7] = 0.00000000
    ENTNEW.N_VERTICES=N_ELEMENTS(XI)
    ENTNEW.VERTICES=PTR_NEW(*pxys[PI])
    ;Create structure for new attributes
    attrNew = mynewshape ->IDLFFSHAPE::GETATTRIBUTES( $
      /ATTRIBUTE_STRUCTURE)
    ;Define the values for the new attributes
    FOR ai=0, NATT-1 DO BEGIN
      ;cmdStr='attrNew.ATTRIBUTE_'+STRTRIM(STRING(ai),2)+' = ATTDAT[ai,pi]'
      cmdStr='attrNew.ATTRIBUTE_'+STRTRIM(STRING(ai),2)+' = ATTDAT[ai*NATT+pi]'
      ;PRINT,cmdstr
      dummy=EXECUTE(cmdstr)
    ENDFOR
    ;print,ATTDAT(5,pi)
    
    ;Add the new entity to new shapefile
    mynewshape -> IDLffShape::PutEntity, entNew
    ;Add the attributes to new shapefile
    mynewshape -> IDLffShape::SetAttributes, pi, attrNew
  ;BREAK
  ENDFOR
  
  if n_params() lt 1 then FOR i=0,np-1 DO PTR_FREE,pxys[i]
  
  ;Close the shapefile
  OBJ_DESTROY, mynewshape
  PRINT,'['+PROG+']Normal end.'
  
END