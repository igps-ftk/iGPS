;+
; Name:
;		SHP_POINT
; Purpose:
;		Create Point shapefile.
; Calling Sequence:
;		SHP_POINT,OFNAME, ATTDAT, XI=XI, YI=YI, CAPS=CAPS
;
;+++
;-
PRO SHP_POINT,  $
    OFNAME,   $ ;output shapefile name
    ATTDAT,   $ ;attribute array (optional)
    XI=XI,    $ ;x-coordiantes of points
    YI=YI,    $ ;y-coordiantes of points (the dimensions of xi and yi must equal)
    CAPS=CAPS, $   ;if attribute array is present, the caps array gives the field
    CAPTIONS=CAPTIONS
  ;name for each column of ATTDAT
  ;
    
  IF N_ELEMENTS(XI) EQ 0 || N_ELEMENTS(YI) EQ 0 ||    $
    N_ELEMENTS(xi) NE N_ELEMENTS(yi) THEN BEGIN
    PRINT, '[SHP_POINT]ERROR: invalid input X-Y coordinates! Check the dimensions.'
    RETURN
  ENDIF
  
  ;CHECK THE DIMENSIONS OF ATTRIBUTE ARRAY
  NATT=-1
  IF N_ELEMENTS(ATTDAT) NE 0 THEN BEGIN
    NATT=N_ELEMENTS(ATTDAT)
    NATTREC=N_ELEMENTS(*ATTDAT[0])
  ENDIF
  
  ;set column (field) names
  IF N_ELEMENTS(CAPTIONS) NE 0 THEN BEGIN
    CAPS=CAPTIONS
  ENDIF
  IF N_ELEMENTS(CAPS) EQ 0 THEN BEGIN
    PRINT,'[SHP_POINT]WARNING: no input attribute field captions. use default column names!'
    CAPS='COL_'+STRTRIM(INDGEN(NATT)+1,2)
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(CAPS) LT NATT THEN BEGIN
      PRINT,'[SHP_POINT]WARNING: dimension of input attribute field captions is smaller than attribute array. use default column names!'
      CAPS='COL_'+STRTRIM(INDGEN(NATT)+1,2)
    ENDIF
  ENDELSE
  
  IF N_ELEMENTS(OFNAME) EQ 0 THEN BEGIN
    PRINT,'[SHP_POINT]WARNING: no output file specified! Use the default filename (test_igps.shp) in current direcotry.'
    ofname='test_igps.shp'	 ;FOR TEST
  ENDIF
  
  
  ;Create the new shapefile and define the entity type to Point
  mynewshape=OBJ_NEW('IDLffShape', ofname, /UPDATE, ENTITY_TYPE=1)
  IF OBJ_VALID(mynewshape) NE 1 THEN BEGIN
    PRINT, '[SHP_POINT]ERROR: cannot create new shapefile ['+ofname+']!'
    RETURN
  ENDIF
  
  ;Set the attribute definitions for the new Shapefile
  FOR AI=0, NATT-1 DO BEGIN
    mynewshape->IDLFFSHAPE::ADDATTRIBUTE, CAPS[AI], 7, 25
  ENDFOR
  
  ;Insert each point
  FOR pi=0, N_ELEMENTS(xi)-1 DO BEGIN
    ;print,'pi: ',pi
    ;Create structure for new entity
    entNew = {IDL_SHAPE_ENTITY}
    ; Define the values for the new entity
    entNew.SHAPE_TYPE = 1
    entNew.ISHAPE = 1458
    entNew.BOUNDS[0] = xi[pi]
    entNew.BOUNDS[1] = YI[pi]
    entNew.BOUNDS[2] = 0.00000000
    entNew.BOUNDS[3] = 0.00000000
    entNew.BOUNDS[4] =  xi[pi]
    entNew.BOUNDS[5] = YI[pi]
    entNew.BOUNDS[6] = 0.00000000
    entNew.BOUNDS[7] = 0.00000000
    ;Create structure for new attributes
    attrNew = mynewshape ->IDLFFSHAPE::GETATTRIBUTES( $
      /ATTRIBUTE_STRUCTURE)
    ;Define the values for the new attributes
    FOR ai=0, NATT-1 DO BEGIN
      cmdStr='attrNew.ATTRIBUTE_'+STRTRIM(STRING(ai),2)+' = (*ATTDAT[AI])[PI]'
      ;cmdStr='attrNew.ATTRIBUTE_'+STRTRIM(STRING(ai),2)+' = ATTDAT[ai*NATT+pi]'
      ;PRINT,cmdstr
      dummy=EXECUTE(cmdstr)
    ENDFOR
    ;print,ATTDAT(5,pi)
    ;Add the new entity to new shapefile
    mynewshape -> IDLffShape::PutEntity, entNew
    ;Add the attributes to new shapefile
    mynewshape -> IDLffShape::SetAttributes, pi, attrNew
  ENDFOR
  
  ;Close the shapefile
  OBJ_DESTROY, mynewshape
  
END