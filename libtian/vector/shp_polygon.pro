;+
; Name:
;		SHP_POLYGON
; Purpose:
;		Create polygon shapefile.
; Calling Sequence:
;		SHP_POLYGON,OFNAME, ATTDAT, XI=XI, YI=YI, CAPS=CAPS
;
;+++
;-
PRO SHP_POLYGON,  $
    OFNAME,   $ ;output shapefile name
    region=regions,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    ATTDAT=attdat,   $ ;attribute array (optional)
    ;XI=XI,    $ ;x-coordiantes of points
    ;YI=YI,    $ ;y-coordiantes of points (the dimensions of xi and yi must equal)
    CAPS=CAPS, $   ;if attribute array is present, the caps array gives the field
    CAPTIONS=CAPTIONS  ;name for each column of ATTDAT
  ;
    
  IF N_ELEMENTS(regions) EQ 0 || N_ELEMENTS(nps) EQ 0 ||    $
    N_ELEMENTS(regions) NE N_ELEMENTS(nps) THEN BEGIN
    PRINT, '[SHP_POLYGON]ERROR: invalid input X-Y coordinates! Check the dimensions.'
    
    ;STOP
    FILE='F:\Downloads\jules.plate\nuvel_1_plates.psxy'
    file='D:\ICD\projects\DirectorFund\Application.2012\data\forYaorui.2015oct21\study_area_gmt_psxy.txt'
    READ_POLYGON_PSXY,   $
      file,   $ ;input file
      region=regions,   $ ;x,y coorinates of each polygons (pointer type)
      nps=nps, $  ;number of point pairs for each polygon
      count=count,  $ ;number of polygons
      names=attdat   ;region names (if exist)
    ATTDAT=[ATTDAT]
    
    OFNAME=desuffix(FILE)+'.shp'
  ;stop
  ;RETURN
  ENDIF
  
  ;stop
  ;CHECK THE DIMENSIONS OF ATTRIBUTE ARRAY
  NATT=-1
  IF N_ELEMENTS(ATTDAT) NE 0 THEN BEGIN
    NATT=SIZE(ATTDAT,/N_DIMENSIONS)
    IF NATT GT 1 THEN BEGIN
      NATTREC=N_ELEMENTS(ATTDAT[0,*])
    ENDIF ELSE BEGIN
      NATTREC=N_ELEMENTS(ATTDAT)
    ENDELSE
  ENDIF
  
  ;set column (field) names
  IF N_ELEMENTS(CAPTIONS) NE 0 THEN BEGIN
    CAPS=CAPTIONS
  ENDIF
  IF N_ELEMENTS(CAPS) EQ 0 THEN BEGIN
    PRINT,'[SHP_POLYGON]WARNING: no input attribute field captions. use default column names!'
    CAPS='COL_'+STRTRIM(INDGEN(NATT)+1,2)
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(CAPS) LT NATT THEN BEGIN
      PRINT,'[SHP_POLYGON]WARNING: dimension of input attribute field captions is smaller than attribute array. use default column names!'
      CAPS='COL_'+STRTRIM(INDGEN(NATT)+1,2)
    ENDIF
  ENDELSE
  
  IF N_ELEMENTS(OFNAME) EQ 0 THEN BEGIN
    PRINT,'[SHP_POLYGON]WARNING: no output file specified! Use the default filename (test_igps.shp) in current direcotry.'
    ofname='test_igps.shp'	 ;FOR TEST
  ENDIF
  
  
  ;Create the new shapefile and define the entity type to Point
  mynewshape=OBJ_NEW('IDLffShape', ofname, /UPDATE, ENTITY_TYPE=5)
  IF OBJ_VALID(mynewshape) NE 1 THEN BEGIN
    PRINT, '[SHP_POLYGON]ERROR: cannot create new shapefile ['+ofile+']!'
    RETURN
  ENDIF
  
  ;Set the attribute definitions for the new Shapefile
  FOR AI=0, NATT-1 DO BEGIN
    mynewshape->IDLFFSHAPE::ADDATTRIBUTE, CAPS[AI], 7, 128
  ENDFOR
  
  ;Insert each POLYGON
  FOR pi=0, N_ELEMENTS(regions)-1 DO BEGIN
    ;print,'pi: ',pi
    XI=REFORM((*REGIONS[PI])[0,*])
    YI=REFORM((*REGIONS[PI])[1,*])
    ;Create structure for new entity
    entNew = {IDL_SHAPE_ENTITY}
    ; Define the values for the new entity
    entNew.SHAPE_TYPE = 5
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
    ENTNEW.VERTICES=PTR_NEW(*REGIONS[PI])
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
  
  ;Close the shapefile
  OBJ_DESTROY, mynewshape
  PRINT,'[]Normal end.'
  
END