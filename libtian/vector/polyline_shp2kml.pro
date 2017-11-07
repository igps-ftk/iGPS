;+
;
;+++
;-
PRO POLYLINE_SHP2KML, FILE, OFILE
  ;!!Note: make sure that the input file is using the correct projection (usually geographic: wgs84).
  IF N_ELEMENTS(FILE) EQ 0 THEN BEGIN
    FILE=DIALOG_PICKFILE(TITLE='Input Fault Lines File (Shapefile) in Suitable Map Projection?',FILTER='*.shp')
    IF FILE EQ '' THEN RETURN
    CD,GETPATHNAME(FILE)
  ENDIF
  IF N_ELEMENTS(OFILE) EQ 0 THEN BEGIN
    OFILE=DIALOG_PICKFILE(TITLE='Save as:',FILTER='*.kml')
    IF OFILE EQ '' THEN RETURN
    CD,GETPATHNAME(OFILE)
  ENDIF
  ;
  oShapefile = OBJ_NEW('IDLffShape', file)
  IF NOT OBJ_VALID(oShapefile) THEN BEGIN
    void = DIALOG_MESSAGE('Unable to create IDLffShape Object from ' + fname)
    du=DIALOG_MESSAGE(!error_state)
    RETURN
  ENDIF
  oShapefile->GETPROPERTY, N_ENTITIES=n_entities
  IF n_entities EQ 0 THEN $
    RETURN
  IF NOT PTR_VALID(pEnts) THEN BEGIN
    pEnts = PTR_NEW(/ALLOCATE_HEAP)
  END
  IF N_ELEMENTS(*pEnts) EQ 0 THEN BEGIN
    *pEnts = oShapefile->GETENTITY(/ALL, /ATTRIBUTES)
  ENDIF ELSE BEGIN
    *pEnts = [oShapefile->GETENTITY(/ALL, /ATTRIBUTES), *pEnts]
  ENDELSE
  ;
  ;
  OPENW,lun,ofile,/get_lun
  kml_headers=['<?xml version="1.0" encoding="UTF-8"?>',$
    ;'<kml xmlns="http://www.opengis.net/kml/2.2"',$
    ;'xmlns:gx="http://www.google.com/kml/ext/2.2">   <!-- required when using gx-prefixed elements -->',$
    '<kml xmlns="http://earth.google.com/kml/2.0">', $
    '<Document><Folder>',$
    '<name>'+desuffix(GETFILENAME(file))+'</name>',$
    ;    '<Style id="linestyleExample">',$
    ;    '<LineStyle>',$
    ;    '<color>7f00ffff</color>',$
    ;    '<width>3</width>',$
    ;    '</LineStyle>',$
    ;    '</Style>']
    '']
    
    
  PRINTF,lun,kml_headers,format='(a)'

  FOR i=0, N_ELEMENTS(*pEnts)-1 DO BEGIN
    kml_line=[      '<Placemark>',$
    ;'<styleUrl>#linestyleExample</styleUrl>',$
    '<name>'+desuffix(GETFILENAME(file))+STRING(i,format='(i)')+'</name>',$
    '<LineString>',$
    ;'<extrude>0</extrude>',$
    ;'<gx:altitudeMode>clampToGround</gx:altitudeMode>',$
    ;'<Style><LineStyle><color>ff0000ff</color></LineStyle>  <PolyStyle><fill>0</fill></PolyStyle></Style>',$
    '<coordinates>']
    
    FOR j=0, (*pents)[i].N_partS-1 DO BEGIN
      PRINTF,lun,kml_line,format='(a)'
      IF j EQ (*pents)[i].N_partS-1 THEN BEGIN
        ;PRINTF,lun,(*((*pents)[i].VERTICES))[*,(*((*pents)[i].parts))[j]:*],format='(f,",",f,",6000")'
        tmpstr=STRING((*((*pents)[i].VERTICES))[*,(*((*pents)[i].parts))[j]:*],format='(f20.13,",",f20.13)')
        tmpstr1=tmpstr
        FOR tsi=0,N_ELEMENTS(tmpstr)-1 DO BEGIN
          tmpstr1i=STRJOIN(STRSPLIT(tmpstr[tsi],/extract))
          tmpstr1[tsi]=tmpstr1i
        ENDFOR
        
        PRINTF,lun,strjoin(tmpstr1,' '),format='(a)'
      ENDIF ELSE BEGIN
        ;PRINTF,lun,(*((*pents)[i].VERTICES))[*,(*((*pents)[i].parts))[j]:(*((*pents)[i].parts))[j+1]-1],format='(f,",",f,",6000")'
        ;PRINTF,lun,(*((*pents)[i].VERTICES))[*,(*((*pents)[i].parts))[j]:(*((*pents)[i].parts))[j+1]-1],format='(f,",",f)'
        tmpstr=STRING((*((*pents)[i].VERTICES))[*,(*((*pents)[i].parts))[j]:(*((*pents)[i].parts))[j+1]-1],format='(f,",",f)')
        tmpstr1=tmpstr
        FOR tsi=0,N_ELEMENTS(tmpstr)-1 DO BEGIN
          tmpstr1i=STRJOIN(STRSPLIT(tmpstr[tsi],/extract))
          tmpstr1[tsi]=tmpstr1i
        ENDFOR
        ;strs=''
        ;stop
        ;for strsi=0,n_elements(tmpstr1)-1 do begin
          ;strsi=strsi+' '+tmpstr1[strsi]
        ;endfor
        PRINTF,lun,strjoin(tmpstr1,' '),format='(a)'
      ENDELSE
      
      PRINTF,lun,['</coordinates>', $
        '</LineString>',$
        '<Style><LineStyle><color>ff0000ff</color></LineStyle>  <PolyStyle><fill>0</fill></PolyStyle></Style>',$
        '</Placemark>'],format='(a)'
    ENDFOR
  ;goto, out_loop
  ENDFOR
  ;
  out_loop:
  kml_tail=[ '</Folder></Document>',$
    '</kml>']
  PRINTF,lun,kml_tail,format='(a)'
  FREE_LUN,lun
  OBJ_DESTROY, oShapeFile
  IF PTR_VALID(pEnts) THEN BEGIN
    PTR_FREE, pEnts
  ENDIF
END
;///