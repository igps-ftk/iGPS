;
PRO KML_POLYLINE, OFILE,   $ ;output KML OFILE name
    names=names,  $ ; names for the polylines
    LLS=LLS, $  ; vertices for each poline (pointer array)
    INFOS=INFOS, $  ;
    IS_FREE_PTR=IS_FREE_PTR,  $ ;If 1, free the LLS pointers when done.
    IS_FINALIZE=IS_FINALIZE ;if 1, link the first and last vertices (i.e., repeat the first vertex following the last one).
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'['+PROG+']Usage: '+PROG+', OFILE, names=names, LLS=LLS',FORMAT='(A)'
    ;TEST
    OFILE='C:\tmp\test_polyline.kml'
    NAMES='NQXM'
    LLS=[[       101.63048D0     ,  24.614679],$
      [ 99.161514   ,    25.031153],$
      [ 99.477791  ,     26.662165],$
      [ 101.98238  ,     26.248726]]
      
    LLS=PTR_NEW(LLS)
    
  ;RETURN
  ENDIF
  
  IF N_ELEMENTS(INFOS) EQ 0 THEN INFOS=STRARR(N_ELEMENTS(names))
  IF N_ELEMENTS(IS_FINALIZE) EQ 0 THEN IS_FINALIZE=0
  IF N_ELEMENTS(IS_FREE_PTR) EQ 0 THEN IS_FREE_PTR=1
  
  OPENW,FID,OFILE,/GET_LUN
  HEADER=['<?xml version="1.0" encoding="UTF-8"?>',$
    '<kml xmlns="http://earth.google.com/kml/2.0">', $
    '<Document>']
    
  PRINTF,FID,HEADER,FORMAT='(A)'
  
  TMPSTR=['    <Style id="yellowLineGreenPoly">',  $
    '      <LineStyle>       ',  $
    '        <color>7f00ffff</color>',  $
    '        <width>2</width>     ',  $
    '      </LineStyle>     ',  $
    '      <PolyStyle>       ',  $
    '        <color>7f00ff00</color>',  $
    '      </PolyStyle>   ',  $
    '    </Style>']
  PRINTF,FID,TMPSTR,FORMAT='(A)'
  
  FOR I=0,N_ELEMENTS(names)-1 DO BEGIN
    PRINTF,FID,'    <Placemark>',FORMAT='(A)'
    PRINTF,FID,'      <name>'+names[I]+'</name>',FORMAT='(A)'
    PRINTF,FID,'      <description>'+names[I]+INFOS[i]+'</description>',FORMAT='(A)'
    PRINTF,FID,'      <visibility>1</visibility>',FORMAT='(A)'
    PRINTF,FID,'      <styleUrl>#yellowLineGreenPoly</styleUrl>',FORMAT='(A)'
    PRINTF,FID,'      <LineString>',FORMAT='(A)'
    PRINTF,FID,'        <tessellate>1</tessellate>',FORMAT='(A)'
    PRINTF,FID,'        <altitudeMode>clampToGround</altitudeMode>',FORMAT='(A)'
    TMPSTR='        <coordinates>'
    FOR PI=0, N_ELEMENTS((*LLS[I])[0,*])-1 DO BEGIN
      TMPSTR=TMPSTR+STRING(STRTRIM(STRING((*LLS[I])[0,PI],FORMAT='(F20.15)'),2),STRTRIM(STRING((*LLS[I])[1,PI],FORMAT='(F20.15)'),2),'0', FORMAT='(1X,A,",",A,",",A)')
    ENDFOR
    ;link the last vertex to the first one
    IF IS_FINALIZE EQ 1 THEN BEGIN
      PI=0
      TMPSTR=TMPSTR+STRING(STRTRIM(STRING((*LLS[I])[0,PI],FORMAT='(F20.15)'),2),STRTRIM(STRING((*LLS[I])[1,PI],FORMAT='(F20.15)'),2),'0', FORMAT='(1X,A,",",A,",",A)')
    ENDIF
    TMPSTR=TMPSTR+'</coordinates>'
    PRINTF,FID,TMPSTR,FORMAT='(A)'
    PRINTF,FID,'      </LineString>',FORMAT='(A)'
    PRINTF,FID,'    </Placemark>',FORMAT='(A)'
  ENDFOR
  
  PRINTF,FID,'   </Document>',FORMAT='(A)'
  PRINTF,FID,'</kml>',FORMAT='(A)'
  
  FREE_LUN,FID
  
  IF IS_FREE_PTR EQ 1 THEN FOR I=0,N_ELEMENTS(LLS)-1 DO  IF PTR_VALID(LLS[I]) THEN PTR_FREE,LLS[I]
END
