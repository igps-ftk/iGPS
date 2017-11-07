;
PRO KML_POINT, FILE, SITES=SITES, LLH=LLH
  PROG='KML_POINT'
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'['+PROG+']Usage: '+PROG+', file, sites=sites, llh=llh',FORMAT='(A)'
    RETURN
  ENDIF
  
  OPENW,FID,FILE,/GET_LUN
  HEADER=['<?xml version="1.0" encoding="UTF-8"?>',$
    '<kml xmlns="http://earth.google.com/kml/2.0">', $
    '<Document>']
    
  FOR I=0,N_ELEMENTS(HEADER)-1 DO BEGIN
    PRINTF,FID,HEADER[I],FORMAT='(A)'
  ENDFOR
  
  FOR I=0,N_ELEMENTS(SITES)-1 DO BEGIN
    PRINTF,FID,'    <Placemark>',FORMAT='(A)'
    PRINTF,FID,'      <name>'+SITES[I]+'</name>',FORMAT='(A)'
    PRINTF,FID,'      <description>'+SITES[I]+'</description>',FORMAT='(A)'
    PRINTF,FID,'      <visibility>1</visibility>',FORMAT='(A)'
    PRINTF,FID,'      <Point>',FORMAT='(A)'
    PRINTF,FID,'        <extrude>0</extrude>',FORMAT='(A)'
    PRINTF,FID,'        <altitudeMode>clampToGround</altitudeMode>',FORMAT='(A)'
    PRINTF,FID,'        <coordinates>'+STRING(LLH[0,I],FORMAT='(F20.15)')+','+STRING(LLH[1,I],FORMAT='(F20.15)')+',0</coordinates>',FORMAT='(A)'
    PRINTF,FID,'      </Point>',FORMAT='(A)'
    PRINTF,FID,'    </Placemark>',FORMAT='(A)'
  ENDFOR
  
  PRINTF,FID,'   </Document>',FORMAT='(A)'
  PRINTF,FID,'</kml>',FORMAT='(A)'
  
  FREE_LUN,FID
END
