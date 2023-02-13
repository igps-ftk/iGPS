;
PRO KML_POINT, FILE, SITES=SITES, LLH=LLH
  PROG='KML_POINT'
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'['+PROG+']Usage: '+PROG+', file, sites=sites, llh=llh',FORMAT='(A)'
    RETURN
  ENDIF
  
  LONS=REFORM(LLH[0,*])
  LATS=REFORM(LLH[1,*])
  IF N_ELEMENTS(LLH[*,0]) EQ 3 THEN BEGIN
    HGTS=REFORM(LLH[2,*])
    MODE_ELEV='absolute'
  ENDIF ELSE BEGIN
    HGTS=REPLICATE(0,N_ELEMENTS(LONS))
    MODE_ELEV='clampToGround'
  ENDELSE
  
  
  OPENW,FID,FILE,/GET_LUN
  HEADER=[  $
    ;'<?xml version="1.0" encoding="UTF-8"?>',$
    ;'<kml xmlns="http://earth.google.com/kml/2.0">', $
    '<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">',  $
    
    '<Document>', $
    '<name>'+getfilename(file)+'</name>',  $
      '<Style id="s_ylw-pushpin_hl">                                         ', $
  ' <IconStyle>                                                          ', $
  '   <color>ff00aaff</color>                                            ', $
  '   <scale>0.354545</scale>                                            ', $
  '   <Icon>                                                             ', $
  '     <href>http://maps.google.com/mapfiles/kml/shapes/donut.png</href>', $
  '   </Icon>                                                            ', $
  ' </IconStyle>                                                         ', $
  ' <LabelStyle>                                                         ', $
  '   <scale>0</scale>                                                   ', $
  ' </LabelStyle>                                                        ', $
  ' <ListStyle>                                                          ', $
  ' </ListStyle>                                                         ', $
  '</Style>                                                              ', $
  '<Style id="s_ylw-pushpin">                                            ', $
  ' <IconStyle>                                                          ', $
  '   <color>ff00aaff</color>                                            ', $
  '   <scale>0.3</scale>                                                 ', $
  '   <Icon>                                                             ', $
  '     <href>http://maps.google.com/mapfiles/kml/shapes/donut.png</href>', $
  '   </Icon>                                                            ', $
  ' </IconStyle>                                                         ', $
  ' <LabelStyle>                                                         ', $
  '   <scale>0</scale>                                                   ', $
  ' </LabelStyle>                                                        ', $
  ' <ListStyle>                                                          ', $
  ' </ListStyle>                                                         ', $
  '</Style>                                                              ', $
  '<StyleMap id="m_ylw-pushpin">                                         ', $
  ' <Pair>                                                               ', $
  '   <key>normal</key>                                                  ', $
  '   <styleUrl>#s_ylw-pushpin</styleUrl>                                ', $
  ' </Pair>                                                              ', $
  ' <Pair>                                                               ', $
  '   <key>highlight</key>                                               ', $
  '   <styleUrl>#s_ylw-pushpin_hl</styleUrl>                             ', $
  ' </Pair>                                                              ', $
  '</StyleMap>                                                           ', $
  '']
    
  FOR I=0,N_ELEMENTS(HEADER)-1 DO BEGIN
    PRINTF,FID,HEADER[I],FORMAT='(A)'
  ENDFOR
  
  FOR I=0,N_ELEMENTS(SITES)-1 DO BEGIN
    PRINTF,FID,'    <Placemark>',FORMAT='(A)'
    PRINTF,FID,'      <name>'+SITES[I]+'</name>',FORMAT='(A)'
    PRINTF,FID,'      <description>'+SITES[I]+'</description>',FORMAT='(A)'
    PRINTF,FID,'      <styleUrl>#m_ylw-pushpin</styleUrl>',FORMAT='(A)'
    PRINTF,FID,'      <visibility>1</visibility>',FORMAT='(A)'
    PRINTF,FID,'      <Point>',FORMAT='(A)'
    PRINTF,FID,'        <extrude>0</extrude>',FORMAT='(A)'
    PRINTF,FID,'        <altitudeMode>'+MODE_ELEV+'</altitudeMode>',FORMAT='(A)'
    PRINTF,FID,'        <coordinates>'+STRTRIM(STRING(LONS[I],FORMAT='(F20.15)'),2)+','+STRTRIM(STRING(LATS[I],FORMAT='(F20.15)'),2)+','+STRTRIM(STRING(HGTS[I],FORMAT='(F20.15)'),2)+'</coordinates>',FORMAT='(A)'
    PRINTF,FID,'      </Point>',FORMAT='(A)'
    PRINTF,FID,'    </Placemark>',FORMAT='(A)'
  ENDFOR
  
  PRINTF,FID,'   </Document>',FORMAT='(A)'
  PRINTF,FID,'</kml>',FORMAT='(A)'
  
  FREE_LUN,FID
END
