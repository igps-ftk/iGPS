;
PRO KML_LINE, FILE, SITES=SITES, LLH=LLH, INFOS=INFOS
  PROG='KML_LINE'
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'['+PROG+']Usage: '+PROG+', file, sites=sites, llh=llh',FORMAT='(A)'
    ;TEST
    FILE='C:\tmp\test_arrow.kml'
    SITES='NQXM'
    LON=91.738888888888894D0
    LAT=31.152777777777775
    VN=11D0
    VE=50D0
    SF=5D-3
    
    PRINT,'ARROW LENGTH:', VN*SF, VE*SF
    
    LEN_ERRARR=.02D0 ;IN DEGREES
    ANG_ERRARR=30D0 ;ANGLE IN DEGREES
    PRINT,'ERROR ARROW LENGTH:', LEN_ERRARR
    
    A0=[LON+VN*SF,LAT+VE*SF]
    R1=VN/VE
    ALPHA=ATAN(VN,VE)*180D0/!DPI
    ALPHA1=(ALPHA+(180D0-ANG_ERRARR))*!DPI/180D0
    ALPHA2=-1D0*(180D0-ALPHA-ANG_ERRARR)*!DPI/180D0
    
    PRINT,ALPHA
    PRINT,ALPHA2*180D0/!DPI
    
    
    A1=[LON+VE*SF,LAT+VN*SF]
    A2=[LON+VE*SF+LEN_ERRARR*COS(ALPHA1),LAT+VN*SF+LEN_ERRARR*SIN(ALPHA1)]
    PRINT,ALPHA1*180D0/!DPI
    PRINT,SIN(ALPHA1),COS(ALPHA1)
    PRINT,LON+VE*SF,LEN_ERRARR*COS(ALPHA1),LAT+VN*SF,LEN_ERRARR*SIN(ALPHA1)
    A3=[LON+VE*SF+LEN_ERRARR*COS(ALPHA2),LAT+VN*SF+LEN_ERRARR*SIN(ALPHA2)]
    
    TMP=[ [LON,LAT],[A1],[A2],[A3],[A1] ]
    
    ;STOP
    
    LLH=PTR_NEW(TMP)
    
  ;RETURN
  ENDIF
  
  IF N_ELEMENTS(INFOS) EQ 0 THEN INFOS=STRARR(N_ELEMENTS(SITES))
  
  OPENW,FID,FILE,/GET_LUN
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
  
  FOR I=0,N_ELEMENTS(SITES)-1 DO BEGIN
    PRINTF,FID,'    <Placemark>',FORMAT='(A)'
    PRINTF,FID,'      <name>'+SITES[I]+'</name>',FORMAT='(A)'
    PRINTF,FID,'      <description>'+SITES[I]+INFOS[i]+'</description>',FORMAT='(A)'
    PRINTF,FID,'      <visibility>1</visibility>',FORMAT='(A)'
    PRINTF,FID,'      <styleUrl>#yellowLineGreenPoly</styleUrl>',FORMAT='(A)'
    PRINTF,FID,'      <LineString>',FORMAT='(A)'
    PRINTF,FID,'        <tessellate>1</tessellate>',FORMAT='(A)'
    PRINTF,FID,'        <altitudeMode>clampToGround</altitudeMode>',FORMAT='(A)'
    TMPSTR='        <coordinates>'
    FOR PI=0, N_ELEMENTS((*LLH[I])[0,*])-1 DO BEGIN
      TMPSTR=TMPSTR+STRING(STRTRIM(STRING((*LLH[I])[0,PI],FORMAT='(F20.15)'),2),STRTRIM(STRING((*LLH[I])[1,PI],FORMAT='(F20.15)'),2),'0', FORMAT='(1X,A,",",A,",",A)')
    ENDFOR
    TMPSTR=TMPSTR+'</coordinates>'
    PRINTF,FID,TMPSTR,FORMAT='(A)'
    PRINTF,FID,'      </LineString>',FORMAT='(A)'
    PRINTF,FID,'    </Placemark>',FORMAT='(A)'
  ENDFOR
  
  PRINTF,FID,'   </Document>',FORMAT='(A)'
  PRINTF,FID,'</kml>',FORMAT='(A)'
  
  FREE_LUN,FID
  
  FOR I=0,N_ELEMENTS(LLH)-1 DO  IF PTR_VALID(LLH[I]) THEN PTR_FREE,LLH[I]
END
