PRO SAR_S1_MANIFEST2OUTLINE, path

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
  
    path='C:\Downloads\esa.data\S1'
    path='C:\Downloads\esa.data\S1\tmp'
    
  ENDIF
  
  
  files=FILE_SEARCH(path+PATH_SEP()+'*.manifest.safe', count=nf)
  IF nf LE 0 THEN BEGIN
    RETURN
  ENDIF
  
  regions=REPLICATE(PTR_NEW(),nf)
  nps=INTARR(nf)
  names=STRARR(nf)
  obtyps=INTARR(nf)
  
  FOR fi=0, nf-1 DO BEGIN
    file=files[fi]
    names[fi]=GETFILENAME(file)
    ;
    lines=read_txt(file)
    line=grepi(lines,'coordinates')
    ; <gml:coordinates>32.437653,89.348289 32.836613,91.993134 31.158100,92.323769 30.756710,89.728622</gml:coordinates>
    line=STRTRIM(line,2)
    line_p1=STRSPLIT(line,'>',/extract)
    line_p2=STRSPLIT(line_p1[1],'<',/extract)
    tmp=line_p2[0]
    tmp_p=STRSPLIT(tmp,/extract)
    nps[fi]=N_ELEMENTS(tmp_p)+1
    xys=DBLARR(2,nps[fi])
    FOR pi=0,nps[fi]-2 DO BEGIN
      tmp_xy=STRSPLIT(tmp_p[pi],',',/extract)
      xys[*,pi]=DOUBLE(tmp_xy[[1,0]])
    ENDFOR
    xys[*,nps[fi]-1]=xys[*,0]
    ;print,xys
    regions[fi]=PTR_NEW(xys)
    
    ;descending/ascending
    line=grepi(lines,'pass')
    line1=strrep(line,'/','')
    line2=STRSPLIT(line1,'<s1:pass>',/extract)
    CASE line2[1] OF
      'ASCENDING': obtyps[fi]=0
      'DESCENDING': obtyps[fi]=1
    ENDCASE
    
  ENDFOR
  
  ;stop
  pos=WHERE(obtyps EQ 0)
  HELP,pos
  IF pos[0] NE -1 THEN BEGIN
    ofile=path+PATH_SEP()+'outline_ascending.shp'
    SHP_POLYGON,  $
      ofile,   $ ;output shapefile name
      region=regions[pos],   $ ;x,y coorinates of each polygons (pointer type)
      nps=nps[pos], $  ;number of point pairs for each polygon
      attdat=names[pos]
      
    ofile2=desuffix(ofile)+'.kml'
    POLYLINE_SHP2KML, oFILE, OFILE2
  ENDIF
  ;
  pos=WHERE(obtyps EQ 1)
  HELP,pos
  IF pos[0] NE -1 THEN BEGIN
    ofile=path+PATH_SEP()+'outline_descending.shp'
    SHP_POLYGON,  $
      ofile,   $ ;output shapefile name
      region=regions[pos],   $ ;x,y coorinates of each polygons (pointer type)
      nps=nps[pos], $  ;number of point pairs for each polygon
      attdat=names[pos]
      
    ofile2=desuffix(ofile)+'.kml'
    POLYLINE_SHP2KML, oFILE, OFILE2
  ENDIF
  
END