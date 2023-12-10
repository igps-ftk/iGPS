;calculate fault strikes

PRO FA_STRIKE_CAL,  file, $   ;fault file (if given); otherwise fault coordinate (xys_fa) should be given
    xys_fa=xys_fa,  $   ;fault coordinates (lon, lat) array (2*N)
    strikes=strikes,  $ ;output strikes
    auto_strike=auto_strike ;strike mode
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  is_debug=0
  IF is_debug THEN BEGIN
    file='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    file='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    auto_strike=2
  ENDIF
  
  IF N_ELEMENTS(auto_strike) EQ 0 THEN auto_strike=2
  ;    auto_strike=3 ;use starting-ending points to calcualte strike angle
  ;    auto_strike=2 ;use mean of strikes of all segments
  ;    auto_strike=1 ;use individual strike for each segment
  
  
  
  IF N_ELEMENTS(file) EQ 0 || file EQ '' THEN BEGIN
    IF N_ELEMENTS(xys_fa) LT 4 THEN BEGIN
      PRINT,'['+PROG+']ERROR: no fault file or coordiantes given!!'
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
    READ_PSXY,   $
      file,   $ ;input file
      region=regions,   $ ;x,y coorinates of each polygons (pointer type)
      nps=nps, $  ;number of point pairs for each polygon
      count=count,  $ ;number of polygons
      igpsmode=igpsmode,  $ ;whether skip comments lines (i.e., non-blank first column lines)
      names=names   ;region names (if exist)
      
    xys_fa=*regions[0]
  ENDELSE
  
  ;stop
  IF auto_strike EQ 3 THEN BEGIN ;calculate a strike using the staring and ending points of fault
    xy1=xys_fa[*,0]
    xy2=xys_fa[*,N_ELEMENTS(xys_fa[0,*])-1]
    ;PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],color='ffff00'x,thick=2,psym=-2
    tmp=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
    slope=ATAN(tmp)
    strikes=slope
  ;PRINT, '['+PROG+']Start-end fault strike is: ',strike_se*180/!dpi, ' degrees'
  ENDIF
  
  IF auto_strike EQ 1 OR auto_strike EQ 2 THEN BEGIN
  
    strikes=DBLARR(N_ELEMENTS(xys_fa[0,*])-1)
    FOR i=0,N_ELEMENTS(xys_fa[0,*])-1-1 DO BEGIN
      xy1=xys_fa[*,i]
      xy2=xys_fa[*,i+1]
      ;PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],color='ffff00'x,thick=2,psym=-2
      IF ABS(xy1[0]-xy2[0]) LE 1d-6 AND ABS(xy1[1]-xy2[1]) LE 1d-6  THEN BEGIN
        PRINT,'['+PROG+']ERROR: lines have duplicate lines('+STRTRIM(i+1,2)+', '+STRTRIM(i+2,2)+')!!'
        RETURN
      ENDIF
      tmp=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
      slope=ATAN(tmp)
      strikes[i]=slope
    ;PRINT,'fault strike for segment ',i+1,' is: ',slope*180/!dpi, ' degrees'
    ;STOP
    ENDFOR
    
    IF auto_strike EQ 2 THEN BEGIN
      strike_avg=MEAN(strikes)
      strikes=strike_avg
    ENDIF
  ENDIF
  
  IF is_debug THEN BEGIN
    PRINT, '['+PROG+']INFO: mode of fault strike calculation: ', auto_strike
    PRINT, '['+PROG+']INFO: fault strike is: ',strikes*180/!dpi, ' degrees'
  ENDIF
  
;STOP
END