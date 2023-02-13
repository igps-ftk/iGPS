PRO CAL_FAULT_STRIKE, file, $
    fa=fa,  $
    xys=xys,  $
    auto_strike=auto_strike,  $
    slope=slope,  $
    strike=strike,  $
    dummy=dummy
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_ELEMENTS(file) EQ 0 AND N_ELEMENTS(fa) EQ 0 AND N_ELEMENTS(xys) EQ 0 THEN BEGIN
    fa='fa_xsh'
  ENDIF
  
  IF N_ELEMENTS(xys) EQ 0 THEN BEGIN
    IF  N_ELEMENTS(file) EQ 0 AND N_ELEMENTS(fa) NE 0 THEN BEGIN
      file=fa_name2file(fa)
    ENDIF
    
    ;stop
    lines_fvec=read_txt(file)
    lines_fvec2=STRTRIM(lines_fvec,2)
    pos=WHERE(strmids(lines_fvec2,0,1) NE '>')
    IF N_ELEMENTS(pos) LT 2 THEN BEGIN
      PRINT,'['+prog+']ERROR: invalid fault line vector file <'+ffile+'>!!'
      RETURN
    ENDIF
    xys=DOUBLE(str_lines2arr(lines_fvec2[pos]))
  ;xys[1,0]=28
  ;stop
  ENDIF
  
  ;strike=75d0*!dpi/180d0
  
  ;spacing=
  IF N_ELEMENTS(auto_strike) EQ 0 THEN BEGIN
    auto_strike=3 ;use starting-ending points to calcualte strike angle
  auto_strike=2 ;use mean of strikes of all segments
  ;auto_strike=1 ;use individual strike for each segment
  ENDIF
  
  IF N_ELEMENTS(isplot) EQ 0 THEN isplot=0
  IF N_PARAMS() LT 1 THEN isplot=1
  
  
  ;  ;convert km to degree using the mean latitude
  ;  latmid=MEAN(xys[1,*])
  ;  km_per_deg=MAP_2POINTS(0,latmid,1,latmid,/meters)*1d-3
  ;
  ;
  lonmin=MIN(xys[0,*],max=lonmax)
  latmin=MIN(xys[1,*],max=latmax)
 
  IF auto_strike EQ 1 OR auto_strike EQ 2 THEN BEGIN
    ;    1:
    ;    2: BEGIN ;no strike from input
    ;calculate a mean strike for all fault segments
    slope=DBLARR(N_ELEMENTS(xys[0,*])-1)
    strike=slope
    FOR i=0,N_ELEMENTS(xys[0,*])-1-1 DO BEGIN
      xy1=xys[*,i]
      xy2=xys[*,i+1]
      CAL_LINE_SLOPE,xy1,xy2,intercept=intercept_i,strike=strike_i,rate=rate_I,slope=slope_i
      ;PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],color='ffff00'x,thick=2,psym=-2
;      tmp=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
;      slope=ATAN(tmp)
;      IF slope LT 0 THEN BEGIN
;        slope=slope+2*!dpi
;      ENDIF
      ;      IF  xy2[0] GT xy1[0]  THEN BEGIN
      ;        strike=!DPI/2-slope
      ;      ENDIF ELSE BEGIN
      ;        strike=!DPI*3d0/2-slope
      ;      ENDELSE
      ;      strikes[i]=strike
      slope[i]=slope_i
      strike[i]=strike_i
    ;PRINT,'fault strike for segment ',i+1,' is: ',(!dpi/2d0-slope)*180/!dpi, ' degrees'
    ;stop
    ENDFOR
    
    strike_avg=MEAN(strike)
    CASE auto_strike OF
      1: BEGIN
        PRINT,'fault strike for segment ',i+1,' is: ',strike[i]*180/!dpi, ' degrees'
      END
      2: BEGIN
        PRINT, '['+PROG+']Mean fault strike is: ',strike_avg*180/!dpi, ' degrees'        
      END
    ENDCASE
  
  ENDIF
  ;
  IF auto_strike  EQ 3 THEN BEGIN ;calculate a  strike using the staring and ending points of fault segments
    xy1=xys[*,0]
    xy2=xys[*,N_ELEMENTS(xys[0,*])-1]
    CAL_LINE_SLOPE,xy1,xy2,intercept=intercept,strike=strike,rate=rate,slope=slope
  ENDIF
  
  
  IF N_PARAMS() LT 1 THEN BEGIN
    xmin=MIN(xys[0,*],max=xmax)
    ymin=MIN(xys[1,*],max=ymax)
    WINDOW,1;,xsize=1800,ysize=800
    !p.MULTI=-1
    
    PLOT,[xmin,xmax],[ymin,ymax],background='ffffff'x,color='0'x, $
      title='fault: '+fa+'   strike mode: '+STRTRIM(auto_strike,2), $
      /nodata,/ynozero,/iso
    PLOTS,xys[0,*],xys[1,*],color='ff0000'x,psym=-2
    
    IF auto_strike EQ 3 THEN BEGIN
      PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],color='ffff00'x,thick=2,psym=-2
      XYOUTS,(xy1[0]+xy2[0])/2d0,(xy1[1]+xy2[1])/2d0,STRING(slope*180/!dpi,strike*180/!dpi,format='(f7.2,1x,f7.2)'),/data,color='0'x
      XYOUTS,xy1[0],xy1[1],'P1',color='ff0000'x,charsize=1.5
      XYOUTS,xy2[0],xy2[1],'P2',color='ff0000'x,charsize=1.5
    ENDIF    
    IF auto_strike EQ 1 or auto_strike eq 2 THEN BEGIN
      for i=0, n_elements(slope)-1 do begin
      
      xy1=xys[*,i]
      xy2=xys[*,i+1]
      PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],color='ffff00'x,thick=2,psym=-2
      XYOUTS,(xy1[0]+xy2[0])/2d0,(xy1[1]+xy2[1])/2d0,STRING(slope[i]*180/!dpi,strike[i]*180/!dpi,format='(f7.2,1x,f7.2)'),/data,color='0'x
      XYOUTS,xy1[0],xy1[1],'P1',color='ff0000'x,charsize=1.
      XYOUTS,xy2[0],xy2[1],'P2',color='ff0000'x,charsize=1.
      endfor
    ENDIF
    
    ;STOP
  ENDIF
  
  
  PRINT, '['+PROG+']INFO:normal end.'
  
END