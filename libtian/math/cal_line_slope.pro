;;y=a*x+b
;FUNCTION CAL_LINE_SLOPE,xy1,xy2,intercept=intercept,strike=strike,rate=rate
;
;  IF ABS(xy1[0]-xy2[0]) LE 1d-6 THEN BEGIN
;    slope=!values.D_NAN
;    intercept=!values.D_NAN
;    strike=0d0
;    IF xy1[1] GT xy2[1] THEN BEGIN
;      strike=strike+!dpi
;    ;PRINT,strike*180/!dpi
;    ENDIF
;
;  ENDIF ELSE BEGIN
;    rate=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
;    slope=ATAN(rate)
;    intercept=xy1[1]-rate*xy1[0]
;
;    strike=!dpi/2-slope
;    ;PRINT,strike*180/!dpi
;
;    ;stop
;    IF ABS(xy2[0]-xy1[0])/(xy2[0]-xy1[0]) EQ -1 THEN BEGIN
;      strike=strike+!dpi
;    ;PRINT,strike*180/!dpi
;    ENDIF
;  ENDELSE
;
;  RETURN,slope
;END

PRO CAL_LINE_SLOPE,xy1,xy2,intercept=intercept,strike=strike,rate=rate,slope=slope
  IF N_PARAMS() LT 2 THEN BEGIN
    xy1=[1d0,3]
    xy2=[6d0,9]
    
    xy2=[1d0,3]
    xy1=[6d0,9]
    
    xy1=[5d0,-3]
    xy2=[-5d0,9]
    ;
    xy2=[5d0,-3]
    xy1=[-5d0,9]
    
    
    ;  ;horizontal lines
    ;  xy1=[1d0,3]
    ;  xy2=[6d0,3]
    ;
    ;  xy2=[1d0,3]
    ;  xy1=[6d0,3]
    
    ;  ;vertical lines
    ;  xy1=[1d0,3]
    ;  xy2=[1d0,9]
    ;  ;
    ;  xy2=[1d0,3]
    ;  xy1=[1d0,9]
    
    
    xy1=[5d0,8]
    xy2=[-5d0,-9]
  ENDIF
  ;
  
  ;slope=cal_line_slope(xy1,xy2,intercept=intercept,strike=strike,rate=rate)
  IF ABS(xy1[0]-xy2[0]) LE 1d-6 THEN BEGIN
    slope=!values.D_NAN
    intercept=!values.D_NAN
    strike=0d0
    IF xy1[1] GT xy2[1] THEN BEGIN
      strike=strike+!dpi
    ;PRINT,strike*180/!dpi
    ENDIF
    
  ENDIF ELSE BEGIN
    rate=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
    slope=ATAN(rate)
    intercept=xy1[1]-rate*xy1[0]
    
    strike=!dpi/2-slope
    ;PRINT,strike*180/!dpi
    
    ;stop
    IF ABS(xy2[0]-xy1[0])/(xy2[0]-xy1[0]) EQ -1 THEN BEGIN
      strike=strike+!dpi
    ;PRINT,strike*180/!dpi
    ENDIF
  ENDELSE
  
  IF N_PARAMS() LT 2 THEN BEGIN
  
    WINDOW,1,xsize=512,ysize=512
    PLOT,[xy1[0],xy2[0]],[xy1[1],xy2[1]],thick=2,psym=-2,background='ffffff'x,color='0'x, $
      /iso, $
      xrange=[-10,10],yrange=[-10,10],  $
      title='slope: '+STRTRIM(slope*180/!dpi,2)+ ' degrees',  $
      xtitle='strike: '+STRTRIM(strike*180/!dpi,2)+ ' degrees'
    OPLOT,[-1d10,1d10],[0,0],linestyle=1,color='0'x
    OPLOT,[0,0],[-1d10,1d10],linestyle=1,color='0'x
    OPLOT,[-1d10,1d10],REPLICATE((xy1[1]+xy2[1])/2d0,2),linestyle=2,color='0'x
    OPLOT,REPLICATE((xy1[0]+xy2[0])/2d0,2),[-1d10,1d10],linestyle=2,color='0'x
    XYOUTS,(xy1[0]+xy2[0])/2d0,(xy1[1]+xy2[1])/2d0,STRTRIM(slope*180/!dpi,2)+ ' degrees',/data,color='0000ff'x
    XYOUTS,xy1[0],xy1[1],'P1',color='ff0000'x,charsize=1.5
    XYOUTS,xy2[0],xy2[1],'P2',color='ff0000'x,charsize=1.5
    
    xs=INDGEN(21)-10
    ys=rate*xs+intercept
    PLOTS,xs,ys,psym=1,color='000fff'x
  ENDIF
END