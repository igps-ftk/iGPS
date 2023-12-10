;+
; :DESCRIPTION:
;    Given a line segment (two vertices:xy1 and xy2),
;          a starting point on it (xy3),
;          and a distance,
;    return the point on the line with the given distance to the starting point (oxy).
;
; :PARAMS:
;    xy1 - coordinates of the first vertex of the line segment (in plane coordinate)
;    xy2 - coordinates of the second vertex of the line segment
;    xy3 - coordinates of the starting point (on the line segment)
;    distance - the distance in km
;
; :KEYWORDS:
;    oxy - the x/y of the output point
;
; :AUTHOR: tianyf on Wed, Jun 28, 2017 10:55:00 PM
;-
PRO GEO_LINE_XY_FAR_DIST_XY, xy1,  $ ;e.g. xy1=[100,32]
    xy2, $  ;e.g. xy2=[103,35]
    xy3,   $ ;e.g. xy3=xy1
    distance,   $ ;e.g. distance=100 km
    oxy=oxy
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 4 THEN BEGIN
    PRINT,'['+prog+']WARNING: invalid input parameters. Using testing ones!'
    xy1=[646925.89,       3870924.1]
    xy2=[647432.89,       3875237.1]
    
;    xy1=[647432.89,       3875237.1]
;    xy2=[646925.89,       3870924.1]
    
    xy1=[646925.89,       3875237.1]
    xy2=[647432.89,       3870924.1]
    
;    xy1=[647432.89,       3870924.1]
;    xy2=[646925.89,       3875237.1]
    
    xy3=xy1
    distance=2900  ;
    
    ;
    
  ENDIF
  
  ;first, derive the line equation (rate; b=intercept on the Y-axis)
  rate=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
  b=xy2[1]-rate*xy2[0]
  
  ;second, search for the optimal output point.
  ;currently, no analytic solution.
  nmax=100
  xs=INDGEN(nmax)*((xy2[0]-xy1[0])*1d0/nmax)+xy1[0]
  ys=xs*rate+b
  ;OPLOT,xs,ys,color='ff0000'x
  ds=DBLARR(nmax)
  FOR i=0,nmax-1 DO BEGIN
    ds[i]=SQRT( (xy3[0]-xs[i])^2+(xy3[1]-ys[i])^2 )
  ENDFOR
  tmp=MIN(ABS(ds-distance),indmin)
  oxy=[xs[indmin],ys[indmin]]
  
  IF N_PARAMS() LT 4 THEN BEGIN
    oldwin=!D.WINDOW
    WINDOW,/free
    PLOT,[xy1[0],xy2[0]],[xy1[1],xy2[1]],background='ffffff'x,color='0'x, $
      /ynozero,psym=-6,symsize=3,$
      xtitle='Longitude',ytitle='Latitude',title='Example for '+prog
    PLOTS,oxy[0],oxy[1],psym=5,color='0000ff'x,symsize=3
    XYOUTS,xy1[0],xy1[1],'xy1',color='0'x
    XYOUTS,xy2[0],xy2[1],'xy2',color='0'x
    XYOUTS,oxy[0],oxy[1],'oxy',color='0'x
    OPLOT,[xy1[0],oxy[0]],[xy1[1],oxy[1]],color='ff0000'x,linestyle=2,thick=2
    XYOUTS,(xy1[0]+oxy[0])/2,(xy1[1]+oxy[1])/2, $
      STRING(distance,format='(F9.2,"")'),color='0'x
    WSET,oldwin
    print,oxy
  ENDIF
  
;STOP
END