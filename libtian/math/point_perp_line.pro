;+
; :Description:
;    Calulate the perpendicular distance from a point to a line.
;
; :Params:
;    a1 - the first point defining the straight line
;    b1 - the second point defining the line
;    c1 - the outer point
;    d1 - the intersect point of line (a1,b1) and its perpendicular line passing c1.
;
;
; :Modifications:
;   +Create on Tue, Nov 03, 2015  4:10:57 PM by tianyf
;
; :Author: tianyf
;-
PRO POINT_PERP_LINE, a1, b1, c1, d1
  IF N_PARAMS() LT 3 THEN BEGIN
    b1=[100.320,    25.841]*1d0
    c1=[104.769,    27.037]*1d0
    a1=[105.687,    29.356]*1d0
    
;    ;for vertical line    
;    b1=[100.320,    25.841]*1d0
;    c1=[104.769,    27.037]*1d0
;    a1=[100.320,    29.356]*1d0
;    
;    ;for horizontal line
;    b1=[100.320,    25.841]*1d0
;    c1=[104.769,    27.037]*1d0
;    a1=[105.687,    25.841]*1d0
  ENDIF
  
  ;if vertical line
  if abs(b1[0]-a1[0]) le 1e-6 then begin
    rate1=!values.d_infinity
    intercept1=0
    rate2=0
    intercept2=c1[1]
    d1=[a1[0],c1[1]]
    goto, end_prog
  endif
  
  ;if horizontal line
  if abs(b1[1]-a1[1]) le 1e-6 then begin
    rate2=!values.d_infinity
    intercept1=a1[1]
    rate1=0
    intercept2=0
    d1=[c1[0],a1[1]]
    goto, end_prog
  endif
  
  rate1=(b1[1]-a1[1])/(b1[0]-a1[0])
  intercept1=a1[1]-rate1*a1[0]
  alpha1=ATAN(rate1)
  alpha2=alpha1+!dpi/2
  rate2=TAN(alpha2)
  intercept2=c1[1]-rate2*c1[0]
  
  x1=a1[0]
  y1=rate2*(x1-c1[0])+c1[1]
  
  x2=(intercept1-intercept2)/(rate2-rate1)
  y2=rate1*x2+intercept1
  
  d1=[x2,y2]
  
  end_prog:
  ;stop
  IF N_PARAMS() LT 3 THEN BEGIN
    WINDOW,1
    PLOT,[a1[0],b1[0]],[a1[1],b1[1]],color='0'x,background='ffffff'x,/ynozero, $
      /iso,yrange=[23,30],xrange=[98,110],thick=3
    PLOTS,[a1[0],b1[0]],[a1[1],b1[1]],color='0'x,psym=5
    PLOTS,c1[0],c1[1],psym=2,color='0'x
    ;OPLOT,[c1[0],x1],[c1[1],y1],color='0'x,thick=3
    
    x=INDGEN(1000)
    PLOTS,x,x*rate1+intercept1,color='00ff00'x,psym=-1
    PLOTS,x,x*rate2+intercept2,color='00ff00'x,psym=-1
    PLOTS,d1[0],d1[1],psym=4,color='0'x,symsize=2
    
    jfile='J:\phd\expt\gpsf\external\addon\vel\fault\point_perp_line.eg.jpg'
    write_jpeg, jfile, tvrd(true=1),true=1,quality=100
  ENDIF
END