;+
; :DESCRIPTION:
;    Calulate the intersect point (d2) of a line (defined by two points: a1, b1) and
;      a line defined by one point (c2) and its rate (rate2).
;
; :PARAMS:
;    a1 - the first point defining the straight line
;    b1 - the second point defining the line
;    c2 - the outer point
;    rate2 - the rate of the second line (passing c2)
;    d2 - the intersect point of line (a1,b1) and the line passing c2.
;
;
; :Modifications:
;   +Create on Tue, Nov 05, 2015 11:10:57 PM by tianyf
;
; :AUTHOR: tianyf
;-
PRO POINT_CROSS_LINE, a1, b1, c2, rate2, d2
  IF N_PARAMS() LT 4 THEN BEGIN
;    b1=[100.320,    25.841]*1d0
;    c2=[104.769,    27.037]*1d0
;    a1=[105.687,    29.356]*1d0
;    rate2=.35d0
;    rate2=-.95d0
;    ;rate2=0d0
;    ;rate2=!values.D_INFINITY
    
  ;    ;for vertical line
  ;    b1=[100.320,    25.841]*1d0
  ;    c2=[104.769,    27.037]*1d0
  ;    ;c2=[100.320,    27.037]*1d0
  ;    a1=[100.320,    29.356]*1d0
  ;    rate2=.35d0
  ;    ;rate2=0
  ;    ;rate2=!values.D_INFINITY
  ;
;      ;for horizontal line
;      b1=[100.320,    25.841]*1d0
;      c2=[104.769,    27.037]*1d0
;      a1=[105.687,    25.841]*1d0
;      rate2=.35d0
;      ;rate2=0
;      ;rate2=tan(!dpi/2)
;      ;rate2=!values.D_INFINITY
      
      a1=[86.7303772579d0, 29.4418913671d0]
      b1=[85.5126849284d0, 29.4418913671d0]
      c2=[87.1,30]
      rate2=.5d0
  ENDIF
  
  ;stop
  xmin=MIN([a1[0],b1[0]],max=xmax)
  
  ;if horizontal line
  IF ABS(b1[1]-a1[1]) LE 1e-6 THEN BEGIN
    ;rate2=!values.D_INFINITY
    intercept1=a1[1]
    rate1=0
    IF FINITE(rate2) NE 1 THEN BEGIN
      d2=[c2[0],a1[1]]
      intercept2=!values.D_INFINITY
      GOTO, end_prog
    ENDIF
  ENDIF
  
  intercept2=c2[1]-rate2*c2[0]
  
  ;if vertical line
  IF ABS(b1[0]-a1[0]) LE 1e-6 THEN BEGIN
    rate1=!values.D_INFINITY
    intercept1=0
    
    
    ;rate2=0
    ;intercept2=c2[1]
    IF FINITE(rate2) NE 1 THEN BEGIN
      IF ABS(b1[0]-c2[0]) LE 1e-6 THEN BEGIN ;if two lines are identical, then stop for further debug
        STOP
      ENDIF
    ;      d2=[c2[0],a1[1]]
    ;      intercept2=!values.D_INFINITY
    ;      GOTO, end_prog
    ENDIF
    d2=[a1[0],rate2*a1[0]+intercept2]
    ;stop
    GOTO, end_prog
  ENDIF
  
  rate1=(b1[1]-a1[1])/(b1[0]-a1[0])
  intercept1=a1[1]-rate1*a1[0]
  alpha1=ATAN(rate1)
  alpha2=alpha1+!dpi/2
  ;rate2=TAN(alpha2)
  
  x1=a1[0]
  
  
  IF FINITE(rate2) NE 1 THEN BEGIN
    x2=c2[0]
    y2=rate1*x2+intercept1
    intercept2=!values.D_INFINITY
  ENDIF ELSE BEGIN
    y1=rate2*(x1-c2[0])+c2[1]
    x2=(intercept1-intercept2)/(rate2-rate1)
    y2=rate1*x2+intercept1
  ENDELSE
  
  
  d2=[x2,y2]
  
  end_prog:
  ;stop
  IF N_PARAMS() LT 3 THEN BEGIN
    print,a1
    print,b1
    print,c2
    PRINT,d2
    PRINT,FINITE(d2)
    print,a1[1]-d2[1]
    WINDOW,1
    PLOT,[a1[0],b1[0]],[a1[1],b1[1]],color='0'x,background='ffffff'x,/ynozero, $
      /iso;,yrange=[23,30],xrange=[98,110],thick=3
    PLOTS,[a1[0],b1[0]],[a1[1],b1[1]],color='0'x,psym=5
    PLOTS,c2[0],c2[1],psym=2,color='0'x
    ;OPLOT,[c1[0],x1],[c1[1],y1],color='0'x,thick=3
    
    x=INDGEN(1000)
    PLOTS,x,x*rate1+intercept1,color='00ff00'x,psym=-1
    PLOTS,x,x*rate2+intercept2,color='00ff00'x,psym=-1
    PLOTS,d2[0],d2[1],psym=4,color='0'x,symsize=2
    
    jfile=FILEPATH('point_cross_line.eg.jpg',root_dir=!igps_root,subdirectory=['libtian','math'])
    WRITE_JPEG, jfile, TVRD(true=1),true=1,quality=100
  ENDIF
END