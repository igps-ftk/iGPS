;+
; :DESCRIPTION:
;    Calulate the intersectoin point of two straight lines
;
; :INPUT:
;    a1, b1 -  two points of the first line
;    c2, rate2 - one point and the rate of the second line
;
; :OUTPUT:
;    d2 - the intersection
;
;
; :Modifications:
;   +Create on Tue, Nov 05, 2015 11:10:57 PM by tianyf
;
; :AUTHOR: tianyf
;-
PRO LINE_INTERSECT_LINE, a1, b1, c2, rate2, d2

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 4 THEN BEGIN
    ;    b1=[100.320,    25.841]*1d0
    ;    c2=[104.769,    27.037]*1d0
    ;    a1=[105.687,    29.356]*1d0
    ;  rate2=.35d0
    ;;      rate2=-.95d0
    ;;      rate2=0d0
    ;;      rate2=!values.D_INFINITY
  
    ;    ;for vertical line
    ;    b1=[100.320,    25.841]*1d0
    ;    c2=[104.769,    27.037]*1d0
    ;    ;c2=[100.320,    27.037]*1d0
    ;    a1=[100.320,    29.356]*1d0
    ;    rate2=.35d0
    ;    ;rate2=-.25d0
    ;;      rate2=0
    ;;      rate2=!values.D_INFINITY
    ;
    ;    ;for horizontal line
    ;    b1=[100.320,    25.841]*1d0
    ;    c2=[104.769,    27.037]*1d0
    ;    a1=[105.687,    25.841]*1d0
    ;    rate2=.35d0
    ;    rate2=-.91d0
    ;rate2=0
    ;rate2=tan(!dpi/2)
    ;rate2=!values.D_INFINITY
  
    a1=[86.7303772579d0, 29.4418913671d0]
    b1=[85.5126849284d0, 29.4418913671d0]
    c2=[87.1,30]
    rate2=.5d0
    
    a1=[   77.898577d0,       40.191932]
    b1=[   79.655042d0,       28.480390]
    c2=[   78.800079d0,       34.336161]
     rate2=[-0.00000000d0]
  ENDIF
  
  ;stop
  xmin=MIN([a1[0],b1[0]],max=xmax)
  
  ;if horizontal line
  IF ABS(b1[1]-a1[1]) LE 1e-6 THEN BEGIN
    intercept1=a1[1]
    ;
    rate1=0
    IF N_ELEMENTS(rate2) EQ 0 THEN BEGIN  ;no rate given for the 2nd line
      ;assuming perpendicular to the 1st line
      rate2=!values.D_INFINITY
      intercept2=a1[1]
      d2=[c2[0],a1[1]]
    ENDIF ELSE BEGIN
      IF FINITE(rate2) EQ 1 THEN BEGIN
        intercept2=c2[1]-rate2*c2[0]
        d2=[(a1[1]-intercept2)/rate2,a1[1]]
      ;stop
      ENDIF
    ENDELSE
    
    GOTO, end_prog
  ENDIF
  
  
  ;if vertical line
  IF ABS(b1[0]-a1[0]) LE 1e-6 THEN BEGIN
    rate1=!values.D_INFINITY
    intercept1=0
    
    
    IF N_ELEMENTS(rate2) EQ 0 THEN BEGIN  ;no rate given for the 2nd line
      ;assuming perpendicular to the 1st line
      rate2=0
      intercept2=c2[1]
    ENDIF ELSE BEGIN
      IF FINITE(rate2) NE 1 THEN BEGIN
        IF ABS(b1[0]-c2[0]) LE 1e-6 THEN BEGIN ;if two lines are identical, then stop for further debug
          STOP
        ENDIF
      ;      d2=[c2[0],a1[1]]
      ;      intercept2=!values.D_INFINITY
      ;      GOTO, end_prog
      ENDIF
      intercept2=c2[1]-rate2*c2[0]
    ENDELSE
    d2=[a1[0],rate2*a1[0]+intercept2]
    ;stop
    GOTO, end_prog
  ENDIF
  
  rate1=(b1[1]-a1[1])/(b1[0]-a1[0])
  intercept1=a1[1]-rate1*a1[0]
  alpha1=ATAN(rate1)
  
  x1=a1[0]
  
  
  IF N_ELEMENTS(rate2) EQ 0 THEN BEGIN  ;no rate given for the 2nd line
    ;assuming perpendicular to the 1st line
    alpha2=alpha1+!dpi/2
    rate2=TAN(alpha2)
  ENDIF
  
  intercept2=c2[1]-rate2*c2[0]
  
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
    PRINT,a1
    PRINT,b1
    PRINT,c2
    PRINT,d2
    print,d2,format='(2(1x,f))'
    PRINT,FINITE(d2)
    PRINT,a1[1]-d2[1]
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
    
;    jfile=FILEPATH(STRLOWCASE(prog)+'_eg.jpg',root_dir=!igps_root,subdirectory=['libtian','math'])
;    WRITE_JPEG, jfile, TVRD(true=1),true=1,quality=100
  ENDIF
END