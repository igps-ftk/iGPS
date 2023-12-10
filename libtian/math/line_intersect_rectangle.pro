;+
; :DESCRIPTION:
;    Calulate the intersect point (d2) of a rectangle (defined by two points: lower-left a1, upper-right b1) and
;      a line defined by one point (c2) and its rate (rate2).
;
; :PARAMS:
;    rect - the rectangle [lower-left-x, lower-left-y, upper-right-x, upper-right-y]
;    a1 - the outer point
;    rate1 - the rate of the  line (passing a1)
;    b1,c1 - the intersect point of line (a1,b1) and the line passing c2.
;
;
; :Modifications:
;   +Create on Thu, Nov 05, 2015 11:56:30 PM by tianyf
;
; :AUTHOR: tianyf
;-
PRO LINE_INTERSECT_RECTANGLE, rect, a1, rate1, b1, c1
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 3 THEN BEGIN
    rect=DOUBLE(STRSPLIT('100.32000       23.985100       104.76900       28.874500',/extract))
    ;b1=[100.320,    25.841]*1d0
    a1=[103.769,    27.037]*1d0
    ;a1=[105.687,    29.356]*1d0
    rate1=-.000d0
    rate1=1.25d0
    ;rate1=(rect[1]-a1[1])/(rect[0]-a1[0])
    ;rate1=!values.d_infinity
    
    ;    ;for vertical line
    ;    b1=[100.320,    25.841]*1d0
    ;    c1=[104.769,    27.037]*1d0
    ;    a1=[100.320,    29.356]*1d0
    ;
    ;    ;for horizontal line
    ;    b1=[100.320,    25.841]*1d0
    ;    c1=[104.769,    27.037]*1d0
    ;    a1=[105.687,    25.841]*1d0
    ;
    ;
    rect=DOUBLE(STRSPLIT('94.671687       25.844816       108.50326       35.119381',/extract))
    a1=[103.90954 ,      31.213408]*1d0
    rate1=-1.1644416d0
    
  ENDIF
  
  ;  ;if vertical line
  ;  IF ABS(b1[0]-a1[0]) LE 1e-6 THEN BEGIN
  ;    rate1=!values.D_INFINITY
  ;    intercept1=0
  ;    rate2=0
  ;    intercept2=c1[1]
  ;    d1=[a1[0],c1[1]]
  ;    GOTO, end_prog
  ;  ENDIF
  ;
  ;  ;if horizontal line
  ;  IF ABS(b1[1]-a1[1]) LE 1e-6 THEN BEGIN
  ;    rate2=!values.D_INFINITY
  ;    intercept1=a1[1]
  ;    rate1=0
  ;    intercept2=0
  ;    d1=[c1[0],a1[1]]
  ;    GOTO, end_prog
  ;  ENDIF
  ;
  ;
  x1=a1[0]
  y1=rate1*(x1-a1[0])+a1[1]
  intercept1=a1[1]-rate1*a1[0]
  
  ;check 1st edge
  r1=[rect[0],rect[1]]
  r2=[rect[2],rect[1]]
  r3=[rect[2],rect[3]]
  r4=[rect[0],rect[3]]
  ;r1-r2
  LINE_INTERSECT_LINE, r1, r2, a1, rate1, o1
  ;PRINT,'o1:',o1
  LINE_INTERSECT_LINE, r3, r2, a1, rate1, o2
  ;PRINT,'o2:',o2
  LINE_INTERSECT_LINE, r3, r4, a1, rate1, o3
  ;PRINT,'o3:',o3
  LINE_INTERSECT_LINE, r1, r4, a1, rate1, o4
  ;PRINT,'o4:',o4
  out_ps=[[o1],[o2],[o3],[o4]]
  pos=WHERE((out_ps[0,*] GE rect[0] AND out_ps[0,*] LE rect[2]) AND $
    (out_ps[1,*] GE rect[1] AND out_ps[1,*] LE rect[3]))
  IF N_ELEMENTS(pos) LT 2 THEN STOP   ;error
  IF N_ELEMENTS(pos) GT 2 THEN BEGIN
    ;exactly one corner (two duplicate points)
    ;remove the duplicates
    out_ps=out_ps[*,pos]
    FOR pi=0, N_ELEMENTS(pos)-1 DO BEGIN
      IF  FINITE(out_ps[0]) NE 1 THEN CONTINUE
      FOR pj=pi+1,N_ELEMENTS(pos)-1 DO BEGIN
        IF (out_ps[0,[pi]]-out_ps[0,[pj]])^2+(out_ps[1,[pi]]-out_ps[1,[pj]])^2 LE 1e-6 THEN BEGIN
          out_ps[*,[pj]]=!values.D_NAN
          CONTINUE
        ENDIF
      ENDFOR
    ENDFOR
    pos=WHERE(FINITE(out_ps[0,*]))
  ENDIF
  b1=out_ps[*,pos[0]]
  c1=out_ps[*,pos[1]]
  
  ;STOP
  ;
  ;  end_prog:
  ;stop
  IF N_PARAMS() LT 3 THEN BEGIN
    WINDOW,1
    PLOT,[rect[0],rect[2],rect[2],rect[0],rect[0]], $
      [rect[1],rect[1],rect[3],rect[3],rect[1]], $
      color='0'x,background='ffffff'x,/ynozero, $
      /iso, $
      ;yrange=[23,30],xrange=[98,110],  $
      thick=3
    PLOTS,[a1[0]],[a1[1]],color='0'x,psym=2
    ;OPLOT,[c1[0],x1],[c1[1],y1],color='0'x,thick=3
    
    x=INDGEN(1000)
    PLOTS,x,x*rate1+intercept1,color='00ff00'x,psym=-1
    ;PLOTS,x,x*rate2+intercept2,color='00ff00'x,psym=-1
    ;PLOTS,d1[0],d1[1],psym=4,color='0'x,symsize=2
    
    
    OPLOT,[a1[0],o1[0]],[a1[1],o1[1]],linestyle=2,color='0'x
    PLOTS,o1[0],o1[1],psym=4,color='0'x,symsize=2
    OPLOT,[a1[0],o2[0]],[a1[1],o2[1]],linestyle=2,color='0'x
    PLOTS,o2[0],o2[1],psym=4,color='0'x,symsize=2
    OPLOT,[a1[0],o3[0]],[a1[1],o3[1]],linestyle=2,color='0'x
    PLOTS,o3[0],o3[1],psym=4,color='0'x,symsize=2
    OPLOT,[a1[0],o4[0]],[a1[1],o4[1]],linestyle=2,color='0'x
    PLOTS,o4[0],o4[1],psym=4,color='0'x,symsize=2
    PLOTS,[b1[0],c1[0]],[b1[1],c1[1]],psym=-4,symsize=3,color='ff0000'x
    
    jfile=FILEPATH(STRLOWCASE(prog)+'_eg.jpg',root_dir=!igps_root,subdirectory=['libtian','math'])
    WRITE_JPEG, jfile, TVRD(true=1),true=1,quality=100
    
    PRINT,b1
    PRINT,c1
  ENDIF
END