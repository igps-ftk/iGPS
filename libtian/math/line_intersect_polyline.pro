;
;+
; :DESCRIPTION:
;    Find the distance from point to a polyline along the perpendicular or any direction
;
; :INPUT:
;    xys_polyline -  vertices of the polyline
;    xy_point - outside point
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
PRO  LINE_INTERSECT_POLYLINE, xys_polyline,  $
    xy_point,  $
    xy_intersect=xy_intersect, $
    auto_strike=auto_strike,  $
    strikes=strikes,  $
    in_or_out=in_or_out,  $
    xy_buffer=xy_buffer,  $
    segment_index=segment_index, $
    dist_point2polyline=dist_point2polyline
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  ;
  s1=systime(/seconds)
  IF N_ELEMENTS(xys_polyline) EQ 0 THEN BEGIN
    xys_polyline=[[92.0861129999999970d0,      28.2706699999999990], $
      [89.5549620000000030,      28.6762920000000000], $
      [89.8731379999999970,      30.3064980000000010], $
      [92.4469069999999960,      29.9035450000000010] ]
    xy_point=[91,29d0]
    
    file='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    file='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    READ_PSXY,   $
      file,   $ ;input file
      region=regions,   $ ;x,y coorinates of each polygons (pointer type)
      nps=nps, $  ;number of point pairs for each polygon
      count=count,  $ ;number of polygons
      igpsmode=igpsmode,  $ ;whether skip comments lines (i.e., non-blank first column lines)
      names=names   ;region names (if exist)
      
    xys_polyline=*regions[0]
    xy_point=[97,35d0]
    xy_point=[97.12d0,   31.16d0]  ;CAD1_GPS
    ;auto_strike=2
    
    is_plot=0
  ENDIF
;  s1=systime(/seconds)
  IF N_ELEMENTS(is_plot) EQ 0 THEN is_plot=0
  
  IF N_ELEMENTS(auto_strike) EQ 0 THEN auto_strike=2
  
  
  IF N_ELEMENTS(rect) EQ 0 THEN BEGIN
    xmin=MIN(xys_polyline[0,*],max=xmax)
    ymin=MIN(xys_polyline[1,*],max=ymax)
    rect=[xmin,xmax,ymin,ymax]
  ENDIF
  
  ;  IF N_ELEMENTS(xy_buffer) EQ 0 THEN BEGIN
  ;    n_buffer=50d0
  ;    x_buffer=(xmax-xmin)/n_buffer
  ;    y_buffer=(ymax-ymin)/n_buffer
  ;    xy_buffer=x_buffer
  ;    IF y_buffer GT x_buffer THEN xy_buffer=y_buffer
  ;    ;tmp=MAP_2POINTS(0,MEAN(xys_polyline[1,*]),xy_buffer,MEAN(xys_polyline[1,*]),/meter)
  ;    ;
  ;    ;STOP
  ;    xy_buffer=0d0
  ;  ENDIF
  
  IF is_plot EQ 1 THEN BEGIN
    WINDOW,0,xsize=1024
    !p.MULTI=-1
    DEVICE, DECOMPOSED=1
    PLOT,[REFORM(xys_polyline[0,*]),xy_point[0]],[REFORM(xys_polyline[1,*]),xy_point[1]],background='ffffff'x,color='0'x,psym=-0, $
      /iso, $
      /yno, $
      ;xrange=rect[0:1],yrange=rect[2:3], $
      /nod
    OPLOT, xys_polyline[0,*],xys_polyline[1,*],color='0'x,psym=-2
    PLOTS,xy_point[0],xy_point[1],psym=6,color='ff00ff'x
  ENDIF
  ;stop
  
  IF N_ELEMENTS(strikes) EQ 0 THEN BEGIN
    FA_STRIKE_CAL,  $   ;fault file (if given); otherwise fault coordinate (xys_fa) should be given
      xys_fa=xys_polyline,  $   ;fault coordinates (lon, lat) array (2*N)
      strikes=strikes,  $ ;output strikes
      auto_strike=auto_strike ;strike mode
  ENDIF
  
  npt_polyline=N_ELEMENTS(xys_polyline[0,*])
  odists=DBLARR(npt_polyline-1)
  xys_perp=DBLARR(2,npt_polyline-1)
  in_or_out=INTARR(npt_polyline-1)
  segment_index=-1
  dists_2_vertex=DBLARR(npt_polyline)
  ;;
  ;s1=systime(/seconds)
  FOR li=0,npt_polyline-2 DO BEGIN
    a1=xys_polyline[*,li]
    IF li EQ npt_polyline-1 THEN li_next=0 ELSE li_next=li+1
    b1=xys_polyline[*,li_next]
    
    c1=xy_point
    
    dists_2_vertex[li]=MAP_2POINTS(c1[0],c1[1],a1[0],a1[1],/meter)*1d-3*(c1[0]-a1[0])/ABS(c1[0]-a1[0])
    dists_2_vertex[li_next]=MAP_2POINTS(c1[0],c1[1],b1[0],b1[1],/meter)*1d-3*(c1[0]-b1[0])/ABS(c1[0]-b1[0])
    
    xmin1=MIN([a1[0],b1[0]],max=xmax1)
    ymin1=MIN([a1[1],b1[1]],max=ymax1)
    
    ;    IF is_plot EQ 1 THEN BEGIN
    ;      OPLOT,[a1[0],b1[0]],[a1[1],b1[1]],psym=-1,color='00ffff'x
    ;    ;XYOUTS,b1[0],b1[1],'b1',color='ff0000'x
    ;    ;XYOUTS,a1[0],a1[1],'a1',color='ff0000'x
    ;    ENDIF
    
    CASE auto_strike OF
      1: BEGIN
        POINT_PERP_LINE, a1, b1, c1, d1
      END
      2: BEGIN
        rate1=-1d0/TAN(strikes)
        LINE_INTERSECT_LINE, a1, b1, c1, rate1, d1
      END
      3: BEGIN
        rate1=-1d0/TAN(strikes)
        LINE_INTERSECT_LINE, a1, b1, c1, rate1, d1
      END
      ELSE: BEGIN
        PRINT, '['+PROG+']ERROR: invalid strike mode (auto_strike='+STRTRIM(auto_strike,2)+'!!'
        RETURN
      END
    ENDCASE
    
    
    
    ;outside globe
    IF d1[0] GT 360 OR d1[0] LT -360 OR d1[1] GT 90 OR d1[1] LT -90 THEN BEGIN
      in_or_out[li]=0
      ;PRINT,'out of globe'
      CONTINUE
    ENDIF
    
    IF d1[0] GE xmin1 AND d1[0] LE xmax1 AND d1[1] GE ymin1 AND d1[1] LE ymax1 THEN BEGIN
      ;IF d1[0] GE xmin1-xy_buffer AND d1[0] LE xmax1+xy_buffer AND d1[1] GE ymin1-xy_buffer AND d1[1] LE ymax1+xy_buffer THEN BEGIN
      in_or_out[li]=1
    ;stop
    ENDIF ELSE BEGIN
      in_or_out[li]=0
    ENDELSE
    
    IF is_plot EQ 1 THEN BEGIN
      IF in_or_out[li] EQ 1 THEN BEGIN
        OPLOT,[c1[0],d1[0]],[c1[1],d1[1]],psym=-1,color='00ffff'x
      ;XYOUTS,c1[0],c1[1],'c1',color='ff0000'x
      ;XYOUTS,d1[0],d1[1],'d1',color='ff0000'x
      ENDIF
    ENDIF
    
    
    ;    e1=a1
    ;    f1=b1
    ;    IF a1[0] GT b1[0] THEN BEGIN
    ;      e1=b1
    ;      f1=a1
    ;    ENDIF
    ;
    ;    g1=d1
    ;    IF d1[0] LT e1[0] THEN g1=e1
    ;    IF d1[0] GT f1[0] THEN g1=f1
    ;
    ;    tmp=MAP_2POINTS(c1[0],c1[1],g1[0],g1[1],/meter)
    ;
    tmp=MAP_2POINTS(c1[0],c1[1],d1[0],d1[1],/meter)
    odists[li]=tmp*1d-3*(c1[0]-d1[0])/ABS(c1[0]-d1[0])
    
    xys_perp[*,li]=d1
  ;STOP
  ENDFOR
  
  s2=systime(/seconds)
  inds=WHERE(in_or_out EQ 1)
  IF inds[0] NE -1 THEN BEGIN
    ;IF N_ELEMENTS(inds) GT 1 THEN BEGIN
    ;PRINT,'['+PROG+']WARNING: more than one points found! ONLY return the first one.'
    ;ENDIF
    ;print,odists[inds]
    tmp=MIN(abs(odists[inds]),ind_min)
    ;stop
    xy_intersect=xys_perp[*,inds[ind_min]]
    segment_index=inds[ind_min]
    FOR i=0, N_ELEMENTS(inds)-1 DO BEGIN
      IF is_plot EQ 1 THEN BEGIN
        OPLOT, xys_polyline[0,inds[i]:inds[i]+1],xys_polyline[1,inds[i]:inds[i]+1],psym=-2, color='0000ff'x
      ;print,xys_polyline[0,inds[i]:inds[i]+1],xys_polyline[1,inds[i]:inds[i]+1]
      ;stop
      ENDIF
    ENDFOR
    IF is_plot EQ 1 THEN OPLOT, xys_polyline[0,segment_index:segment_index+1],xys_polyline[1,segment_index:segment_index+1],psym=-2, color='0000ff'x, thick=2
    
  dist_point2polyline=odists[inds[ind_min]]
  ENDIF ELSE BEGIN
    ;xy_intersect=[!values.D_NAN,!values.D_NAN]
    ;PRINT,'['+PROG+']INFO: no intersection found. Return the nearest vertex.'
    tmp=MIN(abs(dists_2_vertex),ind_min)
    xy_intersect=xys_polyline[*,ind_min]
    segment_index=ind_min
    IF ind_min LT npt_polyline-1 THEN BEGIN
      in_or_out[ind_min]=2 ;not on the line segment, but the distance is shortest!
    ENDIF ELSE BEGIN
      in_or_out=[in_or_out,2]
    ENDELSE
  dist_point2polyline=dists_2_vertex[ind_min]
  ;stop
  ENDELSE
  
  
  IF N_PARAMS() LT 2 THEN BEGIN
    HELP, xys_polyline, xy_point, xy_intersect, in_or_out, segment_index, dist_point2polyline
    print,xy_intersect
    IF is_plot EQ 1 THEN OPLOT,[c1[0],xy_intersect[0]],[c1[1],xy_intersect[1]],psym=-1,color='00ffff'x,thick=2
    
  s2=systime(/seconds)
  print,'elapsed time:',s2-s1,' seconds'
  ENDIF
;STOP
  
  
END
