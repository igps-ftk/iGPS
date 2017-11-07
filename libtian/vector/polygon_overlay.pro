FUNCTION POLYGON_OVERLAY,xys1,xys2,i1=i1, xstep=xstep, ystep=ystep

  np1=N_ELEMENTS(xys1[0,*])
  np2=N_ELEMENTS(xys2[0,*])
  IF np1 NE np2 || np1 NE 4 THEN STOP
  
  xmin=MIN([REFORM(xys1[0,*]),REFORM(xys2[0,*])],max=xmax)
  ymin=MIN([REFORM(xys1[1,*]),REFORM(xys2[1,*])],max=ymax)
  
  rect=[xmin,xmax,ymin,ymax]
  if n_elements(xstep) eq 0 then xstep=.1
  if n_elements(xstep) eq 0 then ystep=.1
  
  ;  ;WINDOW,0,xsize=900,ysize=900
  ;  FOR pi=0,np1-1 DO BEGIN
  ;    a1=xys1[*,pi]
  ;    IF pi EQ np1-1 THEN pi_next=0 ELSE pi_next=pi+1
  ;    b1=xys1[*,pi_next]
  ;    xmin1=MIN([a1[0],b1[0]],max=xmax1)
  ;    ymin1=MIN([a1[1],b1[1]],max=ymax1)
  ;    FOR pj=0,np2-1 DO BEGIN
  ;      c1=xys2[*,pj]
  ;      IF pj EQ np2-1 THEN pj_next=0 ELSE pj_next=pj+1
  ;      d1=xys2[*,pj_next]
  ;      xmin2=MIN([c1[0],d1[0]],max=xmax2)
  ;      ymin2=MIN([c1[1],d1[1]],max=ymax2)
  ;
  ;      PLOT,xys1[0,[0,1,2,3,0]],xys1[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=[85,100],yrange=[25,35]
  ;      OPLOT,xys2[0,[0,1,2,3,0]],xys2[1,[0,1,2,3,0]],color='0'x,psym=-2
  ;    ;
  ;    ;      XYOUTS,d1[0],d1[1],'d1',color='ff0000'x
  ;    ;      XYOUTS,c1[0],c1[1],'c1',color='ff0000'x
  ;    ;      XYOUTS,b1[0],b1[1],'b1',color='ff0000'x
  ;    ;      XYOUTS,a1[0],a1[1],'a1',color='ff0000'x
  ;    ;      LINT, a1, b1, c1, d1, i1, i2, flag=flag
  ;    ;      PLOTS,i1[0],i1[1],psym=4,color='0000ff'x
  ;    ;      XYOUTS,i1[0],i1[1],'i1',color='0000ff'x
  ;    ;      ;stop
  ;    ;      ;if i1[0] gt xmax || i1[0] lt xmin || i1[1] gt ymax || i1[1] lt ymin then continue
  ;    ;      IF (i1[0] GE xmin1 && i1[0] LE xmax1 && i1[1] GE ymin1 && i1[1] LE ymax1) && $; on one segment
  ;    ;        (i1[0] GE xmin2 && i1[0] LE xmax2 && i1[1] GE ymin2 && i1[1] LE ymax2) THEN BEGIN ;or on another segment
  ;    ;        STOP
  ;    ;      ENDIF ELSE BEGIN  ;the intersect is far away
  ;    ;        CONTINUE
  ;    ;      ENDELSE
  ;    ;      RETURN,1
  ;    ;STOP
  ;    ENDFOR
  ;  ;stop
  ;  ENDFOR
  POLYGON_RASTERIZE, xys1,rect=rect,xstep=xstep,ystep=ystep, $  ;,nx=nx,ny=ny
    odata=odata1,oxs=oxs1,oys=oys1
  POLYGON_RASTERIZE, xys2,rect=rect,xstep=xstep,ystep=ystep, $  ;,nx=nx,ny=ny
    odata=odata2,oxs=oxs2,oys=oys2
  odata3=odata1+odata2
  pos=WHERE(odata3 EQ 2)
  pos1=WHERE(odata1 EQ 1)
  IF pos[0] NE -1 THEN RETURN,1d0*N_ELEMENTS(pos)/N_ELEMENTS(pos1) ELSE RETURN, 0
  
;STOP
;RETURN,0
  
END