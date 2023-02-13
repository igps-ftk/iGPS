PRO SAR_S1_TRACK_AUTO_ROI, path, opath

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 2 THEN BEGIN
    path='C:\Downloads\esa.data\safe\manifest.safe\D135'
    opath='C:\Downloads\esa.data\safe\manifest.safe\D135\tst'
  ENDIF
  
  files=FILE_SEARCH(path+PATH_SEP()+'*.manifest.safe', count=nf)
  HELP, nf
  IF nf LE 0 THEN BEGIN
    PRINT,'no files found'
    RETURN
  ENDIF
  obtnames=['ascending','descending']
  
  xys_all=DBLARR(2,4,nf)
  names_all=STRARR(nf)
  obtyps_all=INTARR(nf)
  
  lbls_all=STRTRIM(INDGEN(4)+1,2)
  
  oldwin=!d.WINDOW
  WINDOW,/free,xsize=1800,ysize=600
  DEVICE, decomposed=1
  !p.MULTI=[0,3,1]
  ;stop
  PLOT,[0],[0],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,$
    xrange=[83,101],yrange=[28,42],$
    title='Footprints of All Scenes',$
    xtitle='Longitude',ytitle='Latitude',charsize=1.2
    
  FOR fi=0, nf-1 DO BEGIN
    file=files[fi]
    pos=STRPOS(file,'E456')
    ;if pos[0] ne -1 then stop
    names_all[fi]=GETFILENAME(file)
    ;
    lines=read_txt(file)
    line=grepi(lines,'coordinates')
    ; <gml:coordinates>32.437653,89.348289 32.836613,91.993134 31.158100,92.323769 30.756710,89.728622</gml:coordinates>
    line=STRTRIM(line,2)
    line_p1=STRSPLIT(line,'>',/extract)
    line_p2=STRSPLIT(line_p1[1],'<',/extract)
    tmp=line_p2[0]
    tmp_p=STRSPLIT(tmp,/extract)
    
    xys_i=DBLARR(2,4)
    FOR pi=0,3 DO BEGIN
      tmp_xy=STRSPLIT(tmp_p[pi],',',/extract)
      xys_i[*,pi]=DOUBLE(tmp_xy[[1,0]])
    ENDFOR
    xys_all[*,*,fi]=xys_i
    ;PRINT,xys_i
    ;stop
    
    ;descending/ascending
    line=grepi(lines,'pass')
    line1=strrep(line,'/','')
    line2=STRSPLIT(line1,'<s1:pass>',/extract)
    CASE line2[1] OF
      'ASCENDING': obtyps_all[fi]=0
      'DESCENDING': obtyps_all[fi]=1
    ENDCASE
    
    OPLOT,xys_i[0,[0,1,2,3,0]],xys_i[1,[0,1,2,3,0]],color='00ffaa'x
  ;XYOUTS,xys_i[0,*],xys_i[1,*],lbls,color='ff0000'x,charsize=2
  ;ofile=file+'.jpg'
  ;WRITE_JPEG,ofile,TVRD(true=1),true=1,quality=100
  ;STOP
    
  ENDFOR
  
  
  STOP
  
  FOR fi=ind_fi[0],ind_fi[0] DO BEGIN
    xys_fi=REFORM(xys[*,*,fi])
    xmin_fi=MIN(xys_fi[0,*],max=xmax_fi)
    ymin_fi=MIN(xys_fi[1,*],max=ymax_fi)
    ;
    FOR fj=0, np-1 DO BEGIN
      xys_fj=REFORM(xys[*,*,fj])
      xmin_fj=MIN(xys_fj[0,*],max=xmax_fj)
      ymin_fj=MIN(xys_fj[1,*],max=ymax_fj)
      ;stop
      IF xmin_fj GT xmax_fi || xmax_fj LT xmin_fi || ymin_fj GT ymax_fi || ymax_fj LT ymin_fj THEN BEGIN
        tmp=0d0
      ENDIF ELSE BEGIN
        tmp=POLYGON_OVERLAY(xys_fi,xys_fj,xstep=.15,ystep=.15)
      ENDELSE
      PRINT,np,fi,fj,tmp
      percs[fi,fj]=tmp
      IF tmp EQ 0 THEN CONTINUE
      
    ;      WINDOW,0,xsize=900,ysize=900
    ;      PLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=[85,100],yrange=[25,35]
    ;      OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color='0'x,psym=-2
      
      
    ;STOP
    ENDFOR
  ENDFOR
  
  STOP
  
END