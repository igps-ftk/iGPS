

PRO SAR_S1_MANIFEST_OVERLAPPING_SPECIFIED, path=path, ofile=ofile, target=target, xys=xys, names=names,  $
    onames=onames, perc_min=perc_min, files=files,opath=opath
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_ELEMENTS(path) EQ 0 THEN BEGIN
    path='C:\Downloads\esa.data\safe\all\'
  ;path='C:\Downloads\esa.data\safe\S1.2'
  ;    path='C:\Downloads\esa.data\safe\safe_outside'
  ;path='C:\Downloads\luoyi\xiluodu\safe'
  ;path='C:\Downloads\esa.data\S1\tmp'
  ;    path='\\gpsac4\root\g4d\esa.data\S1'
  ;    path='\\gpsac5\root\h2\esa.data\S1'
  ;
  ;path='C:\Downloads\esa.data\manifest.tibet\MANIFEST'
  ;path='C:\Downloads\S1'
  ENDIF
  
  IF N_ELEMENTS(opath) EQ 0 || N_ELEMENTS(target) EQ 0 THEN BEGIN
  
  
    target='target_wulan1'
    opath='D:\gsar\des\wulan1'
    orb_type=1
    perc_min=.75
    
  ENDIF
  
  IF N_ELEMENTS(target) EQ 0 THEN BEGIN
    PRINT,'ERROR: no target specified!!'
    RETURN
  ENDIF
  
  PROFILE_NAME2VECTORFILE,   $
    target,   $ ;input, fault name
    ffile=ffile   ;output, fault file
    
  ;stop
  ;read fault vector (if specified)
  IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
    lines_fvec=read_txt(ffile)
    lines_fvec2=STRTRIM(lines_fvec,2)
    pos=WHERE(strmids(lines_fvec2,0,1) NE '>')
    IF N_ELEMENTS(pos) LT 2 THEN BEGIN
      PRINT,'['+prog+']ERROR: invalid fault line vector file <'+ffile+'>!!'
      RETURN
    ENDIF
    xys_fvec=DOUBLE(str_lines2arr(lines_fvec2[pos]))
    xys_fvec=xys_fvec[*,1:*]
  ;stop
  ENDIF
  
  IF N_ELEMENTS(perc_min) EQ 0 THEN perc_min=.83
  ;  stop
  ofile=opath+PATH_SEP()+'overlapping.'+target+'.txt'
  HELP,ofile
  
  IF N_ELEMENTS(xys) EQ 0 THEN BEGIN  ;if no xys/names inputs, then read files
    IF N_ELEMENTS(files) EQ 0 THEN BEGIN
      files=FILE_SEARCH(path+PATH_SEP()+'S1*_IW_SLC*.manifest.safe', count=nf)
      ;files=FILE_SEARCH(path+PATH_SEP()+'manifest.safe-*', count=nf)
      IF nf LE 0 THEN BEGIN
        RETURN
      ENDIF
      
    ENDIF ELSE BEGIN
      nf=N_ELEMENTS(files)
    ENDELSE
    ;
    ;stop
    ;nf=600
    
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
      IF (fi MOD 50) EQ 0 THEN PRINT,fi,nf,(fi+1d0)/nf*100
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
    
  ENDIF
  ;//read-files-end
  ;  tmp=grepi(names_all,target)
  ;  IF tmp[0] EQ '' || N_ELEMENTS(pos) GT 1 THEN STOP
  ;  target=tmp
  ;  IF N_ELEMENTS(target) GT 1 THEN BEGIN
  ;    PRINT,'[WARNING]More than one records matched! The first one used.'
  ;    target=target[0]
  ;  ENDIF
  ;  ind_fi=REFORM(WHERE(names_all EQ target[0]))
  PRINT,'target scene:',target
  
  ;STOP
  
  pos=WHERE(obtyps_all EQ orb_type )
  IF pos[0] EQ -1 THEN RETURN
  ;
  ;stop
  ;pos=pos[0:590]  ;only for test
  xys=xys_all[*,*,pos]
  names=names_all[pos]
  ;ind_fi=REFORM(WHERE(names EQ target[0]))
  ;
  np=N_ELEMENTS(pos)
  
  
  ;stop
  ;IF ind_fi[0] EQ -1 THEN RETURN
  
  ind_fi=0
  ;STOP
  percs=DBLARR(np,np)
  
  ;FOR fi=0, np-1 DO BEGIN
  ;FOR fi=ind_fi[0],ind_fi[0] DO BEGIN
  ;;xys_fi=REFORM(xys[*,*,fi])
  xys_fi=xys_fvec
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
    IF (fj MOD 50) EQ 0 THEN PRINT,np,ind_fi,fj,tmp
    percs[ind_fi,fj]=tmp
    IF tmp EQ 0 THEN CONTINUE
    
  ;      WINDOW,0,xsize=900,ysize=900
  ;      PLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=[85,100],yrange=[25,35]
  ;      OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color='0'x,psym=-2
    
    
  ;STOP
  ENDFOR
  ;ENDFOR
  
  
  pos=WHERE(percs[ind_fi,*] GE perc_min)
  IF pos[0] EQ -1 THEN BEGIN
    RETURN
  ENDIF
  onames=names[pos]
  ;WINDOW,0,xsize=900,ysize=900
  xys_fi=xys_fvec  ;REFORM(xys[*,*,ind_fi[0]])
  PLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]],background='ffffff'x,  $
    title='Footprints of Matching Scenes',$
    xtitle='Longitude',ytitle='Latitude',charsize=1.2,$
    color='000000'x,/iso,/yno,/nodata   ;,xrange=[85,100],yrange=[25,35]
  OPLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]], $
    color='0000ff'x,psym=-4,symsize=4,thick=3
  dx=(MAX(xys_fi[0,*])-MIN(xys_fi[0,*]))/N_ELEMENTS(pos)
  ;stop
  ;DEVICE,decomposed=0
  FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
    xys_fj=REFORM(xys[*,*,pos[i]]);
    ;POLYFILL,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color=i*256
    ;OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color='ff0000'x,psym=-3,linestyle=2,thick=2
    OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color=i*(!d.N_COLORS/N_ELEMENTS(pos)),psym=-3,linestyle=2,thick=1
    y1=MAX(xys_fj[1,*],indmax1)
    tmp=xys_fj
    tmp[1,indmax1]=-9999
    y2=MAX(tmp[1,*],indmax2)
    xy1=xys_fj[*,indmax1]
    xy2=xys_fj[*,indmax2]
    IF xy1[0] GT xy2[0] THEN BEGIN
      tmp=xy2
      xy2=xy1
      xy1=tmp
    ENDIF
    xi=xy1[0]+dx*i
    rate=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
    b=xy1[1]-rate*xy1[0]
    yi=xi*rate+b
    XYOUTS,xi,yi, $
      STRTRIM(i+1,2),color=i*(!d.N_COLORS/N_ELEMENTS(pos))
  ENDFOR
  ;DEVICE,decomposed=1
  ;STOP
  
  OPENW,fid,ofile,/get_lun
  LLS_KML=REPLICATE(PTR_NEW(),N_ELEMENTS(POS))
  NAMES_KML=NAMES[POS]
  FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
    PRINTF,fid,names[pos[i]],percs[ind_fi,pos[i]],format='(1x,a,1x,f)'
    LLS_KML[I]=PTR_NEW(REFORM(XYS[*,*,POS[I]]))
  ENDFOR
  FOR fi=0,np-1 DO BEGIN
    PRINTF,fid,names[fi],percs[ind_fi,fi],format='("#",a,1x,f)'
  ENDFOR
  FREE_LUN,fid
  
  hp=HISTOGRAM(percs[ind_fi,*],binsize=.02,locations=hx)
  ;WINDOW,1
  pos=WHERE(hp GT 0)
  
  PLOT,[0,1.1],[0,0],background='ffffff'x,color='000000'x,psym=-2, $
    xtitle='Percentage of Overlapping (%)',  $
    ytitle='Count (#)', $
    title=target,/nodata,charsize=1.5
  IF N_ELEMENTS(hx) GE 2 THEN BEGIN
    OPLOT,[hx[1:*],1.1],[hp[1:*],0],color='000000'x,psym=-2
  ENDIF
  IF pos[0] NE -1 THEN BEGIN
    OPLOT,hx[pos],hp[pos],color='ff0000'x,psym=-4
  ENDIF
  
  jfile=desuffix(ofile)+'.jpg'
  PRINT,jfile
  ;STOP
  WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
  newwin=!d.WINDOW
  WSET,oldwin
  
  OFILE=DESUFFIX(OFILE)+'.kml'
  KML_POLYLINE, OFILE, names=names_KML, LLS=LLS_KML, IS_FINALIZE=1, IS_FREE_PTR=0
  OFILE=DESUFFIX(OFILE)+'.psxy'
  PSXY_POLYLINE, OFILE, names=names_KML, LLS=LLS_KML, IS_FINALIZE=1
  
;WDELETE,newwin
;STOP
  
END