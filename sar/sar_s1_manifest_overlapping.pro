

PRO SAR_S1_MANIFEST_OVERLAPPING, path=path,   $ ;where to search *.manifest.safe files 
    files=files, $  ;filenames of *.manifest.safe files, if path is not given, 
    target=target,  $ ;frame name to match
    opath=opath,  $ ;output path
    ofile=ofile,  $ ;output file name. If not given, use "opath+target"
    perc_min=perc_min,  $ ;minimum percentage of overlapping between two frames, default 83% (0.83)
    xys=xys,  $
    names=names,  $
    onames=onames,  $
    exclude_date=exclude_date
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  
  ;*.manifest.safe path
  IF N_ELEMENTS(path) EQ 0 THEN BEGIN
    ;for test
    path='C:\Downloads\esa.data\metainfo\manifest.safe\D018'
  ENDIF
  
  IF N_ELEMENTS(opath) EQ 0 || N_ELEMENTS(target) EQ 0 THEN BEGIN
    ;for test
    target='S1B_IW_SLC__1SDV_20190531T223417_20190531T223444_016494_01F0B6_383C.manifest.safe'
    opath='D:\tmp'
    perc_min=.8
  ENDIF
  
  ;stop
  IF N_ELEMENTS(perc_min) EQ 0 THEN BEGIN ;if no perc_min is given, then check the config.txt file
    file_config=opath+PATH_SEP()+'config.txt'
    perc_min_config=-9999d0
    IF FILE_TEST(file_config,/regular) EQ 1 THEN BEGIN
      lines=read_txt(file_config, comment='~ ')
      ;help, lines
      tmp=grepi(lines,'perc_min')
      IF tmp[0] NE '' THEN BEGIN
        tmp_p=STRSPLIT(last(tmp),'=',/extract)
        perc_min_config=DOUBLE(tmp_p[1])
      ENDIF
    ENDIF
    IF perc_min_config NE -9999d0 THEN BEGIN
      perc_min=perc_min_config
    ENDIF
  ENDIF
  IF N_ELEMENTS(perc_min) EQ 0 THEN perc_min=.83
  PRINT,'['+PROG+']INFO: perc_min =',perc_min
  IF perc_min LT .51d0 THEN BEGIN
    PRINT,'['+PROG+']WARNING: minimum overlapping percentage is rather small ('+STRTRIM(perc_min,2)+')!'
  ENDIF
  
  
  IF N_ELEMENTS(target) EQ 0 THEN BEGIN
    PRINT,'['+PROG+']ERROR: no target specified!!'
    RETURN
  ENDIF
  
  IF N_ELEMENTS(exclude_date) EQ 0 THEN BEGIN
    exclude_date=''
    file_exclude=opath+PATH_SEP()+'exclude_date.txt'
    IF FILE_TEST(file_exclude,/regular) EQ 1 THEN BEGIN
      exclude_date=read_txt(file_exclude,comment='~ ')
      exclude_date=STRTRIM(exclude_date,2)
    ENDIF
  ENDIF
  HELP,exclude_date
  ;stop
  
  
  
  
  ;stop
  ofile=opath+PATH_SEP()+'overlapping.'+target+'.txt'
  
  IF N_ELEMENTS(xys) EQ 0 THEN BEGIN  ;if no xys/names inputs, then read files
    IF N_ELEMENTS(files) EQ 0 THEN BEGIN
      files=FILE_SEARCH(path+PATH_SEP()+'S1*_IW_SLC*.manifest.safe', count=nf)
      ;files=FILE_SEARCH(path+PATH_SEP()+'manifest.safe-*', count=nf)
      IF nf LE 0 THEN BEGIN
        RETURN
      ENDIF
      
    ENDIF
    ;STOP
    
    ;remove excluded scenes
    IF N_ELEMENTS(exclude_date) NE -1 && exclude_date[0] NE '' THEN BEGIN
    
      fnames=GETFILENAME(files)
      inds_exclude=INTARR(N_ELEMENTS(fnames))
      inds_exclude[*]=0
      FOR ei=0,N_ELEMENTS(exclude_date)-1 DO BEGIN
        tmp=grepi(fnames, exclude_date[ei], line_number=ind_ei)
        ;stop
        IF tmp[0] NE '' THEN BEGIN
          inds_exclude[ind_ei]=1
        ENDIF
      ENDFOR
      pos=WHERE(inds_exclude EQ 0, complement=ind_out)
      IF pos[0] EQ -1 THEN BEGIN
        PRINT,'[]WARNING: no files left after excluding scens!!'
        RETURN
      ENDIF
      IF N_ELEMENTS(pos) NE N_ELEMENTS(fnames) THEN BEGIN
        FOR i=0, N_ELEMENTS(ind_out)-1 DO PRINT,'[]INFO: exclude '+fnames[ind_out[i]]
        PRINT,'[]INFO:'+STRTRIM(N_ELEMENTS(fnames)-N_ELEMENTS(pos),2)+' scenes excluded.'
        ;fnames=fnames[pos]
        files=files[pos]
        nf=N_ELEMENTS(files)
        PRINT,'[]INFO: remain scenes '+ STRTRIM( nf,2)
      ;STOP
      ENDIF
    ENDIF
    nf=N_ELEMENTS(files)
    
    ;return
    ;
    ;stop
    
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
      xrange=[-180,180],yrange=[-90,90],$
      title='Footprints of All Scenes',$
      xtitle='Longitude',ytitle='Latitude',charsize=1.2
      
    PRINT, '['+PROG+']INFO:reading '+STRTRIM(N_ELEMENTS(files),2)+' *.safe files ...'
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
    
  ENDIF
  ;//read-files-end
  tmp=grepi(names_all,target)
  IF tmp[0] EQ '' || N_ELEMENTS(pos) GT 1 THEN STOP
  target=tmp
  IF N_ELEMENTS(target) GT 1 THEN BEGIN
    PRINT,'['+PROG+']WARNING]: more than one records matched! The first one used.'
    target=target[0]
  ENDIF
  ind_fi=REFORM(WHERE(names_all EQ target[0]))
  HELP, ind_fi
  PRINT, '['+PROG+']INFO:target scene:',target
  
  ;STOP
  
  pos=WHERE(obtyps_all EQ (obtyps_all[ind_fi])[0] )
  IF pos[0] EQ -1 THEN RETURN
  ;
  ;stop
  ;pos=pos[0:590]  ;only for test
  xys=xys_all[*,*,pos]
  names=names_all[pos]
  ind_fi=REFORM(WHERE(names EQ target[0]))
  HELP, ind_fi
  ;
  np=N_ELEMENTS(pos)
  
  
  ;stop
  IF ind_fi[0] EQ -1 THEN RETURN
  
  
  ;STOP
  percs=DBLARR(np,np)
  
  PRINT, '['+PROG+']INFO: matching scenes ...'
  ;FOR fi=0, np-1 DO BEGIN
  FOR fi=ind_fi[0],ind_fi[0] DO BEGIN
    xys_fi=REFORM(xys[*,*,fi])
    xmin_fi=MIN(xys_fi[0,*],max=xmax_fi)
    ymin_fi=MIN(xys_fi[1,*],max=ymax_fi)
    ;
    step_n=FIX(np/10.)
    ;stop
    FOR fj=0, np-1 DO BEGIN
      ;print,fj,  (fj+1) MOD step_n
    
      IF ( ( (fj+1) MOD step_n) EQ 0) THEN PRINT,'['+PROG+']INFO: ',STRTRIM( CEIL((fj+1d0)/np*100),2), '% complete'
      xys_fj=REFORM(xys[*,*,fj])
      xmin_fj=MIN(xys_fj[0,*],max=xmax_fj)
      ymin_fj=MIN(xys_fj[1,*],max=ymax_fj)
      ;stop
      IF xmin_fj GT xmax_fi || xmax_fj LT xmin_fi || ymin_fj GT ymax_fi || ymax_fj LT ymin_fj THEN BEGIN
        tmp=0d0
      ENDIF ELSE BEGIN
        tmp=POLYGON_OVERLAY(xys_fi,xys_fj,xstep=.15,ystep=.15)
      ENDELSE
      ;PRINT,np,fi,fj,tmp
      percs[fi,fj]=tmp
      IF tmp EQ 0 THEN CONTINUE
      
    ;      WINDOW,0,xsize=900,ysize=900
    ;      PLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=[85,100],yrange=[25,35]
    ;      OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color='0'x,psym=-2
      
      
    ;STOP
    ENDFOR
  ENDFOR
  
  
  ;stop
  pos=WHERE(percs[ind_fi,*] GE perc_min)
  IF pos[0] EQ -1 THEN BEGIN
    RETURN
  ENDIF
  onames=names[pos]
  ;WINDOW,0,xsize=900,ysize=900
  xys_fi=REFORM(xys[*,*,ind_fi[0]])
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
  
  
  PRINT, '['+PROG+']INFO: output to '+ofile
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,user=user
  IF N_ELEMENTS(path) EQ 0 || STRTRIM(path[0],2) EQ '' THEN BEGIN
    IF N_ELEMENTS(files) NE 0 THEN BEGIN
      PRINTF,fid,getpathname(files[0]),format='("* source path:",1x,a)'
    ENDIF
  ENDIF ELSE BEGIN
    PRINTF,fid,path[0],format='("* source path:",1x,a)'
  ENDELSE
  PRINTF,fid,target,format='("* target:",1x,a)'
  PRINTF,fid,perc_min,format='("* minimum percentage of overlapping:",1x,f)'
  PRINTF,fid,N_ELEMENTS(pos),format='("* number of matched scenes:",1x,i)'
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
  PRINT, '['+PROG+']INFO: output to '+jfile
  ;STOP
  WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
  newwin=!d.WINDOW
  WSET,oldwin
  
  OFILE=DESUFFIX(OFILE)+'.kml'
  
  PRINT, '['+PROG+']INFO: output to '+ofile
  KML_POLYLINE, OFILE, names=names_KML, LLS=LLS_KML, IS_FINALIZE=1, IS_FREE_PTR=0
  OFILE=DESUFFIX(OFILE)+'.psxy'
  PRINT, '['+PROG+']INFO: output to '+ofile
  PSXY_POLYLINE, OFILE, names=names_KML, LLS=LLS_KML, IS_FINALIZE=1
  
  ;WDELETE,newwin
  ;STOP
  
  PRINT, '['+PROG+']INFO: normal end'
  
END