PRO NEU_SMOOTH, $
    path, $ ;input path containing sio/neu format time series files.
    opath=opath, $  ;output path
    residpath=residpath, $
    files=files, $  ;if input files given, the path will be ignored.
    szwin=szwin, $  ;width of smoothing window
    xsize=xsize, $  ;size of plot window
    ysize=ysize, $
    psym=psym, $
    symsize=symsize, $
    xrange=xrange, $  ;plot range of x-axes in decimal years
    yrange=yranges, $ ;plot range of y-axes in mm (position)
    sf=sf, $          ;scale factor for position unit (default sio/neu is in meter; yranges is in millimeter; therefore the sf should be 1d3)
    verbose=verbose, $  ;whether or not output detailed running information
    out_neu=out_neu   ;whether write out smoothed time series files (only jpeg files by default).
    
  prog='NEU_SMOOTH'
  
  IF N_ELEMENTS(path) EQ 0 && N_ELEMENTS(files) EQ 0 THEN BEGIN
    path=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','cmc','PBO'],'WNAM_Clean_ResidNeuTimeSeries_comb_20150318.test')
    path='D:\gpse\rerun.lutai\comb\trnsLTCM3b\gsoln\pos.neu.cln'
    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.20161023\gamit\raw.neu.resid.cmc0\cmc0'
    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\raw.neu.cln.resid'
    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\raw.neu.resid.flt'
    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\raw.neu.resid.cmc\flt0'
    path='C:\tmp\jpl\resid.neu.glb.cn.flt'
    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\raw.neu.cln.resid.flt'
    path='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos.neu.Huabei2017.resid'
;    path='C:\tmp\jpl\resid.neu.glb'
    ;sf=1d0
    ;sf=1d3
    
    out_neu=1
    
    ;here can handle multiply input paths (not work when called from outside)
    FOR i=0,N_ELEMENTS(path)-1 DO BEGIN
      IF N_ELEMENTS(opath) EQ 0 THEN BEGIN
        opath=getpathname(path[i]+PATH_SEP()+'test.t')+'.smoothed'
      ENDIF
      IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
      
      IF N_ELEMENTS(residpath) EQ 0 THEN BEGIN
        opath_resid=getpathname(path[i]+PATH_SEP()+'test.t')+'.smoothed_resid'
        IF FILE_TEST(opath_resid,/directory) NE 1 THEN FILE_MKDIR,opath_resid
      ENDIF
      NEU_SMOOTH,path[i],opath=opath,residpath=opath_resid,szwin=szwin, out_neu=out_neu, sf=sf, XSIZE=1500,YSIZE=700
      ;NEU_SMOOTH,path[i],opath=opath,residpath=opath_resid,szwin=szwin, out_neu=out_neu, sf=sf, XSIZE=1100,YSIZE=500
    ENDFOR
    RETURN
  ENDIF
  
  IF N_ELEMENTS(files) EQ 0 THEN BEGIN
    files=FILE_SEARCH(path+PATH_SEP()+'*.neu',count=nf)
  ENDIF ELSE BEGIN
    nf=N_ELEMENTS(files)
  ENDELSE
  ;nf=5
  ;print,nf
  
  IF N_ELEMENTS(sf) EQ 0 THEN sf=1d3
  IF N_ELEMENTS(xsize) EQ 0 THEN xsize=1100
  IF N_ELEMENTS(ysize) EQ 0 THEN ysize=500
  IF N_ELEMENTS(psym) EQ 0 THEN psym=1
  IF N_ELEMENTS(symsize) EQ 0 THEN symsize=1
  IF N_ELEMENTS(verbose) EQ 0 THEN verbose=1
  ;help, sf
  
  IF N_ELEMENTS(opath) EQ 0 || STRTRIM(opath,2) EQ '' THEN BEGIN
    opath=getpathname(files[0])+'.smoothed'
    IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
    IF N_ELEMENTS(opath_resid) NE 0 THEN BEGIN
      opath_resid=getpathname(files[0])+'.smoothed_resid'
      IF FILE_TEST(opath_resid,/directory) NE 1 THEN FILE_MKDIR,opath_resid
    ENDIF
  ENDIF
  
  IF N_ELEMENTS(out_neu) EQ 0 THEN out_neu=1
  IF N_ELEMENTS(szwin) EQ 0 THEN szwin=30
  
  IF N_ELEMENTS(ytitles) EQ 0 THEN ytitles=['North','East','Vertical']
  IF N_ELEMENTS(xtitle) EQ 0 THEN xtitle=GETFILENAME(getpathname(files[0]))
  
  IF N_ELEMENTS(yranges) EQ 0 THEN BEGIN
    yranges=DBLARR(2,3)
    yranges[*,0]=[-.005,.005]*1d3
    yranges[*,1]=[-.005,.005]*1d3
    yranges[*,2]=[-.01,.01]*1d3
  ENDIF
  yranges=yranges*2d0
  
  IF N_ELEMENTS(xrange) EQ 0 THEN BEGIN
    xrange=[1999.,2012]
    xrange=[1999.,2014]
    ;xrange=[2010.5,2014]
    xrange=[1993.,2014]
    ;xrange=[2005.8,2014]
    xrange=[2009.,2015]
    ;xrange=[2011.,2012]
    xrange=[1992.,2016]
    xrange=[1997.,2017]
    ;xrange=[2010.,2017]
;    xrange=[2010.,2016.5]
    ;  xrange=[2008.,2012]
    ;xrange=[2006.,2010.6]
    ;  xrange=[2005.,2012]
    ;xrange=[2008,2011]
    ;xrange=[2014.85,2015.5]
  ;
  ;  xrange=[2006.,2010.6]
  ;xrange=[2013.7,2014.8]
;    xrange=[2010.62,2016.08]
  ;
  ENDIF
  
  FOR fi=0,nf-1 DO BEGIN
    file=files[fi]
    site=STRMID(GETFILENAME(file),0,4)
    ;print,file
    IF verbose THEN PRINT,'['+prog+']Processing ',STRTRIM(fi+1,2), STRTRIM(nf,2), $
      STRTRIM(FIX((fi+1)*100./nf),2),GETFILENAME(file), $
      getpathname(file),' ...', $
      format='(a,"(",a,"/",a,":",a,"%) <",a,"> in <",a,">",a)'
    READ_SIO,file,data=data
    data[3:*,*]=data[3:*,*]*sf
    ;data[3:*,*]=data[3:*,*]*1d2
    odata=data
    rdata=data
    rdata[3:5,*]=0
    
    WINDOW,1,xsize=xsize,ysize=ysize,title=file,/pixmap
    DEVICE, decomposed=1
    !p.MULTI=[0,1,3]
    IF N_ELEMENTS(data[0,*]) LE szwin THEN BEGIN
      PRINT,'['+PROG+']WARNING:too short time series!'
      CONTINUE
    ENDIF
    ;print,'szwin:',szwin
    
    FOR i=0,2 DO BEGIN
      odata[3+i,*]=SMOOTH(data[3+i,*],szwin,/edge_truncate )
      datai=REFORM(data[3+i,*])
      pos=FINITE(datai)
      pos=WHERE(pos EQ 1)
      IF pos[0] EQ -1 THEN CONTINUE ;which case?
      IF N_ELEMENTS(pos) LE szwin THEN CONTINUE ;% smooth: width must be nonnegative and smaller than array dimensions:
      odatai=SMOOTH(datai[pos],szwin,/edge_truncate)
      odata[3+i,pos]=odatai
      rdata[3+i,pos]=data[3+i,pos]-odata[3+i,pos]
      ;stop
      PLOT,data[0,*],data[3+i,*],background='ffffff'x,color='000000'x, $
        title=site+' '+xtitle, $
        xrange=xrange, $  ;for cmonoc
        ytitle=ytitles[i]+'/mm',$
        yrange=yranges[*,i], $
        charsize=1., $
        ;charsize=1.5, $
        /nodata,$
        xstyle=1, $
        ystyle=1
      OPLOT,data[0,*],data[3+i,*],color='00ff00'x,psym=psym,symsize=symsize
      OPLOT,odata[0,pos],odata[3+i,pos],color='ff0000'x,thick=1
    ;print,'odata[3+i,pos] max:',max(odata[3+i,pos])
    ENDFOR
    
    !p.MULTI=0
    img=TVRD(true=1)
    WDELETE,1
    
    ofile=opath+PATH_SEP()+GETFILENAME(file)
    IF out_neu EQ 1 THEN BEGIN
      odata[3:*,*]=odata[3:*,*]*1d-3
      WRITE_SIO,ofile,data=odata,user=user,prog=prog
    ENDIF
    ;continue
    jfile=desuffix(ofile)+'.jpg'
    WRITE_JPEG,jfile,img,true=1
    
    IF N_ELEMENTS(residpath) EQ 0 THEN BEGIN
      CONTINUE
    ENDIF
    
    
    
    WINDOW,1,xsize=xsize,ysize=ysize,title=file,/pixmap
    DEVICE, decomposed=1
    !p.MULTI=[0,1,3]
    FOR i=0,2 DO BEGIN
      ;stop
    
      PLOT,rdata[0,*],rdata[3+i,*],background='ffffff'x,color='000000'x, $
        title=site+' '+xtitle, $
        xrange=xrange, $  ;for cmonoc
        ytitle=ytitles[i]+'/mm',$
        yrange=yranges[*,i], $
        charsize=1.8, $
        ;charsize=1.5, $
        /nodata,$
        xstyle=1, $
        ystyle=1
      ;oplot,data[0,*],data[3+i,*],color='00ffff'x,psym=0
      ;            oplot,data[0,*],data[3+i,*],color='0000ff'x,psym=0
      OPLOT,rdata[0,*],rdata[3+i,*],color='0000ff'x,psym=psym
      OPLOT,xrange,[1,1],color='ff0000'x,thick=1
      OPLOT,xrange,[-1,-1],color='ff0000'x,thick=1
    ENDFOR
    
    !p.MULTI=0
    imgresid=TVRD(true=1)
    WDELETE,1
    ;
    ;
    ofile=residpath+PATH_SEP()+desuffix(GETFILENAME(file))+'_resid.neu'
    IF out_neu EQ 1 THEN BEGIN
      rdata[3:*,*]=rdata[3:*,*]*1d-3
      WRITE_SIO,ofile,data=rdata,user=user,prog=prog
      jfile=desuffix(ofile)+'.jpg'
      WRITE_JPEG,jfile,imgresid,true=1
    ENDIF
    
;  break
  ENDFOR
  
  IF verbose THEN PRINT,'['+prog+']Normal end.'
END