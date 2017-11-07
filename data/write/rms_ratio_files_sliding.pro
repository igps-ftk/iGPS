PRO RMS_RATIO_FILES_SLIDING, path1, path2, opath
  IF N_PARAMS() LT 3 THEN BEGIN
    path1='J:\gpse\rerun.trnabc\comb\g3trns.noEF\gsoln\neu.demean.cln\resid.smoothed_resid\cln'
    path2='J:\gpse\rerun.trnabc\comb\gmf.atml\gsoln\neu.demean.cln\resid.smoothed_resid\cln'
    opath='J:\gpse\rerun.trnabc\comb\gmf.atml\gsoln\neu.demean.cln\resid.smoothed_resid\cln.rms.ratio.gmf'
    
    IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
    
    
  ENDIF
  
  file1s=FILE_SEARCH(path1+PATH_SEP()+'*.neu', count=nf1)
  IF nf1 LE 0 THEN BEGIN
    PRINT,'[RMS_RATIO_FILES_SLIDING]WARNING: no files found!
    RETURN
  ENDIF
  
  szwin=60
  hszwin=szwin/2
  
  HELP, file1s
  neustr=['n','e','u']
  ;xrange=[2011,2013]
  xrange=[2010,2013]
  ;xrange=[2011,2012]
  ;xrange=[2009,2013]
  ;xrange=[2010,2013]
  ;xrange=[2008,2011]
  ;xrange=[2007,2008]
  
  IF N_ELEMENTS(YRANGES) EQ 0 THEN BEGIN
    YRANGES=DBLARR(2,3)
    YRANGErS=DBLARR(2,3)
    
    YRANGES[*,0]=[-.005,.005]*1d3
    YRANGES[*,1]=[-.005,.005]*1d3
    YRANGES[*,2]=[-.01,.01]*2d3
    
    YRANGErS[*,0]=[-.00,.005]*1d3
    YRANGErS[*,1]=[-.00,.005]*1d3
    YRANGErS[*,2]=[-.0,.01]*2d3
    
  ;  YRANGES[*,0]=[-.003,.003]*1d3
  ;  YRANGES[*,1]=[-.003,.003]*1d3
  ;  YRANGES[*,2]=[-.008,.008]*1d3
    
  ;    YRANGES[*,0]=[-.005,.005]*2d3
  ;    YRANGES[*,1]=[-.005,.005]*2d3
  ;    YRANGES[*,2]=[-.01,.01]*2d3
    
  ;  YRANGES[*,0]=[-.005,.005]*10d3
  ;  YRANGES[*,1]=[-.005,.005]*10d3
  ;  YRANGES[*,2]=[-.005,.005]*10d3
  ;yranges=yranges*2
  ENDIF
  
  FOR fi=0, nf1-1 DO BEGIN
    file1=file1s[fi]
    PRINT,'[RMS_RATIO_FILES_SLIDING]Processing ',STRTRIM(fi+1,2), STRTRIM(nf1,2), $
      STRTRIM(FIX((FI+1)*100./NF1),2),getfilename(file1), $
      getpathname(file1),' ...', $
      FORMAT='(A,"(",A,"/",A,":",A,"%) <",A,"> in <",A,">",A)'
    site=STRMID(GETFILENAME(file1),0,4)
    
    file2s=FILE_SEARCH(path2+PATH_SEP()+site+'*.neu', count=nf2)
    IF nf2 LE 0 THEN BEGIN
      PRINT,'[]ERROR: cannot find 2nd file for '+site+'!'
      CONTINUE
    ENDIF
    file2=file2s[0]
    READ_SIO, file1, data=data1
    READ_SIO, file2, data=data2
    YDOYSTR1=STRING(DATA1[1,*],FORMAT='(I04)')+STRING(DATA1[2,*],FORMAT='(I03)')
    YDOYSTR2=STRING(DATA2[1,*],FORMAT='(I04)')+STRING(DATA2[2,*],FORMAT='(I03)')
    YDOYSTR=SET_INTERSECT(YDOYSTR1,YDOYSTR2,IND0=IND1,IND1=IND2)
    
    ODATA1=DATA1[*,IND1]
    ODATA2=DATA2[*,IND2]
    
    oData=odata1[0:5,*]
    
    ;stop
    
    WINDOW,1,xsize=1000,ysize=600,/pixmap
    !p.MULTI=[-1,3,3]
    
    FOR neui=0,2 DO BEGIN
      FOR di=0,N_ELEMENTS(odata1[0,*])-1 DO BEGIN
        xmin=(di-hszwin) > 0
        xmax=(di+hszwin) < (N_ELEMENTS(odata1[0,*])-1)
        oData[neui+3,di]=(RMS(odata1[neui+3,xmin:xmax])-RMS(odata2[neui+3,xmin:xmax]))/  $
          RMS(odata1[neui+3,xmin:xmax])*100d0
        ;STOP
      ENDFOR
      
      ;stop
      PLOT,data1[0,*],data1[neui+3,*]*1e3,background='ffffff'x,color='0'x,$
        xrange=xrange, $
        psym=-1,title=site+' '+neustr[neui]+', ts1',$
        ;yrange=yRangeS[*,neuI], $
        charsize=2
      PLOT,data2[0,*],data2[neui+3,*]*1e3,background='ffffff'x,color='0'x,$
        xrange=xrange, $
        psym=-1,title=site+' '+neustr[neui]+', ts2',$
        ;yrange=yRangeS[*,neuI], $
        charsize=2
      PLOT,odata[0,*],odata[neui+3,*]*1e0,background='ffffff'x,color='0'x,$
        xrange=xrange,$
        ytitle='%', $
        psym=1,title=site+' '+neustr[neui]+', rms ratio',$
        ;yrange=yRangerS[*,neuI], $
        charsize=2
      ;STOP
    ENDFOR
    !p.MULTI=-1
    ;stop
    ofile=opath+PATH_SEP()+site+'.gmf.vs.atml.rms.ratio.neu'
    WRITE_SIO, ofile, data=odata,prog='RMS_RATIO_FILES_SLIDING',user=user,src=[file1,file2], $
      headers='# Sliding window width: '+STRTRIM(szwin,2)+' days'
    jfile=desuffix(ofile)+'.jpg'
    WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
    ;BREAK
  ENDFOR
  
  
END