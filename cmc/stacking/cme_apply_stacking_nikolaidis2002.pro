;
;PURPOSE:
;  Calculate CME using regional stacking algorighm of Nikolaidis 2002.
;INPUT:
;  Residual time series (SIO/NEU format).
;OUTPUT:
;  Common-mode Errors time series (SIO/NEU format)
;
PRO CME_APPLY_STACKING_NIKOLAIDIS2002, $
    PATH, $
    OPATH, $
    CMEFILE, $    ;cme data file
    SF=SF, $ ;FOR PLOT SETTINGS
    XSIZE=XSIZE, $
    YSIZE=YSIZE, $
    ;XRANGE=XRANGE, $
    ;PSYM=PSYM, $
    SRC=SRC, $ ;OUTPUT HEADER
    ISPLOT=ISPLOT,  $
    _EXTRA=_EX
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PATH=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','eq.nepal20150425'], $
      'pos.neu')
;    OPATH=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','eq.nepal20150425'], $
;      'pos.neu.flt')
;    CMEFILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','eq.nepal20150425'], $
;      'cmes.neu')
      
;    path='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos3.neu.npst'
;    opath='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos3.neu.npst.flt'
;    cmefile='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\cme3.neu'
;    
;    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\resid.with.seasonal'
;    opath='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\resid.with.seasonal.flt'
;    cmefile='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\cmeyn.neu'
;    
;    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\resid.with.seasonal.flt'
;    opath='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\resid.with.seasonal.flt.de-anncme'
;    cmefile='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\anncme.neu'
;    
;    path='D:\gpse\eq.20161113.NewZealand.Mw7.8\daily\pos.neu.ngz'
;    opath='D:\gpse\eq.20161113.NewZealand.Mw7.8\daily\pos.neu.ngz.flt'
;    cmefile='D:\gpse\eq.20161113.NewZealand.Mw7.8\daily\cmes.neu'
;    
;    path='D:\gsar\asc\jiali.b\asc_F1\SBAS\outp_eq_gbjd2'
;    cmefile='D:\gsar\asc\jiali.b\asc_F1\SBAS\outp_eq_gbjd2\cmes.txt'
;    opath='D:\gsar\asc\jiali.b\asc_F1\SBAS\outp_eq_gbjd2\flt'
;    
;    path='D:\gsar\des\dangxiong2.b\des_F1\SBAS\outp_eq_gbjd2'
;    opath='D:\gsar\des\dangxiong2.b\des_F1\SBAS\outp_eq_gbjd2\flt'
;    cmefile='D:\gsar\des\dangxiong2.b\des_F1\SBAS\outp_eq_gbjd2\cmes.txt'
    ;
    NEUERRIS=[-1,-1,-1]
    ;
    path='C:\Downloads\izmi\izmi.neu\eu'
    
    path='D:\gsar\auto_grid\A113_0111\F1\sbas.2.0.0001.0072.20210801.20220609.027.0141.01.___\roi_11031\ts'
    
    
   cmefile=getpathname(path)+path_sep()+'cmes.neu'
  ;stop
    
  ENDIF
  
  IF N_ELEMENTS(SF) EQ 0 THEN SF=1D3 ;SCALE METER TO MILLIMETER
  IF N_ELEMENTS(XSIZE) EQ 0 THEN XSIZE=600
  IF N_ELEMENTS(YSIZE) EQ 0 THEN YSIZE=500
  IF N_ELEMENTS(ISPLOT) EQ 0 THEN ISPLOT=0
  ;
  IF N_ELEMENTS(cmefile) EQ 0 THEN BEGIN
    cmefile=getpathname(getpathname(path+PATH_SEP()+'ts.txt'))+PATH_SEP()+'cmes.neu'
    ;cmefile='\\gpsac5\root\g5c\gsar\cona1\des_F3\SBAS3\x10\cmes.neu'
    ;cmefile='D:\gsar\des\mila2s\des_F3\SBAS.556.atm0\x10\cmes.neu'
    ;    cmefile='\\gpsac5\root\g5c\tianyf\wulan3\asc_F2.2\SBAS3\x30\cmes.neu'
    ;cmefile='\\vmicd\root\c2\gsar\xiangyang_boruo1\asc_F2\SBAS5\x90\cmes.neu'
    PRINT,cmefile
  ;stop
  ENDIF
  IF N_ELEMENTS(opath) EQ 0 THEN BEGIN
    opath=getpathname(path+PATH_SEP()+'test.txt')+'.flt'
  ENDIF
  
  
  FILES = FILE_SEARCH(PATH+PATH_SEP()+'*.neu', COUNT=NF)
  IF NF LT 1 THEN BEGIN
    PRINT, '['+PROG+']ERROR:no time series files found!!', $
      FORMAT='(A)'
    RETURN
  ENDIF
  ;PRINT,CFILE
  ;STOP
  IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
  
  NEUIS=[3,4,5]
  IF N_ELEMENTS(NEUERRIS) EQ 0 THEN  NEUERRIS=NEUIS+3
  
  READ_SIO,CMEFILE,DATA=CME_NEU
  CMEMJDS=DBLARR(N_ELEMENTS(CME_NEU[0,*]))
  FOR I=0,N_ELEMENTS(CMEMJDS)-1 DO BEGIN
    JD=YDOY2MJD(CME_NEU[1,I],CME_NEU[2,I])
    CMEMJDS[I]=JD
  ENDFOR
  ;STOP
  
  ;;DO CORRECTION FOR EACH SITE
  FOR FI=0, NF-1 DO BEGIN
    ;stop
    FILE=FILES[FI]
    SITE=STRLOWCASE(STRMID(GETFILENAME(FILE),0,4))
    PRINT, '['+PROG+']Correct CME for '+SITE+' ...', $
      FORMAT='(A)'
    IF ISPLOT EQ 1 THEN BEGIN
      WINDOW,/FREE,XSIZE=XSIZE,YSIZE=YSIZE, $
        TITLE='Filtered [Nikolaidis, 2002] Positions for '+STRUPCASE(SITE),$
        /PIXMAP
      YTITLES=['North (mm)','East (mm)','Up (mm)']
      !P.MULTI=[0,1,3]
    ENDIF
    READ_SIO,FILE,DATA=DATA
    YRS=DATA[1,*]
    DOYS=DATA[2,*]
    JDS=YDOY2MJD(YRS, DOYS)
    RDATA=DATA[0:3,*]
    RDATA[*,*]=0
    FOR DI=0, N_ELEMENTS(DATA[0,*])-1 DO BEGIN
      ;;
      IND=WHERE(JDS[DI] EQ CMEMJDS)
      ;PRINT,IND
      IF IND[0] EQ -1 THEN BEGIN
        CME=[0,0,0]
      ENDIF ELSE BEGIN
        ;;
        CME=CME_NEU[3:5,IND]
      ENDELSE
      ;PRINT,CME
      RDATA[1:3,DI]=DATA[NEUIS,DI]-CME
      RDATA[0,*]=DATA[0,*]
    ENDFOR
    ;;
    ;stop
    ;;PLOT EACH COMPONENT
    ;print,first(data[0,*]),last(data[0,*])
    
    CAPLEN=(LAST(DATA[0,*])-FIRST(DATA[0,*]))/200D0
    
    FOR TI=0,2 DO BEGIN
      NEUI=NEUIS[TI]
      
      IF NEUERRIS[TI] NE -1 THEN BEGIN
        YU=DATA[NEUIS[TI],*]+DATA[NEUERRIS[TI],*]
        YL=DATA[NEUIS[TI],*]-DATA[NEUERRIS[TI],*]
        Y=REFORM(DATA[NEUIS[TI],*])
        YE=DATA[NEUERRIS[TI],*]
      ENDIF ELSE BEGIN
        YU=DATA[NEUIS[TI],*]
        YL=DATA[NEUIS[TI],*]
        YE=[-9999D0]
      ENDELSE
      
      
      YRANGE=[MIN(YL), MAX(YU)]*SF
      
      IF ISPLOT EQ 1 THEN BEGIN
        PLOT,DATA[0,*],DATA[NEUI,*]*SF, $
          BACKGROUND='FFFFFF'X, $
          COLOR=0, $
          TITLE='Filtered [Nikolaidis, 2002] Positions for '+STRUPCASE(SITE), $
          YTITLE=YTITLES[TI],$
          YRANGE=YRANGE, $
          CHARSIZE=1.8, $
          /NODATA
          
          
        IF NEUERRIS[TI] NE -1 THEN BEGIN
          FOR J=0,N_ELEMENTS(YU)-1 DO BEGIN
            OPLOT,[DATA[0,J],DATA[0,J] ],[ YU[J],YL[J] ]*SF, COLOR='00AAAA'X
            OPLOT,[ DATA[0,J]-CAPLEN,DATA[0,J]+CAPLEN ],[ YU[J],YU[J] ]*SF, COLOR='00AAAA'X
            OPLOT,[ DATA[0,J]-CAPLEN,DATA[0,J]+CAPLEN ],[ YL[J],YL[J] ]*SF, COLOR='00AAAA'X
          ENDFOR
        ENDIF
        OPLOT,DATA[0,*],DATA[NEUI,*]*SF,COLOR='0000FF'X, PSYM=-5,LINESTYLE=2
        OPLOT,RDATA[0,*],RDATA[TI+1,*]*SF,COLOR='FF0000'X, PSYM=-6,LINESTYLE=1
        
      ENDIF
    ENDFOR
    ;STOP
    ;BREAK
    ;WAIT,.5
    OFILE=OPATH+PATH_SEP()+GETFILENAME(FILE)
    print,ofile
    ODATA=DATA
    ODATA[NEUIS,*]=RDATA[1:3,*]
    WRITE_SIO, OFILE, DATA=ODATA, SRC=[FILE, CMEFILE], USER=USER, PROG=PROG
    
    IF ISPLOT EQ 1 THEN BEGIN
      JFILE=OPATH+PATH_SEP()+DESUFFIX(GETFILENAME(FILE))+'.jpg'
      T = TVRD(TRUE=1)
      WRITE_JPEG, JFILE, T, TRUE=1, QUALITY=100
    ENDIF
  ;STOP
  ;BREAK
  ENDFOR
  PRINT, '['+PROG+']Normal end.'
END