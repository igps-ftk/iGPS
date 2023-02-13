;+
;
;
;-
PRO CMC_CAL_OPTIMAL, $
    path, $  ;residual path
    corr_file, $  ;correlation coefficients
    opath, $  ;output cmc series path
    files=files,$ ;input files (overriding the path parameter)
    szwin=szwin, xrange=xrange, yranges=yranges, $  ;for smoothing time series
    dmin=dmin, $
    is_use_sav=is_use_sav, $
    tlb=tlb, $  ; used for update igps status text, if specified., $
    status=status, $
    fids=fids, $  ; file ids for files to process (use indgen(nf) to process all)
    sf=sf, $
    preview=preview, $  ;output smoothed cmc and filtered time series
    opath_cmc_raw=opath_cmc_raw, $
    opath_flt_raw=opath_flt_raw, $
    nmin=nmin, $
    prog=prog, $
    overwrite=overwrite, $
    _extra=_ex
    
  PROG='CMC_CAL_OPTIMAL'
  
  IF N_PARAMS() LT 3 THEN BEGIN
  
    ;for all pbo sites (2015mar24)
    PATH='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318'
    ;opath='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc0'
    sf=1d0
    ;if use saved session and you want to change opath, then you should change the opath after restore the session!
    CORR_FILE='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.corr\pbo_sio_neu.snx'
    ;
    ;@gpsac4, TOTAL time:         6153 seconds OR      102.5644166668256100 minutes
    
    ;path='E:\Papers.data\Paper.SpatialFltering\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318'
    ;corr_file='E:\Papers.data\Paper.SpatialFltering\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.corr\pbo_sio_neu.snx'
    
    ;path='/home/tianyf/garner/WNAM_Clean_ResidNeuTimeSeries_comb_20150318'
    ;corr_file='/home/tianyf/garner/WNAM_Clean_ResidNeuTimeSeries_comb_20150318.corr/pbo_sio_neu.snx'
    
    ;    path='E:\Papers.data\Paper.SpatialFltering\pbo\pbo.final_igs08.pos.neu.demean'
    ;    corr_file='E:\Papers.data\Paper.SpatialFltering\pbo\pbo.final_igs08.pos.neu.demean.corr\pbo_pbo_neu.snx'
    ;
    ;    path='/home/tianyf/Papers.data/Paper.SpatialFltering/jpl/resid.enu'
    ;    corr_file='/home/tianyf/Papers.data/Paper.SpatialFltering/jpl/resid.neu.corr/glb_jpl_neu.snx'
    
    ;for seasonal error analysis, sio
    path='E:\Papers.data\Paper.Seasonal.Positioning.Error\sio\GLB_Clean_ResidNeuTimeSeries_sopac_20160116.smoothed_resid.cln'
    corr_file='E:\Papers.data\Paper.Seasonal.Positioning.Error\sio\GLB_Clean_ResidNeuTimeSeries_sopac_20160116.smoothed_resid.cln.corr\glb_sio_neu.snx'
    ;    ;for jpl
    ;    path='e:\Papers.data\Paper.Seasonal.Positioning.Error\jpl\point.neu.smoothed_resid.cln'
    ;    corr_file='e:\Papers.data\Paper.Seasonal.Positioning.Error\jpl\point.neu.smoothed_resid.cln.corr\glb_jpl_neu.snx'
    sf=1d3
    ;nmin=3
    
    
    
    
    
    ;
    ;opath='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc3.5'
    OPATH='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc0b'
    OPATH='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc0c'
    opath='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc0_2016jan24'
    ;    opath='E:\Papers.data\Paper.SpatialFltering\pbo\pbo.final_igs08.pos.neu.demean.cmc0_ps'
    ;dmin=3.5d0
    ;opath='E:\Papers.data\Paper.SpatialFltering\pbo\pbo.final_igs08.pos.neu.demean.cmc3.5_ps'
    ;
    ;    opath='/home/tianyf/Papers.data/Paper.SpatialFltering/jpl/resid.enu.cmc0'
    ;    dmin=3.5d0
    ;    opath='/home/tianyf/Papers.data/Paper.SpatialFltering/jpl/resid.enu.cmc3.5'
    ;
    ;opath='/home/tianyf/garner/WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc0_2015may13'
    ;opath='E:\Papers.data\Paper.SpatialFltering\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc0_p'
    
    
    ;    PATH='D:\Papers.data\Paper.SpatialFltering\cmonoc\raw.resid'
    ;    OPATH='D:\Papers.data\Paper.SpatialFltering\cmonoc\raw.resid.cmc0'
    ;    CORR_FILE='D:\Papers.data\Paper.SpatialFltering\cmonoc\raw.resid.cmc0\corr\cmonoc_iscea_corr_neu.snx'
    opath='E:\Papers.data\Paper.Seasonal.Positioning.Error\sio\GLB_Clean_ResidNeuTimeSeries_sopac_20160116.smoothed_resid.cln.cmc0'
    ;    opath='E:\Papers.data\Paper.Seasonal.Positioning.Error\jpl\point.neu.smoothed_resid.cln.cmc0'
    
    
    path='D:\gsar\asc\mila1s\asc_F1\SBAS.948.atm0\x5\raw.resid'
    corr_file='D:\gsar\asc\mila1s\asc_F1\SBAS.948.atm0\x5\raw.cmc\corr\GLB_IGS_corr_neu.snx'
    opath='D:\gsar\asc\mila1s\asc_F1\SBAS.948.atm0\x5\raw.cmc'
    
    IS_USE_SAV=0
    IS_USE_SAV=1
    
    ;fids=indgen(3)
    ;fids=indgen(829)+830 ;Total time:         4863 seconds or       81.0591450492540986 minutes
    ;fids=indgen(9=830) ;Total time:         6278 seconds or      104.6464868823687198 minutes
    ;fids=[1004] ;site P405
    ;fids=249
    ;FIDS=635  ;GOUG FOR JPL
    
    preview=0
    
    
  ENDIF
  
  status=0
  
  LBL_ID=-1
  IF N_ELEMENTS(TLB) NE 0 THEN BEGIN
    LBL_ID=WIDGET_INFO(TLB, FIND_BY_UNAME='LBL_STATUS')
  ENDIF
  
  
  ;Model control parameters
  IF N_ELEMENTS(DMIN) EQ 0 THEN DMIN=0d0  ;first dmin = 0, then dmin=3.5
  IF N_ELEMENTS(NMIN) EQ 0 THEN NMIN=5  ;MINIMUM NUMBER OF CMC SITES
  IF N_ELEMENTS(TITLE_NEU) EQ 0 THEN $
    TITLE_NEU=['# Nsit/Esit/Usit: Number of base stations used to derive CMC.', $
    '#   DecYear Year DoY     North(m)      East(m)  Vertical(m)  #NSIT #ESIT #USIT ']
  IF N_ELEMENTS(FMTSTR) EQ 0 THEN FMTSTR='((1x,F10.5,1X,I04,1X,I03,3(1X,F12.7),1X,3(1X,I5)))'
  ;Grid search parameters
  ;
  ;    TAUS=INDGEN(50)+1
  ;    WS=INDGEN(50)+1
  ;
  ;grid size 5*5
  IF N_ELEMENTS(TAUS) EQ 0 THEN TAUS=INDGEN(10)*5+1
  IF N_ELEMENTS(WS) EQ 0 THEN WS=INDGEN(10)*5+1
  
  IF N_ELEMENTS(IS_USE_SAV) EQ 0 THEN IS_USE_SAV=0
  IF N_ELEMENTS(NEUSTR) EQ 0 THEN NEUSTR=['N','E','U']
  
  IF N_ELEMENTS(DT_QUERYSTR) EQ 0 THEN DT_QUERYSTR='*.neu'
  IF N_ELEMENTS(CFILE) EQ 0 THEN CFILE=GET_CFILE()
  IF N_ELEMENTS(SF) EQ 0 THEN SF=1D3
  IF N_ELEMENTS(PREVIEW) EQ 0 THEN PREVIEW=0
  IF N_ELEMENTS(OVERWRITE) EQ 0 THEN OVERWRITE=0
  
  ;outputting directories
  OPATH_FLT_RAW=OPATH+PATH_SEP()+'flt'+STRTRIM(DMIN,2) ;filtered time series (by CMC)
  OPATH_CMC_RAW=OPATH+PATH_SEP()+'cmc'+STRTRIM(DMIN,2) ; CMC
  OPATH_FLT_SMOOTHED=OPATH_FLT_RAW+'.smoothed' ;smoothed filtered time series (by CMC)
  OPATH_CMC_SMOOTHED=OPATH_CMC_RAW+'.smoothed' ; smoothed CMC
  ;ofile_stat ; report file
  IF FILE_TEST(OPATH_FLT_RAW) NE 1 THEN BEGIN
    FILE_MKDIR, OPATH_FLT_RAW
  ENDIF ELSE BEGIN
    IF OVERWRITE EQ 1 THEN BEGIN
      FILE_DELETE,OPATH_FLT_RAW,/RECURSIVE,/VERBOSE
      FILE_MKDIR, OPATH_FLT_RAW
    ENDIF
  ENDELSE
  IF FILE_TEST(OPATH_CMC_RAW) NE 1 THEN BEGIN
    FILE_MKDIR, OPATH_CMC_RAW
  ENDIF ELSE BEGIN
    IF OVERWRITE EQ 1 THEN BEGIN
      FILE_DELETE,OPATH_CMC_RAW,/RECURSIVE,/VERBOSE
      FILE_MKDIR, OPATH_CMC_RAW
    ENDIF
  ENDELSE
  IF PREVIEW EQ 1 THEN BEGIN
    IF FILE_TEST(OPATH_FLT_SMOOTHED) NE 1 THEN BEGIN
      FILE_MKDIR, OPATH_FLT_SMOOTHED
    ENDIF ELSE BEGIN
      IF OVERWRITE EQ 1 THEN BEGIN
        FILE_DELETE,OPATH_FLT_SMOOTHED,/RECURSIVE,/VERBOSE
        FILE_MKDIR, OPATH_FLT_SMOOTHED
      ENDIF
    ENDELSE
    IF FILE_TEST(OPATH_CMC_SMOOTHED) NE 1 THEN BEGIN
      FILE_MKDIR, OPATH_CMC_SMOOTHED
    ENDIF ELSE BEGIN
      IF OVERWRITE EQ 1 THEN BEGIN
        FILE_DELETE,OPATH_CMC_SMOOTHED,/RECURSIVE,/VERBOSE
        FILE_MKDIR, OPATH_CMC_SMOOTHED
      ENDIF
    ENDELSE
  ENDIF
  
  ;GRIDDING SIZE
  NTAU=N_ELEMENTS(TAUS)
  NW=N_ELEMENTS(WS)
  
  ;OUTPUT PLOT AXIS RANGES
  
  IF N_ELEMENTS(XRANGE) EQ 0 THEN BEGIN
    XRANGE=[2006.,2010.6]
    XRANGE=[2005.8,2014.]
    XRANGE=[1993,2014]
    XRANGE=[2003,2015.5]
    XRANGE=[1992,2015.2]
    XRANGE=[1992,2016.1]
  ENDIF
  
  IF N_ELEMENTS(YRANGES) EQ 0 THEN BEGIN
    YRANGES=DBLARR(2,3)
    YRANGES[*,0]=[-.005,.005]*1D3
    YRANGES[*,1]=[-.005,.005]*1D3
    YRANGES[*,2]=[-.01,.01]*1D3
    
  ;    YRANGES[*,0]=[-.005,.005]*2d3
  ;    YRANGES[*,1]=[-.005,.005]*2d3
  ;    YRANGES[*,2]=[-.01,.01]*2d3
  ENDIF
  
  ;HELP, FILES
  ;STOP
  
  NF=N_ELEMENTS(FILES)
  IF NF NE 0 THEN BEGIN
    PATH=GETPATHNAME(FILES[0])
  ENDIF ELSE BEGIN
    FILES = FILE_SEARCH(PATH+PATH_SEP()+DT_QUERYSTR, COUNT=NF)
  ENDELSE
  IF NF LE 0 THEN BEGIN
    STR_STATUS=STRING('['+PROG+'] ERROR: no files found in <',PATH,'>!!')
    PRINT, STR_STATUS
    IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
    RETURN
  ENDIF
  
  
  STR_STATUS=STRING('['+PROG+'] Read time sereis data in <'+PATH+'>.')
  PRINT, STR_STATUS
  IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
  ;first, read all time series and save as a *.sav file.
  ;help, overwrite
  ;STOP
  NEU2SAV, $
    PATH, $   ;Input PATH
    FILES=FILES, $
    TLB=TLB, $
    PROG=PROG, $
    OVERWRITE=OVERWRITE
  ;
  ;then, restore the time series file.
  FILE_SAVED=GETPATHNAME(PATH+PATH_SEP()+'text.txt')+'.sav'
  RESTORE,FILENAME=FILE_SAVED
  ;
  ;if no sig, then set to 1
  SZ=(SIZE(DATAA,/DIMENS))
  IF SZ[0] EQ 3 THEN BEGIN
    TMP=DBLARR(SZ[0]*2,SZ[1],SZ[2])
    TMP[0:2,*,*]=DATAA
    POS=WHERE(TMP[0,*,0] NE 0)
    IF POS[0] NE -1 THEN BEGIN
    TMP[3:*,POS,*]=1
    ENDIF
    DATAA=TMP
  ENDIF
  ;STOP
  ;
  ;Read/restore inter-station correlations
  FILE_SAVED=CORR_FILE+'.sav'
  IF N_ELEMENTS(IS_USE_SAV) GT 0 && IS_USE_SAV EQ 1 && FILE_TEST(FILE_SAVED) THEN BEGIN
    ;PRINT, '['+PROG+']Restoring saved session ...'
    STR_STATUS=STRING('['+PROG+'] Restoring saved correlations ...')
    PRINT, STR_STATUS
    IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
    RESTORE,FILENAME=FILE_SAVED
  ENDIF ELSE BEGIN
    STR_STATUS=STRING('['+PROG+'] Reading correlation coefficients ...')
    PRINT, STR_STATUS
    IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
    ;read correlation analysis sites (time-consuming for large network)
    READ_CORR_SNX, $
      CORR_FILE, $
      SITES=SITES, $
      CORR=CORRS, $
      BLEN_DEG=BLEN, $
      BLEN_KM=BLEN_KM, $
      LLH=LLHS
    ; save as .sav file for later use
    SAVE,FILENAME=FILE_SAVED,SITES,CORRS,BLEN, BLEN_KM, LLHS
  ENDELSE
  
  ;stop
  ;no correlation weighting (for pbo 746 sites 2014mar30 ONLY!)
  ;CORRS[*]=1
  
  ;CORRECT NaN VALUES IN CORRS IF ANY
  POS=FINITE(CORRS,/NAN)
  TMP=WHERE(POS EQ 1)
  IF TMP[0] NE -1 THEN BEGIN
    PRINT,'['+PROG+'] WARNING: NaN values detected in inter-station correlations!'
    CORRS[TMP]=0
  ENDIF
  
  
  ;
  ;  ;FILE TO SAVE OPTIMAL VALUES OF TAU AND W
  ;  OFILE_PARAM=OPATH+PATH_SEP()+'param_tau_w.txt'
  ;  OPENW,FIDO_PARAM,OFILE_PARAM,/GET_LUN
  ;  WRITE_SYS_INFO,FIDO_PARAM,PROG='CMC_CAL_OPTIMAL',SRC=OPATH
  ;  PRINTF,FIDO_PARAM,'SITE','NEU','TAU','W',FORMAT='("*",A4,1X,A3,1X,A9,1X,A9)'
  ;  FLUSH,FIDO_PARAM
  ;STOP
  
  T0=SYSTIME(/SECONDS)
  
  ;help,fids
  ;print,fids
  ;return
  IF N_ELEMENTS(FIDS) EQ 0 THEN BEGIN
    ;FIDS=INDGEN(9)
    ;FIDS=7
    FIDS=INDGEN(NF)
  ENDIF
  
  NFID=N_ELEMENTS(FIDS)
  
  ;FOR FI=0, NFID-1 DO BEGIN
  FOR FI=0, NFID-1 DO BEGIN
    CUR_FID=FIDS[FI]
    INIT_CMC_FILE_CMDSTR, CUR_FID, CMDSTR=CMDSTR
    ;STOP
    ;PRINT,'['+prog+']Processing '+FILES[CUR_FID]+' ...'
    STR_STATUS=STRING('['+prog+'] Processing '+STRTRIM(FI+1,2)+'/'+STRTRIM(NFID,2)+' <'+FILES[CUR_FID]+'> with dmin='+STRTRIM(DMIN,2)+' ...')
    PRINT, STR_STATUS
    IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
    ;STOP
    tmp=EXECUTE(CMDSTR)
    DELTA_T=SYSTIME(1)-T0*1ULL
    
  ENDFOR
  
  
  
  DELTA_T=SYSTIME(1)-T0*1ULL
  ;PRINT,'Total time: '+STRING(DELTA_T,FORMAT='(I)')+' seconds or '+STRING(DELTA_T/60D0,FORMAT='(F)')+' minutes'
  STR_STATUS=STRING('['+prog+'] Total time: '+STRING(DELTA_T,FORMAT='(I)')+' seconds or '+STRING(DELTA_T/60D0,FORMAT='(F)')+' minutes')
  PRINT, STR_STATUS
  IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
  
  
  
  ;RELEASE MEMORY
  CMC_ALL=-1
  CORRS=-1
  BLEN=-1
  BLEN_KM=-1
  DATAA=-1
  NSITS_ALL=-1
  DATES=-1
  TS=-1
  SSIGMA=-1
  CMC=-1
  FOR PI=0,N_ELEMENTS(INDS)-1 DO IF PTR_VALID(INDS[PI]) THEN PTR_FREE,INDS[PI]
  INDS=-1
  FOR I=0,N_ELEMENTS(HEADERS)-1 DO IF PTR_VALID(HEADERS[I]) THEN PTR_FREE,HEADERS[I]
  
  CBLEN=-1
  CCORR=-1
  CSITES=-1
  C_IJ=-1
  C_W_IJ=-1
  D_IJ=-1
  FILES=-1
  FLT_RMSS=-1
  HEADERS=-1
  HEADER=-1
  IND=-1
  LAT=-1
  LLHS=-1
  LLH_CMCS=-1
  LON=-1
  MJDMINS=-1
  MJDS=-1
  MJDSFI=-1
  NSITS=-1
  NSITES=-1
  ODATA=-1
  FOR I=0,N_ELEMENTS(PDATA)-1 DO IF PTR_VALID(PDATA[I]) THEN PTR_FREE,PDATA[I]
  PDATA=-1
  POS=-1
  RESID_FLT=-1
  RESID_UNF=-1
  SBLEN=-1
  SBLEN_=-1
  SCORR=-1
  SCORR_=-1
  SITES=-1
  SLLHS_=-1
  SSITES_=-1
  TMPIND=-1
  
  status=1
  ;STOP
  PRINT,'['+PROG+'] Normal end.'
;THAT'S ALL.
END
