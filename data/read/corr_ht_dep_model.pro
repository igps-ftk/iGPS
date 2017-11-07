PRO CORR_HT_DEP_MODEL,FILE, OPATH

  IF N_PARAMS() LT 2 THEN BEGIN
  
    FILE='E:\Papers.data\Paper.SpatialFltering\cleanedNeuUnfTimeSeries.MEASURES_Combination.20111112.toFlt.demean.-2010.resid.963.cmc0_optimal_CORR_D_TAU_lin\cmc0.corr_lin\pbo600_sio_neu.snx'
    OPATH='E:\Papers.data\Paper.SpatialFltering\cleanedNeuUnfTimeSeries.MEASURES_Combination.20111112.toFlt.demean.-2010.resid.963.cmc0_optimal_CORR_D_TAU_lin\cmc0.corr_lin\ht_dep_model'
    
;    file='E:\Papers.data\Paper.SpatialFltering\cleanedNeuUnfTimeSeries.MEASURES_Combination.20111112.toFlt.demean.-2010.resid.963.cmc0_optimal_CORR_D_TAU\cmc0.600stable2.smoothed_resid.corr\pbo600_sio_neu.snx'
;    file='E:\Papers.data\Paper.SpatialFltering\cleanedNeuUnfTimeSeries.MEASURES_Combination.20111112.toFlt.demean.-2010.resid.963.cmc0_optimal_CORR_D_TAU\cmc0.600stable.smoothed.resid.corr\600_cmc_pbo_neu.snx'
;    file='E:\Papers.data\Paper.SpatialFltering\cleanedNeuUnfTimeSeries.MEASURES_Combination.20111112.toFlt.demean.-2010.resid.963.cmc0_optimal_CORR_D_TAU\cmc0.600stable.resid.corr\600_cmc_pbo_neu.snx'
  ENDIF
  
;  READ_CORR_SNX, $
;    FILE, $
;    SITES=SITES, $
;    CORR=CORR, $
;    BLEN_DEG=BLEN_DEG, $
;    BLEN_KM=BLEN_KM, $
;    LLH=LLHS
    
  SAVFILE=FILE+'.sav'  ;
  
  ;SAVE,FILENAME=SAVFILE, SITES,CORR,BLEN_DEG,BLEN_KM,LLHS
  RESTORE,FILENAME=SAVFILE
  
  HELP,CORR,BLEN_DEG,BLEN_KM,SITES,LLH
  R=50D0 ;IN KM
  
  PRINT,BLEN_KM[0:10]
  
  POS=WHERE(BLEN_KM LE R AND BLEN_KM GT 0)
  IF POS[0] EQ -1 THEN BEGIN
    PRINT,'NO DATA FOUND!'
    RETURN
  ENDIF
  
  X=DBLARR(N_ELEMENTS(POS))
  Y=CORR[POS]
  
  NCOL=N_ELEMENTS(BLEN_KM[*,0])
  NROW=N_ELEMENTS(BLEN_KM[0,*])
  
  FOR SI=0ULL,N_ELEMENTS(POS)-1 DO BEGIN
    ;
    CI=(POS[SI] MOD NCOL)
    RI=POS[SI]/NCOL
    X[SI]=ABS(LLHS[2,CI]-LLHS[2,RI])
    ;STOP
  ENDFOR
  
  IND=SORT(X)
  X=X[IND]
  Y=Y[IND]
  
  WINDOW,1;,XSIZE=800,YSIZE=600
  PLOT,X,Y,PSYM=3,BACKGROUN='FFFFFF'X,COLOR='0'X,XTITLE='Elevation Difference', $
    YTITLE='Correlation Coefficient'
  
  ;STOP
  
END