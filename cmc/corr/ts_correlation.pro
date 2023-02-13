;+
;
; PURPOSE:
;	  Calcualte correlation coefficients matrix for a set of cgps sites.
;
;   *)time series coorelation analysis is based upon rsiduals.
;   *) > 500 common epochs.
;   *)blenance is in angular degrees.
;   *)remove common mode signal will produce zero coefficients.
; INPUT:
;    SIO/IND_NEU (ONLY)
;
;-
PRO TS_CORRELATION, $
  path, $	;input time series path
  opath, $						;output correlation coefficients files path
  files=files,$ ;input files (overriding the path parameter)
  ind_neu=ind_neu, $					;column index for n/e/u, respectively
  ind_time=ind_time, $						;index for decimal year (or mjd, etc. )
  ind_err=ind_err, $
  blen_unit=blen_unit, $			;'degree', 'meter'
  ts_type=ts_type,$					;time series type: sio/pbo/...
  ll_rect=ll_rect, $
  dt_querystr=dt_querystr, $
  coords_file=coords_file, $
  gpsi_path=gpsi_path, $
  corr_file=corr_file, $
  blen_deg_file=blen_deg_file, $
  blen_km_file=blen_km_file, $
  prefix_network=prefix_network, $
  prefix_agency=prefix_agency, $
  snxfile=snxfile, $
  overwrite=overwrite, $
  corr_type=corr_type, $
  tlb=tlb, $  ; used for update igps status text, if specified.
  is_use_sav=is_use_sav, $
  verbose=verbose, $  ;whether or not output running information
  preview=preview, $  ;output baseline-correlation for each site
  errstr=errstr, $
  prog=prog_in, $
  status=status

  FORWARD_FUNCTION GETFILENAME, ydoy2mjd

  prog='TS_CORRELATION'
  IF N_ELEMENTS(PROG_IN) NE 0 THEN PROG=PROG_IN+':'+PROG

  t0=SYSTIME(/seconds)

  IF N_PARAMS() LT 2 THEN BEGIN

    path='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos15.neu.npst.cmc\cmc.diff.0-5'
    opath='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos15.neu.npst.cmc\cmc.diff.0-5.corr'
    coords_file='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos2.net'
    
    path='D:\gsar\asc\xiangyang_boruo1\asc_F2\SBAS\x10\raw'
    opath='D:\gsar\asc\xiangyang_boruo1\asc_F2\SBAS\x10\raw\cmc\corr'
    coords_file='D:\gsar\asc\xiangyang_boruo1\asc_F2\SBAS\x10\raw\sites.net'

    IS_USE_SAV=0
    ;IS_USE_SAV=1
  ENDIF

  STATUS=0;  0-RETURN WITH ERROR;  1-NORMAL END
  ERRSTR=''

  IF N_ELEMENTS(PREFIX_NETWORK) EQ 0 THEN PREFIX_NETWORK='UNKN'
  IF N_ELEMENTS(PREFIX_AGENCY) EQ 0 THEN PREFIX_AGENCY='UNKN'
  IF N_ELEMENTS(COORDS_FILE) EQ 0 THEN COORDS_FILE=GET_CFILE()  ;USE THE DEFAULT
  IF N_ELEMENTS(CORR_TYPE) EQ 0 THEN CORR_TYPE=0  ;USE THE DEFAULT [concordance correlation]

  IF N_ELEMENTS(IND_TIME) EQ 0 THEN IND_TIME=0 ;SIO/IND_NEU (ONLY)
  IF N_ELEMENTS(IND_NEU) EQ 0 THEN IND_NEU=[3,4,5]
  IF N_ELEMENTS(IND_ERR) EQ 0 THEN IND_ERR=[3,4,5]+3

  IF N_ELEMENTS(NEUSTR) EQ 0 THEN NEUSTR=['N','E','U']
  IF N_ELEMENTS(DT_QUERYSTR) EQ 0 THEN DT_QUERYSTR='*.neu'

  IF N_ELEMENTS(preview) EQ 0 THEN preview=0
  IF N_ELEMENTS(OVERWRITE) EQ 0 THEN OVERWRITE=1  ;Default is overwriting existing output files

  IF N_ELEMENTS(IS_USE_SAV) EQ 0 THEN IS_USE_SAV=1
  IF N_ELEMENTS(VERBOSE) EQ 0 THEN VERBOSE=1

  IS_READ_DATA=1

  LBL_ID=-1
  IF N_ELEMENTS(TLB) NE 0 THEN BEGIN
    LBL_ID=WIDGET_INFO(TLB, FIND_BY_UNAME='LBL_STATUS')
  ENDIF

  ;HELP, FILES
  ;STOP
  NF=N_ELEMENTS(FILES)
  IF NF EQ 0 THEN BEGIN
    FILES = FILE_SEARCH(PATH+PATH_SEP()+DT_QUERYSTR, COUNT=NF)
  ENDIF
  ;FOR DEBUG: ------------------------------------>>>>>>>>>> NF
  ;NF=10

  BFILE_DEG=OPATH+PATH_SEP()+PREFIX_NETWORK+'_'+PREFIX_AGENCY+'_baseline_deg.bln'
  BFILE_KM=OPATH+PATH_SEP()+PREFIX_NETWORK+'_'+PREFIX_AGENCY+'_baseline_km.bln'
  SNXFILE=OPATH+PATH_SEP()+PREFIX_NETWORK+'_'+PREFIX_AGENCY+'_corr_neu'+'.snx'
  OFILES=OPATH+PATH_SEP()+PREFIX_NETWORK+'_'+PREFIX_AGENCY+'_corr_'+NEUSTR+'.tcm'

  IF OVERWRITE EQ 0 THEN BEGIN
    IF FILE_TEST(BFILE_DEG) THEN BEGIN
      STR_STATUS=STRING('['+PROG+'] WARNING: output file <'+BFILE_DEG+'> already exist!! Skipped.')
      ERRSTR=STR_STATUS
      ;PRINT, STR_STATUS
      ;help, lbl_id
      IF LBL_ID NE -1 THEN MSGBOX,STR_STATUS,TITLE='iGPS',DIALOG_PARENT=TLB,/ERR
      RETURN
    ENDIF
  ENDIF

  ;if no PATH but FILES given (the PATH is blank or ignored in such case)
  IF N_ELEMENTS(FILES) NE 0 && FILES[0] NE '' THEN BEGIN
    PATH=GETPATHNAME(FILES[0])
  ENDIF

  NEU2SAV, $
    PATH, $   ;Input PATH
    FILES=FILES, $
    TLB=TLB, $
    PROG=PROG, $
    OVERWRITE=OVERWRITE


  FILE_SAVED=GETPATHNAME(PATH+PATH_SEP()+'text.txt')+'.sav'
  RESTORE,FILENAME=FILE_SAVED
  ;    ;get a priori coordinates for all sites
  READ_NET, COORDS_FILE, SITE=SITES,LLH=LLHS
  ;check site a priori coordinate. If no information, stop.
  POS=WHERE(FINITE(LLHS[0,*]) EQ 0)
  IF POS[0] NE -1 THEN BEGIN
    str_status=STRING('['+prog+'] ERROR:no priori coordinates for {',SITES(POS), '}!!', $
      FORMAT='(A,'+STRTRIM(N_ELEMENTS(POS)+1,2)+'(A4,:,1X),A)')
    ERRSTR=STR_STATUS
    PRINT, str_status
    IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
    RETURN
  ENDIF
  LLS=LLHS[0:1,*]
  HTS=REFORM(LLHS[2,*])

  ;STOP
  CORR_FILE=STRARR(N_ELEMENTS(IND_NEU))

  ;CALCULATE BLENANCE MATRIX
  str_status=STRING('['+prog+'] Calculating distances...', FORMAT='(A,$)')
  PRINT, str_status
  IF lbl_id NE -1 THEN WIDGET_CONTROL,lbl_id,set_value=str_status
  SITES_BLEN_DEG = DBLARR(NF,NF)
  SITES_BLEN_KM = DBLARR(NF,NF)
  OPENW,FIDB_DEG,BFILE_DEG,/GET_LUN
  OPENW,FIDB_KM,BFILE_KM,/GET_LUN
  CMDSTR="PRINTF,FIDB_DEG,' ',SITES[0:"+STRTRIM(NF-1,2)+"],FORMAT='(A4,1x,"+STRTRIM(NF,2)+"A12)'"
  TMP=EXECUTE(CMDSTR)
  CMDSTR="PRINTF,FIDB_KM,' ',SITES[0:"+STRTRIM(NF-1,2)+"],FORMAT='(A4,1x,"+STRTRIM(NF,2)+"A12)'"
  TMP=EXECUTE(CMDSTR)
  FOR CURI=0, NF-1 DO BEGIN
    ALONG0=LLS[0,CURI]
    ALAT0=LLS[1,CURI]
    FOR FI=CURI, NF-1 DO BEGIN
      ALONG1=LLS[0,FI]
      ALAT1=LLS[1,FI]
      ;ANGULAR BLENANCE IN DEGREE
      SITE_ANGLES = MAP_2POINTS( ALONG0, ALAT0, ALONG1, ALAT1 )
      SITES_BLEN_DEG[CURI,FI] = SITE_ANGLES[0]
      SITES_BLEN_DEG[FI,CURI] = SITE_ANGLES[0]
      ;METERS
      SITE_ANGLES = MAP_2POINTS( ALONG0, ALAT0, ALONG1, ALAT1, /METERS)
      SITES_BLEN_KM[CURI,FI] = SITE_ANGLES[0]/1000D0
      SITES_BLEN_KM[FI,CURI] = SITE_ANGLES[0]/1000D0

    ENDFOR
    CMDSTR="PRINTF,FIDB_DEG,SITES[CURI],SITES_BLEN_DEG[CURI,*],FORMAT='(A4,1X,"+STRTRIM(NF,2)+"(1X,F11.5))'"
    TMP=EXECUTE(CMDSTR)
    CMDSTR="PRINTF,FIDB_KM,SITES[CURI],SITES_BLEN_KM[CURI,*],FORMAT='(A4,1X,"+STRTRIM(NF,2)+"(1X,F11.3))'"
    TMP=EXECUTE(CMDSTR)
  ENDFOR
  FREE_LUN,FIDB_DEG
  FREE_LUN,FIDB_KM


  str_status=STRING('['+prog+'] Calculating correlation coefficients [PLEASE BE PATIENT!] ... ',FORMAT='(A)')
  PRINT, str_status, format='(A,$)'
  IF lbl_id NE -1 THEN WIDGET_CONTROL,lbl_id,set_value=str_status
  SITE_CORR = DBLARR(NF, NF, 3)
  T0=SYSTIME(/SECONDS)
  PERC_OLD=0
  FIDOS=INTARR(3)
  FOR NEUI=0,2 DO BEGIN
    OPENW,FID,OFILES[NEUI],/GET_LUN
    FIDOS[NEUI]=FID
    PRINTF,FIDOS[NEUI],' ',SITES[0:NF-1],FORMAT='(A4,1X,'+STRTRIM(NF,2)+'A12)'
  ENDFOR

  FOR CURI=0, NF-1 DO BEGIN
    INDI=REFORM(INDSA[*,CURI])
    FOR FI=CURI, NF-1 DO BEGIN
      PERC=FIX(((NF-1D0)*NF/2-(NF-CURI-1D0)*(NF-CURI)/2D0)/((NF-1D0)*NF/2D0)*100)
      IF PERC NE PERC_OLD THEN BEGIN
        IF verbose THEN BEGIN
          STR_STATUS=STRING(PERC,'%',FORMAT='(1X,I3,A1,$)')
          PRINT, STR_STATUS, FORMAT='(A,$)'
          IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE='['+prog+'] Calculating correlation coefficients: '+STR_STATUS
        ENDIF
        PERC_OLD=PERC
      ENDIF
      SITE1 = SITES[FI]

      ;MJD1 = REFORM((*MJDS[FI]))
      ;TMP=SET_INTERSECT(MJD0,MJD1,IND0=IND0,IND1=IND1)  ;1750 seconds

      INDJ=REFORM(INDSA[*,FI])
      POSIJ=WHERE(INDI EQ 1 AND INDJ EQ 1) ;NEW METHOD  WITH         312 seconds
      IF POSIJ[0] EQ -1 THEN BEGIN
        ;NO COMMON EPOCHS
        CONTINUE
      ENDIF
      NEPOCH=N_ELEMENTS(POSIJ)

      FOR NEUIND=0, N_ELEMENTS(IND_NEU)-1 DO BEGIN
        NEUI=IND_NEU[NEUIND]
        TS0=DATAA[NEUIND,POSIJ,CURI]
        TS1=DATAA[NEUIND,POSIJ,FI]

        INDNAN0=FINITE(TS0)
        INDNAN1=FINITE(TS1)
        INDNAN01=WHERE(INDNAN0 EQ 1 AND INDNAN1 EQ 1)

        IF INDNAN01[0] EQ -1 THEN BEGIN
          TMPCORR=0
        ENDIF ELSE BEGIN
          TS0=TS0[INDNAN01]
          TS1=TS1[INDNAN01]
          CASE CORR_TYPE OF
            1: BEGIN
              TMPCORR=CORRELATE(TS0, TS1,/DOUBLE)
            END
            0: BEGIN
              TMPCORR=CONCORDANCE_CORRELATION(TS0, TS1)
            END
          ENDCASE
        ENDELSE

        SITE_CORR[CURI,FI,NEUIND] = TMPCORR
        SITE_CORR[FI,CURI,NEUIND] = TMPCORR
      ENDFOR
    ENDFOR

    FOR NEUI=0,2 DO BEGIN
      PRINTF,FIDOS[NEUI],SITES[CURI],SITE_CORR[CURI,*,NEUI],FORMAT='(A4,1X,'+STRTRIM(NF,2)+'(1X,F11.5))'
    ENDFOR

  ENDFOR
  FOR NEUI=0,2 DO FREE_LUN,FIDOS[NEUI]
  T1=SYSTIME(/SECONDS)
  STR_STATUS=STRING('DONE WITH',T1-T0,FORMAT='(1X,A,I,1X,"seconds")')
  PRINT, STR_STATUS, FORMAT='(A)'
  IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS


  ;OUTPUT TO SINEX FILE
  STR_STATUS=STRING('['+prog+'] Writing corr sinex file '+SNXFILE+' ... ',FORMAT='(A,$)')
  PRINT, STR_STATUS, FORMAT='(A)'
  IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
  IF N_ELEMENTS(SNXFILE) GT 0 THEN BEGIN
    OPENW, FID, SNXFILE, /GET_LUN
    NEUS=['N','E','U']
    PRINTF, FID, '+SITE/ID'
    PRINTF, FID, '*Code','Longitude','Latitude','Height',$
      FORMAT='(A4,1X,3(A20,1X),:)'
    FOR SI=0, N_ELEMENTS(SITES)-1 DO BEGIN
      PRINTF,FID,SITES[SI],LLS[0,SI],LLS[1,SI],HTS[SI],FORMAT='(A4,1X,3(F20.8,1X),:)'
    ENDFOR
    PRINTF, FID, '-SITE/ID'

    FOR NEUI=0, N_ELEMENTS(IND_NEU)-1 DO BEGIN
      ;PRINTF, FID, NEUS[NEUI]
      SITES_CORR =REFORM(SITE_CORR[*,*,NEUI])
      SITES_CORR = MAT_TRANSFILL(TRANSPOSE(SITES_CORR))
      PRINTF, FID, '+CORR/'+NEUS[NEUI]
      FMT="( 2(I"+STRTRIM(STRLEN(STRTRIM(N_ELEMENTS(SITES),2)),2)+",1X),3(E10.3,1X),: )"
      FOR SI=0, N_ELEMENTS(SITES)-1 DO BEGIN
        NLINE=CEIL(N_ELEMENTS(SITES)/3D0)
        FOR I=0, NLINE-1 DO BEGIN
          PRINTF,FID,SI+1, I*3+1, SITES_CORR[SI,I*3:(I*3+2)<(N_ELEMENTS(SITES)-1)],FORMAT=FMT
        ENDFOR
      ENDFOR
      PRINTF, FID, '-CORR/'+NEUS[NEUI]
    ENDFOR

    SITES_BLEN_DEG = MAT_TRANSFILL(TRANSPOSE(SITES_BLEN_DEG))
    PRINTF, FID, '+BLEN/DEG'
    FMT="( 2(I"+STRTRIM(STRLEN(STRTRIM(N_ELEMENTS(SITES),2)),2)+",1X),3(E10.3,1X),: )"
    FOR SI=0, N_ELEMENTS(SITES)-1 DO BEGIN
      NLINE=CEIL(N_ELEMENTS(SITES)/3D0)
      FOR I=0, NLINE-1 DO BEGIN
        PRINTF,FID,SI+1, I*3+1, SITES_BLEN_DEG[SI,I*3:(I*3+2)<(N_ELEMENTS(SITES)-1)],FORMAT=FMT
      ENDFOR
    ENDFOR
    PRINTF, FID, '-BLEN/DEG'

    SITES_BLEN_KM = MAT_TRANSFILL(TRANSPOSE(SITES_BLEN_KM))
    PRINTF, FID, '+BLEN/KM'
    FMT="( 2(I"+STRTRIM(STRLEN(STRTRIM(N_ELEMENTS(SITES),2)),2)+",1X),3(E10.3,1X),: )"
    FOR SI=0, N_ELEMENTS(SITES)-1 DO BEGIN
      NLINE=CEIL(N_ELEMENTS(SITES)/3D0)
      FOR I=0, NLINE-1 DO BEGIN
        PRINTF,FID,SI+1, I*3+1, SITES_BLEN_KM[SI,I*3:(I*3+2)<(N_ELEMENTS(SITES)-1)],FORMAT=FMT
      ENDFOR
    ENDFOR
    PRINTF, FID, '-BLEN/KM'
    FREE_LUN, FID
  ENDIF

  ;RETURN
  ;Convert matrix to xy, for the purpose of ploting distance-correlation curves.
  ;Default NO output.
  ;  IF preview EQ 1 THEN BEGIN
  ;    CFILE=CORR_FILE
  ;    IF KEYWORD_SET(KM) THEN BEGIN
  ;      DFILE=BFILE_KM
  ;    ENDIF ELSE BEGIN
  ;      DFILE=BFILE_DEG
  ;    ENDELSE
  ;    PATH_CORR_XY=OPATH+PATH_SEP()+'.XY'
  ;    IF FILE_TEST(PATH_CORR_XY, /DIRECTORY) NE 1 THEN $
  ;      FILE_MKDIR, PATH_CORR_XY
  ;    CORR_MAT2XY, $
  ;      CFILE, $  ;CORRELATION FILES
  ;      DFILE, $  ;BASELINE FILES
  ;      PATH_CORR_XY, $  ;
  ;      NEUSTR=NEUSTR
  ;  ENDIF

  STATUS=1

  PRINT,'['+prog+'] Normal end.'
END
