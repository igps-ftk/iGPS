PRO ON_CORR_CLUSGTER_GUI_LOAD_RAW, ev
  COMMON data_raw, dataa_raw, dates_raw, indsa_raw, sites_raw
  ;HELP,ev,/st
  ;handle track event first
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Load the raw position time series for display.  *** If the data location changes, the data should be reloaded! ***'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  ;RAW TIME SERIES
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_RAW')
  WIDGET_CONTROL,ID,GET_VALUE=PATH
  ;help, file[0]
  PATH=STRTRIM(PATH[0],2)
  IF PATH EQ '' THEN BEGIN
    TMP=DIALOG_MESSAGE('No raw position time series selected!',/error,title='CHC')
    RETURN
  ENDIF
  
  WIDGET_CONTROL,ev.ID,set_value='X'
  FILE_SAV=GETPATHNAME(PATH+PATH_SEP()+'TEST.TXT')+'.sav'
  NEU2SAV, PATH, TLB=EV.TOP
  RESTORE, GETPATHNAME(PATH+PATH_SEP()+'TEST.TXT')+'.sav'
  dataa_raw=TEMPORARY(dataa)
  dates_raw=TEMPORARY(dates)
  indsa_raw=TEMPORARY(indsa)
  sites_raw=TEMPORARY(sites)
  WIDGET_CONTROL,ev.ID,set_value='Load'
  
  STR='Dataset loaded: '+PATH+'.sav'+' [Please delete the *sav file manually, if you want to re-create it].'
  WIDGET_CONTROL,id_lbl_status, set_value=str
  
END

PRO ON_CORR_CLUSGTER_GUI_LOAD_FLT, ev

  COMMON data_FLT, dataa_FLT, dates_FLT, indsa_FLT, sites_FLT
  
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Load the filtered position time series for display.  *** If the data location changes, the data should be reloaded! ***'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  ;FLT TIME SERIES
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_FLT')
  WIDGET_CONTROL,ID,GET_VALUE=PATH
  ;help, file[0]
  PATH=STRTRIM(PATH[0],2)
  IF PATH EQ '' THEN BEGIN
    TMP=DIALOG_MESSAGE('No filtered position time series selected!',/error,title='CHC')
    RETURN
  ENDIF
  
  
  WIDGET_CONTROL,ev.ID,set_value='X'
  FILE_SAV=GETPATHNAME(PATH+PATH_SEP()+'TEST.TXT')+'.sav'
  NEU2SAV, PATH, TLB=EV.TOP
  RESTORE, GETPATHNAME(PATH+PATH_SEP()+'TEST.TXT')+'.sav'
  dataa_FLT=TEMPORARY(dataa)
  dates_FLT=TEMPORARY(dates)
  indsa_FLT=TEMPORARY(indsa)
  sites_FLT=TEMPORARY(sites)
  WIDGET_CONTROL,ev.ID,set_value='Load'
  STR='Dataset loaded: '+PATH+'.sav'+' [Please delete the *sav file manually, if you want to re-create it].'
  WIDGET_CONTROL,id_lbl_status, set_value=str
  
  PRINT,sites_flt
END

PRO ON_CORR_CLUSGTER_GUI_LOAD_CMC, ev
  COMMON data_CMC, dataa_CMC, dates_CMC, indsa_CMC, sites_CMC
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Load the CMC difference time series for display.  *** If the data location changes, the data should be reloaded! ***'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  ;FLT TIME SERIES
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_CMC')
  WIDGET_CONTROL,ID,GET_VALUE=PATH
  ;help, file[0]
  PATH=STRTRIM(PATH[0],2)
  IF PATH EQ '' THEN BEGIN
    TMP=DIALOG_MESSAGE('No CMC difference time series selected!',/error,title='CHC')
    RETURN
  ENDIF
  
  
  WIDGET_CONTROL,ev.ID,set_value='X'
  FILE_SAV=GETPATHNAME(PATH+PATH_SEP()+'TEST.TXT')+'.sav'
  NEU2SAV, PATH, TLB=EV.TOP
  RESTORE, GETPATHNAME(PATH+PATH_SEP()+'TEST.TXT')+'.sav'
  dataa_CMC=TEMPORARY(dataa)
  dates_CMC=TEMPORARY(dates)
  indsa_CMC=TEMPORARY(indsa)
  sites_CMC=TEMPORARY(sites)
  WIDGET_CONTROL,ev.ID,set_value='Load'
  STR='Dataset loaded: '+PATH+'.sav'+' [Please delete the *sav file manually, if you want to re-create it].'
  WIDGET_CONTROL,id_lbl_status, set_value=str
  
  
END


PRO ON_CORR_CLUSGTER_GUI_RAD_RAW,ev
  WIDGET_CONTROL,ev.TOP,get_uvalue=st,/no_copy
  st.CUR_DT=0
  WIDGET_CONTROL,ev.TOP,set_uvalue=st,/no_copy
  ON_CORR_CLUSGTER_GUI_TREE_EVENTS, ev
END

PRO ON_CORR_CLUSGTER_GUI_RAD_FLT,ev
  WIDGET_CONTROL,ev.TOP,get_uvalue=st,/no_copy
  st.CUR_DT=1
  WIDGET_CONTROL,ev.TOP,set_uvalue=st,/no_copy
  ON_CORR_CLUSGTER_GUI_TREE_EVENTS, ev
END

PRO ON_CORR_CLUSGTER_GUI_RAD_CMC,ev
  WIDGET_CONTROL,ev.TOP,get_uvalue=st,/no_copy
  st.CUR_DT=2
  WIDGET_CONTROL,ev.TOP,set_uvalue=st,/no_copy
  ON_CORR_CLUSGTER_GUI_TREE_EVENTS, ev
END

PRO ON_CORR_CLUSTER_GUI_BTN_SAVE_RESULTS, EV

  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Save clustering results to a text file (using its left correlative distance threshold).'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  ;
  ofile=DIALOG_PICKFILE(title='Output file name?',filter='*.txt')
  IF ofile EQ '' THEN RETURN
  
  WIDGET_CONTROL, ev.ID, get_value=lbl_bak
  WIDGET_CONTROL, ev.ID, set_value='Working...'
  ;INPUT CORR FILE
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_IN')
  WIDGET_CONTROL,ID,GET_VALUE=FILE
  FILE=STRTRIM(FILE[0],2)
  IF FILE EQ '' THEN BEGIN
    TMP=DIALOG_MESSAGE('No input file selected!',/error,title='CHC')
    WIDGET_CONTROL, ev.ID, set_value=lbl_bak
    RETURN
  ENDIF
  
  ;LINKAGE OPTION
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='DP_LINKAGE')
  DP_LINKAGE=WIDGET_INFO(ID,/DROPLIST_SELECT)
  
  ;CORRELATIVE DISTANCE CUTOFF
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_CUTOFF')
  WIDGET_CONTROL,ID,GET_VALUE=CD_CUTOFF
  CD_CUTOFF=STRTRIM(CD_CUTOFF[0],2)
  IF CD_CUTOFF EQ '' THEN BEGIN
    TMP=DIALOG_MESSAGE('No correlative distance threshold entered!',/error,title='CHC')
    WIDGET_CONTROL, ev.ID, set_value=lbl_bak
    RETURN
  ENDIF
  CD_CUTOFF=DOUBLE(CD_CUTOFF)
  
  ;component
  
  ;HELP, FILE, DP_LINKAGE, CD_CUTOFF, NEUI
  
  ;
  ;  IF GETFILESUFFIX(FILE) EQ 'sav' THEN BEGIN
  ;    RESTORE,FILENAME=FILE
  ;  ENDIF ELSE BEGIN
  ;    IF GETFILESUFFIX(FILE) EQ 'snx' THEN BEGIN
  ;      READ_CORR_SNX, $
  ;        FILE, $
  ;        SITES=SITES, $
  ;        CORR=CORR, $
  ;        BLEN_DEG=BLEN, $
  ;        BLEN_KM=BLEN_KM, $
  ;        LLH=LLHS
  ;    ENDIF ELSE BEGIN
  ;      MSGBOX,['[CORR_CLUSTER_GUI]ERROR', $
  ;        '  Invalid file type <'+FILE+'>!!'],/ERROR, DIALOG_PARENT=EV.TOP
  ;      WIDGET_CONTROL, ev.ID, set_value=lbl_bak
  ;      RETURN
  ;    ENDELSE
  ;  ENDELSE
  
  
  WIDGET_CONTROL, ev.TOP, get_uvalue=st
  NEUI=ST.NEUI
  
  CORR_CLUSTER, $
    FILE, $  ;CORRELATION FILES
    OFILES=OFILE, $  ;
    d_threshold=CD_CUTOFF, $
    LINKAGE=DP_LINKAGE, $
    NEU=NEUi ;n/e/u ids, a combination of (0,1,2)
    
    
  WIDGET_CONTROL, ev.ID, set_value=lbl_bak
  
;  CORLINS=REFORM(CORR[*,*,NEUI])
;  IND1=FINITE(CORLINS)
;  POS0=WHERE(IND1 EQ 0)
;  IF POS0[0] NE -1 THEN BEGIN ;If there are NaN values, assign 0 to them.
;    CORLINS[POS0]=0
;  ENDIF
;  TMP=TOTAL(ABS(CORLINS),1)
;  POS=WHERE(TMP EQ 0)
;  SITES_OUT=''
;  SITES_USE=SITES
;  IF POS[0] NE -1 THEN BEGIN
;    SITES_OUT=SITES[POS]
;    INDEX_OUT=POS
;    ;MSGBOX,['[CORR_CLUSTER_GUI]WARNING','', $
;    ;'  No correlations for <'+STRJOIN(SITES[POS],' ')+'>!!'],/INFO
;    PRINT,'[CORR_CLUSTER_GUI]WARNING:no correlations for <'+STRJOIN(SITES[POS],' ')+'>!!'
;    POS=WHERE(TMP NE 0)
;    TMP=CORLINS[POS,*]
;    CORLINS=TMP[*,POS]
;    SITES_USE=SITES[POS]
;  ENDIF
;  ;stop
;  NSIT=N_ELEMENTS(CORLINS[0,*])
;  CORRELATIVE_DISTANCE = SQRT((1-CORLINS)/2)
;  ;N_NODES=10
;  N_NODES=N_ELEMENTS(SITES_USE)
;  CORRELATIVE_DISTANCE1=CORRELATIVE_DISTANCE[0:N_NODES-1,0:N_NODES-1]
;
;  ;perform clustering
;  CLUSTERS = CLUSTER_TREE(CORRELATIVE_DISTANCE1, LINKDISTANCE,LINKAGE=LINKAGE)
;
  
  
END

PRO ON_CORR_CLUSTER_GUI_BTN_SEARCH_SITE, EV
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Find and select the specified sites in the left text field (4-char site names separated by comma ","  ; case-insensitive).'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_SEARCH')
  WIDGET_CONTROL,ID,GET_VALUE=TMP
  TMP=STRTRIM(TMP[0],2)
  IF TMP EQ '' THEN RETURN  ;NO SITES ENTERED
  SITES_TO_FIND=STRSPLIT(TMP,',',/EXTRACT)
  SITES_TO_FIND=STRTRIM(SITES_TO_FIND,2)
  
  
  ID_TREE_ROOT=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TREE_ROOT')
  WIDGET_CONTROL,ID_TREE_ROOT,GET_UVALUE=ST_TREE
  WIDS=*ST_TREE.WIDS
  SITES=*ST_TREE.SITES
  
  SITES_TO_FIND=STRUPCASE(SITES_TO_FIND)
  SITES=STRUPCASE(SITES)
  TMP=SET_INTERSECT(SITES_TO_FIND, SITES, IND0=IND0, IND1=IND1)
  ;stop
  IF IND1[0] NE -1 THEN BEGIN
    WIDGET_CONTROL, ID_TREE_ROOT, SET_TREE_SELECT=0
    WIDGET_CONTROL, ID_TREE_ROOT, SET_TREE_SELECT=WIDS[IND1]
    POS=WHERE(STRUPCASE(SITES) EQ STRUPCASE(SITES_TO_FIND[0]))
    WIDGET_CONTROL, WIDS[POS[0]], /SET_TREE_VISIBLE
    ON_CORR_CLUSGTER_GUI_TREE_EVENTS, ev
  ENDIF
  
  IF IND0[0] EQ -1 || N_ELEMENTS(IND0) NE N_ELEMENTS(SITES_TO_FIND) THEN BEGIN
    IND0_INV=INV_IND(IND0, TOP=N_ELEMENTS(SITES_TO_FIND))
    STR=['[CORR_CLUSTER_GUI]WARNING',$
      '',$
      '  Cannot find sites: '+STRJOIN(SITES_TO_FIND[IND0_INV],', ')+'!', $
      '']
    MSGBOX,STR,/ERROR,DIALOG_PARENT=EV.TOP
  ENDIF
  
  
END

PRO ON_CORR_CLUSGTER_GUI_CKB_FIX_TIME,ev

END
PRO ON_CORR_CLUSGTER_GUI_CKB_FIX_Y,ev

END
PRO ON_CORR_CLUSGTER_GUI_DP_NEU, EV

  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='DP_NEU')
  NEUI=WIDGET_INFO(ID,/DROPLIST_SELECT)
  
  WIDGET_CONTROL, EV.TOP, GET_UVALUE=ST
  ST.NEUI=NEUI
  WIDGET_CONTROL, EV.TOP, SET_UVALUE=ST
  
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TREE_ROOT')
  ;clear the tree widget if needed
  IF WIDGET_INFO(id,/n_children) GT 0 THEN BEGIN
    cis=WIDGET_INFO(id,/all_children)
    FOR j=0, N_ELEMENTS(cis)-1 DO $
      WIDGET_CONTROL,cis[j],/destroy
  ENDIF
  
END


PRO ON_CORR_CLUSGTER_GUI_DP_PSYM, EV

  ;HELP, EV, /ST
  ;stop
  ;ID=WIDGET_INFO(EV.TOP, FIND_BY_UNAME='DP_PSYM')
  ;SEL=WIDGET_INFO(ID, /DROPLIST_SELECT)
  SEL=EV.INDEX
  IF SEL GT 7 THEN BEGIN
    SEL=-1*(SEL-7)
  ENDIF
  WIDGET_CONTROL, EV.TOP, GET_UVALUE=ST
  ST.PSYM=SEL
  WIDGET_CONTROL, EV.TOP, SET_UVALUE=ST
  ON_CORR_CLUSGTER_GUI_TREE_EVENTS, ev
END

PRO ON_CORR_CLUSGTER_GUI_DRAW_TS, ev
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    ;HELP,ev,/st
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Display position time series. First, select and LOAD the corresponding time series. Then, click sites in the left tree.'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
END

PRO ON_CORR_CLUSGTER_GUI_TREE_EVENTS, ev
  COMMON DATA_RAW, DATAA_RAW, DATES_RAW, INDSA_RAW, SITES_RAW
  COMMON DATA_FLT, DATAA_FLT, DATES_FLT, INDSA_FLT, SITES_FLT
  COMMON DATA_CMC, DATAA_CMC, DATES_CMC, INDSA_CMC, SITES_CMC
  
  dtstr=['raw','flt','cmc']
  
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Clustering tree. Click the below "Show Tree" to generate one. Select sites to display their time series. Hold down SHIFT/CTRL key when click to select multiple sites.'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  WIDGET_CONTROL,ev.TOP,get_uvalue=st
  ;help, st, /st
  cur_dt=st.CUR_DT
  psym=st.PSYM
  neui=st.NEUI
  sf=st.SF
  ;PRINT,'psym:',psym,NEUI,SF,CUR_DT
  
  id_draw=WIDGET_INFO(ev.TOP,find_by_uname='DRAW_TS')
  WIDGET_CONTROL,id_draw,get_value=tmp
  id_draw_ts=tmp[0]
  WSET,id_draw_ts
  
  ;HELP,ev,/st
  ;widget_control,ev.id,get_value=tmp
  ;help,tmp
  id_tree_root=WIDGET_INFO(ev.TOP,find_by_uname='TREE_ROOT')
  ids=WIDGET_INFO(id_tree_root,/tree_select)
  IF ids[0] EQ -1 THEN BEGIN
    sz=WIDGET_INFO(id_draw,/geometry)
    TV,MAKE_ARRAY(/byte,value=255,dimension=[sz.SCR_XSIZE,sz.SCR_YSIZE])
    ;stop
    ;      PLOT,[0,0],[0,0],background='ffffff'x,color='0'x, $
    ;         charsize=1.2,xmargin=[7,0.5],xstyle=1,xrange=xrange,ymargin=[2,.15],/nodata
    RETURN
  ENDIF
  sites=STRARR(N_ELEMENTS(ids))
  FOR i=0,N_ELEMENTS(ids)-1 DO BEGIN
    WIDGET_CONTROL,ids[i],get_value=site
    sites[i]=site
  ENDFOR
  ;HELP,sites
  ;PRINT,sites
  
  
  nsit=N_ELEMENTS(sites)
  sids=REPLICATE(-1L,nsit)
  xmins=DBLARR(nsit)
  xmaxs=DBLARR(nsit)
  
  cmdstr='isLoad=n_elements(sites_'+dtstr[cur_dt]+')'
  tmp=EXECUTE(cmdstr)
  IF isLoad EQ 0 THEN RETURN
  ;stop
  FOR si=0, N_ELEMENTS(sites)-1 DO BEGIN
    ;sid=WHERE(sites_raw EQ STRMID(sites[si],0,4))
    cmdstr='sid=WHERE(strupcase(sites_'+dtstr[cur_dt]+') EQ strupcase(STRMID(sites[si],0,4)))'
    tmp=EXECUTE(cmdstr)
    IF sid[0] EQ -1 THEN CONTINUE
    sids[si]=sid
    
    
    cmdstr='pos_t=WHERE(indsa_'+dtstr[cur_dt]+'[*,sids[si]] EQ 1)'
    tmp=EXECUTE(cmdstr)
    ;stop
    xmins[si]=first(pos_t)
    xmaxs[si]=last(pos_t)
  ENDFOR
  ;help, sids
  IF N_ELEMENTS(sids) EQ 1 && sids[0] EQ -1 THEN RETURN
  
  cmdstr='xrange=dates_'+dtstr[cur_dt]+'[0, [MIN(xmins),MAX(xmaxs)]]'
  tmp=EXECUTE(cmdstr)
  
  !p.MULTI=[0,1,Nsit]
  
  FOR si=0,Nsit-1 DO BEGIN
    IF sids[si] EQ -1 THEN BEGIN
      PLOT,[0,0],[0,0],background='ffffff'x,color='0'x, $
        charsize=1.2,xmargin=[7,0.5],xstyle=1,xrange=xrange,ymargin=[2,.15],/nodata
      CONTINUE
    ENDIF
    cmdstr='pos_t=WHERE(indsa_'+dtstr[cur_dt]+'[*,sids[si]] EQ 1)'
    tmp=EXECUTE(cmdstr)
    
    ;HELP,neui
    IF pos_t[0] EQ -1 THEN BEGIN
      PLOT,0,0,background='ffffff'x,color='0'x, $
        psym=psym, $
        charsize=1.2,xmargin=[7,0.5],xstyle=1,xrange=xrange,ymargin=[2,.15],/nodata
      CONTINUE
    ENDIF ELSE  BEGIN
      cmdstr="PLOT,dates_"+dtstr[cur_dt]+"[0,pos_t],sf*dataa_"+dtstr[cur_dt]+  $
        "["+STRTRIM(neui,2)+",pos_t,sids[si]],background='ffffff'x,color='0'x, psym=psym, "+  $
        "ytitle=sites[si], charsize=1.2,xmargin=[7,0.5],xstyle=1,xrange=xrange,ymargin=[2,.15],/nodata"
      tmp=EXECUTE(cmdstr)
      cmdstr="oPLOT,dates_"+dtstr[cur_dt]+"[0,pos_t],sf*dataa_"+dtstr[cur_dt]+  $
        "["+STRTRIM(neui,2)+",pos_t,sids[si]],color='0000ff'x, psym=psym "
      tmp=EXECUTE(cmdstr)
    ENDELSE
  ENDFOR
  !p.MULTI=-1
END

PRO ON_CORR_CLUSGTER_GUI_CLEAR_SELECTION, ev
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Clear the selection of sites in the clustering tree.'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  ;
  id_tree_root=WIDGET_INFO(ev.TOP,find_by_uname='TREE_ROOT')
  WIDGET_CONTROL,id_tree_root,set_tree_select=0
  ON_CORR_CLUSGTER_GUI_TREE_EVENTS, ev
  
END

PRO ON_BASE_CORR_CLUSTER_GUI_INIT, tlb

  ;  ID = WIDGET_INFO(tlb, FIND_BY_UNAME='DRAW_MAP')
  ;  WIDGET_CONTROL,ID,GET_VALUE=TMPIDN
  ;  WSET,TMPIDN[0]
  ;  INFO=WIDGET_INFO(ID,/GEOMETRY)
  ;  ;;HELP,INFO,/ST
  ;  TV,MAKE_ARRAY(INFO.SCR_XSIZE,INFO.SCR_YSIZE,VALUE=255)
  ;
  ;  ID = WIDGET_INFO(tlb, FIND_BY_UNAME='DRAW_TS')
  ;  WIDGET_CONTROL,ID,GET_VALUE=TMPIDN
  ;  WSET,TMPIDN[0]
  ;  INFO=WIDGET_INFO(ID,/GEOMETRY)
  ;  ;;HELP,INFO,/ST
  ;  TV,MAKE_ARRAY(INFO.SCR_XSIZE,INFO.SCR_YSIZE,VALUE=255)

  ;  ID = WIDGET_INFO(tlb, FIND_BY_UNAME='DRAW_MAP')
  ;  WIDGET_CONTROL,ID,GET_VALUE=TMPIDN
  ;  WSET,TMPIDN[0]
  ;  INFO=WIDGET_INFO(ID,/GEOMETRY)
  ;  ;;HELP,INFO,/ST
  ;  TV,MAKE_ARRAY(INFO.SCR_XSIZE,INFO.SCR_YSIZE,VALUE=255)

  ID = WIDGET_INFO(tlb, FIND_BY_UNAME='DRAW_TS')
  WIDGET_CONTROL,ID,GET_VALUE=TMPIDN
  WSET,TMPIDN[0]
  INFO=WIDGET_INFO(ID,/GEOMETRY)
  ;;HELP,INFO,/ST
  TV,MAKE_ARRAY(INFO.SCR_XSIZE,INFO.SCR_YSIZE,VALUE=255)
  
  
  id=WIDGET_INFO(tlb,find_by_uname='RAD_RAW')
  WIDGET_CONTROL,id,/set_button
  
  
END


PRO ON_CORR_CLUSGTER_GUI_BTN_IN, EV
  FILE=DIALOG_PICKFILE(FILTER=['*.snx','*.sav'])
  IF FILE EQ '' THEN RETURN
  CD,GETPATHNAME(FILE)
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_IN')
  WIDGET_CONTROL,ID,SET_VALUE=FILE
END

PRO ON_CORR_CLUSGTER_GUI_BTN_RAW, EV
  FILE=DIALOG_PICKFILE(FILTER=['*.sav'])
  IF FILE EQ '' THEN RETURN
  CD,GETPATHNAME(FILE)
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_RAW')
  WIDGET_CONTROL,ID,SET_VALUE=FILE
END

PRO ON_CORR_CLUSTER_GUI_BTN_OK, ev

  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    ;HELP,ev,/st
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Generate the hierarchical clustering tree and show it. Please make sure an inter-station correlation file is selected.'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  ;WIDGET_CONTROL, ev.ID, get_value=lbl_bak
  lbl_bak='Show Tree'
  WIDGET_CONTROL, ev.ID, set_value='Working...'
  ;INPUT CORR FILE
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_IN')
  WIDGET_CONTROL,ID,GET_VALUE=FILE
  FILE=STRTRIM(FILE[0],2)
  IF FILE EQ '' THEN BEGIN
    TMP=DIALOG_MESSAGE('No input file selected!',/error,title='CHC')
    WIDGET_CONTROL, ev.ID, set_value=lbl_bak
    RETURN
  ENDIF
  
  ;LINKAGE OPTION
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='DP_LINKAGE')
  DP_LINKAGE=WIDGET_INFO(ID,/DROPLIST_SELECT)
  
  ;CORRELATIVE DISTANCE CUTOFF
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TXT_CUTOFF')
  WIDGET_CONTROL,ID,GET_VALUE=CD_CUTOFF
  CD_CUTOFF=STRTRIM(CD_CUTOFF[0],2)
  IF CD_CUTOFF EQ '' THEN BEGIN
    TMP=DIALOG_MESSAGE('No correlative distance threshold entered!',/error,title='CHC')
    WIDGET_CONTROL, ev.ID, set_value=lbl_bak
    RETURN
  ENDIF
  CD_CUTOFF=DOUBLE(CD_CUTOFF)
  
  ;component
  
  ;HELP, FILE, DP_LINKAGE, CD_CUTOFF, NEUI
  
  ;
  IF GETFILESUFFIX(FILE) EQ 'sav' THEN BEGIN
    RESTORE,FILENAME=FILE
  ENDIF ELSE BEGIN
    IF GETFILESUFFIX(FILE) EQ 'snx' THEN BEGIN
      READ_CORR_SNX, $
        FILE, $
        SITES=SITES, $
        CORR=CORR, $
        BLEN_DEG=BLEN, $
        BLEN_KM=BLEN_KM, $
        LLH=LLHS
    ENDIF ELSE BEGIN
      MSGBOX,['[CORR_CLUSTER_GUI]ERROR', $
        '  Invalid file type <'+FILE+'>!!'],/ERROR, DIALOG_PARENT=EV.TOP
      WIDGET_CONTROL, ev.ID, set_value=lbl_bak
      RETURN
    ENDELSE
  ENDELSE
  
  
  WIDGET_CONTROL, ev.TOP, get_uvalue=st
  NEUI=ST.NEUI
  
  CORLINS=REFORM(CORR[*,*,NEUI])
  IND1=FINITE(CORLINS)
  POS0=WHERE(IND1 EQ 0)
  IF POS0[0] NE -1 THEN BEGIN ;If there are NaN values, assign 0 to them.
    CORLINS[POS0]=0
  ENDIF
  TMP=TOTAL(ABS(CORLINS),1)
  POS=WHERE(TMP EQ 0)
  SITES_OUT=''
  SITES_USE=SITES
  IF POS[0] NE -1 THEN BEGIN
    SITES_OUT=SITES[POS]
    INDEX_OUT=POS
    ;MSGBOX,['[CORR_CLUSTER_GUI]WARNING','', $
    ;'  No correlations for <'+STRJOIN(SITES[POS],' ')+'>!!'],/INFO
    PRINT,'[CORR_CLUSTER_GUI]WARNING:no correlations for <'+STRJOIN(SITES[POS],' ')+'>!!'
    POS=WHERE(TMP NE 0)
    TMP=CORLINS[POS,*]
    CORLINS=TMP[*,POS]
    SITES_USE=SITES[POS]
  ENDIF
  ;stop
  NSIT=N_ELEMENTS(CORLINS[0,*])
  CORRELATIVE_DISTANCE = SQRT((1-CORLINS)/2)
  ;N_NODES=10
  N_NODES=N_ELEMENTS(SITES_USE)
  CORRELATIVE_DISTANCE1=CORRELATIVE_DISTANCE[0:N_NODES-1,0:N_NODES-1]
  
  ;perform clustering
  CLUSTERS = CLUSTER_TREE(CORRELATIVE_DISTANCE1, LINKDISTANCE,LINKAGE=LINKAGE)
  
  
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='TREE_ROOT')
  ;clear the tree widget if needed
  IF WIDGET_INFO(id,/n_children) GT 0 THEN BEGIN
    cis=WIDGET_INFO(id,/all_children)
    FOR j=0, N_ELEMENTS(cis)-1 DO $
      WIDGET_CONTROL,cis[j],/destroy
  ENDIF
  pairs=clusters
  
  ;
  ;stop
  node_pids=INTARR(N_ELEMENTS(pairs[0,*]))  ;parent node id (if equals -1 then the leaf node)
  pid=n_nodes-1
  FOR li=0, N_ELEMENTS(pairs[0,*])-1 DO BEGIN
    pid=pid+1
    node_pids[li]=pid
  ;PRINT,'pair:', pairs[*,li], ' to make ',pid , format='(a,2(1x,i),1x,a,1x,i)'
  ENDFOR
  
  ;stop
  ;root
  ;;cmdstr='node_'+STRTRIM(last(node_pids),2)+'=widget_tree(id, /FOLDER, /EXPANDED,value="root",xsize=500,ysize=700,/multiple,draggable=0,event_pro="ON_CORR_CLUSGTER_GUI_tree_events",uname="TREE_ROOT1")'
  cmdstr='node_'+STRTRIM(last(node_pids),2)+'=id'
  ;PRINT,cmdstr
  tmp=EXECUTE(cmdstr)
  ;node_38=widget_tree(base_tree, /FOLDER, /EXPANDED,value='root')
  ;
  ;invalid sites
  WIDS_OUT=-1
  IF sites_out[0] NE '' THEN BEGIN
    WIDS_OUT=INTARR(N_ELEMENTS(SITES_OUT))
    FOR si=0, N_ELEMENTS(sites_out)-1 DO BEGIN
      cmdstr="node_"+STRTRIM(INDEX_OUT[SI],2)+"=widget_tree(ID, value='"+SITES_OUT[SI]+"')"
      ;PRINT,cmdstr
      tmp=EXECUTE(cmdstr)
      CMDSTR="WIDS_OUT[SI]=NODE_"+STRTRIM(INDEX_OUT[SI],2)
      TMP=EXECUTE(CMDSTR)
    ENDFOR
  ENDIF
  
  WIDS_IN=INTARR(N_ELEMENTS(node_pids)*2)
  SITES_IN=STRARR(N_ELEMENTS(node_pids)*2)
  ;stop
  IF last(pairs[0,*]) GT n_nodes THEN BEGIN
    pos=WHERE(node_pids EQ last(pairs[0,*])) & pos=pos[0]
    cmdstr="node_"+STRTRIM(last(pairs[0,*]),2)+"=widget_tree(node_"+STRTRIM(last(node_pids),2)+", /FOLDER, /EXPANDED,value='node_"+STRTRIM(last(pairs[0,*]),2)+"/"+STRTRIM(linkdistance[pos],2)+"')"
    site="node_"+STRTRIM(last(pairs[0,*]),2)+"/"+STRTRIM(last(linkdistance),2)
  ENDIF ELSE BEGIN
    cmdstr="node_"+STRTRIM(last(pairs[0,*]),2)+"=widget_tree(node_"+STRTRIM(last(node_pids),2)+", value='"+SITES_USE[last(pairs[0,*])]+"')"
    site=SITES_USE[last(pairs[0,*])]
  ENDELSE
  ;PRINT,cmdstr
  tmp=EXECUTE(cmdstr)
  ;node_35=widget_tree(node_38, /FOLDER, /EXPANDED,value="1")
  CMDSTR="WIDS_IN["+STRTRIM(N_ELEMENTS(node_pids)*2-2,2)+"]=NODE_"+STRTRIM(last(pairs[0,*]),2)
  TMP=EXECUTE(CMDSTR)
  SITES_IN[N_ELEMENTS(node_pids)*2-2]=SITE
  
  
  IF last(pairs[1,*]) GE n_nodes THEN BEGIN
    pos=WHERE(node_pids EQ last(pairs[1,*])) & pos=pos[0]
    cmdstr="node_"+STRTRIM(last(pairs[1,*]),2)+"=widget_tree(node_"+STRTRIM(last(node_pids),2)+", /FOLDER, /EXPANDED,value='node_"+STRTRIM(last(pairs[1,*]),2)+"/"+STRTRIM(linkdistance[pos],2)+"')"
    site="node_"+STRTRIM(last(pairs[1,*]),2)+"/"+STRTRIM(last(linkdistance),2)
  ENDIF ELSE BEGIN
    cmdstr="node_"+STRTRIM(last(pairs[1,*]),2)+"=widget_tree(node_"+STRTRIM(last(node_pids),2)+", value='"+SITES_USE[last(pairs[1,*])]+"')"
    site=SITES_USE[last(pairs[1,*])]
  ENDELSE
  ;PRINT,cmdstr
  tmp=EXECUTE(cmdstr)
  CMDSTR="WIDS_IN["+STRTRIM(N_ELEMENTS(node_pids)*2-1,2)+"]=NODE_"+STRTRIM(last(pairs[1,*]),2)
  TMP=EXECUTE(CMDSTR)
  SITES_IN[N_ELEMENTS(node_pids)*2-1]=SITE
  
  
  
  FOR pi=N_ELEMENTS(node_pids)-2, 0, -1 DO BEGIN
    cmdstr="node_"+STRTRIM(pairs[0,pi],2)+"=widget_tree(node_"+STRTRIM(node_pids[pi],2)
    IF pairs[0,pi] LT n_nodes THEN BEGIN
      site=SITES_USE[pairs[0,pi]]
    ENDIF ELSE BEGIN
      pos=WHERE(node_pids EQ pairs[0,pi]) & pos=pos[0]
      site='node_'+STRTRIM(pairs[0,pi],2)+"/"+STRTRIM(linkdistance[pos],2)
      cmdstr=cmdstr+", /FOLDER, /EXPANDED"
    ENDELSE
    cmdstr=cmdstr+",value='"+site+"')"
    
    ;PRINT,cmdstr
    tmp=EXECUTE(cmdstr)
    CMDSTR="WIDS_IN["+STRTRIM(PI*2,2)+"]=NODE_"+STRTRIM(pairs[0,PI],2)
    TMP=EXECUTE(CMDSTR)
    SITES_IN[PI*2]=SITE
    
    
    cmdstr="node_"+STRTRIM(pairs[1,pi],2)+"=widget_tree(node_"+STRTRIM(node_pids[pi],2)
    IF pairs[1,pi] LT n_nodes THEN BEGIN
      site=SITES_USE[pairs[1,pi]]
    ENDIF ELSE BEGIN
      pos=WHERE(node_pids EQ pairs[1,pi]) & pos=pos[0]
      site='node_'+STRTRIM(pairs[1,pi],2)+"/"+STRTRIM(linkdistance[pos],2)
      cmdstr=cmdstr+", /FOLDER, /EXPANDED"
    ENDELSE
    cmdstr=cmdstr+",value='"+site+"')"
    ;PRINT,cmdstr
    tmp=EXECUTE(cmdstr)
    CMDSTR="WIDS_IN["+STRTRIM(PI*2+1,2)+"]=NODE_"+STRTRIM(pairs[1,PI],2)
    TMP=EXECUTE(CMDSTR)
    SITES_IN[PI*2+1]=SITE
  ENDFOR
  
  st.LLHS = PTR_NEW(llhs)
  ;HELP, llhs
  WIDGET_CONTROL, ev.TOP, set_uvalue=st
  WIDGET_CONTROL, ev.ID, set_value=lbl_bak
  
  ;HELP, WIDS_OUT,WIDS_IN
  ;PRINT,WIDS_OUT
  
  IF WIDS_OUT[0] NE -1 THEN BEGIN
    WIDS_TREE=[WIDS_OUT,WIDS_IN]
    SITES_TREE=[SITES_OUT, SITES_IN]
  ENDIF ELSE BEGIN
    WIDS_TREE=WIDS_IN
    SITES_TREE=SITES_IN
  ENDELSE
  ST={WIDS: PTR_NEW(WIDS_TREE), SITES: PTR_NEW(SITES_TREE)}
  WIDGET_CONTROL,ID,SET_UVALUE=ST
;STOP
  
END

PRO ON_CORR_CLUSTER_GUI_BTN_QUIT, ev

  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Exit this program.'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  WIDGET_CONTROL, ev.TOP, /destroy
END

PRO ON_CORR_CLUSTER_GUI_BTN_HELP, ev
  ID_LBL_STATUS=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  IF( TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' ) THEN BEGIN
    CASE EV.ENTER OF
      0: BEGIN
        STR='Ready!'
      END
      1: BEGIN
        STR='Help about using the software.'
      END
    ENDCASE
    WIDGET_CONTROL,id_lbl_status, set_value=str
    RETURN
  ENDIF
  
  str=['Hierarchical Clustering based upon Correlation',  $
    '', $
    '(c)Copyright Yunfeng Tian 2015']
  tmp=DIALOG_MESSAGE(str,/info)
END

PRO CORR_CLUSTER_GUI_EVENT, ev

END

PRO CORR_CLUSTER_GUI, vTxtIn=vTxtIn, $
    vTxtRaw=vTxtRaw, $
    vTxtFlt=vTxtFlt, $
    vTxtCmc=vTxtCmc
    
  IF N_ELEMENTS(vTxtIn) EQ 0 THEN vTxtIn=FILEPATH(ROOT_DIR=!IGPS_ROOT, $
    SUBDIRECTORY=['example','eq.nepal20150425', $
    'pos.neu.cmc','cmc.diff.0-5.corr' $
    ],'GLB_CMCDIFF_corr_neu.snx')
  IF N_ELEMENTS(vTxtRaw) EQ 0 THEN vTxtRaw=FILEPATH(ROOT_DIR=!IGPS_ROOT, $
    SUBDIRECTORY=['example','eq.nepal20150425'], $
    'pos.neu')
  IF N_ELEMENTS(vTxtFlt) EQ 0 THEN vTxtFlt=FILEPATH(ROOT_DIR=!IGPS_ROOT, $
    SUBDIRECTORY=['example','eq.nepal20150425','pos.neu.cmc'], $
    'flt0')
  IF N_ELEMENTS(vTxtCmc) EQ 0 THEN vTxtCmc=FILEPATH(ROOT_DIR=!IGPS_ROOT, $
    SUBDIRECTORY=['example','eq.nepal20150425','pos.neu.cmc'], $
    'cmc.diff.0-5')
    
  ;input parameters
  base=WIDGET_BASE(/COL,TITLE='Hierarchical Clustering based upon Correlation', $
    notify_realize='on_base_corr_cluster_gui_init',space=0,xpad=0,ypad=0)
  BASE_IN=WIDGET_BASE(BASE,/ROW,/align_right,space=0,xpad=0,ypad=0)
  ;LBL_IN=WIDGET_LABEL(BASE_IN,VALUE='Input Correlation file:')
  ;TXT_IN=WIDGET_TEXT(BASE_IN,UNAME='TXT_IN',EDITABLE=1,XSIZE=120,value='E:\Papers.data\Paper.SpatialFltering\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc_diff.corr\WNAM_CMC_neu.snx.sav')
  ;BTN_IN=WIDGET_BUTTON(BASE_IN, VALUE='...',EVENT_PRO='ON_CORR_CLUSGTER_GUI_BTN_IN')
  TXT_IN = CW_DIRFILE(BASE_IN, TITLE = 'Input Inter-station Correlation File:', UNAME='TXT_IN', $
    value=vTxtIn, $
    STYLE='FILE', FILTER=['*.snx'], $
    XSIZE=100,FRAME=0)
    
    
  BASE_LINKAGE=WIDGET_BASE(BASE,/ROW,space=0,xpad=0,ypad=0,/ALIGN_CENTER)
  DP_LINKAGE=WIDGET_DROPLIST(BASE_LINKAGE,VALUE=['0 - nearest neighbor', $
    '1 - furthest neighbor','2 - weighted pairwise average','3 - weighted centroid'], $
    title='Linkage Method:',uname='DP_LINKAGE')
  DP_NEU=WIDGET_DROPLIST(BASE_LINKAGE,VALUE=['North','East','Up'], $
    title='Position Component:', uname='DP_NEU', $
    event_pro='ON_CORR_CLUSGTER_GUI_DP_NEU')
    
  ;showing results
    
    
  BASE_RAW=WIDGET_BASE(BASE,/ROW,/align_right,space=0,xpad=0,ypad=0)
  ;LBL_RAW=WIDGET_LABEL(BASE_RAW,VALUE='Raw Residual Time Series:')
  ;TXT_RAW=WIDGET_TEXT(BASE_RAW,UNAME='TXT_RAW',EDITABLE=1,XSIZE=120,value='E:\Papers.data\Paper.SpatialFltering\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.sav')
  ;BTN_RAW=WIDGET_BUTTON(BASE_RAW, VALUE='...',EVENT_PRO='ON_CORR_CLUSGTER_GUI_BTN_RAW')
  TXT_RAW = CW_DIRFILE(BASE_RAW, TITLE = 'Raw Position Time Series:', UNAME='TXT_RAW', $
    value=vTxtRaw, $
    STYLE='DIRECTORY', FILTER='*.sav', $
    XSIZE=100,FRAME=0)
  btn_load_raw=WIDGET_BUTTON(BASE_RAW,value='Load', event_pro='ON_CORR_CLUSGTER_GUI_load_raw',UNAME='BTN_LOAD_RAW',/TRACKING_EVENTS)
  
  BASE_FLT=WIDGET_BASE(BASE,/ROW,/align_right,space=0,xpad=0,ypad=0)
  ;LBL_FLT=WIDGET_LABEL(BASE_FLT,VALUE='Filtered Residual Time Series:')
  ;TXT_FLT=WIDGET_TEXT(BASE_FLT,UNAME='TXT_FLT',EDITABLE=1,XSIZE=120,value='F:\tmp\flt.sav')
  ;BTN_FLT=WIDGET_BUTTON(BASE_FLT, VALUE='...',EVENT_PRO='ON_CORR_CLUSGTER_GUI_BTN_FLT')
  TXT_FLT = CW_DIRFILE(BASE_FLT, TITLE = 'Filtered Position Time Series:', UNAME='TXT_FLT', $
    value=vTxtFlt, $
    STYLE='DIRECTORY', FILTER='*.sav', $
    XSIZE=100,FRAME=0)
  btn_load_flt=WIDGET_BUTTON(BASE_FLT,value='Load', event_pro='ON_CORR_CLUSGTER_GUI_load_flt',UNAME='BTN_LOAD_FLT',/TRACKING_EVENTS)
  
  
  BASE_CMC=WIDGET_BASE(BASE,/ROW,/align_right,space=0,xpad=0,ypad=0)
  ;LBL_CMC=WIDGET_LABEL(BASE_CMC,VALUE='Delta CMC Time Series:')
  ;TXT_CMC=WIDGET_TEXT(BASE_CMC,UNAME='TXT_CMC',EDITABLE=1,XSIZE=120,value='F:\tmp\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc_diff.sav')
  ;BTN_CMC=WIDGET_BUTTON(BASE_CMC, VALUE='...',EVENT_PRO='ON_CORR_CLUSGTER_GUI_BTN_CMC')
  TXT_CMC = CW_DIRFILE(BASE_CMC, TITLE = 'CMC Difference Time Series:', UNAME='TXT_CMC', $
    value=vTxtCmc, $
    STYLE='DIRECTORY', FILTER='*.sav', $
    XSIZE=100,FRAME=0)
  thisLetter = "154B
  greekLetter = '!4' + STRING(thisLetter) + '!X'
  btn_load_cmc=WIDGET_BUTTON(BASE_CMC,value='Load', event_pro='ON_CORR_CLUSGTER_GUI_load_cmc',UNAME='BTN_LOAD_CMC',/TRACKING_EVENTS)
  
  
  lbl_status=WIDGET_LABEL(BASE,value='Ready!',uname='LBL_STATUS',/DYNAMIC_RESIZE)
  
  base_cluster_result=WIDGET_BASE(base,/row,space=0,xpad=0,ypad=0)
  base_cluster_tree=WIDGET_BASE(base_cluster_result,/row)
  base_tree_root=WIDGET_TREE(base_cluster_tree,xsize=600,ysize=520,/folder,/expanded,/multiple,uname='TREE_ROOT',event_pro="ON_CORR_CLUSGTER_GUI_tree_events",/TRACKING_EVENTS)
  
  
  base_map_ts=WIDGET_BASE(base_cluster_result,/col,space=0,xpad=0,ypad=0)
  
  ;  lbl_map=WIDGET_LABEL(base_map_ts,value='Map View',uname='LBL_MAP')
  ;  draw_map=WIDGET_DRAW(base_map_ts,xsize=360,ysize=150,uname='DRAW_MAP')
  draw_ts=WIDGET_DRAW(base_map_ts,xsize=360,ysize=500,uname='DRAW_TS',/TRACKING_EVENTS,event_pro='ON_CORR_CLUSGTER_GUI_draw_ts')
  
  
  base_draw_param=WIDGET_BASE(base_map_ts,/row,space=0,xpad=0,ypad=0)
  base_draw_param_ts_type=WIDGET_BASE(base_draw_param,/exclusive,frame=0,/row,space=0,xpad=0,ypad=0)
  rad_raw=WIDGET_BUTTON(base_draw_param_ts_type,value='Raw',event_pro='ON_CORR_CLUSGTER_GUI_rad_raw',uname='RAD_RAW',/TRACKING_EVENTS)
  rad_flt=WIDGET_BUTTON(base_draw_param_ts_type,value='Filtered',event_pro='ON_CORR_CLUSGTER_GUI_rad_flt',/TRACKING_EVENTS)
  rad_cmc=WIDGET_BUTTON(base_draw_param_ts_type,value='delta_CMC',event_pro='ON_CORR_CLUSGTER_GUI_rad_cmc',/TRACKING_EVENTS)
  
  base_draw_param_fix_time=WIDGET_BASE(base_draw_param,/nonexclusive,space=0,xpad=0,ypad=0)
  ckb_fix_time=WIDGET_BUTTON(base_draw_param_fix_time,value='Fix Time',event_pro='ON_CORR_CLUSGTER_GUI_ckb_fix_time',/TRACKING_EVENTS,SENSITIVE=0)
  
  base_draw_param_fix_y=WIDGET_BASE(base_draw_param,/nonexclusive,space=0,xpad=0,ypad=0)
  ckb_fix_y=WIDGET_BUTTON(base_draw_param_fix_y,value='Fix Y',event_pro='ON_CORR_CLUSGTER_GUI_ckb_fix_y',/TRACKING_EVENTS, SENSITIVE=0)
  
  DP_PSYM = WIDGET_DROPLIST(base_draw_param, UNAME='DP_PSYM', $
    VALUE=['-','+','*','.','<>','^','[]','X','-+-', '-*-','-.-','-<>-', $
    '-^-','-[]-','-X-'],TRACKING_EVENTS=0, EVENT_PRO='ON_CORR_CLUSGTER_GUI_DP_PSYM')
    
    
  ;buttons
  BASE_BTNS=WIDGET_BASE(base,/row,space=0,xpad=0,ypad=0)
  BTN_OK=WIDGET_BUTTON(base_btns,value='Show Tree',event_pro='on_corr_cluster_gui_btn_ok',/TRACKING_EVENTS)
  BTN_CLEAR=WIDGET_BUTTON(base_btns,value='Clear Selected Sites', event_pro='ON_CORR_CLUSGTER_GUI_clear_selection',/TRACKING_EVENTS)
  
  
  TXT_SEARCH=WIDGET_TEXT(base_btns,VALUE='BJFS,lhaz,kkn4,daej,hyde',EDITABLE=1,UNAME='TXT_SEARCH',xsize=30,/TRACKING_EVENTS)
  BTN_SEARCH=WIDGET_BUTTON(base_btns,value='Find Sites',event_pro='on_corr_cluster_gui_btn_search_site',/TRACKING_EVENTS)
  
  
  LBL_CUTOFF=WIDGET_LABEL(base_btns,VALUE='   Cutoff Correlative Distance:')
  TXT_CUTOFF=WIDGET_TEXT(base_btns,VALUE='0.3',EDITABLE=1,UNAME='TXT_CUTOFF',xsize=6,/TRACKING_EVENTS)
  BTN_save=WIDGET_BUTTON(base_btns,value='Save Clustering Results',event_pro='on_corr_cluster_gui_btn_save_results',/TRACKING_EVENTS)
  
  
  btn_quit=WIDGET_BUTTON(base_btns,value='&Quit',event_pro='on_corr_cluster_gui_btn_quit',/TRACKING_EVENTS)
  btn_help=WIDGET_BUTTON(base_btns,value='&Read me first',event_pro='on_corr_cluster_gui_btn_help',/TRACKING_EVENTS)
  
  st={isLoadRaw: 0, $
    isLoadFlt: 0, $
    isLoadCmc: 0, $
    
    cur_dt: 0, $  ;0-raw; 1-flt; 2-cmc
    
    llhs: PTR_NEW(), $
    
    linkage: 0, $
    cutoff: 0.3, $
    neui: 0, $  ;0-north; 1-east; 2-up
    psym:0, $
    sf:1d3, $ ;for sio/neu format, the scale factor is 1D3 (m -> mm)
    
    dummy:-1}
    
    
  WIDGET_CONTROL,BASE,SET_UVALUE=ST
  WIDGET_CONTROL,BASE,/REALIZE
  
  XMANAGER,'CORR_CLUSTER_GUI',BASE,/NO_BLOCK
END