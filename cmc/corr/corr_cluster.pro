PRO GET_CLASSES, clusters, gids_init, n_node, classes=classes
  ;gids_init=clusters[*,[class_ids]]
  pos=WHERE(gids_init NE -1)
  IF pos[0] EQ -1 THEN RETURN
  
  ;PRINT,gids_init
  FOR i=N_ELEMENTS(gids_init)-1,0,-1 DO BEGIN
    gid=gids_init[i]
    IF gid EQ -1 THEN CONTINUE
    IF gid LT n_node THEN BEGIN
    ;PRINT,'found class: group has only one node', gid
    ENDIF ELSE BEGIN
      gid_child=clusters[*,gid-n_node]
      pos1=WHERE(gids_init EQ gid_child[0])
      pos2=WHERE(gids_init EQ gid_child[1])
      IF pos1[0] EQ -1 && pos2[0] EQ -1 THEN BEGIN
      ;PRINT,'found class: parent group', gid
      ENDIF ELSE BEGIN
        gids_init[i] = -1
        GET_CLASSES, clusters, gids_init, n_node, classes=classes
      ;stop
      ENDELSE
    ;STOP
    ENDELSE
  ENDFOR
END


FUNCTION EXTRACT_CLUSTER_NODES, clusters, ci_ids, n_node
  ;PRINT,'input nodes:',ci_ids
  ;STOP
  IF ci_ids[0] GE n_node THEN BEGIN
    ;PRINT,'searching children for group id:', ci_ids[0]
    rowid=ci_ids[0]-n_node
    s1=EXTRACT_CLUSTER_NODES(clusters, clusters[*,rowid], n_node)
  ENDIF ELSE BEGIN
    s1=ci_ids[0]
  ;PRINT,'found node: ',ci_ids[0]
  ENDELSE
  
  IF ci_ids[1] GE n_node THEN BEGIN
    ;PRINT,'searching children for group id:', ci_ids[1]
    rowid=ci_ids[1]-n_node
    s2=EXTRACT_CLUSTER_NODES(clusters, clusters[*,rowid], n_node)
  ENDIF ELSE BEGIN
    s2=ci_ids[1]
  ;PRINT,'found node: ',ci_ids[1]
  ENDELSE
  
  
  
  RETURN, [s1,s2]
END

FUNCTION CORR_CLUSTER_FIND_PAIR, CORLINS,SITE_NAMES

  SITE_PAIRS=STRARR(N_ELEMENTS(CORLINS))
  NPAIR=0
  
  NSIT=N_ELEMENTS(CORLINS[0,*])
  FOR I=0,NSIT-1 DO BEGIN
    IND_ISGRP=INTARR(NSIT)
    FOR J=0,N_ELEMENTS(CORLINS[*,0])-1 DO BEGIN
      IF I EQ J THEN CONTINUE
      
      POS=WHERE(CORLINS[J,*] GT CORLINS[J,I] AND CORLINS[J,*] LT 1)
      ;STOP
      IF POS[0] NE -1 THEN CONTINUE ;NOT CURRENT CATEGORY
      ;STOP
      IND_ISGRP[J]=1
    ENDFOR
    POS=WHERE(IND_ISGRP EQ 1)
    IF POS[0] EQ -1 THEN BEGIN
      CONTINUE
    ENDIF ELSE BEGIN
      ;IF N_ELEMENTS(POS) GT 1 THEN STOP
      SITE_PAIRS[NPAIR]=SITE_NAMES[I]+','+SITE_NAMES[[POS]]
      NPAIR=NPAIR+N_ELEMENTS(POS)
      ;HELP,I,SITE_PAIRS
      PRINT,I,NPAIR
    ;HELP,POS
    ENDELSE
    
  ENDFOR
  
  RETURN,SITE_PAIRS[0:NPAIR-1]
END
;+
; PURPOSE:
;    CONVERT CORRELATION COEFFICIENTS MATRIX TO X-Y FORMAT.
;-
PRO CORR_CLUSTER, $
    FILE, $  ;CORRELATION FILES
    OFILES=OFILES, $  ;
    d_threshold=d_threshold, $
    LINKAGE=LINKAGE, $
    NEU=NEUis ;n/e/u ids, a combination of (0,1,2)
    
  PROG = 'CORR_CLUSTER'
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file='E:\Papers.data\Paper.SpatialFltering\cleanedNeuUnfTimeSeries.MEASURES_Combination.20111112.toFlt.demean.-2010.resid.963.corr_lin\pbo_sio_neu.snx'
    file='E:\Papers.data\Paper.SpatialFltering\cleanedNeuUnfTimeSeries.MEASURES_Combination.20111112.toFlt.demean.-2010.resid.963.CMC_optimal_CORR_D_TAU_LIN\cmc_0_3.5.cln.corr\pboCMC_sio_neu.snx'
    ;    file='E:\Papers.data\Paper.SpatialFltering\2014apr\cleanedNeuUnfTimeSeries.MEASURES_Combination.20130929.cln.cln.resid.cmc0_optimal_CORR_D_TAU_lin_3dot5\cmc_0_3.5b.CLN.corr\pboCMC_sio_neu.snx'
    file='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc_diff.corr\WNAM_CMC_neu.snx'
    file='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc_diff.test_la.corr\WNAM_CMC_neu.snx'
    file='E:\Papers.data\Paper.SpatialFltering\pbo\pbo.final_igs08.pos.neu.demean.cmc0_diff.corr\pbo_CMC_neu.snx'
    file='X:\home\tianyf\Papers.data\Paper.SpatialFltering\jpl\resid.enu.cmc_diff.corr\glb_jplCmc_neu.snx'
    ;file='C:\garner\CMC.corr\wnam_CMC_neu.snx'
    ;file='E:\Papers.data\Paper.SpatialFltering\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc_diff.corr\WNAM_CMC_neu.snx'
    ;file='C:\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318.cmc_diff.1999-.corr\WNAM_CMC_neu.snx'
    file='J:\phd\expt\gpsf\external\iGPS\example\eq.nepal20150425\pos.neu.transient\cmc.diff.0-3.corr\GLB_CMCDIFF_corr_neu.snx'
    IF FILE[0] EQ '' THEN RETURN
    
    IS_SAVED=1
    IS_SAVED=0
    
    
    ;CFILE='D:\phd\expt\gpsf\cgps\conf\sio.cmonoc.bjgps.llhxyz'
    cfile='C:\garner\aprioriFile_2015_088.net'
    ;cfile='E:\Papers.data\Paper.SpatialFltering\garner\aprioriFile_2015_088.net'
    CFILE='J:\phd\expt\gpsf\external\iGPS\example\eq.nepal20150425\lfile.nepl.net'
    
    NEUIS=[0,1]
  ;NEUIS=[1]
    
  ENDIF
  
  IF N_ELEMENTS(SF) EQ 0 THEN SF=1D0
  IF N_ELEMENTS(NEUIS) EQ 0 THEN NEUIS=0
  IF N_ELEMENTS(d_threshold) EQ 0 THEN d_threshold=.3d0*sf
  IF N_ELEMENTS(NEUSTR) EQ 0 THEN NEUSTR=['N','E','U']
  IF N_ELEMENTS(LINKAGE) EQ 0 THEN LINKAGE=0
  IF N_ELEMENTS(OFILES) GT 0 && N_ELEMENTS(NEUIS) NE N_ELEMENTS(OFILES) THEN BEGIN
    PRINT,'['+prog+']ERROR: number of input components and output filenames do not agree!!'
    RETURN
  ENDIF
  
  PRINT,'['+PROG+']Reading corr file ...'
  SAVFILE=FILE+'.sav'
  IF N_ELEMENTS(IS_SAVED) EQ 0 || IS_SAVED EQ 0 THEN BEGIN
    READ_CORR_SNX, $
      FILE, $
      SITES=SITES, $
      CORR=CORR, $
      BLEN_DEG=BLEN_DEG, $
      BLEN_KM=BLEN_KM, $
      LLH=LLHS
    SAVE,FILENAME=SAVFILE, SITES,CORR,BLEN_DEG,BLEN_KM,LLHS
  ENDIF ELSE BEGIN
    RESTORE,FILENAME=SAVFILE
  ENDELSE
  
  ;HELP,CORR,BLEN_DEG,BLEN_KM,SITES,LLHS
  
  
  ;  path=getpathname(file)
  ;  net_ext=(STRSPLIT(GETFILENAME(file),'_',/extract))[0]
  ;  org_ext=(STRSPLIT(GETFILENAME(file),'_',/extract))[1]
  ;NEUI=1
  
  FOR NEU_IND=0,N_ELEMENTS(NEUIS)-1 DO BEGIN
    NEUI=NEUIS[NEU_IND]
    IF N_ELEMENTS(OFILES) NE 0 && STRTRIM(OFILES[NEU_IND],2) NE '' THEN BEGIN
      OFILE=OFILES[NEU_IND]
    ENDIF ELSE BEGIN
      OFILE=getpathname(file)+PATH_SEP()+'class-'+DESUFFIX(GETFILENAME(FILE))+'-'+NEUSTR[NEUI]+STRING(d_threshold,format='("-dist",f07.3)')+STRING(LINKAGE,format='("-L",i1)')+'.txt'
    ENDELSE
    
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
    
    ;
    ;
    ;
    ;
    ;
;    CORLINS=REFORM(CORR[*,*,NEUI])
;    SITE_NAMES=SITES
;    NSIT=N_ELEMENTS(CORLINS[0,*])
;    NGROUP=0
;    nite=0
;    ;GOTO,no_while
;    Correlative_distance = SQRT((1-corlins)/2)
;    ;Correlative_distance = ((1-corlins))
;    ;stop
;    ;PRINT,correlative_distance[0:5,0:5]
;    ;n_node=30
;    ;n_node=10
    n_node=N_ELEMENTS(sites_use)
;    ;offset=100
;    correlative_distance1=correlative_distance[0:n_node-1,0:n_node-1]
;    ;correlative_distance1=correlative_distance[0+offset:n_node-1+offset,0+offset:n_node-1+offset]
;    ;HELP,correlative_distance1
;    
;    ;    fmtstr='('+STRTRIM(N_ELEMENTS(correlative_distance1[*,0]),2)+'(1x,f6.2))'
;    ;        ofile_t='c:\garner\correlative_distance1.txt'
;    ;    OPENW,fid,ofile_t,/get_lun
;    ;    FOR i=0,N_ELEMENTS(correlative_distance1[0,*])-1 DO $
;    ;      PRINTF,fid,correlative_distance1[*,i],format=fmtstr
;    ;    FREE_LUN,fid
;    ;    stop
;    ;
;    ;perform clustering
;    CLUSTERS = CLUSTER_TREE(correlative_distance1, linkdistance,LINKAGE=LINKAGE)
    ;STOP
    ;    for ci=0, n_elements(clusters[0,*])-1 do begin
    ;      print,clusters[*,ci],n_elements(sites)+ci,format='(3(1x,i))'
    ;    endfor
    
    ;return
    
    linkdistance=linkdistance*sf
    
    ; Create the dendrogram.
    DENDROGRAM, clusters, linkdistance, outverts, outconn, $
      LEAFNODES = LEAFNODES
      
      
      
    LEAFNODES=sites_use[LEAFNODES]
    ;PRINT, STRTRIM(LEAFNODES, 2)
    ;PRINT,linkdistance
    
    ;    set_plot,'ps'
    ;    device,file='j:\tmp\dendrogram.ps'
    ;      DENDRO_PLOT, clusters, linkdistance, $
    ;   POSITION = [0.1, 0.1, 0.38, 0.50], $
    ;   XSTYLE = 9, YSTYLE = 9, $
    ;   XTITLE = '', YTITLE = 'Correlative Distance', $
    ;   label_names=LEAFNODES, background='ffffff'x, color='0'x,font=1,charsize=1.5,$
    ;   yrange=[0,.81]
    ;
    ;   oplot,[-100,1000],[.6,.6],color='0000ff'x,linestyle=2,thick=2
    ;   device,/close
    ;   set_plot,'win'
    ;    return
    
    
    ;STOP
    ;
    ;    OPOLY = OBJ_NEW('IDLgrPolyline', outverts, $
    ;      POLYLINES = outconn)
    ;    LOC = FLTARR(2, n_node)
    ;    LOC[0, *] = FINDGEN(n_node)
    ;    OTEXT = OBJ_NEW('IDLgrText', STRTRIM(LEAFNODES,2), $
    ;      ALIGNMENT = 1, VERTICAL_ALIGN = 0.5, $
    ;      BASELINE = [0,1,0], UPDIR = [-1,0,0], $
    ;      CHAR_DIM = [1,1], LOCATIONS = loc)
    ;    OAXIS = OBJ_NEW('IDLgrAxis', 1, /EXACT, $
    ;      LOCATION = [-1,0,0], RANGE = [0, MAX(linkdistance)])
    ;    OAXIS -> GetProperty, TICKTEXT = oTick
    ;    OTICK -> SetProperty, CHAR_DIM = [1,1]
    ;    OMODEL = OBJ_NEW('IDLgrModel')
    ;    OMODEL -> Add, oPoly
    ;    OMODEL -> Add, oText
    ;    OMODEL -> Add, oAxis
    ;    XOBJVIEW, oModel
    ;
    ;    RETURN
    ;    STOP
    
    class_ids=WHERE(linkdistance GT d_threshold)
    IF class_ids[0] EQ -1 THEN BEGIN
      ;all belongs to a category
      STOP
      RETURN
    ENDIF
    
    ;stop
    ;get classes
    gids_init=clusters[*,class_ids]
    GET_CLASSES,clusters, gids_init, n_node,classes=classes
    
    ;STOP
    ;ofile=opath+PATH_SEP()+'tmp-classes-'+neustr[neui]+STRING(d_threshold,format='("-dist",f07.3)')+STRING(LINKAGE,format='("-L",i1)')+'.txt'
    ;OPENW,fid,ofile,/get_lun
    ;WRITE_SYS_INFO,fid,prog='CORR_CLUSTER',src=[file],user=user
    ;find members for each class
    pos=WHERE(gids_init NE -1)
    classes=gids_init[pos]
    n_class=N_ELEMENTS(classes)
    nsit_class=INTARR(n_class)
    member_id_class=INTARR(n_class,n_node)
    linkage_class=DBLARR(n_class)
    
    FOR ci=0,N_ELEMENTS(classes)-1 DO BEGIN
      PRINT,'class:',ci+1
      IF classes[ci] LT n_node THEN BEGIN
        PRINT,'class memers: #',1,' - ',sites[classes[ci]],format='(a,1x,i5,1x,10000(1x,a4))'
        ;PRINTF,fid,ci+1,1,sites[classes[ci]],format='(1x,i5,1x,i5,1x,10000(1x,a4))'
        nsit_class[ci]=1
        member_id_class[ci,classes[ci]]=1
        CONTINUE
      ENDIF ELSE BEGIN
        rowid=classes[ci]-n_node
      ENDELSE
      ci_ids=clusters[*,rowid]
      linkage_class[ci]=linkdistance[rowid]
      ci_node_ids=EXTRACT_CLUSTER_NODES(clusters,ci_ids,n_node)
      PRINT,ci_node_ids
      PRINT,'class members: #',N_ELEMENTS(ci_node_ids),' - ',sites_use[ci_node_ids],format='(a,1x,i5,1x,10000(1x,a4))'
      ;PRINTF,fid,ci+1,N_ELEMENTS(ci_node_ids),sites[ci_node_ids],format='(1x,i5,1x,i5,1x,10000(1x,a4))'
      nsit_class[ci]=N_ELEMENTS(ci_node_ids)
      member_id_class[ci,[ci_node_ids]]=1
    ;    STOP
    ENDFOR
    
    ;FREE_LUN,fid
    
    
    
    
    ;output classes
    OPENW,fid,ofile,/get_lun
    WRITE_SYS_INFO,FID,PROG=PROG,SRC=FILE,USER=USER
    PRINTF,FID,'group','#site','cor_d','member_site_list_...', format='("*",a5,1x,a5,1x,a7,1x,a)'
    ;STOP
    
    pos1=WHERE(nsit_class GT 1)
    pos2=SORT(nsit_class[pos1])   ;sort by number of member sites
    pos2b=REVERSE(pos2)
    FOR cI=0,N_ELEMENTS(pos2)-1 DO BEGIN
      sitids=WHERE(member_id_class[pos1[pos2b[ci]],*] EQ 1)
      ;ci_sites=sites[sitids]
      ;ci_sites=ci_sites[sort(ci_sites)]
      ;printf,fid,sitids
      PRINTF,fid,cI+1,nsit_class[pos1[pos2b[ci]]],linkage_class[pos1[pos2b[ci]]], $
        sites_use[sitids],format='(1X,I5,1X,I5,1X,F7.2,1X,'+STRTRIM(nsit_class[pos1[pos2b[ci]]]-1,2)+'(a4,","),a4)'
    ENDFOR
    
    ;out sporadic site
    pos1=WHERE(nsit_class EQ 1)
    pos2=SORT(linkage_class[pos1])
    pos2b=REVERSE(pos2)
    FOR cI=0,N_ELEMENTS(pos2)-1 DO BEGIN
      sitids=WHERE(member_id_class[pos1[pos2b[ci]],*] EQ 1)
      ;ci_sites=sites[sitids]
      ;ci_sites=ci_sites[sort(ci_sites)]
      ;printf,fid,sitids
      PRINTF,fid,0,nsit_class[pos1[pos2b[ci]]],linkage_class[pos1[pos2b[ci]]], $
        sites_use[sitids],format='(1X,I5,1X,I5,1X,F7.2,1X,a4)'
    ENDFOR
    if sites_out[0] ne '' then begin
    FOR cI=0,N_ELEMENTS(sites_out)-1 DO BEGIN
      PRINTF,fid,0,0,0, $
        sites_out[ci],format='(1X,I5,1X,I5,1X,F7.2,1X,a4)'
    ENDFOR
    endif
    FREE_LUN,fid
  ;stop
    
  ENDFOR
;STOP
  
END