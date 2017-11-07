

;+
; :Description:
;    Create QOCA Map format time series from GLOBK *.org files.
;
; :Params:
;    PATH
;    OPATH
;
; :Modifications:
;   + Mon Oct 26 15:31:37 CST 2015 by tianyf
;      - Revised the match string for seaching sites.
;        The old and new globk programs create data lines with different number of spaces following the 'Loc.' identifier.
;        e.g.
;        'Loc.  VILL_GPS U coordinate  (m)'  is the old format.
;        'Loc.   VILL_GPS U coordinate  (m)'  is the new format.
;
; :Author: tianyf
;-
PRO ORG2QOCAMAP, PATH, OPATH
  IF N_PARAMS() LT 2 THEN BEGIN
    PATH='E:\gpse\rerun.trnabc\comb\st_filter\globk\gsoln'
    OPATH='E:\gpse\rerun.trnabc\comb\st_filter\globk\map'
    
    path='\\gpsac4\dirs\home\tianyf\gpse\rerun.trnabc\comb\g3trns\gsoln'
    expt='g3ts'
    opath='E:\gpse\rerun.trnabc\comb\g3trns\gsoln\qmap'
    
    path='\\Gpsac\root\home\tianyf\gpse\rerun.cmonoc\comb\g3smjb\gsoln'
    expt='cmon'
    opath='\\Gpsac\root\home\tianyf\gpse\rerun.cmonoc\comb\g3smjb\gsoln\qmap'
    
    path='E:\tmp\gsoln'
    opath='E:\tmp\qoca'
    expt='0136'
    
    path='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln'
    opath='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\qmap'
    expt='labc'
    
    path='\\gpsac5\root\home\tianyf\gpse\lhasa\comb\trnh\gsoln\'
    opath='\\gpsac5\root\home\tianyf\gpse\lhasa\comb\trnh\gsoln\qmap'
    expt='lasa'
    
    path='D:\gpse\eq.nepal.2015apr25.M7.9\daily\gsoln'
    opath='D:\gpse\eq.nepal.2015apr25.M7.9\daily\gsoln.qmap'
    
    path='/home/tianyf/gpse/rerun.lutai/comb/trnsLTCM/gsoln'
    opath='/home/tianyf/gpse/rerun.lutai/comb/trnsLTCM/gsoln/qmap'
    expt='ltcm'
    
  ENDIF
  
  CFILE=GET_CFILE()
  
  FILES=FILE_SEARCH(PATH+PATH_SEP()+'globk_'+expt+'_?????.org',COUNT=NF)
  ;FILES=FILE_SEARCH(PATH+PATH_SEP()+'globk_'+expt+'_09[0]??.org',COUNT=NF)
  ;FILES=FILE_SEARCH(PATH+PATH_SEP()+'globk_'+expt+'_[90]9???.org',COUNT=NF)
  IF NF LE 0 THEN BEGIN
    HELP,NF
    RETURN
  ENDIF
  
  TFILE=OPATH+PATH_SEP()+'tmp_'
  
 
  
  
  OPENW,FID,TFILE,/GET_LUN
  PRINTF,FID,'****************************************************************************************************', $
    '*   time           E        N        Se       Sn      Ren      U        Su      Reu     Rnu     site      long     lati', $
    FORMAT='(A)'
    
  SITES=''
  
  FOR FI=0,NF-1 DO BEGIN
    FILE=FILES[FI]
    PRINT,'Processing file '+FILE+'...'
    LINES=READ_TXT(FILE)
    TMPL=GREPI(LINES,' Solution refers to')
    TMPL_=STRSPLIT(TMPL,'(',/EXTRACT)
    TMPL_=STRSPLIT(TMPL_[1],')',/EXTRACT)
    DYR=DOUBLE(TMPL_[0])
    ;HELP,TMPL
    TMPLS=GREPI(LINES,'Loc.  ')
    NSIT=N_ELEMENTS(TMPLS)/3
    
    FOR LI=0,N_ELEMENTS(TMPLS)/3-1 DO BEGIN
      TMPL=TMPLS[0+LI*3]
      TMPL_1=STRSPLIT(TMPL,/EXTRACT)
      TMPL=TMPLS[1+LI*3]
      TMPL_2=STRSPLIT(TMPL,/EXTRACT)
      TMPL=TMPLS[2+LI*3]
      TMPL_3=STRSPLIT(TMPL,/EXTRACT)
      
      SITE_GPS=TMPL_1[1]
      SITE=STRMID(TMPL_1[1],0,4)
      POS=WHERE(SITES EQ SITE)
      IF POS[0] EQ -1 THEN BEGIN
        IF SITES[0] EQ '' THEN BEGIN
          SITES=SITE
          READ_NET, CFILE, SITE=STRMID(SITE_GPS,0,4), LLH=LLH
          LLHS=LLH
        ENDIF ELSE BEGIN
          SITES=[SITES,SITE]
          READ_NET, CFILE, SITE=STRMID(SITE_GPS,0,4), LLH=LLH
          LLHS=[[LLHS],[LLH]]
        ENDELSE
      ENDIF
      
      ;POS=STRPOS(LINES,'Loc.   '+SITE_GPS+' U coordinate  (m)')
      POS=STRPOS(LINES,''+SITE_GPS+' U coordinate  (m)')
      PI=WHERE(POS NE -1)
      TMPL=LINES[PI+1]
      TMPL_4=STRSPLIT(TMPL,/EXTRACT)
      
      N=DOUBLE(TMPL_1[5])
      SN=DOUBLE(TMPL_1[7])
      E=DOUBLE(TMPL_2[5])
      SE=DOUBLE(TMPL_2[7])
      U=DOUBLE(TMPL_3[5])
      SU=DOUBLE(TMPL_3[7])
      RNE=DOUBLE(TMPL_4[3])
      RNU=DOUBLE(TMPL_4[4])
      REU=DOUBLE(TMPL_4[5])
      
      POS=WHERE(SITES EQ SITE)
      LLH=LLHS[*,POS]
      
      PRINTF,FID,DYR,E,N,SE,SN,RNE,U,SU,REU,RNU,SITE_GPS,LLH[0:1], $
        ;FORMAT='(1X,F12.8,4F9.2,F8.4,2F9.2,2F8.4,2X,A8,2F10.4)'
        FORMAT='(1X,F13.8,4F20.5,F8.4,2F20.5,2F8.4,2X,A8,2F10.4)'
    ;STOP
    ;BREAK
    ENDFOR
    
    
  ENDFOR
  FREE_LUN,FID
  
  ;convert temporary file to qoca-map files
   READ_COLS_ASCII,TFILE,DATA=DATA_,SKIP=2
  SITES_ALL=strmids(DATA_[10,*],0,4)
  ;POS=WHERE(strmids(DATA_[10,*],0,4) EQ SITES[SI])
  SITES=SITES_ALL[SORT(SITES_ALL)]
  SITES=SITES[UNIQ(SITES)]
  
  FOR SI=0,N_ELEMENTS(SITES)-1 DO BEGIN
    PRINT,SI+1,'/',N_ELEMENTS(SITES),' ', SITES[SI]
    OFILE=OPATH+PATH_SEP()+SITES[SI]+'.list'
    ;POS=WHERE(DATA_[10,*] EQ SITES[SI])
    POS=WHERE(SITES_ALL EQ SITES[SI])
    LINES=DATA_[*,POS]
    DYR=DOUBLE(LINES[0,*])
    IND=SORT(DYR)
    LINES1=LINES[*,IND]
    AVGE=MEAN(DOUBLE(LINES1[1,*]))
    AVGN=MEAN(DOUBLE(LINES1[2,*]))
    AVGU=MEAN(DOUBLE(LINES1[6,*]))
    OPENW,FID,OFILE,/GET_LUN
    FOR LI=0,N_ELEMENTS(LINES1[0,*])-1 DO BEGIN
      PRINTF,FID,DOUBLE(LINES1[0,LI]),[DOUBLE(LINES1[1,LI])-AVGE, DOUBLE(LINES1[2,LI])-AVGN, $
        DOUBLE(LINES1[3:4,LI]) ]*1D3, DOUBLE(LINES1[5,LI]), $
        [DOUBLE(LINES1[6,LI])-AVGU, DOUBLE(LINES1[7,LI]) ]*1D3, $
        DOUBLE(LINES1[8:9,LI]), $
        SITES[SI]+'_GPS', $
        ;strmid(LINES1[10,LI],0,4)+'_GPS', $
        DOUBLE(LINES1[11:12,LI]), $
        FORMAT='(1X,F13.8,4F9.2,F8.4,2F9.2,2F8.4,2X,A8,2F10.4)'
        ;FORMAT='(1X,F13.8,4F20.5,F8.4,2F20.5,2F8.4,2X,A8,2F10.4)'
    ENDFOR
    FREE_LUN,FID
    ;STOP
    
  ENDFOR
  
  ;STOP
  
  ;RETURN
  
END