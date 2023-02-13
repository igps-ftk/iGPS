;+
; :Description:
;    Create QOCA NET a priori coordiante file (*.net) from ITRF/L-FILE APR files.
;
; :Params:
;    FILE
;    OFILE
;
; :Modifications:
; + On Sat, Nov 28, 2015 10:39:56 PM by tianyf
;   Add the RECT keyword to extract sites for a specific geographical range
;   RECT=[lon_min, lon_max, lat_min, lat_max]
;
;
; :Author: tianyf
;-
PRO LLHXYZ2NET, $
    FILE, $
    OFILE, $
    RECT=RECT
    
  PROG='LLHXYZ2NET'
  
  IF N_PARAMS() LT 2 THEN BEGIN
    ;FILE=DIALOG_PICKFILE(TITLE='Input iGPS LLHXYZ *.llhxyz file?', $
    ;  FILTER=[['*.llhxyz','*'],['iGPS LLHXYZ File (*.llhxyz)','iGPS LLHXYZ File (*)']])
    file='D:\phd\expt\gpsf\cgps\conf\cmonoc_reg.llhxyz'
    file='Z:\home\tianyf\gpse\rerun.cmonoc\solut\gmf\xian\lfile.cmo3'
    file='D:\phd\expt\gpsf\cgps\conf\sio.cmonoc.bjgps.llhxyz'
    file='D:\phd\expt\gpsf\cgps\conf\cmonoc\cmonoc_jb.llhxyz'
    file='E:\gpse\eq.NewZealand.20100904.Mw7.2\map\nzg.llhxyz'
    file='E:\gpse\rerun.nepal\solut\gmf2.mht2\wuhn.llhxyz'
    file='D:\phd\expt\gpsf\external\iGPS\tables\nzg.llhxyz'
    file='E:\load\predicted\forGlobal\drv\gridp_2.5.llhxyz'
    file='E:\Papers.data\Paper.SpatialFltering\pbo\gage_gps.igs08.llhxyz'
    file='E:\Papers.data\Paper.SpatialFltering\pbo\pbo.final_igs08.llhxyz'
    file='D:\tmp\lgn.llhxyz'
    file='D:\tmp\lasa\pos2.llhxyz'
    file='C:\mirror_ftp\everest.mit.edu\pub\TimeSeries\mit.llhxyz'
    file='\\gpsac4\tianyf\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos.llhxyz'
    file='D:\tmp\tst1\icds\icd.llhxyz'  
    file='C:\Downloads\pbo\pbo.llhxyz'  
    file='D:\jiang\2018.10.21.gps.tibet.site\ICD.Continuous.GNSS.Tibet.2018..llhxyz'
    
    IF FILE EQ '' THEN RETURN
    CD,GETPATHNAME(FILE)
    ;OFILE=DP(/WRITE,FILTER=[['*.net'],['QOCA Network File (*.net)']],/AF)
    OFILE=DESUFFIX(FILE)+'.net'
    IF OFILE EQ '' THEN RETURN
    CD,GETPATHNAME(OFILE)
    
    ;RECT=[100,105,23,28]*1D0
  ENDIF
  
  READ_LLHXYZ, FILE, SITE=SITES,LLH=LLHS
  
  IF N_ELEMENTS(RECT) EQ 4 THEN BEGIN
    POS=WHERE(LLHS[0,*] GE RECT[0] AND LLHS[1] LE RECT[1] AND $
      LLHS[1,*] GE RECT[2] AND LLHS[1,*] LE RECT[3])
    IF POS [0] EQ -1 THEN BEGIN
      PRINT,'['+PROG+']WARNING: no sites in the geographical range!'
      RETURN
    ENDIF
    SITES=STIES[POS]
    LLHS=LLHS[*,POS]
  ENDIF
  
  NS=N_ELEMENTS(SITES)
  IF NS EQ 0 THEN RETURN
  
  EPOCHS=DBLARR(NS)
  EPOCHS[*]=2005
  
  
  WRITE_NET, $
    OFILE, $
    SITES=SITES, $
    LLHS=LLHS, $
    USER=USER, $
    PROG=PROG, $
    SRC=FILE, $
    EPOCHS=EPOCHS
    
  PRINT,'['+PROG+']Normal end.'
  
END