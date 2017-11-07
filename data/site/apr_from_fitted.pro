;Create your own itrf apr file from fitted XYZ time series
;
PRO APR_FROM_FITTED, path, ofile
  IF N_PARAMS() LT 2 THEN BEGIN
    path='/home/tianyf/gpse/rerun.nepal/comb/20100716igs/gsoln/rawXyz.neu.cln_166_saa.resid/.fitted'
    ofile='/home/tianyf/gpse/rerun.nepal/comb/20100716igs/tables/itrf05_tyf.apr'
    
    path='E:\data\nepal\tian\20100716t\rawXyz.neu.forFrame.cln.resid/.fitted'
    ofile='E:\data\nepal\tian\20100716t\itrf05_trnabc_rf63.apr'
    
    path='E:\gpse\rerun.cmonoc\comb\vmf1trns2\gsoln\xyz.south39.neu.cln.resid\.fitted'
    ofile='E:\gpse\rerun.cmonoc\comb\vmf1trns2\gsoln\itrf05.rf39.apr'
    
    path='E:\data\cmonoc\rerun\20100128b\xyz.cmonoc_frm.neu.cln.resid\.fitted'
    ofile='E:\gpse\rerun.cmonoc\comb\cmonoc.ref\tables\cmonoc25_ref_frame.apr'
    
    path='E:\gpse\rerun.cmonoc\comb\vmf1trns2\gsoln\xyz.Tigs05.neu.cln.resid\.fitted'
    ofile='E:\gpse\rerun.cmonoc\comb\vmf1trns2\gsoln\Tigs05.apr'
    
    path='E:\gpse\rerun.cmonoc\comb\vmf1trns2\gsoln\xyz.neu.test4.cln.resid\.fitted'
    ofile='E:\gpse\rerun.cmonoc\comb\vmf1trns2\gsoln\itrf05-vmf1-test4.apr'
    
    path='E:\gpse\rerun.cmonoc\comb\20100128b\gsoln\xyz.neu.frame.cln.resid\.fitted'
    ofile='E:\gpse\rerun.cmonoc\comb\20100128b\gsoln\trnabc101.apr'
    
    path='E:\gpse\rerun.trnabc\comb\gtrnks\gsoln\xyz.neu.frm153.cln.cln.cut.resid\.fitted'
    ofile='E:\gpse\rerun.trnabc\comb\gtrnks\gsoln\trnsref145.apr'
    
    path='E:\gpse\rerun.cmonoc\comb\gmf12\gsoln\xyz.neu.frame_China.to_neu.resid\.fitted'
    ofile='E:\gpse\rerun.cmonoc\comb\gmf12\gsoln\frameChina.apr'
    ofile_eq='E:\gpse\rerun.cmonoc\comb\gmf12\gsoln\frameChina.eq_rename'
    
    path='E:\gpse\rerun.cmonoc\comb\gmf12b\gsoln\xyz.frame_global.neu.to_neu.resid\.fitted'
    ofile='E:\gpse\rerun.cmonoc\comb\gmf12b\gsoln\frame.global.apr'
    ofile_eq='E:\gpse\rerun.cmonoc\comb\gmf12b\gsoln\frame.global.eq_rename'
    
    path='E:\gpse\rerun.trnabc\comb\g3trns\gsoln\xyz.test1.neu.cln.resid\.fitted'
    ofile='D:\phd\expt\gpsf\proctab\rerun.trnabc\gmf.v3ii\common\frame.test1.apr'
    ofile_eq='D:\phd\expt\gpsf\proctab\rerun.trnabc\gmf.v3ii\common\frame.test1.eq_rename'
    
    path='E:\gpse\rerun.trnabc\comb\g3trns\gsoln\xyz.test1.neu.cln\test2.resid\.fitted'
    ofile='D:\phd\expt\gpsf\proctab\rerun.trnabc\gmf.v3ii\common\frame.test2.apr'
    ofile_eq='D:\phd\expt\gpsf\proctab\rerun.trnabc\gmf.v3ii\common\frame.test2.eq_rename'
    
    path='E:\gpse\rerun.trnabc\comb\g3trns\gsoln\xyz.neu.cln.resid_linear\.fitted'
    ofile='D:\phd\expt\gpsf\proctab\rerun.trnabc\gmf.v3ii\common\frame.igs.apr'
    ofile_eq='D:\phd\expt\gpsf\proctab\rerun.trnabc\gmf.v3ii\common\frame.igs.eq_rename'
    
    path='E:\gpse\rerun.beijing\comb\gmf3\gsoln\pos.xyz.bj.resid\.fitted'
    ofile='E:\gpse\rerun.beijing\comb\gmf3\gsoln\bjgps.apr'
    ofile_eq='E:\gpse\rerun.beijing\comb\gmf3\gsoln\bjgps.eq_rename'
    
    path='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\xyz.cln.tie_ltnn.resid\.fitted'
    ofile='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\tie_ltnn.apr'
    ofile_eq='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\tie_ltnn.eq_rename'
    
    path='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\xyz.cln.lutai.resid\.fitted'
    ofile='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\ltnn.apr'
    ofile_eq='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\ltnn.eq_rename'
    
    path='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\xyz.cln.cmon.resid\.fitted'
    ofile='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\cmon.apr'
    ofile_eq='\\Gpsac4\root\g4\home\tianyf\tmp\trn.lutabc\gsoln\cmon.eq_rename'
    
    
    path='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\xyz.glb_frm.cln.resid\.fitted'
    ofile='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\glb_frm_2015jun25.apr'
    ofile_eq='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\glb_frm_2015jun25.eq_rename'
    
    path='C:\Papers.data\nepal\lasa2011\xyz.stab.igs.resid\.fitted'
    ofile='C:\Papers.data\nepal\lasa2011\xyz.stab.igs.apr'
    ofile_eq='C:\Papers.data\nepal\lasa2011\xyz.stab.igs.eq_ren'
    
  ENDIF
  
  files=FILE_SEARCH(path+PATH_SEP()+'*.neu',count=nf)
  IF nf LE 0 THEN BEGIN
    PRINT,'[APR_FROM_FITTED]WARNING: no input files found!'
    RETURN
  ENDIF
  
  OPENW,fid,ofile,/get_lun
  OPENW,fid_eq,ofile_eq,/get_lun
  PRINTF,fid_eq,'*rename  ACOR     ACOR_2PS  2000  1  9  0  0  2007  3 18  0  0',$
    '*rename  ACOR     ACOR_3PS  2007  3 18  0  0  2100  1  1  0  0',$
    format='(a)'
  curtime=INTARR(5)
  unlimitedtime=[2100,  1,  1,  0,  0]
  FOR fi=0,nf-1 DO BEGIN
    file=files[fi]
    site=STRMID(GETFILENAME(file),0,4)
    ;stop
    QUERY_ICD, FILE, $
      NS=NS, $
      NL=NL, $
      NH=NH, $
      HEADERS=HEADERS,  $
      SLOPE_N=SLOPE_N, PSDECAY_N=PSDECAY_N, OFFSET_N=OFFSET_N, ANNUAL_N=ANNUAL_N, SEMIANNUAL_N=SEMIANNUAL_N,  $
      SLOPE_E=SLOPE_E, PSDECAY_E=PSDECAY_E, OFFSET_E=OFFSET_E, ANNUAL_E=ANNUAL_E, SEMIANNUAL_E=SEMIANNUAL_E,  $
      SLOPE_U=SLOPE_U, PSDECAY_U=PSDECAY_U, OFFSET_U=OFFSET_U, ANNUAL_U=ANNUAL_U, SEMIANNUAL_U=SEMIANNUAL_U,  $
      FAILED = FAILED, $
      XYZ=XYZ
      
    READ_SIO,file,data=data
    
    ;stop
    
    ;print,offset_n,offset_e,offset_u
    ;print,psdecay_n,psdecay_e,psdecay_u
    ;print,slope_n,slope_e,slope_u
    
    ;IF psdecay_n[3,0] NE -9999 || psdecay_e[3,0] NE -9999 || psdecay_u[3,0] NE -9999 THEN BEGIN  ;psdecay exists
    IF 0 THEN BEGIN  ;psdecay exists
    ;...
    ENDIF ELSE BEGIN
      PRINTF,fid,site,data[3:5,0],slope_n[0,0],slope_e[0,0],slope_u[0,0],data[0,0], $
        format='(1x,a4,"_GPS",1x,3f20.5,1x,3f10.5,1x,f10.5)'
      IF offset_n[2,0] NE -9999 || offset_e[2,0] NE -9999 || offset_u[2,0] NE -9999 THEN BEGIN
        ;first, get the number of offsets
        offtimes=-9999
        IF offset_n[2,0] NE -9999 THEN BEGIN
          offtimes=REFORM(offset_n[2,*])
        ENDIF
        IF offset_e[2,0] NE -9999 THEN BEGIN
          IF offtimes[0] EQ -9999 THEN BEGIN
            offtimes=REFORM(offset_e[2,*])
          ENDIF ELSE BEGIN
            offtimes=[offtimes,REFORM(offset_e[2,*])]
          ENDELSE
        ENDIF
        IF offset_u[2,0] NE -9999 THEN BEGIN
          IF offtimes[0] EQ -9999 THEN BEGIN
            offtimes=REFORM(offset_u[2,*])
          ENDIF ELSE BEGIN
            offtimes=[offtimes,REFORM(offset_u[2,*])]
          ENDELSE
        ENDIF
        offtimes=offtimes[SORT(offtimes)]
        offtimes=offtimes[UNIQ(offtimes)]
        FOR oi=0,N_ELEMENTS(offtimes)-1 DO BEGIN
          pos=WHERE(offset_n[2,*] EQ offtimes[oi])
          IF pos[0] EQ -1 THEN BEGIN
            offsize_n=0
          ENDIF ELSE BEGIN
            offsize_n=offset_n[0,pos]
          ENDELSE
          
          pos=WHERE(offset_e[2,*] EQ offtimes[oi])
          IF pos[0] EQ -1 THEN BEGIN
            offsize_e=0
          ENDIF ELSE BEGIN
            offsize_e=offset_e[0,pos]
          ENDELSE
          
          pos=WHERE(offset_u[2,*] EQ offtimes[oi])
          IF pos[0] EQ -1 THEN BEGIN
            offsize_u=0
          ENDIF ELSE BEGIN
            offsize_u=offset_u[0,pos]
          ENDELSE
          
          tmp=MIN(ABS(data[0,*]-offtimes[oi]),pos)
          ;print,pos
          ;stop
          PRINTF,fid,site, oi+1,$
            data[3,0]+slope_n[0,0]*(data[0,pos]-data[0,0])+offsize_n, $
            data[4,0]+slope_e[0,0]*(data[0,pos]-data[0,0])+offsize_e, $
            data[5,0]+slope_u[0,0]*(data[0,pos]-data[0,0])+offsize_u, $
            slope_n[0,0],slope_e[0,0],slope_u[0,0],data[0,pos], $
            format='(1x,a4,"_",i1,"PS",1x,3f20.5,1x,3f10.5,1x,f10.5)'
          ;
          DOY,STRING(offtimes[oi],format='(f10.5)')+'Y', $
            dyear=dyear,day_of_year=doyr,date=curtime
          IF oi EQ N_ELEMENTS(offtimes)-1 THEN BEGIN
            endingtime=unlimitedtime
          ENDIF ELSE BEGIN
            DOY,STRING(offtimes[oi+1],format='(f10.5)')+'Y', $
              dyear=dyear,day_of_year=doyr,date=endingtime
          ENDELSE
          ;stop
          ;curtime=date
          ;eq_rename
          ; rename  ACOR     ACOR_2PS  2000  1  9  0  0  2007  3 18  0  0
          ; rename  ACOR     ACOR_3PS  2007  3 18  0  0  2100  1  1  0  0
          PRINTF,fid_eq,site,site, oi+1,      $
            curtime,$
            endingtime,$
            format='(" rename  ",a4,5x,a4,"_",i1,"PS",2(2x,i4,1x,i2,1x,i2,1x,i2,1x,i2))'
        ENDFOR
      ENDIF
    ENDELSE
    
  ;stop
  ENDFOR
  FREE_LUN,fid
  FREE_LUN,fid_eq
  PRINT,'[APR_FROM_FITTED]Normal end.'
  
END