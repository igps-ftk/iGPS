;+
; :Name:
;   GIF_FROM_JPEG
;
; :Description:
;   Create a GIF animation from JPEG files.
;
; :Params:
;    PATH
;    OFILE
;
; :Keywords:
;    DELAY - in miliseconds. Default to .1 second (=100).
;
; :Examples:
;
; :Modifications:
;   Released on May 13, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
PRO GIF_FROM_JPEG, $
    PATH, $
    OFILE, $
    DELAY=DELAY, $
    LONG_DELAY=DELAY_LONG
    
  IF N_PARAMS() LT 2 THEN BEGIN
    ;PATH=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','sio'],'cleanedNeuUnf.resid.smoothed')
    ;OFILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','sio'],'cleanedNeuUnf.resid.smoothed.gif')
    path='E:\gpse\rerun.trnabc\comb\g3trns\gsoln\comp.resid.jpl_vs_icd\icd'
    ofile='D:\Papers\Paper.repro.igs\figure\S4.resid.ts.JPL.vs.ICD\f-S4.gif'
    path='D:\Papers\Paper.weekly.solut.vs.itrf08\auxiliary.material\ms02.ann.horiz.ellipse.large\frames
    ofile='D:\Papers\Paper.weekly.solut.vs.itrf08\auxiliary.material\ms02.ann.horiz.ellipse.large\ms02.gif'
    path='D:\Papers\Paper.weekly.solut.vs.itrf08\auxiliary.material\ms03.animation.ann.qoca.alloads\frames'
    ofile='D:\Papers\Paper.weekly.solut.vs.itrf08\auxiliary.material\ms03.animation.ann.qoca.alloads\ms03.gif'
    
    path='D:\Papers\Paper.weekly.solut.vs.itrf08\figure\4.linear.seasonal.guao\good'
    ofile='D:\Papers\Paper.weekly.solut.vs.itrf08\auxiliary.material\ms04.linear.annual.others\ms04.gif'
    
    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms01.ann.aniation\frames\resized'
    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms01.ann.aniation\ms01.gif'
    
;    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ani.ann.load\frames'
;    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ani.ann.load\ann.load.gif'
;    
    path='D:\Papers\Paper.Annual.Tibet\figure\6.schematic.mht\vertical.pos.diff\lck2-vs-jmsm'
    ofile='D:\Papers\Paper.Annual.Tibet\figure\6.schematic.mht\vertical.pos.diff.lck2-vs-jmsm.gif'
;    
;    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ani.vertical.position.profile\frames'
;    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ani.vertical.position.profile\vert.pos.gif'
;    
;    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms02.ani.ann.load\frames\resized'
;    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms02.ani.ann.load\ms02.gif'
;    
;    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms03.ani.vertical.position.profile\frames\resized'
;    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms03.ani.vertical.position.profile\ms03.gif'
    
    ;    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms01.ann.aniation\ellipses\test'
    ;    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms01.ann.aniation\ellipses\test\all.gif'
    
    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms03.ani.vertical.position.profile\frames.6s\profile_06'
    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms03.ani.vertical.position.profile\frames.6s\profile_06.gif'
    
    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms01.ann.aniation\frames\tienshan'
    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ms01.ann.aniation\tienshan.gif'
    
    path='J:\tmp\sopac\for.animation\shld'
    ofile='J:\tmp\sopac\for.animation\shld.gif'
    
    path='E:\tmp\geodesy.unr.edu\gps_timeseries\tenv3\IGS08.neu.used.resid.cmc0_optimal_CORR_D_TAU_lin\cmc0.smoothed\p675'
    ofile='E:\tmp\geodesy.unr.edu\gps_timeseries\tenv3\IGS08.neu.used.resid.cmc0_optimal_CORR_D_TAU_lin\cmc0.smoothed\p675\p675_CMC.gif'
    
    path='E:\tmp\geodesy.unr.edu\gps_timeseries\tenv3\IGS08.neu.used.resid.cmc0_optimal_CORR_D_TAU_lin\flt.smoothed\p675'
    ofile='E:\tmp\geodesy.unr.edu\gps_timeseries\tenv3\IGS08.neu.used.resid.cmc0_optimal_CORR_D_TAU_lin\flt.smoothed\p675\p675_filtered.gif'
    
    path='D:\gpse\eq.nepal.2015apr25.M7.9\highrate\2015.115g\xzzf'
    ofile='D:\gpse\eq.nepal.2015apr25.M7.9\highrate\2015.115g\xzzf.1hz.gif'
    
    path='J:\tmp\nqxm'
    ofile='J:\tmp\nqxm.gif'
    
    path='J:\Papers\Paper.Filtering.CME\auxiliary.material\rv2.r1.psd.test\raw.psd.smoothed'
    ofile='J:\Papers\Paper.Filtering.CME\auxiliary.material\rv2.r1.psd.test\psds.gif'
    
    ofile='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ani.vert.motion\vertical.annual.motion.profile.gif'
    path='D:\Papers\Paper.Annual.Tibet\auxiliary.material\ani.vert.motion\test.plot\profile_01\resized'
    
    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\resid.with.seasonal.flt.smoothed'
    ofile='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\test.yn\seasonal.flt.gif'
    
    path='C:\tmp\TEST.EE'
    ofile='C:\tmp\TEST.EE.gif'
    ;ptn='00000000*.jpg'
    
    path='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\raw.neu.cln.resid.corr\modeled.plot'
    ofile='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.2016feb07\gamit\raw.neu.cln.resid.corr\corr_cmonoc2.gif'
    path='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos.neu.nepal\post.disp\frames\pos.plot'
    ofile='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos.neu.nepal\post.disp\post.gif'
    
    path='\\gpsac4\tianyf\tmp'
    ofile='\\gpsac4\tianyf\tmp\out2.gif'
    path='D:\gsar\des\envisat.d.t176f2961.bengco\t176\pp3b'
    ofile='D:\gsar\des\envisat.d.t176f2961.bengco\t176\pp3b\bengco.gif'
    
    path='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\Bengco_T150\pp3b'
    ofile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\Bengco_T150\pp3b\bengco.gif'
    
;    path='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\stacking\APS\pp3b'
;    ofile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\stacking\APS\pp3b\jiali.gif'

path='D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend'
ofile='D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend\disp_los_ascending.gif'

PATH='C:\tmp\111'
OFILE='C:\tmp\111\GIF.GIF'
    
    delay=200
    delay_long=200
    delay=100
    delay_long=100
;    delay=10
;    delay_long=10
    delay=50
    delay_long=50
;    delay=5
;    delay_long=5
    
    IF N_ELEMENTS(PATH) EQ 0 THEN PATH = DIALOG_PICKFILE(/DIRECTORY)
    IF PATH EQ '' THEN RETURN
    IF N_ELEMENTS(OFILE) EQ 0 THEN OFILE = DIALOG_PICKFILE(/WRITE, FILTER='*.gif')
    IF OFILE EQ '' THEN RETURN
    
  ENDIF
  
  IF N_ELEMENTS(PTN) EQ 0 THEN PTN='*.jpg'
  PRINT,'[GIF_FROM_JPEG]Searching JPEG files in '+PATH+'.',FORMAT='(A)'
  
  FILES=FILE_SEARCH(PATH+PATH_SEP()+PTN,COUNT=NF)
  ;stop
  ;PRINT,NF
  IF NF LE 0 THEN BEGIN
    PRINT,'[GIF_FROM_JPEG]Warning: no JPEG files found.',FORMAT='(A)'
    RETURN
  ENDIF
  PRINT,'[GIF_FROM_JPEG]Found '+STRTRIM(NF,2)+' JPEG files.',FORMAT='(A)'
  PRINT,'[GIF_FROM_JPEG]Output to '+OFILE+'.',FORMAT='(A)'
  
  I=0
  READ_JPEG, FILES[I], IMAGE, COLTAB
  SZ=SIZE(IMAGE,/DIMENSION)/1
  ;PRINT,SZ
  
  IMG=IMAGE
  ;IMG=CONGRID(IMG,SZ[0],SZ[1],1)
  IF N_ELEMENTS(DELAY) EQ 0 THEN DELAY=50
  IF N_ELEMENTS(DELAY_LONG) EQ 0 THEN DELAY_LONG=150
  ;IF N_ELEMENTS(DELAY_LONG) EQ 0 THEN DELAY_LONG=20
  WRITE_GIF,OFILE,IMG,COLTAB[*,0],COLTAB[*,1],COLTAB[*,2],REPEAT_COUNT=0,DELAY_TIME=DELAY
  
  ;STOP
  ;IMAGES=BYTARR(SZ[1],SZ[2],NF)
  FOR I=0,NF-1,1 DO BEGIN
    PRINT,'[GIF_FROM_JPEG]Adding ',GETFILENAME(FILES[I]),'...',FORMAT='(3A)'
    READ_JPEG, FILES[I], IMAGE, COLTAB
    
    SZ2=SIZE(IMAGE,/DIMENSION)
    IMG=IMAGE
    IF SZ[0] NE SZ2[0] || SZ[1] NE SZ2[1] THEN BEGIN
      ;stop
      IMG=CONGRID(IMG,SZ[0],SZ[1],1)
    ENDIF
    IF I MOD 2 EQ 0 THEN BEGIN
      WRITE_GIF,OFILE,IMG,COLTAB[*,0],COLTAB[*,1],COLTAB[*,2],/MULTIPLE,DELAY_TIME=DELAY,REPEAT_COUNT=0
    ENDIF ELSE BEGIN
      WRITE_GIF,OFILE,IMG,COLTAB[*,0],COLTAB[*,1],COLTAB[*,2],/MULTIPLE,DELAY_TIME=DELAY_LONG,REPEAT_COUNT=0
    ENDELSE
  ENDFOR
  
  READ_JPEG, FILES[I-1], IMAGE, COLTAB
  IMG=IMAGE
  IMG=CONGRID(IMG,SZ[0],SZ[1],1)
  WRITE_GIF,OFILE,IMG,COLTAB[*,0],COLTAB[*,1],COLTAB[*,2],/MULTIPLE,DELAY_TIME=DELAY,/CLOSE,REPEAT_COUNT=0
  
  PRINT,'[GIF_FROM_JPEG]Normal end.',FORMAT='(A)'
  
END