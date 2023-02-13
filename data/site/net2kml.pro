PRO NET2KML, FILE, $ ;input, QOCA a priori sites coordiantes (Network file format).
    OFILE  ;output [optional]. if not given, a filename with kml as suffix will be used automatically.
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_ELEMENTS(file) EQ 0 THEN BEGIN
    file='C:\Downloads\pbo\pbo.net'
    file='D:\ICD\projects\DirectorFund\Application.2018\Field\2018nov\prefield\sites\suxiaoning\site_yucesuo.sllh.net'
    file='D:\gpse\trns\comb\lasa\gsoln\2020\2020.xyz.net'
    file='C:\Downloads\izmi\org\izmi.xyz.net'
    file='D:\gpse\trns\comb\ls21b\gsoln\ninh.net'
    file='D:\gpse\trns\comb\ls22\gsoln\20221225\icd.apr.net'
  ENDIF
  
  IF N_ELEMENTS(OFILE) EQ 0 THEN BEGIN
    OFILE=DESUFFIX(FILE)+'.kml'
    PRINT,'['+PROG+']WARNING: no output filename given! Using default ('+ofile+').'
  ENDIF
  
  READ_NET, FILE, SITE=SITES,LLH=LLHS
  ;stop
  ;SITES=SITES+'_14gg'
  ;SITES=SITES+'_2015gg'
  ;SITES=SITES+'_b'
  POS=WHERE(LLHS[0,*] NE 0 AND LLHS[1,*] NE 0)
  IF POS[0] EQ -1 THEN BEGIN
    PRINT,'['+PROG+']ERROR: no valid output!!'
    RETURN
  ENDIF
  LLHS=LLHS[*,POS]
  SITES=SITES[POS]
  ;
  WRITE_SITE_KML,OFILE,SITES=SITES,LLH=LLHS
  
END