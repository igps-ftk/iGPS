PRO SAR_S1_MANIFEST2OUTLINES, path, opath,ptn=ptn

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
  
    path='C:\Downloads\esa.data\safe\manifest.safe'
    opath='C:\Downloads\esa.data\safe\footprint.kml'
    
  ENDIF
  
  IF N_ELEMENTS(ptn) EQ 0 THEN BEGIN
    ptn='*'
    ptn='[AD]???'
    ptn='A070*'
  ENDIF
  
  IF N_ELEMENTS(opath) EQ 0 THEN BEGIN
    opath=path
  ENDIF
  
  
  dirs=FILE_SEARCH(path+PATH_SEP()+ptn, count=nd,/test_directory) ;,/fold_case
  IF nd LE 0 THEN BEGIN
    RETURN
  ENDIF
  PRINT,'['+prog+']INFO: #dirs = '+STRTRIM(nd,2)+'.'
  ;stop
  
  
  
  FOR di=0, nd-1 DO BEGIN
    dir=dirs[di]
    track=GETFILENAME(dir)
    IF track EQ 'alll' THEN CONTINUE
    odir=opath+PATH_SEP()+track
    PRINT,di+1,"/",STRTRIM(nd,2),' ',dir
    PRINT,odir
    ;continue
    
    IF FILE_TEST(odir,/directory) NE 1 THEN BEGIN
      FILE_MKDIR, odir
    ENDIF
    SAR_S1_MANIFEST2OUTLINE, dir, odir
  ENDFOR
  
END