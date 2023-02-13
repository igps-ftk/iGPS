PRO SAR_S1_OVERLAPPING_CHECKS, path
  IF N_ELEMENTS(file) EQ 0 THEN BEGIN
    path='D:\gsar\asc\landslide'
    path='D:\gsar\des\landslide'
  ENDIF
  
  PROG=strlowcase((STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0])
  
  dirs=FILE_SEARCH(path+PATH_SEP()+'*_*',/test_directory, count=nd)
  IF nd LE 0 THEN BEGIN
    RETURN
  ENDIF
  
  FOR di=0, nd-1 DO BEGIN
    file=file_search(dirs[di]+PATH_SEP()+'input.lst', count=nf)
    if nf lt 1 then continue
    PRINT,'['+PROG+']Processing '+file+' ...'
    ofile=file+'.ok'
    IF FILE_TEST(ofile,/regular) EQ 1 THEN BEGIN
      PRINT,'['+PROG+']WARNING: output file already exist! skipped.'
      CONTINUE
    ENDIF
  ;
    SAR_S1_OVERLAPPING_CHECK, file, ofile
  ENDFOR
  
;stop
END