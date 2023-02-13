PRO MOVE_PNG_TO_SUBDIRECTORY_BY_DIMENSION
  path='C:\Downloads\intf_all_los_ll_png'
  
  files=FILE_SEARCH(path+PATH_SEP()+'*.png', count=nf)
  IF nf LE 0 THEN RETURN
  
  dims=INTARR(2,nf)
  FOR fi=0, nf-1 DO BEGIN
    file=files[fi]
    res=QUERY_PNG(file,info)
    IF res EQ 0 THEN STOP
    dims[*,fi]=info.DIMENSIONS
  ENDFOR
  
  dimstr=STRING(dims,format='(i05,"x",i05)')
  tmp=dimstr[SORT(dimstr)]
  dims_ids=tmp[UNIQ(tmp)]
  
  FOR di=0, N_ELEMENTS(dims_ids)-1 DO BEGIN
    opath=path+PATH_SEP()+dims_ids[di]
    IF FILE_TEST(opath,/directory) NE 1 THEN BEGIN
      FILE_MKDIR, opath
    ENDIF
    pos=WHERE(dimstr EQ dims_ids[di])
    FOR pi=0, N_ELEMENTS(pos)-1 DO BEGIN
      cmdstr='file_move, "'+files[pos[pi]]+'", "'+opath+'"'
      PRINT, cmdstr
      tmp=execute(cmdstr)
      ;STOP
    ENDFOR
  ENDFOR
  ;STOP
  
  
END