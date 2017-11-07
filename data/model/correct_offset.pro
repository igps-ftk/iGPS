PRO CORRECT_OFFSET, path, opath, deffile, szwin=szwin, overwrite=overwrite
  prog='CORRECT_OFFSET'
  IF N_PARAMS() LT 3 THEN BEGIN
    path='D:\data\yice\2016jan21.offset\raw'
    opath='D:\data\yice\2016jan21.offset\raw.deoffset'
    deffile='D:\data\yice\2016jan21.offset\offset.kmin.2005.def'
    overwrite=1
  ENDIF
  
  if n_elements(overwrite) eq 0 then overwrite=0  
  NEUSTR=['N','E','U']
  szwin=[10,10]  ;days for left and right sides, respectively
  szwin_left_default=szwin[0]
  szwin_right_default=szwin[1]
  
  files=FILE_SEARCH(path+PATH_SEP()+'*.dat',count=nf)
  IF nf LE 0 THEN BEGIN
    PRINT,'['+PROG+']WARNING: no input *.dat files found!'
    RETURN
  ENDIF
  
  
  FOR fi=0, nf-1 DO BEGIN
    file=files[fi]
    print,'['+PROG+']Processing '+file
    ofile=opath+path_sep()+getfilename(file)
    if overwrite eq 0 && file_test(ofile,/regular) eq 1 then begin
      print,'['+PROG+']WARNING: output file already exist. Skipped!'
      continue
    endif
    site=STRUPCASE(STRMID(GETFILENAME(file),0,4))
    ;
    ;lines=read_txt(file)
    ;data=DOUBLE((str_lines2arr(lines))[0:9,*])
    READ_YICE, FILE, DATA=DATA
    
    odata=data
    t=REFORM(data[0,*])
    npt=N_ELEMENTS(t)
    FOR neui=0,2 DO BEGIN
      READ_DEF, DefFILE, SITE=SITE, OFFSET=OFFs, NEU=NEUSTR[NEUI], SZWIN=SZWIN_defs
      ;help, offs
      IF OFFs[0] NE -9999 THEN BEGIN   
        FOR oi=0, N_ELEMENTS(offs)-1 DO BEGIN       
          x=REFORM(odata[1+neui,*])          
          ts_model, t, x, residual=resid
;          plot,t,resid,background='ffffff'x,color='0'x,thick=2,xstyle=1
          
          szwin_left=szwin_left_default
          szwin_right=szwin_right_default
          if szwin_defs[0,oi] ne -9999 then szwin_left=szwin_defs[0,oi]
          if szwin_defs[1,oi] ne -9999 then szwin_right=szwin_defs[1,oi]
          ;print,szwin_left, szwin_right
          
          pos1=WHERE(t LT offs[oi])
          pos2=WHERE(t GE offs[oi])
          ts1=resid[(last(pos1)-szwin_left)>0:last(pos1)]
          ts2=resid[first(pos2):(first(pos2)+szwin_right)<npt]
          offsz=mean(ts2)-mean(ts1)
          
          odata[1+neui,pos2]=odata[1+neui,pos2]-offsz
          ;
;          resid2=resid
;          resid2[pos2]=resid[pos2]-offsz
;          oplot,t,resid2,color='0000ff'x
;          STOP
        ENDFOR
      ENDIF
    ENDFOR
    
    openw,fid,ofile,/get_lun
    for li=0,n_elements(odata[0,*])-1 do begin
      printf,fid,odata[*,li],site,format='(f13.8,1x,3(1x,f8.2),1x,3(1x,f8.2),1x,3(1x,f8.2),1x,a4)'
    endfor
    free_lun,fid
  ENDFOR
  
END