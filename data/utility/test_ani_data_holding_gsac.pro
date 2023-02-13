PRO TEST_ANI_DATA_HOLDING_GSAC


  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  file='D:\ICD\related.work\2018KeyResearch\ppt\figure\data.holding\cddis.txt'
  ofile=file+'.dh'
  
  lines=read_txt(file)
  HELP, lines
  
  nday=N_ELEMENTS(lines)
  
  dyrs=DBLARR(nday)
  etas=DBLARR(nday)
  nsits=INTARR(nday)
  
  FOR di=0ull,nday-1 DO BEGIN
    line_p1=STRSPLIT(lines[di],/extract)
    line_p2=STRSPLIT(line_p1[1],'.',/extract)
    year=FIX(line_p2[1])
    doyr=FIX(line_p2[2])
    DOY,year,1,doyr,dyear=dyr
    dyrs[di]=dyr
    
    nsits[di]=FIX(line_p1[0])-3
    etas[di]=(nsits[di]/50d0)*1d0
  ;stop
    
  ENDFOR
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=PROG,src=file
  FOR di=0ull,nday-1 DO BEGIN
    PRINTF,fid,dyrs[di],nsits[di],etas[di],format='(1x,f10.5,1x,i5,1x,f8.2)'
  ENDFOR
  FREE_LUN,fid
END