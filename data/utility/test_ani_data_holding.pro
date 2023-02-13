PRO TEST_ANI_DATA_HOLDING
  
  file='D:\ICD\related.work\2018KeyResearch\ppt\figure\data.holding\b'
  ofile=file+'.dh'
  
  lines=read_txt(file)
  help, lines
  
  nday=n_elements(lines)
  
  dyrs=dblarr(nday)
  etas=dblarr(nday)
  nsits=intarr(nday)
  
  for di=0ull,nday-1 do begin
    line_p=strsplit(lines[di],/extract)
    year=fix(strmid(line_p[0],2,4))
    doyr=fix(strmid(line_p[0],9,3))
    doy,year,1,doyr,dyear=dyr
    dyrs[di]=dyr
    
    nsits[di]=fix(line_p[1])
    etas[di]=(nsits[di]/50d0)*1d0
    ;stop
    
  endfor
  
  openw,fid,ofile,/get_lun
  write_sys_info,fid,prog='TEST_ANI_DATA_HOLDING',src=file
  for di=0ull,nday-1 do begin
  printf,fid,dyrs[di],nsits[di],etas[di],format='(1x,f10.5,1x,i5,1x,f8.2)'
  endfor
  free_lun,fid
END