pro SIONEU2BIN, path, opath, evtop=evtop, $
    dt_querystr=dt_querystr
    
  if n_params() lt 2 then begin
    path = 'E:\phd.data\chp.4.cme.filtering\series\sio.pbo.2006\unf.3y.wa.to_flt.resid.2006-'
    opath = 'E:\phd.data\chp.4.cme.filtering\series\sio.pbo.2006\unf.3y.wa.to_flt.resid.2006-_bin'
  endif
  
  if n_elements(dt_querystr) eq 0 then dt_querystr='*.neu'
  
  files = file_search(path+path_sep()+dt_querystr, count=nf)
  if nf le 0 then begin
    print,'[SIONEU2BIN]WARMING: no files found!'
    return
  endif
  
  for fi=0, nf-1 do begin
    print, files[fi]
    if n_elements(evtop) ne 0 then begin
      id=widget_info(evtop,find_by_uname='LIST_SITE')
      widget_control, id, set_list_select=fi
      wait,0.01
    endif
    read_sio, files[fi], headers=headers, data=data 
    ;help,headers
    
    ofile=opath+path_sep()+desuffix(getfilename(files[fi]))+'.bin'
    write_sio_bin, ofile, data=data, headers=headers
  ;stop
  endfor
  
end
