pro mv_site_files
  path='E:\phd.data\chp.6.cme.filtering\series\sio.pbo.2006\unf.3y.wa.to_flt.resid.2006-.fltAziShen.smoothed'
  opath='E:\phd.data\chp.6.cme.filtering\series\sio.pbo.2006\unf.3y.wa.to_flt.resid.2006-.fltAziShen.smoothed\la15'
  sfile='E:\phd.data\chp.6.cme.filtering\series\sio.pbo.2006\pbo.3y.sel.wa.la15.sit'
  
  path='E:\phd.data\chp.6.cme.filtering\series\sio.pbo.2006\unf.3y.wa.to_flt.resid.2006-.cme_0.5_voronoi3b.smoothed'
  opath='E:\phd.data\chp.6.cme.filtering\series\sio.pbo.2006\unf.3y.wa.to_flt.resid.2006-.cme_0.5_voronoi3b.smoothed\la15'
  
  rdsit,sfile,site=sites
  help,sites
 
  for si=0,n_elements(sites)-1 do begin
    files=file_search(path+path_sep()+sites[si]+'*',count=nf)
    for fi=0,nf-1 do begin
      file_copy,files[fi],opath,/overwrite
    endfor
  endfor

end