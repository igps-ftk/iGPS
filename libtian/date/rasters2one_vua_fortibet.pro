pro rasters2one_vua_forTibet
  path='E:\jlm\soilw\totian\vua'
  ofile='E:\jlm\soilw\totian\vua\all.2003.raw'
  
  files=file_search(path+path_sep()+'AMSR*.tif',count=nf)
  if nf le 0 then return
  
  ncol=1440
  nrow=720
  data=fltarr(ncol,nrow)
  
  
  mfile='E:\jlm\soilw\totian\mask.soilw.vua.raw'
  
  ;read in mask
  dmask=bytarr(ncol,nrow)
  openr,fid,mfile,/get_lun
  readu,fid,dmask
  free_lun,fid
  pos_mask=where(dmask eq 1)
  pos_mask_out=where(dmask eq 0)
  colis=(pos_mask mod ncol)
  rowis=(pos_mask / ncol)
  col_start=min(colis,max=col_end)
  row_start=min(rowis,max=row_end)
  ;tvscl,dmask[col_start:col_end,row_start:row_end]
  
  ;stop
  openw,fido,ofile,/get_lun
  for fi=0,nf-1 do begin
    ;openr,fid,files[fi],/get_lun
    ;readu,fid,data
    ;free_lun,fid
    data=read_tiff(files[fi])
    help,data
    ;data=double(data)
    ;stop
    writeu,fido,data[col_start:col_end,row_start:row_end]
  endfor
  free_lun,fido
  
  odata=data[col_start:col_end,row_start:row_end]
  oNCol=n_elements(odata[*,0])
  oNRow=n_elements(odata[0,*])
  
  ; Create the items used for the projection
  ;
  
  ulx=-180+col_start*.25-.125
  uly=90-row_start*.25-.125
  
  mc = [.5D,.5, ulx,uly]
  ps = [1d0, 1d0]/4
  units = ENVI_TRANSLATE_PROJECTION_UNITS('Degrees')
  map_info = ENVI_MAP_INFO_CREATE(/geographic, $
    mc=mc, ps=ps, units=units)
  ;stop
  envi_setup_head,fname=ofile,ns=oNCol,nl=oNRow,nb=nf,data_type=1, $
    map_info=map_info, $
    interleave=0,bnames=desuffix(getfilename(files)),/write,/open
    
end