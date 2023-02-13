PRO Convert_tif2envi, file, ofile

  forward_function ENVI_GET_MAP_INFO,ENVI_GET_DATA,envi_translate_projection_units
  
  CD, current=pwd
  PRINT,'pwd:',pwd
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT,log_file='log.txt'
  IF N_PARAMS() LT 2 THEN BEGIN
    file='C:\tmp\t62-honghe-100m\t62-honghe-100m.tif'
    ofile='C:\tmp\t62-honghe-100m\test2'
  ENDIF
  ;ENVI_OPEN_FILE, file, r_fid=fid, /no_realize
  ENVI_OPEN_DATA_FILE, file, /tiff, r_fid=fid
  
  ENVI_FILE_QUERY, fid, dims=dims, nb=nb
  map=ENVI_GET_MAP_INFO(fid=fid)
  ;proj=envi_get_projection(fid=fid)
  map.proj.NAME='Geographic Lat/Lon'
  map.proj.DATUM='WGS-84'
  map.proj.UNITS=envi_translate_projection_units('Degrees')
  
  ;omap=envi_map_info_create(/geographic,proj=proj)
  ;stop
  
  t_fid = LONARR(nb) + fid
  pos  = LINDGEN(nb)
  
  data=ENVI_GET_DATA(dims=dims,fid=fid,pos=pos)
  ;descrip='converted from '+file
  ENVI_WRITE_ENVI_FILE, data, out_name=ofile, $
    ;bnames=bnames,  $
    ;descrip=descrip,  $
    map_info=map,r_fid=r_fid
    
  ;  ENVI_DOIT,'cf_doit',dims=dims,pos=pos,fid=t_fid,r_fid=r_fid ,$
  ;    out_name=ofile,/no_realize
    
  ENVI_FILE_MNG, id=fid, /remove
  ENVI_FILE_MNG, id=r_fid, /remove
  
  ENVI_BATCH_EXIT
;stop
END