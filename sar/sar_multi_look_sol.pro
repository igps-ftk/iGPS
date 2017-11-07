PRO SAR_MULTI_LOOK_SOL, path

  path='C:\Papers.data\sse.mila\multi-look\ts'
  
  ;displacements files (one file each epoch)
  files_disp=FILE_SEARCH(path+PATH_SEP()+'disp*.txt')
  files_disp=last(files_disp)
  nf_disp=N_ELEMENTS(files_disp)
  
  ;satellite looking angles
  file_look='C:\Papers.data\sse.mila\multi-look\ts\incidences.txt'
  READ_COLS_ASCII, file_look, data=data_look
  sites_look=REFORM(data_look[0,*])
  sat_looks=DOUBLE(data_look[3:6,*])
  
  sat_orbits=[347d0, 347d0, 193d0, 193d0 ]*!dpi/180d0
  
  FOR fi=0, nf_disp-1 DO BEGIN
    READ_COLS_ASCII, files_disp[fi], data=data_fi
    odata=DBLARR(5,N_ELEMENTS(data_fi[0,*]))
    FOR si=0,N_ELEMENTS(data_fi[0,*])-1 DO BEGIN
      IF FINITE(data_fi[3,si]) NE 1 THEN CONTINUE
      site=data_fi[0,si]
      pos=WHERE(site EQ sites_look)
      IF pos[0] EQ -1 THEN CONTINUE
      sat_look=sat_looks[*,pos]
      IF TOTAL(FINITE(sat_look)) NE 4 THEN CONTINUE
      ;
      loss=DOUBLE(REFORM(data_fi[3:6,si]))
      SAR_MULTI_LOOK_SOL_LOS_TO_3D, loss, sat_look, sat_orbits-3d0*!dpi/2d0,   $
        disps_3d
        
      lon=DOUBLE(data_fi[0,si])
      lat=DOUBLE(data_fi[1,si])
      odata[*,si]=[lon,lat,REFORM(disps_3d)]
    ;STOP
    ENDFOR
    
    ofile=files_disp[fi]+'.3d'
    OPENW,fid,ofile,/get_lun
    ;WRITE_SYS_INFO,fid,prog='',src=file
    FOR si=0,N_ELEMENTS(odata[0,*])-1 DO BEGIN
      PRINTF,fid,odata[*,si],format='(1x,2(1x,f12.7),1x,3(1x,f10.5))'
    ENDFOR
    FREE_LUN,fid
    stop
  ENDFOR
END