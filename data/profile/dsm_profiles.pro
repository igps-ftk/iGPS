pro dsm_profiles
    demfile='D:\ICD\Eighth\2021\20210331.linyi_dsm\dsm\JJLDsM'
    ffile='D:\ICD\Eighth\2021\20210331.linyi_dsm\dsm\fault.evf'
    opath='D:\ICD\Eighth\2021\20210331.linyi_dsm\dsm\p'
    
      
      evf_id = envi_evf_open(ffile) 
; 
; Get the vector information 
; 
envi_evf_info, evf_id, num_recs=num_recs, $ 
data_type=data_type, projection=projection, $ 
layer_name=layer_name 
; 
; Print information about each record 
; 
print, 'Number of Records: ',num_recs 
for i=0,num_recs-1 do begin 
  record = envi_evf_read_record(evf_id, i) 
  print, 'Number of nodes in Record ' + $ 
         strtrim(i+1,2) + ': ', n_elements(record[0,*]) 
endfor 
; 
; Close the EVF file 
; 
envi_evf_close, evf_id 
xys_fvec=record
;stop
  IF N_ELEMENTS(pfile) EQ 0 THEN BEGIN
    pfile=opath+PATH_SEP()+'profiles_auto.psxy'
    ;generate profiles lines by calling PROFILE_LINES_AUTO program
    Profile_lines_auto_xy, xys_fvec,  oxys=oxys, spacing=90, auto_strike=2, length_profile=1000, ofile=pfile
      ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=400
  ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=40
  ENDIF
  
    ;Generate DEM profiles (optional; Needs ENVI; if not desired, comment out below lines)
    ;stop
    IF FILE_TEST(demfile,/regular) THEN BEGIN
      ;ENVI,/restore_base_save_files
      ;ENVI_BATCH_INIT
      DEM_GRID_EXTRACT_PROFILE, demfile,  $  ;DEM file (ENVI raw image format; with map projection)
        pfile,  $  ;profile line (two vertices; extended GMT psxy format)
        opath,  $  ;output path
        fa_xys=xys_fvec
        ;ffile=ffile  ;fault polyline (extended GMT psxy format)
      ;ENVI_BATCH_EXIT
    ;RETURN
    ENDIF
    ;STOP
    
  

end