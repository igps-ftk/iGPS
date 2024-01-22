;
PRO READ_GNSS_VELH_QOCA_MAP, file,   $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('wang_shen_2019JB018774_Table.S4S5.qocamap',root_dir=!igps_root,subdirectory=['tables'])
  ENDIF
  
  ;read qoca map velocity field
  ;   0         1           2      3        4         5       6      7       8        9      10    11
  ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
  ; ARTU_GPS   58.5583   56.4278     0.0     0.0    24.9     0.0     0.0     0.0     6.1     0.0   0.0000
  ;stop
  lines=read_txt(file)
  pos=WHERE(strmids(lines,0,1) EQ ' ')
  lines1=lines[pos]
  lines1=str_lines2arr(lines1)
  ;sites=strmids(lines1[0,*],0,4)
  sites=REFORM(lines1[0,*])
  lls=DOUBLE(lines1[1:2,*])
  vels=DOUBLE(lines1[[1,2,5,9,6,10,11],*])
  ;stop
  nsit=N_ELEMENTS(sites)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,file,sites,lls,vels,nsit
  ENDIF
END