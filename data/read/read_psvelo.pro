;
PRO READ_psvelo, file,   $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('velh.cmm4.psvelo',root_dir=!igps_root,subdirectory=['tables'])
  ENDIF
  
  ;Site Long  Lat Vn  Sn  Ve  Se  Cne
  ;H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064
  ;stop
  lines=read_txt(file)
  ;pos=WHERE(strmids(lines,0,1) EQ ' ')
  ;lines1=REFORM(lines[pos])
  ;stop
  lines1=str_lines2arr(lines)
  ;sites=strmids(lines1[7,*],0,4)
  sites=REFORM(lines1[7,*])
  lls=DOUBLE(lines1[0:1,*])
  vels=DOUBLE(lines1[[0,1,3,5,2,4,6],*])
  ;stop
  nsit=N_ELEMENTS(sites)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,file,sites,lls,vels,nsit
  ENDIF
END