;
PRO READ_PSVELO_EXT, file,   $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('zheng.et.al.2017.jgrb52327.txt',root_dir=!igps_root,subdirectory=['tables'])
  ENDIF
  
  ;  Long.    Lat.      Ve       Vn     Se     Sn    Cne  Sta.      Source
  ; 74.336  39.842   0.170   13.790  0.560  0.503  0.000  I089  This_study
  ; 78.680  29.848  10.840   32.656  1.550  1.450 -0.002  LAN2  Kreemer et al. [2014] from Banerjee et al. [2008]
  lines=read_txt(file)
  lines1=STRARR(8,N_ELEMENTS(lines))
  FOR li=0ull, N_ELEMENTS(lines)-1 DO BEGIN
    line_p=STRSPLIT(lines[li],/extract)
    lines1[*,li]=line_p[0:7]
  ENDFOR
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