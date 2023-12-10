;
PRO READ_GNSS_VELH_SLLNE, file,   $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file='D:\iGPS\tables\velh.cmm4'
  ENDIF
  
  ;Site Long  Lat Vn  Sn  Ve  Se  Cne
  ;H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064
  ;stop
  lines=read_txt(vfile)
  line1=lines[0]
  pos=STRPOS(STRLOWCASE(line1),'site')
  IF pos[0] NE -1 THEN lines1=lines[1:*] ELSE lines=lines
  ;pos=WHERE(strmids(lines,0,1) EQ ' ')
  ;lines1=lines[pos]
  lines1=str_lines2arr(lines1)
  ;sites=strmids(lines1[0,*],0,4)
  sites=REFORM(lines1[0,*])
  lls=DOUBLE(lines1[1:2,*])
  vels=DOUBLE(lines1[1:*,*])  ;stop
  nsit=N_ELEMENTS(sites)
  
END