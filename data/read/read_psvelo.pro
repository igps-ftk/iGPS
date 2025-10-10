;
PRO READ_PSVELO, file,   $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    commenter=commenter,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('velh.cmm4.psvelo',root_dir=!igps_root,subdirectory=['tables'])
  ENDIF
  
  IF N_ELEMENTS(commenter) EQ 0 THEN commenter=''
  
  lines=read_txt(file)
  
  IF commenter NE '' THEN BEGIN
  
    IF commenter EQ '~ ' THEN BEGIN
      pos=WHERE(strmids(lines,0,1) EQ ' ')
    ENDIF ELSE BEGIN
      IF commenter NE '' THEN BEGIN
        pos=WHERE(strmids(lines,0,STRLEN(commenter)) NE commenter)
      ENDIF
    ENDELSE
    
    ;  0     1   2  3   4  5     6   7
    ; Long  Lat  Ve Vn  Se Sn   Cne Site
    ;  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064 H095
    ;stop
    
    ;pos=WHERE(strmids(lines,0,1) EQ ' ')
    IF pos[0] EQ -1 THEN BEGIN
      PRINT,'[]ERROR: no data in file '+file+'!!'
      RETURN
    ENDIF
    lines=REFORM(lines[pos])
  ENDIF
  
  ;stop
  lines1=str_lines2arr(lines)
  ;sites=strmids(lines1[7,*],0,4)
  sites=REFORM(lines1[7,*])
  lls=DOUBLE(lines1[0:1,*])
  vels=DOUBLE(lines1[[0,1,2,4,3,5,6],*])
  ;stop
  nsit=N_ELEMENTS(sites)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,file,sites,lls,vels,nsit
  ENDIF
END