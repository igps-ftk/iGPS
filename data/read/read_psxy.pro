;+
; Name:
;   READ_PSXY
;
; Purpose:
;   Read GMT PSXY files.
;
; Modifications:
;   + Released on  Mar-02-2015 by Tianyf;
;-
PRO  READ_PSXY,   $
    file,   $ ;input file
    region=regions,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    count=count,  $ ;number of polygons
    igpsmode=igpsmode,  $ ;whether skip comments lines (i.e., non-blank first column lines)
    names=names   ;region names (if exist)
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('fa_mbt.psxy',root_dir=!igps_root,subdirectory=['tables'])
  ENDIF
  
  IF N_ELEMENTS(igpsmode) EQ 0 THEN igpsmode=0
  
  IF igpsmode EQ 1 THEN BEGIN
    lines=read_txt(file,comment='~ ')
  ENDIF ELSE BEGIN
    lines=read_txt(file)
  ENDELSE
  count=0
  
  xys=[-9999d0,-9999d0]
  nps=-1
  in_name='none'
  
  ;;stop
  
  FOR li=0ull, N_ELEMENTS(lines)-1 DO BEGIN
    line=lines[li]
    line=STRTRIM(line,2)
    IF line EQ '' THEN CONTINUE ; skip blank lines
    ;STOP
    IF STRMID(line,0,1) EQ '>' THEN BEGIN
      ;if it is the first feature
      IF xys[0] NE -9999d0 && xys[1] NE -9999d0 THEN BEGIN
        ;new feature
        IF nps[0] EQ -1 THEN BEGIN
          nps=N_ELEMENTS(xys[0,*])
          names=in_name
        ENDIF ELSE BEGIN
          nps=[nps,N_ELEMENTS(xys[0,*])]
          names=[names,in_name]
        ENDELSE
        
        ;STOP
        count=count+1
        IF count EQ 1 THEN BEGIN
          regions=PTR_NEW(xys)
        ENDIF ELSE BEGIN
          regions=[regions,PTR_NEW(xys)]
        ENDELSE
        
      ENDIF
      
      
      ;name specified?
      IF STRLEN(line) GT 1 THEN BEGIN
        in_name=STRMID(line,2)
      ENDIF ELSE BEGIN
        in_name='none'
      ENDELSE
      
      
      
      xys=[-9999d0,-9999d0]
      CONTINUE
    ENDIF
    
    line_p=STRSPLIT(line,/extract)
    xy=DOUBLE(line_p)
    IF xys[0] EQ -9999d0 THEN BEGIN
      xys=xy
    ENDIF ELSE BEGIN
      xys=[[xys], [xy] ]
    ENDELSE
    
  ENDFOR
  
  ;stop
  ;IF xys[0] NE -9999d0 && xys[1] NE -9999d0 && nps[count-1] NE N_ELEMENTS(xys[0,*]) THEN BEGIN
  IF xys[0] NE -9999d0 && xys[1] NE -9999d0  THEN BEGIN
    IF nps[0] EQ -1 THEN BEGIN
      nps=N_ELEMENTS(xys[0,*])
      names=in_name
    ENDIF ELSE BEGIN
      nps=[nps,N_ELEMENTS(xys[0,*])]
      names=[names,in_name]
    ENDELSE
    
    ;STOP
    count=count+1
    IF count EQ 1 THEN BEGIN
      regions=PTR_NEW(xys)
    ENDIF ELSE BEGIN
      regions=[regions,PTR_NEW(xys)]
    ENDELSE
  ENDIF
  
  IF N_PARAMS() LT 1 THEN BEGIN
    FOR i=0, count-1 DO BEGIN
      PRINT,i+1,*(regions[i])
    ENDFOR
    HELP,regions,nps,count,names
    PRINT,'names:',names
    PRINT,'nps:',nps
    PRINT,'count:',count
  ;STOP
  ENDIF
  
END