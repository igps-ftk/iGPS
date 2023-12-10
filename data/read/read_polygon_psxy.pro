;+
; Name:
;   READ_POLYGON_PSXY
;
; Purpose:
;   Read revised version of GMT PSXY files.
;   i.e. read psxy files with header lines.
;   Non-blank first column lines are considered as comments.
;
;   Please NOTE, this convention is not the same as native GMT psxy files.
;   To read standard GMT psxy files, use READ_PSXY routine.
;
; Example:
;   Input file sample:
;*polygon defining the central Lhasa Block
; > Central_Lhasa_Block
; 88 29
; 93 29
;* some comments (non-blank-first-column lines)
; 93 32
; 88 32
; 88 29
;
;* the end of the sample file.
;
; Modifications:
;   + Revised on Wed, Oct 21, 2015  5:53:45 PM by tianyf
;   + Released on Mon, Mar 02, 2015  4:54:15 PM by Tianyf;
;-
PRO  READ_POLYGON_PSXY,   $
    file,   $ ;input file
    region=regions,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    count=count,  $ ;number of polygons
    names=names   ;region names (if exist)
    
  PROG='READ_POLYGON_PSXY'
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file='D:\ICD\projects\DirectorFund\Application.2012\data\forYaorui.2015oct21\study_area_gmt_psxy.txt'
  ENDIF
  
  
  count=0 ;number of polygons
  xys=[-9999d0,-9999d0] ;verties for each polygon
  nps=-1  ;number of verties for each polygon (int [count])
  in_name='none' ; polygon names (string [count])
  
  ;first, read all lines in the input file
  lines=read_txt(file)
  
  ;stop
  
  FOR li=0ull, N_ELEMENTS(lines)-1 DO BEGIN
    line=lines[li]
    IF STRMID(line,0,1) NE ' ' THEN CONTINUE
    line=STRTRIM(line,2)  ;remove the leading and trailing blanks of the current line
    ;STOP
    IF line EQ '' THEN CONTINUE ;skip blank lines
    
    ;if multiply section file
    IF STRMID(line,0,1) EQ '>' THEN BEGIN
    
      ;stop
      count=count+1
      
      IF count GT 1 THEN BEGIN
        IF nps[0] EQ -1 THEN BEGIN
          nps=N_ELEMENTS(xys[0,*])
          names=in_name
          regions=PTR_NEW(xys)
        ENDIF ELSE BEGIN
          nps=[nps,N_ELEMENTS(xys[0,*])]
          names=[names,in_name]
          regions=[regions,PTR_NEW(xys)]
        ENDELSE
        xys=[-9999d0,-9999d0]
      ENDIF
      
      ;name specified?
      IF STRLEN(line) GT 1 THEN BEGIN
        in_name=STRMID(line,2)
      ENDIF ELSE BEGIN
        in_name='none'
      ENDELSE
      
    ENDIF ELSE BEGIN
      IF count EQ 0 THEN count=count+1
      line_p=STRSPLIT(line,/extract)
      xy=DOUBLE(line_p)
      IF xys[0] EQ -9999d0 THEN BEGIN
        xys=xy
      ENDIF ELSE BEGIN
        xys=[[xys], [xy] ]
      ENDELSE
    ENDELSE
    
  ;stop
  ENDFOR  
  
  ;process the last polygon
  IF count GE 1 THEN BEGIN
    IF nps[0] EQ -1 THEN BEGIN
      nps=N_ELEMENTS(xys[0,*])
      names=in_name
      regions=PTR_NEW(xys)
    ENDIF ELSE BEGIN
      nps=[nps,N_ELEMENTS(xys[0,*])]
      names=[names,in_name]
      regions=[regions,PTR_NEW(xys)]
    ENDELSE
  ENDIF
  
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,regions,nps,count,names
    PRINT,'names:',names
    PRINT,'nps:',nps
    PRINT,'count:',count
    FOR i=0, count-1 DO BEGIN
      HELP,*(regions[i])
    ENDFOR
  ;STOP
  ENDIF
  
END