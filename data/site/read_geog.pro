PRO READ_GEOG, FILE, SITES=SITES, VALS=VALS, $
    LLHS=LLHS, $
    VLLHS=VLLHS, $
    EPOCHS=EPOCHS, $
    DATA=DATA, $
    LINES=LINES_NOCMT
    
  ;STOP
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'[READ_GEOG]Usage: read_apr,file,sites=sites,vals=vals,LLHs=LLHs,...'    
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT, $
      SUBDIRECTORY=['tables'],$
      'itrf2005.apr.updated.geod')
    ;SITES=['BJFS','SHAO']  ;FOR TEST ONLY
  ENDIF
  
  NL = TXT_LINES(FILE)
  LINES = STRARR(NL)
  OPENR, LUN, FILE, /GET_LUN
  READF, LUN, LINES
  FREE_LUN, LUN
  
  IF N_ELEMENTS(SITES) EQ 0 || SITES[0] EQ '' THEN BEGIN
    FOR LI=0, N_ELEMENTS(LINES)-1 DO BEGIN
      LINE=LINES[LI]
      LINE_BAK=LINE
      LINE=STRUPCASE(LINE)
      IF STRMID(LINE,0,1) NE ' ' THEN CONTINUE ;;COMMENTS LINES, SKIP
      LINE=STRSPLIT(LINE,/EXTRACT)
      IF N_ELEMENTS(DATA) LT 7 THEN BEGIN
        SITES=STRMID(LINE[0],0,4)
        LLHS=DOUBLE(LINE[1:3])
        VLLHS=DOUBLE(LINE[4:6])
        EPOCHS=DOUBLE(LINE[7])
        DATA=DOUBLE(LINE[1:7])
        LINES_NOCMT=LINE_BAK
      ENDIF ELSE BEGIN
        SITES=[SITES,STRMID(LINE[0],0,4)]
        LLHS=[[LLHS],[DOUBLE(LINE[1:3])]]
        VLLHS=[[VLLHS],[DOUBLE(LINE[4:6])]]
        EPOCHS=[EPOCHS,DOUBLE(LINE[7])]
        DATA=[[DATA],[DOUBLE(LINE[1:7])]]
        LINES_NOCMT=[LINES_NOCMT, LINE_BAK]
      ENDELSE
    ENDFOR
    
  ENDIF ELSE BEGIN
    
    
    LLHS=DBLARR(3,N_ELEMENTS(SITES))
    VLLHS=LLHS
    EPOCHS=DBLARR(N_ELEMENTS(SITES))
    FOR SI=0,N_ELEMENTS(SITES)-1 DO BEGIN
      SITE=SITES[SI]
      FOR LI=0,N_ELEMENTS(LINES)-1 DO BEGIN
        LINE=LINES[LI]
        LINE_P=STRSPLIT(LINE,/EXTRACT)
        IF STRUPCASE(STRMID(LINE_P[0],0,4)) EQ STRUPCASE(SITE) THEN BEGIN
          LLHS[*,SI]=DOUBLE(LINE_P[1:3])
          VLLHS[*,SI]=DOUBLE(LINE_P[4:6])
          EPOCHS[SI]=DOUBLE(LINE_P[7])
          ;PRINT,LINE
          GOTO,NEXT_SITE
        ENDIF
      ENDFOR
      NEXT_SITE:
    ENDFOR
    ;STOP
  ENDELSE
  ;STOP
END