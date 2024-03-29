;+
; :Name:
;   GREPI
;
; :DESCRIPTION:
;
; :PARAMS:
;    LINES
;    PTN
;
; :KEYWORDS:
;    LINE_NUMBER
;
; :EXAMPLES:
;
; :Modifications:
;   Released on May 11, 2010
;   2012-NOV-12, TIANYF
;       Add '^ ' function.
;
; :AUTHOR:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION GREPI,LINES,PTN,LINE_NUMBER=LN,FOLD_CASE=FOLD_CASE, LINES_NOT=LINES_NOT
  IF PTN EQ '*' THEN RETURN,LINES
  LINES_TMP=LINES+''
  PTN_TMP=PTN+''
  
  ;IF CASE-INSENSITIVE
  IF KEYWORD_SET(FOLD_CASE) || (N_ELEMENTS(FOLD_CASE) GT 0 && FOLD_CASE EQ 1) THEN BEGIN
    LINES_TMP=STRUPCASE(LINES_TMP)
    PTN_TMP=STRUPCASE(PTN_TMP)
  ENDIF
  
  LINES_REM=''
  
  IF STRMID(PTN_TMP,0,1) EQ '^' THEN BEGIN  ;FORLINES BEGIN WITH A STRING
    PTN_STR=STRMID(PTN_TMP,1)
    LENPTN=STRLEN(PTN_STR)
  ENDIF
  
  INDS=INTARR(N_ELEMENTS(LINES_TMP))
  ;INDS[*]=-1
  
  FOR I=0ULL,N_ELEMENTS(LINES_TMP)-1 DO BEGIN
  
    IF STRMID(PTN_TMP,0,1) EQ '^' THEN BEGIN  ;SEARCH FOR LINES BEGIN WITH A STRING
      IF STRMID(LINES_TMP[I],0,LENPTN) EQ PTN_STR THEN BEGIN
        POS=0
      ENDIF ELSE BEGIN
        POS=-1
      ENDELSE
    ENDIF ELSE BEGIN
      POS=STRPOS(LINES_TMP[I],PTN_TMP)
    ENDELSE
    
    
    IF POS[0] EQ -1 THEN CONTINUE
    ;
    INDS[I]=1
  ;    IF LINES_REM[0] EQ '' THEN BEGIN
  ;      LINES_REM=LINES[I]
  ;      LN=I
  ;    ENDIF ELSE BEGIN
  ;      LINES_REM=[LINES_REM,LINES[I]]
  ;      LN=[LN,I]
  ;    ENDELSE
    
  ENDFOR
  POS=WHERE(INDS EQ 1)
  IF POS[0] EQ -1 THEN BEGIN
    LINES_REM=''
    LN=-1
    ;
    LINES_NOT=LINES
  ENDIF ELSE BEGIN
    LINES_REM=LINES[POS]
    LN=POS
    ;
    POS_NOT=WHERE(INDS NE 1)
    IF POS_NOT[0] EQ -1 THEN BEGIN
      LINES_NOT=''
    ENDIF ELSE BEGIN
      LINES_NOT=LINES[POS_NOT]
    ENDELSE
  ENDELSE
  RETURN,LINES_REM
END

PRO GREPI,LINES,PTN,RES=LINES_REM
  IF N_PARAMS() LT 1 THEN BEGIN
    LINES=['1 3','2 6','3']
    PRINT,'[GREPI]For a string array:'
    FOR I=0,N_ELEMENTS(LINES)-1 DO PRINT,'[GREPI]  ',LINES[I]
  ENDIF
  
  IF N_ELEMENTS(PTN) EQ 0 THEN BEGIN
    PTN='3'
    PRINT,'[GREPI]Searching for ',PTN
  ENDIF
  
  LINES_REM=GREPI(LINES,PTN)
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'[GREPI]The matched lines are:'
    FOR I=0,N_ELEMENTS(LINES_REM)-1 DO PRINT,'[GREPI]  ',LINES_REM[I]
  ENDIF
  
END