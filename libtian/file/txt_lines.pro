
;+
; :Name:
;   TXT_LINES
;
; :Description:
;   Return the number of lines of a text file.
;
; :Params:
;    FILE
;
; :Keywords:
;    NUM_COLS - Number of columns
;    COMM     - Comments symbol
;    FIRST
;    SKIP
;
; :Examples:
;
; :Modifications:
;   Released on May 11, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION TXT_LINES, FILE, NUM_COLS=NUM_COLS, COMM=COMM, FIRST=FIRST, SKIP=SKIP

  ON_IOERROR,IO_ERR
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'[TXT_LINES]Error: not enough input parameters.'
    NUM_COLS=-1
    RETURN,-1
  ENDIF
  
  OPENR,FID,FILE,/GET_LUN
  I=0ULL
  TMP=''
  
  IF N_ELEMENTS(SKIP) NE 0 THEN BEGIN
    FOR LI=0,SKIP-1 DO BEGIN
      READF,FID,TMP
    ENDFOR
    I=SKIP+0ULL
  ENDIF
  
  IF ~ EOF(FID) THEN BEGIN
    READF,FID,TMP
    FIRST_LINE=TMP
    I=I+1
  ENDIF
  
  WHILE ~ EOF(FID) DO BEGIN
    READF,FID,TMP
    I=I+1
  ENDWHILE
  
  IF KEYWORD_SET(FIRST) THEN BEGIN
    NUM_COLS=N_ELEMENTS(STRSPLIT(FIRST_LINE,/EXTRACT))
  ENDIF ELSE BEGIN
    NUM_COLS=N_ELEMENTS(STRSPLIT(TMP,/EXTRACT))
  ENDELSE
  
  FREE_LUN,FID
  RETURN,I
  
  IO_ERR:
  MSGBOX,!ERROR_STATE.MSG
  RETURN,-1
END

PRO TXT_LINES
  PRINT, TXT_LINES(NUM_COLS=NC)
  PRINT, NC
END