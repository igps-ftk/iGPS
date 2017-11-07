;+
; Purpose:
;		get file suffix from file[_path]name
;		
; Calling Sequence:
;		suffix=getFileSuffix(filepathname)
;
;-
FUNCTION GETFILESUFFIX, FNAME
  DOTPOS=STRPOS(FNAME,'.',/REVERSE_S)
  DEL=STRPOS(FNAME,'\',/REVERSE_S)
  INDVALID=WHERE(DOTPOS GT DEL+1)
  IF N_ELEMENTS(INDVALID) EQ 1 THEN $
    IF INDVALID EQ -1 THEN RETURN,''
  TMP=FNAME
  FOR I=0ULL,N_ELEMENTS(INDVALID)-1 DO BEGIN
    TMP(INDVALID(I))=STRMID(FNAME(INDVALID(I)),DOTPOS(INDVALID(I))+1,STRLEN(FNAME(INDVALID(I)))-1)
  ENDFOR
  RETURN,TMP
END

PRO GETFILESUFFIX
  FILE='/home/tianyf/sopac/cleanedNeuUnfTimeSeries20100126/7odmCleanUnf.neu'
  PRINT,'The suffix of filename '+FILE+' is:'+GETFILESUFFIX(FILE),FORMAT='(A)'
  FILE=STRREP(FILE,'/','\')
  PRINT,'The suffix of filename '+FILE+' is:'+GETFILESUFFIX(FILE),FORMAT='(A)'

END