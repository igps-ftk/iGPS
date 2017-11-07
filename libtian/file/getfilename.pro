;+
;
; Get file name from a given file_path_name
;-
;----------------------------------------------------------------------------
FUNCTION GETFILENAME, filename, SEP=SEP

  IF N_ELEMENTS(SEP) EQ 0 THEN BEGIN
    CASE !VERSION.OS_FAMILY OF
      'MacOS': BEGIN
        sep = ':'
      END
      'unix': BEGIN
        sep = '/'
      END
      'vms': BEGIN
        sep = ']'
      END
      'Windows': BEGIN
        sep = '\'
      END
    ENDCASE
  ENDIF
  pos = STRPOS(filename, sep, /REVERSE_SEARCH)
  ;stop
  isvalid=WHERE(pos NE -1)
  IF N_ELEMENTS(isvalid) EQ 1 THEN $
    IF isvalid EQ -1 THEN RETURN,filename
  tmp=filename
  FOR i=0ull,N_ELEMENTS(isvalid)-1 DO BEGIN
    tmp(isvalid(i))=STRMID(filename(isvalid(i)),pos(isvalid(i))+1)
  ENDFOR
  RETURN,tmp
END
