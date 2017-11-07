;+
; Name:
;		WRITE_TXT
;
; Purpose:
;		Write all the string arrar to output file.
;
;-
PRO WRITE_TXT,FILE,STR
  IF N_PARAMS() LT 2 THEN BEGIN
    PRINT,'Usage: write_txt, out_file, strings_to_write'
    RETURN
  ENDIF
  
	OPENW,FID,FILE,/GET_LUN
	PRINTF,FID,STR,FORMAT='(A)'
	FREE_LUN,FID
END