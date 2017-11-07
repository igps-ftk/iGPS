;+
; NMAE:
;       DESUFFIX
;
; PURPOSE:
;       Get rid of the suffix (extension) and the dot (.) from the full path name.
;
; DESCRIPTION:
;		This program find the dot(.) from the end of the filename. If a dot cannot be found before
;		reach tht path seperator (/-Linux;\-Win) then return the full pathname.
;
; AUTHOR:
;       Yunfeng TIAN
;
; CATEGORY
;       String
;
; CALL SEQUENCE:
;       deSufed = DESUFFIX(fname)
;
; RETURN VALUE:
;       The string which contains the fore part of fname.
;
; EXAMPLE:
;       IDL>fname='c:\test\image.raw'
;       IDL>result = DESUFFIX(fname)
;       IDL>print, result
;       We get:
;              c:\test\image
; NOTE:
;       The input parameter can also be a string array, and
;       then return a string array.
;-
FUNCTION DESUFFIX, FNAME
	;
	IF N_PARAMS() LT 1 THEN RETURN, ''
	TMP=FNAME
	FOR I=0ull, N_ELEMENTS(FNAME)-1 DO BEGIN
		DOT_POS=STRPOS(FNAME(I), '.', /REVERSE_SEARCH)
		SEP_POS=STRPOS(FNAME(I), PATH_SEP(), /REVERSE_SEARCH)
		IF DOT_POS EQ -1 OR DOT_POS LT SEP_POS THEN BEGIN
			TMP(I)=FNAME(I)
		ENDIF ELSE BEGIN
			TMP(I)=STRMID(FNAME(I), 0, DOT_POS)
		ENDELSE
		;
	ENDFOR
	RETURN, TMP
END
;//////////////////////////////////////////////////////////////////////////

PRO DESUFFIX
	FNAME='/usr/local/rsi/idl/data/tm.raw'
	PRINT, 'Filename: ',FNAME,' without extension is:',DESUFFIX(FNAME),FORMAT='("[DESUFFIX]",A)'
END