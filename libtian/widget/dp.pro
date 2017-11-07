;+
; Name:
;		dp
;
; Purpose:
;		Abbreviation for Dialog_PickFile function.
;		Enhancement:
;			- Can remember the selected path (Optional);
;			- Can append the default filter to the filename;
;			- Prompt overwrite.
;
; Keywords:
;		rec	-	remember the last selection
;		af	-	append filter to file
;				Its purpose is same to DEFAULT_EXTENSION keyword (IDL6.0).
;		op	-	Check if file is already exist and overwitten?
;				Its purpose is same to OVERWRITE_PROMPT (IDL6.0)
;
;-
FUNCTION DP, _extra=_ex, rec=rec, af=af, op=op
  ;
  start:
  ;
  IF KEYWORD_SET(rec) THEN BEGIN
    IF N_ELEMENTS(_ex) NE 0 THEN BEGIN
      IF Tag_Contains(TAG_NAMES(_ex), 'path') THEN curpath=path ELSE CD, current=path
    ENDIF ELSE BEGIN
      CD, current=curpath
    ENDELSE
    fname=DIALOG_PICKFILE(_extra=_ex,path=curpath,get_path=retpath)
    CD,retpath
  ENDIF ELSE BEGIN
    fname=DIALOG_PICKFILE(_extra=_ex)
  ENDELSE
  ;
  IF fname[0] EQ '' THEN RETURN,''
  ;
  IF KEYWORD_SET(af) THEN BEGIN
    IF tag_contains(TAG_NAMES(_ex), 'filter') THEN BEGIN
      dot_pos=STRPOS(fname,'.',/reverse_search)
      sep_pos=STRPOS(fname,PATH_SEP(),/reverse_search)
      IF dot_pos EQ -1 THEN fname=fname+'.'+getfilesuffix(_ex.FILTER(0))
      IF dot_pos EQ STRLEN(fname)-1 THEN BEGIN
        fname=STRMID(fname,0,dot_pos)+'.'+getfilesuffix(_ex.FILTER(0))
      ENDIF
      IF dot_pos GT sep_pos THEN GOTO, out_af
    ENDIF
  ENDIF
  out_af:
  ;
  RETURN,fname
  ;
  ;IDL 6.0 already has a similar keyword, OVERWRITE_PROMTP, so you do not need it.
  IF KEYWORD_SET(op) THEN BEGIN
    IF FILE_TEST(fname) THEN BEGIN
      yn=DIALOG_MESSAGE('Already exists. Overwrite?',/question,/default_no,title='Overwrite?')
      IF yn EQ 'No' THEN GOTO, start
    ENDIF
  ENDIF
  ;
  RETURN,fname
END

PRO DP
  ;OFILE=DP(/WRITE,filter=[['*.llhxyz'],['iGPS Priori Coordinates File (*.llhxyz)']],/AF)
  x=dp(filter='*.hrf',/af,/rec)
  PRINT,x
END
