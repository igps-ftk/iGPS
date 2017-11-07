;+
; Name:
;		str_lines2arr
; Purpose:
;		Convert a string array(1*N) (in which each line contains M samples) into M*N array.
; Example:
;		string array:
;		gcp=
;			'0 1 2'
;			'3 4 5'
;			'6 7 8'
;		You can get a float array by using str2flt(gcp):
;			IDL> fgcp=arr_strlines(gcp)
;
;	Modifications:
;	  + Tue, Jan 20, 2015  3:18:20 PM by tianyf
;	    Fix a bug when the input file has a TAB separator (the original code use 
;	      a space as the default separator). If PTN is not specified, we now 
;	      use the IDL defaults.
;-
FUNCTION STR_LINES2ARR, STR, PTN
  COUNT=N_ELEMENTS(STR)
  
  IF N_ELEMENTS(PTN) EQ 0 THEN BEGIN
    ARR=STRSPLIT(STR(0),/EXTRACT)
  ENDIF ELSE BEGIN
    ARR=STRSPLIT(STR(0),PTN, /EXTRACT)
  ENDELSE
  
  NS=N_ELEMENTS(ARR)
  INDS=UINTARR(COUNT)
  FOR I=0L, COUNT-1 DO BEGIN
    ;SKIP BLANK LINES   TIANYF JAN15, 2014
    IF STRTRIM(STR[I],2) EQ '' THEN CONTINUE
    INDS[I]=1
    ;NSI=N_ELEMENTS(STRSPLIT(STR[I],PTN,/EXTRACT))
    IF N_ELEMENTS(PTN) EQ 0 THEN BEGIN
      NSI=N_ELEMENTS(STRSPLIT(STR[I],/EXTRACT))
    ENDIF ELSE BEGIN
      NSI=N_ELEMENTS(STRSPLIT(STR[I],PTN,/EXTRACT))
    ENDELSE
    IF NSI LT NS THEN BEGIN
      NS=NSI
    ENDIF
  ENDFOR
  
  ;ARR=(STRSPLIT(STR(0),PTN, /EXTRACT))[0:NS-1]
  ARR=STRARR(NS,COUNT)
  FOR I=0ULL, COUNT-1 DO BEGIN
    ;SKIP BLANK LINES   TIANYF JAN15, 2014
    IF STRTRIM(STR[I],2) EQ '' THEN CONTINUE
    ;ARR[*,I]=(STRSPLIT(STR(I), PTN, /EXTRACT))[0:NS-1]
    IF N_ELEMENTS(PTN) EQ 0 THEN BEGIN
      ARR[*,I]=(STRSPLIT(STR(I), /EXTRACT))[0:NS-1]
    ENDIF ELSE BEGIN
      ARR[*,I]=(STRSPLIT(STR(I), PTN, /EXTRACT))[0:NS-1]
    ENDELSE
    
  ;help, arr, (i+1L)*count
  ENDFOR
  ;ARR=REFORM(ARR,NS,COUNT)
  POS=WHERE(INDS EQ 1)
  
  RETURN, ARR[*,POS]
END

PRO STR_LINES2ARR
  gcp=['0 1 2','3 4 5','6 7 8']
  PRINT,'Before processing:'
  HELP,gcp
  FOR i=0,2 DO PRINT,gcp(i)
  fgcp=str_lines2arr(gcp)
  PRINT,'After..:'
  HELP,fgcp
  PRINT,fgcp
;
END
