;+
; Name:
;   RANK
;
; Purpose:
;   Calculate the rand/order of the elements in an array.
;
; Modifications:
;   + Fri, Jan 23, 2015  5:06:18 PM by tianyf
;     Created.
;-
FUNCTION RANK, arr, reverse=reverse
  ;
  ;sort the array
  sarr=arr[SORT(arr)]
  ;get uniq array elements
  uarr=sarr[UNIQ(sarr)]
  
  rnki=1
  rnks=INDGEN(N_ELEMENTS(arr))
  
  IF ARG_PRESENT(reverse) || (N_ELEMENTS(reverse) NE 0 && REVERSE EQ 1) THEN BEGIN
    FOR uai=N_ELEMENTS(uarr)-1, 0, -1 DO BEGIN
      pos=WHERE(arr EQ uarr[uai])
      rnks[pos]=rnki
      rnki=rnki+1;n_elements(pos)
    ENDFOR
  ENDIF ELSE  BEGIN
    FOR uai=0, N_ELEMENTS(uarr)-1 DO BEGIN
      pos=WHERE(arr EQ uarr[uai])
      rnks[pos]=rnki
      rnki=rnki+1;n_elements(pos)
    ENDFOR
  ENDELSE
  
  ;stop
  RETURN, rnks
END

PRO RANK
  arr=STRSPLIT('5       0       0       0       0       0       0       0       0       8      11       0',/extract)
  ;HELP, arr
  PRINT, 'input string array:', arr
  r=rank(arr)
  ;HELP, r
  PRINT, 'output of string array rank:', r
  
  PRINT, 'input integer array:', arr
  r=rank(arr)
  ;HELP, r
  PRINT, 'output of integer rank:'
  print,r, format='(1x,5i)'
  rr=rank(arr, /rev)
  PRINT, 'output of integer rank:'
  print,rr, format='(1x,5i)'
END