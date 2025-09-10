;
;+
; :Name:
;   SET_INTERSECT
;
; :Description:
;   The intersection of A and B
; :Params:
;    SET0
;    SET1
;
; :Keywords:
;    IND0
;    IND1
;
; :Examples:
;   Compile and run it to get examples.
;
; :Modifications:
;   + Bug found on Nov 09, 2015 by tianyf
;       This routine cannot handle the case of input array with duplicate elements.
;       NOT fixed.
;       For input sets with duplicate elements, use the set_intersect2 code (the old implementation).
; 
;   + Performance Improved. May 06, 2015 by tianyf
;       Not using loop any more. Improved the speed of matching.
;       
;   + Released on May 11, 2010
;
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION SET_INTERSECT, $
    SET0, $ ;the first input array (should be one 1-d array)
    SET1, $ ;the second input array
    IND0=IND0, $  ;the returned index of common elements in SET0
    IND1=IND1, $
    FOUND=FOUND ; 0 - not found;  1 - found overlapping
    
  
  ;initial values: not found
  FOUND=0
  IND0=[-1]
  IND1=[-1]

  ;Bug fixed by tianyf on Fri May 30 16:40:41 CST 2025 
  ;  When both SET0 and SET1 are one-element arrays, the returned result contains
  ;    multiple identical elements.
  ;
  IF N_ELEMENTS(SET0) EQ 1 AND N_ELEMENTS(SET1) EQ 1 THEN BEGIN
    IF SET0 EQ SET1 THEN BEGIN
      FOUND=1
      IND0=[0]
      IND1=[0]
      RETURN,SET0
    ENDIF ELSE BEGIN
      RETURN, MAKE_ARRAY(1,TYPE=SIZE(SET0[0],/TYPE))  ;RETURN VOID
    ENDELSE
  ENDIF

  ;perform sort and uniq operations on  the input arrays first.
;  SET0A=SET0[SORT(SET0)]
;  SET0A=SET0A[UNIQ(SET0A)]
;  SET1A=SET1[SORT(SET0)]
;  SET1A=SET1A[UNIQ(SET1A)]
;  SET01=[SET0A,SET1A]
  SET01=[SET0,SET1]
  INDEX0=ULINDGEN(N_ELEMENTS(SET0))
  INDEX1=ULINDGEN(N_ELEMENTS(SET1))
  INDEX01=[INDEX0,INDEX1]
  SUB0=BYTARR(N_ELEMENTS(SET0))
  SUB1=BYTARR(N_ELEMENTS(SET1))
  SUB1[*]=1
  SUB01=[SUB0,SUB1]
  
  POS_S=SORT(SET01)
  
  SET01_S=SET01[POS_S]
  INDEX01_S=INDEX01[POS_S]
  SUB01_S=SUB01[POS_S]
  
  POS=WHERE( SET01_S EQ SHIFT(SET01_S, -1) )
  IF POS[0] EQ -1 THEN BEGIN  ;NO COMMON PARTS
    ;STOP
    RETURN, MAKE_ARRAY(1,TYPE=SIZE(SET0[0],/TYPE))  ;RETURN VOID
  ENDIF
  SET01=SET01_S[POS]  ;RETURNED SET
  
  IND_DUP=[POS,POS+1]
  INDEX01_R=INDEX01_S[IND_DUP]
  SUB01_R=SUB01_S[IND_DUP]
  SET01_R=SET01_S[IND_DUP]
  
  POS0=WHERE(SUB01_R EQ 0)
  IND0=INDEX01_R[POS0]
  POS1=WHERE(SUB01_R EQ 1)
  IND1=INDEX01_R[POS1]
  
  IND0=IND0[SORT(IND0)]
  IND1=IND1[SORT(IND1)]
  
  FOUND=1
  ;STOP
  
  RETURN,SET01
  
  
;BELOW IS THE OLD ALGORIGHM. MUCH SLOW!
;  ;SETS=-1
;  IND0=[-1]
;  IND1=[-1]
;
;  IND0=LONARR(N_ELEMENTS(SET0))
;  IND1=LONARR(N_ELEMENTS(SET1))
;  SETI=0
;  ;STOP
;  SETS=MAKE_ARRAY(N_ELEMENTS(IND0)+N_ELEMENTS(IND1),TYPE=SIZE(SET0,/TYPE))
;
;
;  T0=SYSTIME(/SECONDS)
;  FOR I=0ULL, N_ELEMENTS(SET0)-1 DO BEGIN
;    TMP = WHERE(SET1 EQ SET0[I])
;    IF TMP[0] EQ -1 THEN CONTINUE
;    IF N_ELEMENTS(TMP) GT 1 THEN BEGIN
;      ;PRINT,SET1[TMP],SET0[I]
;      ;STOP
;      PRINT,'[SET_INTERSECT]WARNING:redaudent elements! Use first matching.'
;      TMP=TMP[0]
;    ENDIF
;    IF IND1[TMP] EQ 1 THEN BEGIN
;      PRINT,'[SET_INTERSECT]WARNING:redundant matching for ',SET0[I],' in the first series!'
;      CONTINUE
;    ENDIF
;
;    SETS[SETI]=SET0[I]
;    SETI=SETI+1ULL
;    IND0[I]=1
;    IND1[TMP]=1
;    ;IF N_ELEMENTS(SETS) EQ 0 THEN SETS=SET0[I] ELSE SETS = [SETS, SET0[I]]
;    ;IND0=[IND0,I]
;    ;IND1=[IND1,TMP]
;  ;PRINT,IND
;  ENDFOR
;
;  T1=SYSTIME(/SECONDS)
;  PRINT,'loop: ',T1-T0, ' SECONDS'
;
;  POS0=WHERE(IND0 EQ 1)
;  IF POS0[0] EQ -1 THEN BEGIN ;NO INTERSECTION
;    IND0=[-1]
;    IND1=[-1]
;    RETURN, MAKE_ARRAY(1,TYPE=SIZE(SET0[0],/TYPE))  ;RETURN VOID
;  ENDIF
;
;  ;found
;  FOUND=1
;  ;
;  IND0=POS0
;  POS1=WHERE(IND1 EQ 1)
;  IND1=POS1
;
;RETURN, SETS[0:SETI-1]
END

PRO SET_INTERSECT

   PROG='SET_INTERSECT'
  PRINT,'['+PROG+']Examples.'
  
  PRINT,'['+PROG+']1. Integer example'
  ;integer arrays
  SET0 = [3 ,3,5,7, 6]
  ;SET0 = [3 ,5, 6,  7]
  SET0 = [1, 0,3]
  SET0 = [ 34,4 ]
  
  SET1 = [1 ,2 ,3 ,65, 7, 8]
  SET1 = [1 ,2 , 8]
  SET1 = [1 ,9 ,3, 8]
  SET1 = [4,44]
  ;
  
  
  PRINT,'['+PROG+']  1st:',SET0, FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(SET0),2)+'(1X,I,:,","),$)'
  PRINT,']'  
  PRINT,'['+PROG+']  2nd:',SET1, FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(SET1),2)+'(1X,I,:,","),$)'
  PRINT,']'
  SET01=SET_INTERSECT(SET0,SET1,IND0=IND0,IND1=IND1)
  PRINT,'['+PROG+']  res:',SET01, FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(SET01),2)+'(1X,I,:,","),$)'
  PRINT,']'
  PRINT,'['+PROG+'] IND0:',IND0, FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(IND0),2)+'(1X,I,:,","),$)'
  PRINT,']'
  PRINT,'['+PROG+'] IND1:',IND1, FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(IND1),2)+'(1X,I,:,","),$)'
  PRINT,']'  
  RETURN
  
  ;String arrays
  PRINT,'['+PROG+']'
  PRINT,'['+PROG+']2. String example'
  SET0=STRING(SET0)
  SET1=STRING(SET1) 
  
  PRINT,'['+PROG+']  1st:',"'"+STRTRIM(SET0,2)+"'", FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(SET0),2)+'(1X,A9,:,","),$)'
  PRINT,']'  
  PRINT,'['+PROG+']  2nd:',"'"+STRTRIM(SET1,2)+"'", FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(SET1),2)+'(1X,A9,:,","),$)'
  PRINT,']'
  SET01=SET_INTERSECT(SET0,SET1,IND0=IND0,IND1=IND1)
  PRINT,'['+PROG+']  res:',"'"+STRTRIM(SET01,2)+"'", FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(SET01),2)+'(1X,A9,:,","),$)'
  PRINT,']'
  PRINT,'['+PROG+'] IND0:',IND0, FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(IND0),2)+'(1X,I,:,","),$)'
  PRINT,']'
  PRINT,'['+PROG+'] IND1:',IND1, FORMAT='(A, "[",'+STRTRIM(N_ELEMENTS(IND1),2)+'(1X,I,:,","),$)'
  PRINT,']'
  RETURN
   
  
  PRINT,'['+PROG+']'
  PRINT,'['+PROG+']3. Large (100, 000 elements) integer example'
  T0=SYSTIME(/SECONDS)  
  SET0=LINDGEN(100000ULL)
  SET1=LINDGEN(100000ULL)+10000
  PRINT,'['+PROG+'] number of elements of SET0: '+STRTRIM(N_ELEMENTS(SET0),2),FORMAT='(A)'
  PRINT,'['+PROG+'] number of elements of SET1: '+STRTRIM(N_ELEMENTS(SET1),2),FORMAT='(A)'
  SET01=SET_INTERSECT(SET0,SET1,IND0=IND0,IND1=IND1)
  PRINT,'['+PROG+'] number of common elements between SET0 and SET1: '+STRTRIM(N_ELEMENTS(SET01),2),FORMAT='(A)'
  HELP,SET01,IND0,IND1  
  T1=SYSTIME(/SECONDS)
  PRINT,'['+PROG+'] time: ',T1-T0, ' seconds.'  
  
END
