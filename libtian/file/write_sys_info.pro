;+
; :Name:
;   WRITE_SYS_INFO
;
; :Description:
;   Write the program running information to the output file,
;     for the purpose of facilitating identifying the source.
;   
; :Params:
;    FID - File id for outputing.
;
; :Keywords:
;    USER - User name (program operator).
;    HOST - The computer on which the program is running.
;    PROG - Program that created this output file.
;    CMT  - Comments symbol. Default is asterisk (*).
;    SRC  - Original places from which the output file was created.
;    COMMENT - Commenting lines. (could be a string array)
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
PRO WRITE_SYS_INFO, FID, USER=USER, $
    HOST=HOST, $
    PROG=PROG, $
    CMT=CMT, $
    SRC=SRC, $
    COMMENT=COMMENT
    
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT, '[WRITE_SYS_INFO]Error: output file id must be present!'
    RETURN
  ENDIF
  
  IF N_ELEMENTS(CMT) EQ 0 THEN CMT=''
  
  IF N_ELEMENTS(SRC) GT 1 THEN BEGIN
    FOR FI=0, N_ELEMENTS(SRC)-2 DO BEGIN
      PRINTF,FID,CMT+'*   SRC: '+SRC[FI]+' &',FORMAT='(A)'
    ENDFOR
    PRINTF,FID,CMT+'*   SRC: '+SRC[FI],FORMAT='(A)'
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(SRC) GT 0 THEN $
      PRINTF,FID,CMT+'*   SRC: '+SRC,FORMAT='(A)'
  ENDELSE
  
  IF N_ELEMENTS(USER) EQ 0 THEN BEGIN
    USER=WHOAMI()
  ENDIF
  IF N_ELEMENTS(HOST) EQ 0 THEN BEGIN
    HOST=HOSTNAME()
  ENDIF
  
  IF N_ELEMENTS(PROG) GT 0 THEN $
    PRINTF, FID, CMT+'*  PROG: '+ PROG, FORMAT='(A)'
  IF USER NE '' THEN BEGIN
    PRINTF, FID, CMT+'*RUN BY: '+USER+' @ '+HOST, FORMAT='(A)'
  ENDIF
  PRINTF, FID, CMT+'*RUN AT: '+!VERSION.OS_NAME+' '+!VERSION.ARCH, FORMAT='(A)'
  PRINTF, FID, CMT+'*RUN ON: '+SYSTIME(/UTC)+' [UTC]', FORMAT='(A)'
  
  IF N_ELEMENTS(COMMENT) GT 0 && COMMENT[0] NE '' THEN BEGIN
    FOR I=0, N_ELEMENTS(COMMENT)-1 DO BEGIN
      PRINTF,FID,CMT+'*'+COMMENT[I],FORMAT='(A)'
    ENDFOR
  ENDIF
  
  
END