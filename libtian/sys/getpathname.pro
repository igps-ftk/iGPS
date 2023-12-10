;+
; Name:
;		GetPathName
;
; Purpose:
;		Get the path name from a given full file-path name.
;
; Calling Sequence:
;		path=GetPathName(Path_file_name [, sep=sepch])
;
; Examples:
;   IDL> print,file
;   /home/tianyf/sopac/cleanedNeuUnfTimeSeries20100126/7odmCleanUnf.neu
;   IDL> print,getpathname(file)
;   /home/tianyf/sopac/cleanedNeuUnfTimeSeries20100126
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION GETPATHNAME, filename, sepch2, sep=sepch

  IF N_ELEMENTS(sepch) EQ 0 THEN BEGIN
    IF N_ELEMENTS(sepch2) NE 0 THEN BEGIN
      sepch=sepch2
    ENDIF ELSE BEGIN
      CASE !VERSION.OS_FAMILY OF
        'MacOS': BEGIN
          sepch = ':'
        END
        'unix': BEGIN
          sepch = '/'
        END
        'vms': BEGIN
          sepch = ']'
        END
        'Windows': BEGIN
          sepch = '\'
        END
      ENDCASE
    ENDELSE
  ENDIF
  
  pos = STRPOS(filename, sepch, /REVERSE_SEARCH)
  isvalid=WHERE(pos NE -1)
  IF N_ELEMENTS(isvalid) EQ 1 THEN $
    IF isvalid EQ -1 THEN RETURN,''
  tmp=filename
  
  ;STOP
  ;BUG found on Mon, Mar 10, 2014 11:37:50 AM by tianyf
  ;  this function fails to remove the multiple separators, e.g., for
  ;    e:\test\\t.exe
  ;  it will return
  ;    e:\test\'
  ;  NOT
  ;    e:\test
  
  sep_width=STRLEN(sepch)
  
  ;loop for each filenames (if multiple supplied)
  FOR i=0ull,N_ELEMENTS(isvalid)-1 DO BEGIN
    tmp(isvalid(i))=STRMID(filename(isvalid(i)),0,pos(isvalid(i)))
    ;
    
    ;STOP
    ;removing additional trailing separators (if any) 
    isStillTrailing=1
    FOR j=pos[isvalid[0]],0,-1*sep_width DO BEGIN
      ch=STRMID(filename[isvalid[i]],j-sep_width,sep_width)
      ;print,ch
      IF isStillTrailing EQ 1 THEN BEGIN
        IF ch EQ sepch THEN BEGIN
          tmp(isvalid(i))=STRMID(filename[isvalid[i]],0,j-sep_width)
          ;print,tmp(isvalid[i])
        ENDIF ELSE BEGIN
          goto,next_filename
        ENDELSE
      ENDIF
    ENDFOR
    
    next_filename:
    
  ENDFOR
  
  RETURN,tmp
  
END


PRO GETPATHNAME
  file='e:\test\\t.exe'
  file=['e:////test////t.exe',file]
  PRINT,GETPATHNAME(file,'//')
END
