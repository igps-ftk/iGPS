;reverse the string
FUNCTION STRREV,c
  c1=''
  FOR i=0,STRLEN(c)-1 DO BEGIN
  
    c1=c1+STRMID(c,i,1,/REVERSE_OFFSET)
    
  ENDFOR
  RETURN, c1
END

PRO STRREV
  ;PRINT,'strrec("hello, world"):',strrev("hello, world")
  PRINT,'The reverse of string "hello, world" is: "'+strrev('hello, world')+'".'
  
END