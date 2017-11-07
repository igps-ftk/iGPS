;+
; Name:
;		SEP
;
; Purpose:
;		Return path serperator for different system. WIN32/Linux/UNIX
;		Newest IDL version has a similar function called PATH_SEP
;
;+++
;-
function sep
	;
	sep=''
	;
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
	;
	RETURN, sep
end
;///
Pro sep
	print,'The path seperator of '+!version.os+' system is: '+sep()
End
