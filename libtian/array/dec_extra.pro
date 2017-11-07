;+
; NAME:
;		DEC_EXTRA
;
; PURPOSE:
;		DECOMPOSE _EXTRA STRUCT INTO INDIVIDUAL KEYWORDS.
;
;		** _extra is the reserved keyword for passing parameters to subroutines.
;
; Description:
;		If someone is writting a new function whose keyword parameters are not known, you
;		can setup a Common User Interface to call this function with user-supplied parameters.
;		e.g.:
;		Function Caller, user-function-name, _Extra=_ex
;		** We don't konw the names of those keyword parameters.
;		Calling by:  isok=Caller('userfunction',x=1,y=2)
;		In Caller:
;			keyword_parameters=dec_extra(_ex)
;			cmdstr='....'+user-function-name+'...'+keyword_parameters
;			..execute(cmdstr)
;			...
;
;-
FUNCTION DEC_EXTRA, _EX
	TGS=TAG_NAMES(_EX)
	STR_EX=''
	FOR IEX=0,N_Elements(TGS)-1 DO BEGIN
		CMDSTR_EX=TGS(IEX)+'=_EX.'+TGS(IEX)
		STR_EX=STR_EX+','+CMDSTR_EX
	ENDFOR
	RETURN,STR_EX
END

;This is the user fucntion. We don't know the what parameters it will pass when write TST_Dec_Extra.
PRO MSG, X=X, Y=Y
	MSGSTR='Transfered parameters: x-'+STRTRIM(X,2)+',y-'+STRTRIM(Y,2)
	DUMMY=DIALOG_MESSAGE(MSGSTR,/INFO)
END
;
;This is the caller function. It accept name of function which will be executed and retrive the
;  parameters the called function has. Then call the function with supplied parameters.
FUNCTION TST_DEC_EXTRA, _EXTRA=_EX
	HELP, _EX, /STRUCT
	RES=DEC_EXTRA(_EX)
	CMDSTR='MSG'+RES
	DUMMY=EXECUTE(CMDSTR)
	RETURN,DUMMY
END

PRO DEC_EXTRA
	TMP=TST_DEC_EXTRA(X=0, Y=1, Z=Z)
	HELP,TMP
END