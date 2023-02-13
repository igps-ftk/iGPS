PRO INIT_GNSS_SITE_NAME, ij, site_name=site_name

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_ELEMENTS(ij) EQ 0 THEN BEGIN
    ij=0
    ;ij=13
    PRINT,'['+prog+']WARNING: no input index given! Using index='+strtrim(ij,2)+'!'
  ENDIF
  
  letts=[STRTRIM(INDGEN(10),2), str2arr(STRTRIM(BINDGEN(26)+97b,2))]
  ;HELP, letts
  nlett=N_ELEMENTS(letts)
  
  IF ij GE 4d0^nLett THEN BEGIN
    PRINT,'['+prog+']ERROR: the index exceeds the maximum number of available names!!'
    site_name=''
    RETURN
  ENDIF
  
  
  ind3=ij/nlett^3
  ind2=(ij - ind3*nlett^3) / nlett^2
  ind1=(ij - ind3*nlett^3 - ind2*nlett^2) / nlett
  ind0=ij MOD nlett
  ;print,(ij - ind3*nlett^3 - ind2*nlett^2 - ind1*nlett)
  ;print,ind3,ind2,ind1,ind0,'=',letts[[ind3,ind2,ind1,ind0]]
  site_name=STRJOIN(letts[[ind3,ind2,ind1,ind0]])
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,ij
    HELP,site_name
  ENDIF
;STOP
END
