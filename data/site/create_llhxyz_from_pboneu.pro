PRO CREATE_LLHXYZ_FROM_PBONEU,PATH,FILE
  IF N_PARAMS() LT 2 THEN BEGIN
    PATH=DIALOG_PICKFILE(/DIRECTORY, $
      TITLE='PBO RAW Time Series Files(GLOBK tssum: *.pos) Directory:')
    IF PATH EQ '' THEN RETURN
    FILE=DIALOG_PICKFILE(/WRITE, $
      FILTER=[['*.llhxyz'],['iGPS Priori Coordinates File (*.llhxyz)']])
    IF FILE EQ '' THEN RETURN
  ENDIF
  FILES=FILE_SEARCH(PATH+PATH_SEP()+'*.pos',COUNT=NF)
  IF NF LE 0 THEN RETURN
  SITES=STRARR(NF)
  LLHS=DBLARR(3,NF)
  XYZS=DBLARR(3,NF)
  OPENW,FID,FILE,/GET_LUN
  WRITE_SYS_INFO,FID,USER=USER,PROG='CREATE_LLHXYZ_FROM_PBONEU',$
    SRC=PATH+PATH_SEP()+'*.pos'
  PRINTF,FID,'Site','Longtitude','Latitude','Height ','X','Y ',' Z',FORMAT='("*",A4,1X,3A20,1X,3A20)'
  FOR I=0,NF-1 DO BEGIN
    FILE=FILES[I]
    SITES[I]=STRMID(GETFILENAME(FILE),0,4)
    query_pbo, file, site = site, $
      firstepoch = firstepoch, $
      lastepoch = lastepoch, $
      xyzref = xyzREF  , $
      neuref = neuref, $
      nh=nh
    IF XYZref[0] EQ 0 || XYZREF[0] EQ -9999 THEN BEGIN
      PRINT,' NO XYZ FOR '+SITES[I]
      CONTINUE
    ENDIF
    ;HELP,NEUREF,XYZREF
    ;STOP
    PRINTF,FID,SITES[I],NEUREF[[1,0,2]],XYZREF, $
      FORMAT='(1X,A4,1X,3F20.12,1X,3F20.5)'
  ENDFOR
  FREE_LUN,FID
END