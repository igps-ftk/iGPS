;APR09 2007 Tian
;  +A little progress for loop, much faster now;
;
;Data Sample:
;	PBO Station Position Time Series
;	Format Version: 1.0.1
;	4-character ID: AB11
;	Station name  : Nome_AnvilAK2006
;	First Epoch   : 20060721 120000
;	Last Epoch    : 20070310 120000
;	Release Data  : 20070404 073140
;	XYZ Reference position :  -2658010.23252  -693674.79470  5737338.58385
;	NEU Reference position :    64.5644967742  194.6265414893  349.44237
; 	20060721 120000 53937.5000 -2658010.23592  -693674.79682  5737338.58572  0.00407  0.00260  0.00736  0.505 -0.677 -0.528
;		      64.5644967558  194.6265415143  349.44631    -0.00264   0.00119   0.00333    0.00282  0.00219  0.00805 -0.006 -0.003  0.295 suppf

PRO QUERY_PBO, FILE, SITE = SITE, $
    FIRSTEPOCH = FIRSTEPOCH, $
    LASTEPOCH = LASTEPOCH, $
    XYZREF = XYZREF, $
    NEUREF = NEUREF, $
    STR = STR, $ ;IF NOT PRESENT, THEN RETURN THE DOUBLE ARRAY; OTHERWISE, RETURN STRING ARRAY
    HEADERS = HEADERS, $  ;non-blank first column lines are header
    NS=NS, $
    NL=NL, $
    NH=NH
  ;
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','pbo', $
      'pos'],'SHLD.pbo.final_frame.pos')
    file='K:\mirror_ftp\data-out.unavco.org\pub\products\position\VNCX\VNCX.pbo.final_igs08.pos'
  ENDIF
  ;
  ;
  OPENR,fid,file,/get_lun
  tmp=''
  headers=''
  
  nh=0
  WHILE NOT EOF(fid) DO BEGIN
    READF,fid,tmp
    ;PRINT,tmp,format='(a)'
    IF STRMID(tmp,0,1) EQ ' ' THEN GOTO, readdata
    nh=nh+1
    IF headers[0] EQ '' THEN BEGIN
      headers=tmp
    ENDIF ELSE BEGIN
      headers=[headers,tmp]
    ENDELSE
    pos=STRPOS(tmp,'4-character ID')
    IF pos[0] NE -1 THEN BEGIN
      tmp2=STRSPLIT(tmp,':',/extract)
      site = tmp2[1]
    ENDIF
    pos=STRPOS(tmp,'First Epoch')
    IF pos[0] NE -1 THEN BEGIN
      tmp2=STRSPLIT(tmp,':',/extract)
      firstepoch = (STRSPLIT(tmp2[1],/extract))
    ENDIF
    pos=STRPOS(tmp,'Last Epoch')
    IF pos[0] NE -1 THEN BEGIN
      tmp2=STRSPLIT(tmp,':',/extract)
      lastepoch = (STRSPLIT(tmp2[1],/extract))
    ENDIF
    pos=STRPOS(tmp,'XYZ Reference position :')
    IF pos[0] NE -1 THEN BEGIN
      tmp2=STRSPLIT(tmp,':',/extract)
      ;print,tmp2,Strsplit(tmp2[1],/extract)
      xyzref = DOUBLE((STRSPLIT(tmp2[1],/extract))[0:2])
    ;XYZ Reference position :  -2339956.30045 -4707748.79815  3601666.09566 (SNARF)
    ;NEU Reference position :    34.5942819000  243.5706214778 1337.83692 (SNARF/WGS84)
    ENDIF
    pos=STRPOS(tmp,'NEU Reference position :')
    IF pos[0] NE -1 THEN BEGIN
      tmp2=STRSPLIT(tmp,':',/extract)
      neuref = DOUBLE((STRSPLIT(tmp2[1],/extract))[0:2])
    ;GOTO, readdata
    ENDIF
    
  ENDWHILE
  ;
  readdata:
  
  IF ARG_PRESENT(ns) || ARG_PRESENT(nl) THEN BEGIN
    datatmp=tmp
    nl=1
    ;READF,fid,datatmp
    tmp=STRSPLIT(datatmp[0],/extract)
    ns=N_ELEMENTS(tmp)
    ;
    WHILE NOT EOF(fid) DO BEGIN
      nl=nl+1
      READF,fid,datatmp
    ;datatmp=[[datatmp],[tmp]]
    ;help,datatmp
    ENDWHILE
  ;
  ;first row
    
    
  ENDIF
  
  endit:
  FREE_LUN,fid
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,site,firstepoch,lastepoch,xyzref,neuref,ns,nl,nh
  ENDIF
  
END

;Appendix A.
;Table 2: PBO GPS Station Position Time Series Format
;Entry Definition
;YYYY	 4-digit year for the given position epoch
;MM	 	2-digit month of year for the given position epoch
;DD 	2-digit day of month for the given position epoch
;HH 	2-digit hour for the given position epoch
;MM 	2-digit minute for the given position epoch
;SS	 2-digit second for the given position epoch
;JJJJJ	 Modified Julian day for the given position epoch
;X Y Z 	ITRF Cartesian coordinates, meters
;xx 	Standard deviation of the X position, meters
;yy	 Standard deviation of the Y position, meters
;zz	 Standard deviation of the Z position, meters
;xy	 Correlation of the X and Y position
;xz	 Correlation of the X and Z position
;yz	 Correlation of the Y and Z position
;N	 North latitude, decimal degrees, relative to WGS-84 ellipsoid
;E 	East longitude, decimal degrees, relative to WGS-84 ellipsoid
;U	 Elevation, meters, relative to WGS-84 ellipsoid
;Ndel	 Change in North component relative to NEU reference position, meters. If the
;		station moves northward, Ndel is positive.
;Edel	Change in East component relative to NEU reference position, meters. If the station
;		moves eastward, Ndel is positive.
;Udel 	Change in vertical component relative to NEU reference position, meters. If the
;		station moves upward, Ndel is positive.
;nn		Standard deviation of Ndel, meters
;ee 	Standard deviation of Edel, meters
;uu 	Standard deviation of Udel, meters
;ne 	Correlation of Ndel and Edel
;nu		 Correlation of Ndel and Udel
;eu		 Correlation of Edel and Udel
;<quality>		 'final' or 'rapid', corresponding to products generated from final or rapid orproducts
;See the PBO web page for a reference for the Modified Julian date.
