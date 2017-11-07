PRO TS_MINUS, file1, file2, ofile

  PROG='TS_MINUS'

  IF N_PARAMS() LT 3 THEN BEGIN
    file1='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos.neu.tibet.sgps\J322.icd.igs08_v16.pos.neu'
    file2='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos.neu.tibet.cgps.resid.cmc\cmc0\XZBG.icd.igs08_v16.poscmc.neu'
    ofile='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos.neu.tibet.sgps.flt\J322.icd.igs08_v16.pos.neu'
  ENDIF
  


    SITE=STRMID(GETFILENAME(FILE1),0,4)
    


    READ_SIO, FILE1, DATA=DATA1
    SRC=FILE1
    READ_SIO, FILE2, DATA=DATA2
    SRC=[SRC,FILE2]

    YDOYSTR1=STRING(DATA1[1,*],FORMAT='(I04)')+STRING(DATA1[2,*],FORMAT='(I03)')
    YDOYSTR2=STRING(DATA2[1,*],FORMAT='(I04)')+STRING(DATA2[2,*],FORMAT='(I03)')
    YDOYSTR12=SET_INTERSECT(YDOYSTR1,YDOYSTR2,IND0=IND1,IND1=IND2)
    ODATA1=DATA1[*,IND1]
    ODATA2=DATA2[*,IND2]

    ODATA=ODATA1;[0:5,*]
    ;ODATA[3:5,*]=0
    FOR NEUI=3,5 DO BEGIN
      ;POS=WHERE(ODATA1[NEUI,*] NE 0 AND ODATA2[NEUI,*] NE 0)
      ;
      ;IF THE 2ND TIME SERIES IS NAN, THEN NO CHANGES FOR THE 1ST TIME SERIES
      POS=WHERE(FINITE(ODATA2[NEUI,*]) EQ 1)
      IF POS[0] EQ -1 THEN CONTINUE
      ODATA[NEUI,POS]=ODATA1[NEUI,POS]-ODATA2[NEUI,POS]
      
      ;ODATA[NEUI,*]=ODATA1[NEUI,*]-ODATA2[NEUI,*]
    ENDFOR

    
    WRITE_SIO, OFILE, DATA=ODATA, USER=USER, $
      PROG=prog, $
      SRC=SRC
    ;BREAK
  PRINT,'['+prog+'] Normal end.'
END