;
;+
; :Name:
;    CMC_FORMULAS
;
; :Description:
;    Derive CMC for given dataset (time series, uncertainities, weights).
;
;
;
; :Keywords:
;    TS - Raw residual time series of sites used to construct CMC (input, double, [NDAYS, NSIT])
;    SSIGMA - Formal errors for time series (input, double, [NDAYS, NSIT])
;    WEIGHTS  - Weights for sites (input, double, [NSIT])
;    CMC  - CMC (output, double, [NDAYS])
;    NSITS  - Number of sites used to derive CMC for each day (output, integer, [NDAYS])
;
; :Modifications:
;    + On 2:31:02 PM Apr 9, 2015 created by tianyf.
;-
PRO CMC_FORMULA, $
    TS=TS,  $
    SSIGMA=SSIGMA, $
    WEIGHTS=WEIGHTS, $
    CMC=CMC,  $
    NSITS=NSITS, $
    NMIN=NMIN
    
    IF N_ELEMENTS(NMIN) EQ 0 THEN NMIN=3
    
  ;; old algorithm (a bit slow)
  ;  ;
  ;  NDAYS=N_ELEMENTS(TS[*,0])
  ;  CMC=DBLARR(NDAYS)
  ;  NSITS=INTARR(NDAYS)
  ;  ;return
  ;
  ;  ;Calculate CMC; loop for each day
  ;  FOR DI=0,NDAYS-1 DO BEGIN
  ;    ;TSI=TS[DI,*]
  ;    ;SIGMAI=SSIGMA[DI,*]
  ;    ;INDDI=WHERE(TSI NE 0 AND SIGMAI NE 0)
  ;    INDDI=WHERE(TS[dI,*] NE 0)
  ;    ;If the number of available sites for current day is less then 3, then skip CMC derivation.
  ;    IF N_ELEMENTS(INDDI) GE 3 THEN BEGIN
  ;      CMCDI=TOTAL(TS[di,INDDI]*WEIGHTS[INDDI]/sSIGMA[di,INDDI]^2)/TOTAL(1D0*WEIGHTS[INDDI]/sSIGMA[di,INDDI]^2)
  ;      CMC[DI]=CMCDI
  ;      NSITS[DI]=N_ELEMENTS(INDDI)
  ;    ENDIF
  ;  ENDFOR  ;end-of-day-loop
  ;
    
  ;Tue, May 12, 2015 10:54:52 AM
  ;NEW METHOD USING MATRIX MULTIPLICATION
  SIGS=SSIGMA
  TSS=TS
  WS=WEIGHTS
  
  ZOS=MAKE_ARRAY(DIMENSION=SIZE(TS,/DIMENSIONS),/BYTE)
  IND1=WHERE(TSS NE 0, COMPLEMENT=IND0)
  IF IND1[0] NE -1 THEN ZOS[IND1]=1
  IF IND0[0] NE -1 THEN SIGS[IND0]=1
  SIGS2=ZOS/SIGS^2
  T_S=TSS*SIGS2
  
  CMC=(T_S#WS)/(SIGS2#WS)
  
  W1=BYTARR(N_ELEMENTS(WS))
  W1[*]=1
  NSITS=ZOS#W1
  POS=WHERE(NSITS LT NMIN)
  IF POS[0] NE -1 THEN CMC[POS]=0  ;IF LESS THEN 3 CMC STATIONS, THEN NO CMC FOR THAT DAY
;STOP
END



