;+
;***Note:
;
;   This program is revised from GGMatlab's calrealsigma code which can be found
;   at http://www-gpsg.mit.edu/~tah/GGMatlab/.
;   
;   **GGMatlab software is covered by the standard GAMIT/GLOBK License agreement.
;   http://www-gpsg.mit.edu/~simon/gtgk/gtgk_download_form.html
;   i.e.:
;   Permission is hereby granted, without written agreement or royalty fee, to 
;   copy, use, and modify this software and its documentation only for any 
;   non-commercial purposes, provided that the copyright notices below and the 
;   following three paragraphs appear on all copies of this software, and 
;   provided the parties using the software notify MIT by sending their name and
;   address by email to both the MIT Technology Licensing Office (TLO@mit.edu) 
;   and Dr. Robert W. King (rwk@chandler.mit.edu). 
;   
;   For GAMIT: Copyright 1985, 1999 Massachusetts Institute of Technology and 
;   The Regents of the University of California, San Diego. All Rights Reserved.
;
;   For GLOBK: Copyright 1986, 1999 Massachusetts Institute of Technology and 
;   Harvard University All Rights Reserved.
;
;   For FONDA: Copyright 1993, 1995 Massachusetts Institute of Technology. All 
;   Rights Reserved. 
;-
PRO REALISTIC_SIGMA,DATA,residual,NRMSR=NRMSR
  on_error, 0
  
  IF N_PARAMS() LT 2 THEN BEGIN
    file='J:\tmp\test.offset\2011\LHAS.icd.final_igs08.pos.neu'
    file='J:\tmp\test.offset\2011\VANU.icd.final_igs08.pos.neu'
    file='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos.neu.cln\DRAO.icd.final_igs08.pos.neu'
    ;file='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos.neu.cln\BILI.icd.final_igs08.pos.neu'
    ;file='J:\gpse\rerun.lutai\comb\trnsLTCM\gsoln\pos.neu.cln\LCKI.icd.final_igs08.pos.neu'
    file='F:\Downloads\Example\Example\ParkEX\NEU\USLO.sio.noamf_frame.pos.neu'
    file='C:\Downloads\fwp\PPP_COOR.disp\neu.demean.2015chile\bton_gfzppp.neu'
    file='C:\Downloads\fwp\PPP_COOR.disp\neu.demean.2015chile\cnba_gfzppp.neu'
    
    READ_SIO, FILE, DATA=DATA1
    DATA=DATA1[[0,3,6],*]
    DATA=DATA1[[0,4,7],*]
    ;    DATA=DATA1[[0,5,8],*]
    X=REFORM(DATA[0,*])
    Y=REFORM(DATA[1,*])
    YE=REFORM(DATA[2,*])
    TS_MODEL,X,Y,YFIT=YFIT,RMSE=RMS,RESIDUAL=residual,COEF=COEF,SLOPE=SLOPE,SIGMA=SIGMA,CCOV=CCOV
    DATA[1:2,*]=DATA[1:2,*]*1D3
    residual=residual*1D3
    STOP
  ENDIF
  ;% Function to compute realistic sigmas
  ;
  ;% Algorithm here uses the changes in the nrms with increasing averaging
  ;% interval to predict infinite averaging NRMS
  ;%
  
  ;stop
  ;% First get some bounds on how much averaging we can do.
  times = REFORM(Data(0,*));
  err = REFORM(Data(2,*));
  start = MIN(times);
  stop = MAX(times);
  minav = 7;
  maxav = (stop-start)*365.25d0/10;
  IF maxav LT minav THEN minav = maxav/4; end
  numav = FIX(maxav/minav);
  ;%
  ;% zero the arrays we need
  savsta = 0;
  timsta = 0;
  ;% Get the "white-noise" part of the error budget by differincing adjucent
  ;% data points.
  nd = N_ELEMENTS(residual);
  dres = residual(0:nd-2)-residual(1:nd-1);
  dvar = err(0:nd-2)^2+err(1:nd-1)^2;
  dchi = SQRT(TOTAL(dres^2/dvar)/nd);
  ;STOP
  ;PRINT,'RealSigma white dchi:',dchi;
  err = err*dchi;
  ;% Loop over the averaging intervals
  FOR i = 0,numav-1 DO BEGIN
    dyr = (i+1)*minav/365.25d0;
    ;PRINT,'dyr:',i+1,dyr
    ;% Compute the averaged residals for this interval
    chi2 = 0;
    num = 0;
    avR = 0;
    avS = 0;
    FOR t = start,stop,dyr DO BEGIN
      sel = WHERE(times GE t AND times LT t+dyr);
      ;HELP, sel
      IF sel[0] EQ -1 THEN CONTINUE
      ;stop
      ars = residual(sel);
      ass = err(sel);
      IF N_ELEMENTS(ars) GT 2 THEN BEGIN
        w = 1d0/ass^2;
        Wmean = w##TRANSPOSE(ars)/TOTAL(w);
        Werr = 1d0/TOTAL(w);
        IF avr[0] EQ 0 THEN BEGIN
          avr=wmean
          avs=werr
        ENDIF ELSE BEGIN
          avR = [avR, Wmean];
          avS = [avS, Werr];
        ENDELSE
        ;if finite(wmean) ne 1 then stop
      ENDIF
    ENDFOR
    ;stop
    ;    % Fit we have more than 2 values get the chi^2
    num = N_ELEMENTS(avR);
    IF num GT 2 THEN BEGIN
      IF savsta[0] EQ 0 THEN BEGIN
        savsta=TOTAL(avR^2/avS)/num;
        timsta = (i+1)*minav
      ENDIF ELSE BEGIN
        savsta = [savsta, TOTAL(avR^2/avS)/num];
        timsta = [timsta, (i+1)*minav];
      ENDELSE
    ENDIF
  ENDFOR
  ;% For the moment write the values
  ;%for i = 1:numav
  ;%    fprintf(1,'Stats: %6.1f day, Chi^2 %10.2f\n',minav*i,savsta(i));
  ;%end
  ;%
  ;% Now do the fitting
  taus = [1l, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384,32768,65536];
  ;taus = [1l, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 5096];
  ntau = N_ELEMENTS(taus);
  nc = N_ELEMENTS(savsta);
  alpha = DBLARR(ntau)
  rmsa = DBLARR(ntau)
  ;STOP
  ;window,1
  ;plot,savsta,background='ffffff'x,color='0',psym=-2
  FOR i = 0,ntau-1 DO BEGIN
    ef = (1-EXP(-1d0*timsta/taus(i)));
    alpha(i) = MEAN(savsta/ef);
    res1 = savsta-(alpha(i)*ef);
    ;oplot,EF*alpha(i),color='00ff00'x
    ;PRINT,ALPHA[I]
    rmsa(i) = STDDEV(res1,/double);
  ENDFOR
  mnr = MIN(rmsa,ir);
  ;print,alpha
  ;print,RMSA
  nrmsR = SQRT(alpha(ir))*dchi;
  ;PRINT,'NRMS Realistic ',nrmsR,' ; Correlation time ',taus(ir),' days'
  ;STOP
  ;%
  ef = (1-EXP(-1d0*timsta/taus(ir)));
  fit = ef*alpha(ir);
  
;% See if we should display the fit
;he = findobj(gcf,'Tag','DisplayFit'); DisplayFit = get(he,'Value');
;if DisplayFit
;    figure;
;    ht = plot(timsta,savsta,'rs',timsta,fit,'g^-');
;    %set(gca,'Title','Chi^2 Fit as function of averaging time', ...
;    %    'XLabel','Averging time (days)', ...
;    %    'YLabel','Chi^2 of Residuals')
;    title(strcat(PlotTitle,' Chi^2 Fit as function of averaging time'));
;    xlabel('Averging time (days)');
;    ylabel('Chi^2 of Residuals');
;end
  if n_params() lt 2 then begin
    help, nrmsr
  endif
END




