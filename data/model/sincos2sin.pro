;Modifications:
;   +Thu, Feb 20, 2014  6:17:53 PM by tianyf
;     -Correct errors when one of (CcosB , CsinB) is zero.
;     
;EXPAND(C*SIN(A*X+B));
;                C SIN(A X) COS(B) + C COS(A X) SIN(B)
PRO SINCOS2SIN, CCOSB, CSINB, AMP=C, PHASE=B, $
    eCcosb=eCcosb,eCsinb=eCsinb, $
    eamp=eamp, $
    epha=epha
  IF N_PARAMS() LT 2 THEN BEGIN
    CCOSB=-0.01205
    CSINB=0.85770
    
    CSINB=-1.8331
    CCOSB=-2.1365
    
    CCOSB=-0.0222
    CSINB=-0.9831
    
    ;Period of  365.250 days,
    ;  cos amp=         1.76 +/-      0.80
    ;  sin amp=         3.63 +/-      0.81
    ;  magnitude=       4.03 +/-      0.81
    CSINB=1.76
    CCOSB=3.63
    ;
    ;ERROR!
    ;CCOSB=1.76
    ;CSINB=3.63
    
    ;bjfs_u_unf.out
    ;postfit chi2      1.000
    ;postfit rms       7.558
    ;white+flicker noise model
    ;white noise amp      3.510
    ;flicker noise amp     20.374
    ; Reference_X   -2148744.079581
    ; Reference_Y    4426641.281567
    ; Reference_Z    4044655.930022
    ; start_epoch  1999.9877
    ;   end_epoch  2008.4740
    ;    num_days    2906
    ; y-intercept  -4361.104   1493.724
    ;     slope_1      2.174      0.746  1999.9877 - 2008.4740
    ;  num_days_1   2906
    ;    sine_ann     -0.387      0.918  1999.9877 - 2008.4740
    ;  cosine_ann     -9.477      0.906  1999.9877 - 2008.4740
    ;   phase_ann      3.182
    ;      annual      9.485      1.290  1999.9877 - 2008.4740
    ;   sine_semi     -0.679      0.659  1999.9877 - 2008.4740
    ; cosine_semi      0.872      0.640  1999.9877 - 2008.4740
    ;  phase_semi      5.622
    ;        semi      1.106      0.918  1999.9877 - 2008.4740
    ;Ccosb=    -0.387
    ;Csinb=    -9.477
    
    
    ;    sine_ann      1.492      0.759  1996.0697 - 2008.7746
    ;  cosine_ann     -4.707      0.765  1996.0697 - 2008.7746
    ;   phase_ann      2.835
    ;      annual      4.938      1.078  1996.0697 - 2008.7746
    
    csinb=1.492
    ccosb=-4.707
    
    ;RIGHT?
    ;ccosb=1.492
    ;csinb=-4.707
    
    ;    sine_ann     -4.109      0.615  1992.3675 - 2008.7773
    ;  cosine_ann     -1.222      0.614  1992.3675 - 2008.7773
    ;   phase_ann      4.423
    ;      annual      4.287      0.869  1992.3675 - 2008.7773
    ;CSINB=-4.109
    ;CCOSB=-1.222
    
    ;CCOSB=-4.109
    ;CSINB=-1.222
    
    
    ;for kunm
    ;cats
    ;+VERT  MLE    :    -10814.088555
    ;+VERT  INTER  :       -7.0774 +-      0.4877
    ;+VERT  SLOPE  :        2.0393 +-      0.1397
    ;+VERT  SIN    :        6.0726 +-      0.2922
    ;+VERT  COS    :       -4.0875 +-      0.3014
    ;+VERT  SIN    :       -2.0474 +-      0.2938
    ;+VERT  COS    :        0.9297 +-      0.2929
    ;+VERT  OFFSET :       -7.2944 +-      0.8578
    ;+VERT  OFFSET :       -1.4247 +-      0.8643
    ;+VERT  WHITE NOISE
    ;+VERT  WH     :       11.0039 +-      0.1462
    
    csinb= -4.0875
    ccosb= 6.0726
    
    ;csinb= 0.9297
    ;ccosb= -2.0474
    
    ;ccosb= 0.9297
    ;csinb= -2.0474
    
    CSINB=1.76
    CCOSB=3.63
    
    CSINB=2.70
    CCOSB=-3.00
    ;
    
    csinb=-8.68
    ccosb=-0.59
    
    csinb= -11.70
    ccosb= -2.6
    
    ;SIO SHAO vertical
    CCOSB=0.085
    CSINB=-5.753
    
    CSINB=0.085
    CCOSB=-5.753
    
    ;shao north
    ;ccosb=0.141
    ;csinb=-0.940
    
    ;shao east
    ;ccosb=0.304
    ;csinb=-0.111
    
    
    csinb=2.95
    ccosb=5.36
    
    csinb=2.01
    ccosb=3.96
    
    ;shao u
    ;flicker + white noises
    csinb=-0.13
    ccosb=2.44
    
    ;ccosb=-0.13
    ;csinb=2.44
    
    
    ;white only
    csinb=-.35
    ccosb=2.34
    
    ;ccosb=-.35
    ;csinb=2.34
    
    ;jpl shao up
    ;csinb=-4.84d0
    ;ccosb=2.35d0
    csinb=-4.84
    ccosb=2.35
    
    ;wuhn u, offsets corrected
    ;     Rate in units per year          -6.0951 +/-           0.7547
    ;     Period of  365.250 days,  cos amp=         1.86 +/-      0.86  sin amp=         3.66 +/-      0.87  magnitude=         4.11 +/-      0.87
    ;     Period of  182.625 days,  cos amp=        -1.29 +/-      0.61  sin amp=        -0.26 +/-      0.62  magnitude=         1.32 +/-      0.61
    csinb=1.86
    ccosb=3.66
    ;;wn only
    csinb=1.58
    ccosb=3.86
    ;ccosb=1.58
    ;csinb=3.86
    csinb=-2.95
    ccosb=3.27
    csinb=-2.60
    ccosb=3.08
    
    csinb=2.81
    ccosb=4.94
    esinb=0.95
    ecosb=0.99
    
    
  ENDIF
  
  IF CCOSB EQ 0 && CSINB EQ 0 THEN BEGIN
    C=0D0
    B=0D0
    ECCOSB=0D0
    ECSINB=0D0
    RETURN
  ENDIF
  
  IF CCOSB EQ 0 THEN BEGIN  ;B = pi/2 or  3*pi/2
    print,'C*cos(B) equals 0'
    stop
    IF csinb GT 0 THEN BEGIN
      C=CSINB
      B=!dpi/2
    ENDIF ELSE BEGIN
      C=-1d0*csinb
      B=!dpi*3d0/2
    ENDELSE
    ECCOSB=0D0
    ECSINB=0D0
    RETURN
  ENDIF
  ;>> expand(C*sin(A*x+B))
  ;   = C*cos(A*x)*sin(B) + C*sin(A*x)*cos(B)
  IF CSINB EQ 0 THEN BEGIN  ;B = 0 or pi
    print,'C*sin(B) equals 0'
    stop
    IF CcosB GT 0 THEN BEGIN
      C=CCOSB
      B=0D0
    ENDIF ELSE BEGIN
      C=ABS(CcosB)
      B=!dpi
    ENDELSE
    ECCOSB=0D0
    ECSINB=0D0
    RETURN
  ENDIF
  
  
  ;langbein est_noise
  fmag=csinb^2 +ccosb^2
  fmag=SQRT(fmag)
  ;print,fmag
  IF N_ELEMENTS(eCsinb) GT 0 && N_ELEMENTS(eCcosb) GT 0 THEN BEGIN
    e_mag=(csinb^2)*(eCsinb^2) +(ccosb^2)*(eCcosb^2)
    e_mag=SQRT(e_mag)/fmag
    eamp=e_mag
  ENDIF
  ;print,e_mag
  
  ;>> expand(C*sin(A*x+B))
  ;   = C*cos(A*x)*sin(B) + C*sin(A*x)*cos(B)
  ; What we know are C*sin(B)=? and C*cos(B)=?.
  ; To be estimated: C and B.
  ;
  ;1) calculate B
  B=ATAN(CSINB,CCOSB) ;;;WRONG?
  ;2) calculate C
  C=CSINB/SIN(B)
  
  ;B=ATAN(CSINB/CCOSB) ;WHAT'S THE DIFFERENCE BETWEEN ABOVE TWO STATEMENTS?
  ;C=CSINB/SIN(B)
  
  
  ;IF B LT 0 THEN B=B+2*!DPI
  
  ;B=B*180./3.1415927D0
  ;IF (B.LT.0) THEN
  ;   B=B+360
  ;ENDIF
  
  IF N_PARAMS() LT 2 THEN BEGIN
    ;IF B LT 0 THEN B=B+2*!DPI
    PRINT,C, B,B*180/!DPI
  ENDIF
END