;+
; :Name:
;    INIT_CMC_FILE_CMDSTR_PS
;
; :Description:
;    Construct running string (CMC derivation loop for each site) for calls from IDL_IDLBridge object.
;
; :Keywords:
;    FID -  Current Site ID (input, integer)
;    CMDSTR - Running string (output, string)
;
;-
PRO INIT_CMC_FILE_CMDSTR, FID, CMDSTR=CMDSTR
  ;
  CMDSTR='TMP=CMC_FILE( '+STRTRIM(FID,2)+', '+  $
    ' FILES=FILES, '+ $
    ' INDSA=INDSA, '+ $
    ' SITES=SITES, '+ $
    ' LLHS=LLHS,  '+  $
    ' CORRS=CORRS,  '+  $
    ' BLEN=BLEN, '+ $
    ' NTAU=NTAU,NW=NW,TAUS=TAUS,WS=WS,  '+  $
    ' DATAA=DATAA, '+ $
    ' OPATH_CMC_RAW=OPATH_CMC_RAW,  '+  $
    ' OPATH_FLT_RAW=OPATH_FLT_RAW,  '+  $
    ' OPATH_FLT_SMOOTHED=OPATH_FLT_SMOOTHED,  '+  $
    ' OPATH_CMC_SMOOTHED=OPATH_CMC_SMOOTHED,  '+  $
    ' NEUSTR=NEUSTR,  '+  $
    ' XRANGE=XRANGE,YRANGES=YRANGES, '+ $
    ' DMIN=DMIN,NMIN=NMIN,  '+  $
    ' DATES=DATES, '+ $
    ' FMTSTR=FMTSTR, '+ $
    ' SF=SF, '+ $
    ' PREVIEW=PREVIEW, '+ $
    ' CORR_FILE=CORR_FILE, PATH=PATH, '+  $
    ' TITLE_NEU=TITLE_NEU)'

END