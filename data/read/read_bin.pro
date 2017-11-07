;APR09 2007 Tian
;  +A little progress for loop, much faster now;
;SIO:
;	comments lines start with a '#'
;
pro read_bin, file, site = site, $
                  firstepoch = firstepoch, $
                  lastepoch = lastepoch, $
                  xyzref = xyzref, $
                  neuref = neuref, $
                  data = data, $
		  headers = headers
  ;
  if n_params() lt 1 then begin
      file = 'E:\phd.data\chp.4.cme.filtering\series\sio.pbo.2006\unf.3y.wa.to_flt.resid.2006-_bin\agmtCleanUnf.bin'
      if file_test(file) ne 1 then file=dialog_pickfile()
      if file eq '' then return
  endif
  ;
  ;
 query_bin, file,  ns=ns, nl=nl, headers=headers

  data = dblarr(ns, nl)

  openr, fid, file, /get_lun;,/SWAP_IF_LITTLE_ENDIAN;,/SWAP_IF_BIG_ENDIAN
  readu, fid, data
  free_lun, fid
  if n_params() lt 1 then begin
  	;help, data
  	;print,data[0,*]
    window,1
    !p.multi=[0,1,3]
  	plot, data[0,*], data[3,*]*1d3,background='ffffff'x,color='0'x
    plot, data[0,*], data[4,*]*1d3,background='ffffff'x,color='0'x
    plot, data[0,*], data[5,*]*1d3,background='ffffff'x,color='0'x
    !p.multi=-1
  endif



  ;print, headers, format='(A)'
  return

end
