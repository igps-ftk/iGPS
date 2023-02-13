PRO SDM_PLOT_CONVERGE
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  file='\\10.4.134.49\tianyf\gsar\co.changning1\sdm\united\strike-slip\converge.dat'
  
  ofile=file+'.jpg'
  
  READ_COLS, file, data=data, skip=2
  HELP, data
  WINDOW,1,    xsize=512, ysize=512
  !p.MULTI=-1
  PLOT,data[3,*], data[2,*], background='ffffff'x, color='0'x, psym=-2, $
    ;xstyle=1, ystyle=1, $
    ytitle='misfit', xtitle='roughness', $
    /ynozero,/nodata
    oPLOT,data[3,*], data[2,*], color='ff0000'x,psym=2
    
   write_jpeg, ofile, tvrd(true=1), true=1, quality=100 
END