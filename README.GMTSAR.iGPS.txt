
iGPS made a few modifications to the original releaes of GMTSAR, to
  + reduce the processing time by not creating unnecessary files;
  + facilitate setting parameters (e.g., --mcf/--mst for snaphu);
  + make the problem more robust, etc.

Modification by tianyf/tyf
+ Tue Feb 21 15:09:16 CST 2023
  \make_s1a_tops.c:  increase the length of tree sting buffer (again from 180 to 1800 
                     chars to accomodate even larger data files(#397).

+ Mon Jun  8 17:33:45 CST 2020
  \snaphu.csh:  switch to --mcf by default
  \cut_slc.c:  automatically adjust the cutting range to fit file dimensions;
               add warning information if the cutting range is invalid
  ###\intf_tops.csh:  in the case of specifying *wrong* ranges for region_cut, get
                   the correct dimensions information from phaes.grd file 
                   before calling snaphu.csh
                   

+ Sun Jun  7 06:17:14 EDT 2020
  \snaphu.csh:  switch to detrend the interferogram by grdtrend
  \snaphu.csh:  created corr_cut.grd file by adding
                "mv -f corr_patch.grd corr_cut.grd" at the end
  \snaphu.csh:  add echo and time information for running snaphu
  \snaphu.csh:  disable unwrap.pdf output (commented out)
  \intf_tops.csh:  add clean script at the end (calling 
                   sh_sar_clean_intf_all_merge)
  \intf_tops_parallel.csh:  add delay parameter (--delay 30) when calling 
                            parallel
  \proj_ra2ll.csh:  change default pixel size to 240 meter for saving disk space
  \merge_unwrap_geocode_tops.csh:  disable some files (los.pdf, ...)
  \geocode.csh:  disable some files (*.pdf)
  \filter.csh:  disable generating *.pdf files
  \assemble_tops (xml.c): fix a bug when the node parent is null (#753, #786)
  ###\make_s1a_tops.c:  increase the length of tree sting buffer (from 80 to 180 
                     chars (#397)


  
