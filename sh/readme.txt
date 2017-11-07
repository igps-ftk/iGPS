1 Download S1 raw data (*.zip) and orbits (*.EOF) from ESA website.
  sh_esa_run_aria2
  sh_esa_s1_get_orb
  sh_esa_s1_unzip_manifest
  MANIFEST2OUTLINE (IDL)
  MANIFEST_OVERLAPPING (IDL)

2 Create links to ESA data in the orig directory.
  (prepare DEM file manually)
  sh_esa_link_orig file_in >& link_orig.log
  sh_esa_link_orb > tiff_EOF.list
  link s1a-aux-cal.xml file.
  PREP_PREP, file, ofile, path_esa=path_esa (IDL) ->sh_xml_create
  dos2unix sh_xml_create && ./sh_xml_create

3 Pre-processing
  0README_prep-1.txt

  1README_prep-2.txt

4 Processing interfeorometric images
  2README_proc.txt

5 Create time series
  3README_sbas.txt
