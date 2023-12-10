*************************************************************************************
*** Paths to external files  ********************************************************
***  this file has to obey Fortran syntax
**** paths names are all limited to 80 characters
*************************************************************************************
** path to kml file header, needed if +kml flag set
      kmlhdr = 
     .   '/home/mccafr/TDEFNODE/kml.header'

** path of Smithsonian volcanoes file, needed if +vtw flag is set
      volcfile = 
     .   '/home/mccafr/TDEFNODE/votw.gmt'

** paths of files of earthquakes to be projected onto profiles, needed if +eqk flag set
**  set num_quakefiles to number of files used, if more than MAX_qfiles reset dimension of
**  MAX_qfiles in tdefcomm1.h
** files should contain longitude, latitude, depth for each quake
c      num_quakefiles = 1
      quakefile(1) = 
     .    '/home/mccafr/TDEFNODE/ehb.gmt'

c     quakefile(2) = '/Users/mccafr/work/dn/pgc_quakes'
c     quakefile(3) = '/Users/mccafr/work/dn/uw_quakes'
c     quakefile(2) = '/Users/mccafr/work/dn/fiordland_2003.quakes'

** paths for CRUST2 rigidity files
** these are used for moment calculations if +cr2 flag is set, otherwise not needed
** see  http://mahi.ucsd.edu/Gabi/rem.dir/crust/crust2.html

      CNkey  =  '/Users/mccafr/work/CRUST2/CNtype2_key.txt'
      CNtype =  '/Users/mccafr/work/CRUST2/CNtype2.txt'
      CNelev =  '/Users/mccafr/work/CRUST2/CNelevatio2.txt'

*************************************************************************************

