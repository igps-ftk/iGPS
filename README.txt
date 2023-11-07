**Note: iGPS main repository has moved to GitHub.
  Please download the latest release of iGPS from
    https://github.com/igps-ftk/iGPS

-----------------------*COPYRIGHT*-------------------------
Please note that somes codes included in iGPS release are from the internet.
  Although they are publicly available, we included those in iGPS for the
  convinience of the users. If you (author of the code) do not agree, please
  contact us to remove it from iGPS release. Thank you!
  Email: tianyf@gmail.com

The copyright of the following programs/data belongs to the original authors.
  + ftk/gglib/ (date conversion functions: from GAMIT/GLOBK; http://geoweb.mit.edu/gg/)
  + ftk/lib/doy (date conversion program: from GAMIT/GLOBK; http://geoweb.mit.edu/gg/)
  + ftk/nr/ (spectral analysis functions; from Numerical Recipe; http://numerical.recipes/)
  + sh/add_look.csh (from GMTSAR forum; Xiaohua Xu, http://gmt.soest.hawaii.edu/boards/6/topics/3271)
  + sh/merge_batch2.csh (from GMTSAR; revised to accept super master)
  + sh/merge_batch2_paralle.csh (from GMTSAR; revised to accept super master)
  + sh/proj_ll2ra_full.csh (from GMTSAR; revised to accept reference grid)
  + tables/HimaTibetMap/ (from https://github.com/HimaTibetMap/HimaTibetMap)
  + tables/wang_shen_2019JB018774_Table.S4.psvelo (from https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2019JB018774)
  + tables/gcmt.psmeca (from https://www.globalcmt.org/)


We might have made a few modifications to them. Use them AT YOUR OWN RISK.
                                                         ^^ ^^^^ ^^^ ^^^^

---------------------*INSTALLATION*------------------------
Obtain the compressed iGPS release from github, and extract
  it to a locaiton, e.g. /usr/local/iGPS

Set the user environment varialbe iGPS to this location, e.g.,
  setenv iGPS /usr/local/iGPS   (for tcsh)
  export iGPS=/usr/local/iGPS   (for bash)

There are two methods to start iGPS GUI (GPS position time series analysis):

1.Include the iGPS root path ($iGPS) and its sub-directories in
  IDL system variable !PATH. Type igps in IDL to start iGPS GUI.

2.Run the ~/iGPS/main/start_igps.pro script directly in IDL.

For more information, please refer to the iGPS Step-by-step Tutorial
  in ${iGPS_ROOT}/doc directory:
  iGPS_tutorial.pdf

InSAR functions are called in the terminal or batch scripts.


*Please note: 

  In the root directory of iGPS, you can run script
    ./check_installation
  to check whether the installation is OK.
  
  The run_me script under $iGPS\example\insar\proc_gmtsar\012-a-m7-0088_0093_0098_0103_0108_0113_0118-tibetan_plateau
    shows an example of the processing flowchart of an ordinary 
    InSAR time series for studying interseismic fault slip.

  If iGPS-ftk executables do not work in your system, try compiling
    the source in $iGPS/ftk/ :
    ./install_igpsftk
  In FreeBSD, use gmake instead of make.
  For GCC > v10, add the following to compiler option:
    -fcommon -fallow-argument-mismatch
    
  If iGPS-ftk executables work but links to bin/ are corruptted, relink them by
    ./install_igpsftk_links


------------------------*CONTENT*--------------------------
iGPS--IDL tool package for GPS

  +cme      GPS common-mode component (CMC) analysis
  +data     read/write/outliers/modeling/offset/psdecay/...
  +doc      Userguide and tutorial
  +example  Various examples
  +ftk      Fortran programs
  +libtian  Common IDL routines
  +m        Matlab code (fault slip rate inversion, etc.)
  +main     iGPS GUI
  +sar      InSAR analysis routines
  +sh       Shell scripts
  +tables   Files used in iGPS (apriori coordinates, sites files, ...)
  +tools    Userful utilities


/_
  |__iGPS-2023feb20/        ;iGPS released on 20 February 2023
  |                         ;add support for InSAR processing with GMTSAR
  |                         ;
  |
  |__iGPS-2016nov25/        ;iGPS released on 25 November 2016
  |                         ; *)add ftk (Fortran ToolKit)
  |
  |__iGPS-2016jan22/        ;iGPS released on 22 January 2016
  |
  |__iGPS-2016jan05/        ;iGPS released on 05 January 2016
  |
  |__iGPS-2015jun10/        ;iGPS released on 10 June 2015
  |
  |__iGPS-2015jun05/        ;iGPS released on 05 June 2015
  |
  |__iGPS-2015may21/        ;iGPS released on 21 May 2015
  |
  |__iGPS-2011apr20/        ;iGPS released on APR 20 2011
  |                         ; *)fixed a critical bug in time series modeling;
  |                         ; *)introduced new features
  |
  |__iGPS-2011apr18/        ;iGPS released on APR 18 2011
  |                         ; *)some new features; bugs fixed
  |
  |__iGPS.tar.gz            ;the first release (source, example, and document); obsolete
  |
  |__iGPS.src.tar.gz        ;only source files (*.pro); obsolete
  |
  |__iGPS.doc.tar.gz        ;only document; obsolete
  |
  |__iGPS.example.tar.gz    ;only example data files; obsolete


Cheers,
Yunfeng Tian (tianyf@gmail.com)

https://github.com/igps-ftk/iGPS
https://sourceforge.net/projects/igps/
http://gps.xinbaibaihuo.com (offline temporary)

--------------------------------------------------------------------------------


Directories
----------------------
${iGPS}/
        cmc/ *common-mode component(CMC) analysis
            cmc/ *CMC extraction
            corr/ *calculate time series correlation
            stacking/ *traditional regional stacking filtering
        data/
             clean/ *remove outliers
             map/ *create map
             model/ *perform time series modeling
             plot/ *plot time series
             read/ *read varied data formats
             site/ *routines dealing with site information
             stat/ *statistics
             utility/ *extra data manipulation tools
             write/ *write output files
        doc/ *mannual and tutorial
        example/ *processing samples
        ftk/ *Fortran tool kit for manipulating GNSS time series, etc.
        libtian/ *common routines
        main/ *iGPS GUI and event handler
        sh/ *various Unix-like shell script for GNSS and InSAR data processing
        tables/ *site coordiantes, etc.
        tools/ *auxiliary routines


Known Issues
----------------------
There are a few known issues with iGPS, however, as of now I have no time and/or
knowledge to resolve these. I will try to fix these issues when possible.
  + iGPS program hangs sometimes without any error messages. The causes are
    unknown. Therefore, please save your critical data (e.g. offset list)
    frequently, to avoid data loss.
  + All operations are supposed to run in the time axis of decimal year (dyr).
    The use of JD or Modified JD is not recommended. For example, avoid using
    "jd" or "mjd" as time axis when defining offset/psdecay events.
  + An issue is found on high DPI display.
    The time series plot areas will be much smaller than the window if the
      a scaling level other than 100% is set, e.g. 125% or 150%, which is the
      default for modern Windows OS on high-definition screens.
     A resize of the window will make the drawing areas to fit.


Major Update History
----------------------
2023NOV05
  + Changed velocity profile file format.
    Now GNSS/InSAR horizontal/vertical velocity profiles share the SAME format.
      *   01site   02pLon  03pLat 04pDist  05VNor 06VeNor  07VPar 08VePar   
          09VUp 10VeUp    11lon   12lat  13distFa  14VLOS 15VeLOS    
          16VE    17VN   18VEe   19VNe   20CEN   21CEU  22CNU
    Changes also made for affected programs.
  + Added more faults in the Qinghai-Tibetan plateau area to tables/ directory.
    GMT vector files for the Altyn Tagh, Dari, Ganzi, Gozha Co, Karakax, Kegang, 
      Maduo-Gande, Tianshen-Daban, Tikelike, Wudaoliang-Qumalai, Xiaoerkule, Zepu, etc.
  + Also included GNSS velocities in Tables S5 of Wang and Shen (2020, JGR-SE).
    The new default GNSS velocity file is 
      iGPS\tables\wang_shen_2019JB018774_Table.S4S5.psvelo
    Old file deleted (wang_shen_2019JB018774_Table.S4.psvelo).
  + Added a new shell script sh_s1_get_baseline to get baseline_table.dat file from
      aligned SLC files. Similar to the GMTSAR's script get_baseline_table.csh, but
      can also create time-baseline plot.
  
2023FEB20
  A major release which contains cumulative updates since the last version, e.g.,
  + Add supplementary tools for InSAR processing with GMTSAR software;
  + Revised the velocity profiling tool and added a GUI;
  + Discard a few obsolete routines;
  + Changed names of a few routines, etc.
  
2016NOV25
  + Add the Fortran ToolKit (ftk) to iGPS package. ftk is a collection of unix
    shell scripts and Fortran 77 programs which can help in running GAMIT/GLOBK
    , converting time series, filtering temporal-spatial CMC, etc.
  + Add routine to extract DEM elevation for a profile line across one fault.
    (iGPS\data\utility\dem_grid_extract_profile.pro)
    This function needs ENVI.
  + Add routine to create InSAR LOS profiles across one fault.
    (iGPS\data\utility\insar_vel_profiling.pro)
  + Add routine to create GPS velocity profiles across one fault.
    (iGPS\data\utility\gps_vel_profiling.pro)
    *Note: No GUI for these above utilities yet. Edit the source code as need.

2016NOV02
  + Change the longitude range of output KML file to -180 ~ 180.
    Otherwise, some sites may not appear in certain version of Google Earth.

2016OCT25
  + Remove NaN data values when reading.

2016JUN08
  + Add support for reading displacement time series from EOST Loading Service
      (http://loading.u-strasbg.fr/).

2016MAY12
  + Add subroutines to convert GPS velocities into Google Earth KML format,
      with arrow head and error ellipse (iGPS\data\plot\error_ellipse.pro
      and iGPS\data\plot\gps_vel_to_kml.pro).
      Input GPS velocity format:
*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
 BMCL_GPS   81.7100   28.6600   0.000   0.000   6.700   0.530   0.000   0.000  32.400   0.530  -0.0108
 ...
      The velocities (Ve and Vn) and their uncertainties (dVe and dVn) are used.

2016APR11
  + Add support for viewing SIO RAW XYZ files (e.g. ftp://garner.ucsd.edu/pub/
     timeseries/measures/rawXyzTimeSeries.Measures_Combination.20160326.tar.gz)

2016MAR20
  + Fix a bug of not removing postseismic decay when showing the residual.
     The old code cannot handle the "all" sitename in psdecay definition file.
  + Fix a bug of not showing modeled curve when "Preview Reslut" checkbox is
     set in "Outlier" panel.
  + Fix a bug when searching outliers when there are offsets in time series.
     The old code use the same offsets for all three components. Now fixed.

2016FEB06
  + Fix a bug when view the residual for other formats (e.g. PBO *.pos NEU).
     The old code will return null plots if switch site in the residual view.
  + Add support for GPS Lab time series format(http://gps.earth.sinica.edu.tw/).
     No formal errors in the time series files.

2016JAN22
  + Add support for reading *.dat files of 1st Crustal Deformation Monitoring
    Center (YICE), China Earthquake Administration (CEA).
  + Add a menu tool for correcting offsets by using averaging adjacent positions
    of the two sides of one jump.
  + Fix a bug when reading offset/psdecay definitions.
    If no component is given in the definition file and there are comments,
    e.g.
      ^ offset kmin 2010.00137000   ;--SZWIN_RIGHT=5 ,
    the old code cannot read it correctly.

2016JAN20
  + Fix a bug when writing filtered time series in "Extract CMC (CWSF)" tool.
    For any day:
      - CMC is written, if CMC is obtained for any of the three components.
      - The filtered data is written only if all three components are corrected
        (note: not the case in previous version).

2016JAN06
  + Add WRMN/NRMS statistics for rate estimation.
  + iGPS now outputs realistic sigmas for rate estimation (realistic sigma code
    ${iGPS}/data/model/realistic_sigma.pro is revised from GGMatlabs's
     counterpart version--CalcRealSigma.m).

2016JAN05
  + Improved the performance (speed of plot, time conversion, etc.).

2015DEC30
  + Improved the estimation of seismic displacements (ts_model.pro).
  + Add support to merge offset/psdecay/slope headers (in Model) for CMONOC
    time series processing.
  + Add support to add trend/annual/semiannual/offset/psdecay to the residual
    time series.
  + Add lsq_velo (similar to QOCA's format) line to residual file header.

2015OCT21
  + Revised libtian\vector\read_polygon_psxy.pro.
  + Use the decimal year (instead of year/doy) to derive MJD.

2015Jun10
  + Add example data for CMC extraction using PBO data.
  + A few bug fixes and improvements.

2015Jun05
  + Add example data for 25 April 2015 Mw7.8 Nepal earthquake.

2015MAY09
  + Add the menu and function of calculating inter-station correlations among
    cGPS position time series. Add cmc/corr/ts_correlation.pro.
  + Add the menu and function of extracting common-mode component (CMC) signals.
    Add cmc/cmc/*.pro files.
  + Improved the efficiency of calculating the intersect of two data sets
    (updated libtian/array/set_intersect.pro).
  + Enable the draw areas to fit automatically resized window.
  + Add utility for smoothing time series (data/utility/neu_smooth.pro).

2015APR10
  + Update support for new PBO time series format (from V1.0.2 to V1.1.0).

2014NOV24
  + Add the menu and function.

2014APR01
  + Add support for SIO NEU ATS file format. More information about ats format
    can be found at
    http://garner.ucsd.edu/pub/timeseries/measures/ats/ATS_TarFile_README.txt
    and http://qoca.jpl.nasa.gov/advclass/tsa_intro.html.

2013NOV03
  + Add support for ISCEA time series format.
    ISCEA: Institute of Seismology, China Earthquake Administration.

2012DEC13
  + Add support for GSI format
    (ftp://terras.gsi.go.jp/data/coordinates_F3/2010jb007567/pos/).

2012APR24
  + Add checkbox buttons for offset/psdecay restoration to import epochs for
    (all / just those in list) sites.
  + Bug fixed: when the first epoch is outlier, it remains in the time series.

2011NOV18
  + RDSIT: A bug found and fixed when there is only one site name in the iGPS
    site list file (*.sit). Without the fix, iGPS will return none when there
    is only one site in the file.

2011OCT07:
  + The residual ( radio button { o raw  *resid} ) will not show when all the
    offset list epochs are outside current time axis. Fixed by tianyf.

2011OCT02:
  + Fix a bug when the time span is fixed. iGPS now can remove points with
    larger sigma when the time axis is fixed.

2011MAY07
  + Fix some bugs in "Quick Site Map" utility.

2011APR20
  + Switch to QOCA network file (*.net) for apriori coordinates.
    The iGPS LLHXYZ file (*.llhxyz) is now obsolete.
  + Add Postscript output (*.ps) support for Quick Site Map tool.
  + More reasonable y-axis range when large position sigma (say, >100mm) exist.


-----------------------*LICENSE*---------------------------
License Agreement of iGPS

Some date conversion routines are rewritten from FORTRAN 77
  source files in GAMIT/GLOBK software.

Permissions had been obtained from MIT/Harvard/Scripps to
  use them in iGPS.

iGPS source files (not including the above ones; referred
  to iGPS in the following paragraphs) are copyrighted by
  Tian, Y.F.
  Luo, Y.
  Feng, W.P.
  Hu, Y.S.

Users that agree with all the following items are qualified to
  use/modify/distribute iGPS for free.
  * Use iGPS only for non-commercial purposes.
  * Use iGPS at your own risk.

It is recommended to acknowledge iGPS when you find it useful.

Tian Y F. 2011. iGPS: IDL tool package for GPS position time series analysis. GPS Solutions£¬15(3): 299-303.
https://link.springer.com/article/10.1007/s10291-011-0219-7
DOI https://doi.org/10.1007/s10291-011-0219-7