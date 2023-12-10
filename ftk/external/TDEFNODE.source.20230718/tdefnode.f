c*******************************************************
c TDEFNODE                                             *
c model block rotation, fault coupling and transients  *
c  based on original program DEFNODE                   *
c  this version uses time series and transients        *
c                                                      *
c Rob McCaffrey                                        *
c mccafr@gmail.com                                     *
c                                                      *
c Do not redistribute this program                     *
c                                                      *
c                                                      *
c*******************************************************
c HISTORY
c
c based on PC versions starting in 1995
c 02.06.11 modified to compile with f77 under Unix/Linux
c 02.07.05 original map options removed, now generates GMT files for maps
c 02.08.03 modified to compile with g77 under cygwin (Unix emulator under WIN)
c 02.08.11 converted profile plotting to use GMT
c 02.08.15 added GPS network rotation adjustment option (GI:)
c 02.09.16 changed block index to use a 4-character string instead of block number
c 02.10.10 added RM: option to remove specific GPS sites from solutions
c 02.11.01 added EQ: option to make nodes equivalent on different faults
c 02.11.05 added BP: option to specify poles for blocks
c 03.01.15 added FS:, FX:, GD: options
c 03.02.04 added CO: option for co-seismic modeling (03.09.02 removed, use 'cose' option in FLags)
c 03.02.28 fixed calculation of hanging wall for surface nodes
c 03.03.06 added OP: option (output specific block poles in GMT format)
c 03.04.16 added CF: option to connect faults (end-to-end)
c 03.06.05 changed slip vector block designations, added .dfndefaults file, 
c            added vertical axis rotation calcs.
c 03.08.01 added option for triangular dislocations (not working yet)
c 03.08.10 changed how RM: works, first entry is velocity file code, block, or ****
c 03.08.21 IN: and GF: options now use distances (kms) for patch sizes instead of number of patches
c           between nodes
c 03.09.02 RO: option to use vertical axis rotation rates as data
c          FL: flags option for controls (see setflags subroutine)
c 03.10.28 added SK: (skip) and CO: (continue) options to skip blocks of input in input file
c 03.11.26 added GPS NE covariance in calculation of chi**2, 
c          added HC: option for imposing hard constraints
c 03.12.15 added MF: option to merge faults at T-junction
c 03.12.31 changed GMT plotting output for faults (use GMT color palettes instead of built in colors)
c          added site names to profile output files (should now use grep '^G' instead of grep G, for example)
c          fixed profiles to fall on great circle path
c 04.01.07 added strain rate calculations as part of inversion (SI:, ST: options) 
c          modified BP: to include specification of strain rate tensor for block
c          added EM: to allow multiple models and command-line model selection
c 04.02.08 changed fault gmt output file; removed color bar (use GMT's psscale for color bar)
c          the > -Z line contains multiple attributes; use awk to select the one to plot
c          3=Vphi, 4=phi, 5=phi_error, 6=V, 7=V(strike-slip), 8=V(convergent)
c          eg awk '{ if ($1 ==">") print $1,$2,$6; else print $1,$2 }' EXPT_flt_atr.gmt|psxy -Ccolor.cpt ...
c 04.04.15 added smoothing SM: option (limits along strike variation in phi)
c          added 'rand' flag to add random errors to velocities in .vec file
c          added FF: to add and remove specific faults from model
c          modified EQ: option, listed order of nodes no longer matters
c 04.04.20 fixed bug in output of velocities in _hc.out and .nod files
c          added 'p' sub-option to EQ: (second node listed takes position of first node)
c 04.05.14 added option to use uniform distribution (min, max) for fault slip rates
c 04.09.23 removed the code for triangular dislocations; placed it in def_tri.include file
c          see notes in code for including it in compilation, but it does not work properly
c 04.10.05 added GPS global weight factor, GW: option
c 04.10.22 cleaned up some output, added 'myflag' to restrict output
c 04.11.01 changed GP: line to include the pole number for the GPS velocity adjustment
c 04.11.04 added end= to many read statements to accommodate other systems
c 04.12.01 changed many flags to use + and - notations
c 04.12.20 added GS: option for grid search controls (removed from SA: line)
c          fixed fault-normal and fault-parallel slip in _mid.vec file
c 05.03.24 removed any input in km, all input in lon,lat
c          added tdefcons.h file to organize constants
c 05.04.01 added line length change rates as data
c          changed how Green's functions are stored and read in, for more efficiency
c 05.04.06 added new parameterization options for inversion (see FT:, PV:, PN:, PX:)
c          fault_fit_type = 0 independent nodes, no down-dip constraint
c                         = 1 independent nodes, decreasing down-dip
c                         = 2 modified Wang exp() function for phi(z)
c                         = 3 boxcar phi(z) 
c                         = 4 Gaussian phi(z) 
c                         = 6 2D Gaussian phi(x,z) 
c
c 05.11.01 removed the WARNING file - all warnings now go to screen
c 05.11.09 POc: also reads in covariance for poles in Cartesian format
c 06.01.18 adding checks on array dimensions
c 06.08.27 added FO: command
c 06.09.01 Gaussian 2D source added
c 06.10.03 Green's functions for layered Earth (EDGRN/EDCMP) (not working yet)
c 07.07.30 Minor adjustments for new release
c 07.10.25 Minor adjustments for new release
c 07.10.29 Adding time series inversions with transents --- tdefnode
c 07.12.06 EF: added
c 07.12.26 added Mogi volcanic source as transient
c 10.12.02 added viscoelastic velocity fields
c 11.11.11 fixed FC: and RF: 
c 12.04.27 add secular velocities to InSAR
c 12.05.02 s_tensor array lines commented out with c*st*
c 14.01.12 DR: added time window
c 14.01.22 added 1D boxcar for transient spatial type, added spatial type to TLI: lines 
c 14.02.25 added spheroid volcanic source
c 14.09.22 added VE relaxation for Mogi source, changed ww and xw to d1 and d2
c 17.02.16 fixed moment constraint problem 
c 17.10.16 fixed BC:, .dsum
c 23.06.27 +rnd added random error to InSAR, minimum sigma for InSAR
c
c*************************************************************************************************
c  to do list
c
c make missing GFs for sites automatically
c write out all fault intersections, auto merge?
c solve for instrument offsets at specified times
c InSAR time series
c check all longitude corrections
c output InSAR in profiles
c ouput calculated velocities at GPS sites in profiles
c output event displacements along profiles
c more than 4 chars for model name
c if no blocks don't check if data in blocks
c enter inter-event velocities and fix
c fix source migration 
c layered structure
c VE time series GFs
c leveling time series
c
c*************************************************************************************************
c Notes:
c
c fflag(): 1 fault locking used            (FF: option)
c          2 3D slip on fault              (FA: option)
c          3 fault locking adjusted        (FI: option)
c          4 fault used to make blocks     (FB: option)
c          5 fault read in                 (FA: option)
c
c************************************************************************************************
c
c START PROGRAM

      program tdefnode

      implicit real*8 (a-h,o-z)

c LMB system() returns INTEGER status
      integer system, status

c* some constants
      include "tdefcons.h"

c tdefcom1 has array dimensions and commons for main programs
      include "tdefcom1.h"

c tdefcom2 has commons for inversion routines
      include "tdefcom2.h"

      character*1 a1, a3, neu(3), a3gps(30)
      character*2 cm, c2, cmgps(30)
      character*3 c3(10), cp, cs, c3a 
      character*4 pg_name(MAX_gps_files), c4tmp(50), block_rename,
     .            c4, c4a, c4b, blank4, bname, block_name, hw, fw
      character*10 fname, c10
      character*12 dat
      character*20 args(100)
      character*30 msg
      character*80 ptfile, c80
      character*80 gps_file, tilt_file, ss_file, bfile, flagline(50),
     .   mtfile, fnout
      character*180 c180
      character*256 gpsline(30)
    
      logical kend, yesno, noargs, kerr, yestop, fexist, block_check
      logical samepole, bothfixed, kstop, iflip, yes, no, fnilim, flock
      logical out_strain_pts, fault_tmp, pio_on_cmd, swapxy, a3in

      dimension iv(MAX_nodes), iv2(MAX_nodes), nrm(10)  
      dimension kfx_change(MAX_nodes,3), fx_change(MAX_nodes,2)
      dimension nfixd_bl_pole(MAX_poles), fixd_bl_pole(MAX_poles,3)
      dimension nfixd_pg_pole(MAX_gps_files),
     .          fixd_pg_pole(MAX_gps_files,3)
      dimension xcc(MAX_corner), ycc(MAX_corner), v(MAX_nodes)  
      dimension pg_pole(MAX_gps_files,3), gv(12)
      dimension xt1(3), yt1(3), zt1(3), xt2(3), yt2(3), zt2(3),
     .          xt3(4), yt3(4), frame_pole(3) 
      
      integer c2i


c* commons for profiles
      character*80 volcfile, quakefile
      common /vq1/ volcfile, quakefile(MAX_qfiles)
c      common /vq2/ num_quakefiles

c* commons for 'getcm'
      character*4, modelname
      logical kskip, modskip, modelin, moremods, noem
      common /cm1/ kskip, modskip, modelin, moremods, noem
      common /cm2/ modelname
      logical write_input
      common /cm3/ write_input
     
c* commons for removing sites
      integer rm_type, rm_reason
      character*180 rm_site
      common /rm1/  rm_site(MAX_rm)
      common /rm2/  nrm_site, nrm_circ, nrm_p, nrm_poly(3),
     .   rm_type(MAX_rm), rm_reason(MAX_gps)
      common /rm3/  rm_circ(20,3), rm_poly(3,20,2)
      
c* commons for selecting sites
      integer se_type
      character*180 se_site
      common /se1/  se_site(MAX_rm)
      common /se2/  nse_site, se_type(MAX_rm)

c* commons for data region; lon1 lat1 lon2 lat2 time1 time2
      common /dr1/ dr_pos(6)
      
c* commons for input error quit
      logical quit_on_error, input_error
      common /er/ quit_on_error, input_error

c* commons for CRUST2 rigidity
      character*80 CNkey, CNtype, CNelev
      common /crust2/ CNkey, CNtype, CNelev
      
c* commons for kml
      character*80 kmlhdr
      common /kml1/ kmlhdr

c* keep track of open file unit numbers      
      logical fileopen
      common /op/ fileopen(100)

c* fixed constants
      data neu /'N', 'E', 'U'/
      data zero,one,two,three,four,five,six,seven,eight 
     .  /0.0d0, 1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0, 7.0d0, 8.0d0/
      data thou / 1.0d3 /
      data izero, ione, itwo, ithree /0, 1, 2, 3 /
      data blank4 /'    '/

c* parameter limits defaults, overridden by PM: command
c* minimum parameter value depending on type
      data psmin  / -1.0d3, -1.0d3, -1.0d10,   0.0d0,  1.0d-1, 
     .               0.0d0,  0.1d0,   1.0d0,   1.0d0,  -1.0d5,
     
     .            -89.99d0, 1.0d-2,  1.0d-2,-360.0d0,   1.0d3,
     .              1.0d-5,  0.0d0,  -1.0d5,  -1.0d5,   0.1d0,
    
     .             -360.d0,-89.0d0,    0.d0,   0.1d0,   0.0d0,
     .               0.1d0,  1.0d3,   1.0d0,   0.1d0,-360.0d0,
     
     .            -360.0d0,  1.0d0,-360.0d0, -360.0d0,  0.1d0,
     .               0.1d0,  0.0d0,   1.0d0,   0.1d0,   0.0d0,
     
     .               0.0d0,  0.0d0,  -1.0d5,  -1.0d5,  -1.0d5,
     .              -1.0d5, -1.0d5,  -1.0d5,   0.0d0,   1.0d10 /

c* maximum parameter value depending on type
      data psmax /   1.0d3,  1.0d3,  1.0d10,   1.0d0,   9.99d0, 
     .              60.0d0, 80.0d0,  60.0d0,  30.0d0,    1.0d5,
     
     .             89.99d0,  1.0d3,   1.0d3, 360.0d0,    3.0d3,  
     .               1.0d3,  1.0d2,   1.0d2,   1.0d5,    1.0d5,
     
     .              360.d0, 89.0d0,  360.d0,   1.0d3,    1.0d5,
     .               1.0d3,  3.0d2,   1.0d2,   1.0d3,  360.0d0,
     
     .             360.0d0,  8.9d1, 360.0d0, 360.0d0,    1.0d0,
     .               1.0d5,  1.0d0,   1.0d5,   1.0d5,    1.0d0,
     
     .               1.0d2,  1.0d2,   1.0d5,   1.0d5,    1.0d0,
     .               1.0d3,  1.0d5,    1.d0,   1.0d0,    1.0d22 /
     
c* penalty factors for constraints
c* (1)moment, (2)node values, (3)depths, (4)downdip constraints, (5)smoothing, (6)hard constraints
      data penalty_factor2 /  5.0d2, 5.0d2, 5.0d1, 5.0d1, 1.0d1, 1.0d0 /
c* penalty factor depending on type
      data penalty_factor
     .          /    1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2, 
     .               1.0d2,  1.0d2,  1.0d2,  1.0d2,  1.0d2   /

c************************************************************************************
c* long name parameter types
      character ptype(50)*15, p2codes(50)*2
      common /ptype1/ ptype, p2codes

      data ptype / 
     . 'GPS field pole ', 'Block pole     ', 'Strain rate    ', 
     . 'Coupling ratio ', 'Gamma value    ', 'Min lock Z     ',
     . 'Max lock Z     ', '1D Gauss Mean Z', '1D Gauss sig Z ',
     . 'Relaxation amp ', 'Velocity Bias  ', '               ',
     . '               ', '               ', '               ',
     . '               ', '               ', '               ',
     . '               ', '               ',
     . 'Longitude      ', 'Latitude       ', 'Depth          ',
     . 'Fault Width    ', 'Amplitude      ', 'Fault length   ',
     . 'Origin time    ', 'Time constant  ', 'Migrate X rate ',
     . 'Migrate W rate ', 'Strike         ', 'Dip            ',
     . 'Rake/SlipAz    ', '2D Gauss Azim  ', 'Poly_radii     ',
     . 'Tau amp        ', '1D Gauss amp   ', '1D Gauss mean  ',
     . '1D Gauss sig   ', '1D Boxcar amp  ', '1D Boxcar Z1   ',
     . '1D Boxcar Z2   ', 'Spheroid A     ', 'Spheroid A/B   ',
     . 'Spheroid Az    ', '               ', '               ',
     . '               ', '               ', 'Moment         ' /

c-- the 2-letter parameter codes     
      data p2codes / 
     .  'vp', 'bp', 'sr', 'ph', 'wg', 'z1', 'z2', 'mi', 'ms', 'rx',
     .  'vb', '  ', '  ', '  ', '  ', '  ', '  ', '  ', '  ', '  ',
     .  'ln', 'lt', 'zh', 'd1', 'am', 'd2', 'to', 'tc', 'xr', 'wr',
     .  'st', 'dp', 'rk', 'az', 'rd', 'ta', 'ga', 'gm', 'gs', 'ba', 
     .  'b1', 'b2', 's1', 's2', 's3', '  ', '  ', '  ', '  ', 'mo' / 
     

c* default grid search controls
      data gs_controls / 0.0, 0.0, 0.0, 0.0, 0.0, 20.0 /

c* default SA controls
      data sa_controls / 0, 0, 0, 1.0d-10 /

c* default Data region
      data dr_pos / -180.0, 360.0, -90.0, 90.0,  1980.0, 0.0 /

c************************************************************************************
c** Paths to files
c*
      include "tdeffiles.h"
c*
c***********************************************************************************
c*** Start
      defnode_version = '2023.07.18'
      PRINT *, '-----------------------------------------------------'
      print *, '           TDEFNODE Version ', defnode_version
      print *, '          DO NOT MODIFY OR REDISTRIBUTE'
      print *, 'https://robmccaffrey.github.io/TDEFNODE/TDEFNODE.html'
      PRINT *, '-----------------------------------------------------'

c Greens functions format, increase when format changes
      gf_version = '10.15'

c* default directory names
      expname = 'temp'

c** read command line to get filename
      noargs = .false.
      call getarg(ione, infile) 

c** see if file exists
 111  call existfile( infile, fexist, 0)

      if ( .not. fexist ) then
       noargs = .true.
       print *, 'Enter file name'
       read *, infile
       goto 111
      endif

c* read in model name if present
      modelname = blank4
      modelin = .true.
      call getarg(itwo, modelname)
      if (noargs) then
       print *, 'Enter model name'
       read *, modelname
      endif
        
      if (modelname.eq.blank4) modelin = .false.

c* read in pio file if present
      pio_on_cmd = .false.
      call getarg(ithree, parmfile)
      call existfile( parmfile, pio_on_cmd, 0)

c interactive stuff
      interactive = .false.

c restart here after pause
 1111  if (interactive) then
          print *, 'Restarting ... '
          print *, 'Press c to continue or q to quit'
          read *, a1
          if (a1.eq.'q') call stop1
           print *, 'Enter model name'
           read *, modelname
       endif

      print *, 'Reading from file ', infile
      if (modelin) print *, ' Looking for model ', modelname

c***********************************************************************
c* set default flags
c***********************************************************************
      kend          = .false.
      readgflag     = .true.
      gps_cov       = .true.
      kcalculate    = .true.
      getcovariance = .true.
      node_decrease = .false.
      apply_penalty = .true.
      okada         = .true.
      check_v_equates = .false.
      check_name_equates = .false.
      get_gps_parm  = .true.
      get_blk_parm  = .true.
      get_flt_parm  = .true.
      get_str_parm  = .true.
      pioflag       = .true.
      chk_data      = .false.
      nofaults      = .false.
      write_input   = .false.
      no_wt_srs     = .false.
      no_wt_svs     = .false.
      invmt         = .false.
      do_trans      = .false.
      do_all_gf     = .true.
      verbose       = .false.
      w_syn         = .true.
      insar_offset  = .false.
      insar_slope   = .false.
      insar_tropo   = .false.
      quit_on_error = .true.
      input_error   = .false.
      sphstrain     = .true.
c      use_sigma_mag = .false.
      l1norm        = .false.
      adjust_hw     = .true.
      adjust_fw     = .false.
      all_fault_0   = .false.
      all_fault_1   = .false.
      all_fault_fix = .false.
      use_blocks    = .true.
      wf1km         = .false.
      wfsegs        = .false.
      add_rand      = .false.
      add_rand0     = .false.
      use_sr_az     = .true.
      insar_gmt     = .false.
      sort_ts       = .false.
      time_dependence = .true.
      use_node_delay  = .false.
      xe3 = .false.
      noem = .true.
      yes = .true.
      no = .false.
      check_boundary = .false.
      last_iter = .false.
      snap = 0.0d0
      num_f_rm = 0
      num_f_off = 0
      rand_seed = .true.

c** longitude range is 0 to 360
      long_pos = .true.
      

c***********************************************************************
c* these control Green's functions, can be changed with GD: option
c*
c* Green's functions default directory
      gfdir = 'gfs'
      
c* proximity (in km) of sites that need unique Green's functions
      gps_near = 1.0d-2
      
c* tolerance (in km) of new node position that triggers new Green's functions
      dx_node_new = 1.0d0

c* don't make GFs for any sites that are too far away from any node
c*   ie, distance > gps_too_far (in kms) from any fault node
      gps_too_far = 1500.0d0

c* default interpolation distances for faults (in km), 
c**  along strike (X) and downdip (W)
      GFx_interp = 10.0d0
      GFw_interp = 5.0d0
c***********************************************************************

c** default patch sizes for output of slip distribution to .atr files
c*  modified with IN: option
      X_interp = 10.0d0
      W_interp = 5.0d0
      
c ** default slip cutoff for output of patches to .atr files
      sd_umin = -1000.0d0  ! slip deficit rate
      tr_umin = -1000.0d0  ! transient slip amplitude

c* initialize data type flags
      do i=1,6
        data_flag(i) = .true.
      enddo

c* temp flag for re-doing GFs
      donew = .false.
      
c* number of parameter types      
      nparm_types = 50

c* max # of iterations
      nitmax = 30
      icontrol(1) = 2
      icontrol(2) = 1
      
c* default increment for synthetic time series (years)
      dtsyn0 = 0.05d0

c* Elastic constants      
      data xmu, xlambda, poisrat  / 4.0d10, 4.0d10, 0.25d0/

c clear some arrays
c      call clearchar (b_name, 4*MAX_block)
      b_name = ""
c      do ic=1,MAX_block
c       b_name(ic) = "    "
c       enddo
       
      call cleareal (tau_smooth, MAX_srce)
       n=15*MAX_gridpts
      call cleareal(u_grid, n)
       n = 9*MAX_gps_files
      call cleareal(gps_pole,n)
       n = 9*MAX_poles
      call cleareal(poles, n)
       n = 8*MAX_grids
      call cleareal(pgrid, n)
      call clearlog (sflag, MAX_srce)
      call clearlog (fflag, 5*MAX_f)
      call clearint (fault_fit_type, MAX_f)
       n = MAX_x * MAX_z * MAX_srce
      call cleareal(tphi, n)
      call clearlog(parmpio, 20)
      call clearlog(from_pio, MAX_f)

      MAX_nix = 0
      MAX_niy = 0

      z = zero

c************************************************************************
c* seed randomizer with hour and minute
c time_string is YYYYMMDDHHSS

      call dater(time_string)
      read(time_string, '(8x,i4)') j
      idum = j

      if ( rand_seed ) then
        idum = 1
        j = idum
c        print *, 'Random seed IDUM = ', idum
      endif

      do k=1,j
c       call normal ( p, zero, one)     
        x=ran1(idum) 
      enddo

c get a random id for this run
      k = int(ran1(idum)*9997) + 1
      call i2c (k, 4, runid )
c      print *, 'Runid = ', runid

c get today as decimal year
      read( time_string, '(i4,2i2)' ) iy, imo, ida
      yr = real(iy)
c imode = 4 convert YYYY. MM DD to YYYY.YYYY     
      call julday(4, yr, imo, ida, jday) 
      today = yr
c      print *, ' Today ', time_string, ' _ ', today


c******************************************************************
      print *, 'Pass 1 ..... reading edits MV/AV/DV and flags FL'
      k10=kfopen(10)
      open (k10, file = infile)

c******************************************************************
c** read in moves, flags, GPS global_weight and fix_depths first
c******************************************************************
      nedits = 0
      numflags = 0
      num_fixd = 0
      n_b_renames = 0
      nummo = 0

      modskip = .false.
      moremods = modelin
      inline=0

      do jline = 1,90000

      read (k10, '(a256)', end=1991) aline
      inline=inline+1

c****
c subroutine getcm parses the input aline
c  cm holds the lower-case 2-letter index, 
c  a3, the third character, is a special flag for some input format options
c  nitems is the number of items on the line
c  aline2 is the rest of the line following the colon, and is read below
c****
c       print *, aline
      call getcm (cm, a3, nitems)
c           print *, aline, cm, a3, nitems
  

c*****************************************************
c*** MV - move vertices
c  MV: X1 Y1 X2 Y2  = move all (X1,Y1) to (X2,Y2)
c*****************************************************
      if (cm.eq.'mv' ) then 
       if (nitems.eq.4) then
        nedits=nedits+1
        read (aline2, *) (ptedit(nedits,i),i=1,4)
        ptedit(nedits,1) = fnlong (ptedit(nedits,1))
        ptedit(nedits,3) = fnlong (ptedit(nedits,3))
        edit_type(nedits) = 1
       else
        call badline(jline, 'MV')
       endif
 
c*****************************************************
c*** AV - add vertices
c  AV: X1 Y1 X2 Y2 X3 Y3  =  place (X3,Y3) between (X1,Y1) and (X2,Y2)
c    if X3=0 and Y3=0 place new point halfway
c*****************************************************
      elseif (cm.eq.'av' ) then 
       if (nitems.eq.6) then
        nedits=nedits+1
        read (aline2, *) (ptedit(nedits,i),i=1,6)
        edit_type(nedits) = 2
       else
        call badline(jline, 'AV')
       endif
 
c*****************************************************
c*** DV: - delete vertices
c  DV: X1 Y1 = delete all (X1,Y1)
c*****************************************************
      elseif (cm.eq.'dv' ) then 
       if (nitems.eq.2) then
        nedits=nedits+1
        read (aline2, *) (ptedit(nedits,i),i=1,2)
        edit_type(nedits) = 3
       else
        call badline(jline, 'DV')
       endif

c*****************************************************
c*** BR: - rename blocks
c  BR: BLK1  BLK2 BLK3 ... BLK2 and rest are renamed BLK1
c*****************************************************
      elseif (cm.eq.'br' ) then 
       if (nitems .ge. 2) then
        n_b_renames = n_b_renames +1
        read (aline2, *) (b_rename(n_b_renames,i),i=1,nitems)
       else
        call badline(jline, 'BR')
       endif

c*****************************************************
c*** FL: - set flags
c*****************************************************
      else if (cm.eq.'fl' ) then 
         if (numflags.eq.50) then
          print *, 'Number of FL: lines exceeded'
         else
          numflags=numflags+1
          read (aline2, '(a80)') flagline(numflags)
         endif

c*****************************************************
c*** SN: - snap polygon/fault points together, snap is tolerance in km
c*****************************************************
      else if (cm.eq.'sn' ) then
        read (aline2, *) snap
c        print *, 'Snap ', snap
      
c*****************************************************
c*** VF: - volcano file
c*****************************************************
      else if (cm.eq.'vf' ) then 
          read (aline2, '(a80)') volcfile

c** path of Smithsonian volcanoes file, needed if +vtw flag is set
c      volcfile = 
c     .   '/home/mccafr/work/fortran/tdefnode/td_source/votw.gmt'

c****************************************************************************************
c*** RI: file of points to remove from InSAR data
c****************************************************************************************
      else if (cm.eq.'ri' ) then 
        num_f_rm = num_f_rm + 1
        krm_filename_type(num_f_rm) = 2
        read (aline2, *) rm_filename(num_f_rm)
c        print *, rm_filename(num_f_rm)


c*****************************************************
c*** QF: - quake file
c*****************************************************
      else if (cm.eq.'qf' ) then 
          read (aline2, *) k, quakefile(k)


c** paths of files of earthquakes to be projected onto profiles, needed if +eqk flag set
c**  set num_quakefiles to number of files used, if more than MAX_qfiles reset dimension of
c**  MAX_qfiles in tdefcomm1.h
c** files should contain longitude, latitude, depth for each quake
c      num_quakefiles = 1
c      quakefile(1) = 
c     .    '/home/mccafr/work/fortran/tdefnode/td_source/ehb.gmt'

c     quakefile(2) = '/Users/mccafr/work/dn/pgc_quakes'
c     quakefile(3) = '/Users/mccafr/work/dn/uw_quakes'
c     quakefile(2) = '/Users/mccafr/work/dn/fiordland_2003.quakes'

c*****************************************************
c*** EN: - end of input
c*****************************************************
      else if (cm.eq.'en') then
        go to 1991

      endif

      enddo

 1991 continue

c* set the flags
      call setflags(numflags, flagline)

c* implement
      if ( gamma5 ) psmax(5) = 5.0d0
      
c************************************************************************
c* remove repeated moves (MV), use the latest one only for a given coordinate pair
c* ie, if the same point is to be moved more than once, use only the last move
c* (set longitude of earlier moves to 999 )
c************************************************************************
      if ( nedits.gt.0) then
        do i=1,nedits-1
         do j=i+1,nedits
          if (edit_type(i).eq.1 .and. edit_type(j).eq.1) then
           if ( ptedit(i,1).eq.ptedit(j,1) .and. 
     .          ptedit(i,2).eq.ptedit(j,2) ) 
     .        ptedit(i,1) = 999.0d0
          endif
         enddo
        enddo
      endif

c************************************************************************
c** read input to extract blocks and model name 
c************************************************************************
      rewind (k10)
      nblocks = 0
      jblock = 0
      modskip = .false.
      moremods = modelin
      block_check = .false.
      inline=0

       print *, 'Pass 2 ..... reading blocks BL and model name MO'
      do jline = 1,90000

      read (k10, '(a256)', end=199) aline
      inline=inline+1

      call getcm (cm, a3, nitems)

c*****************************************************
c*** BL -- read in blocks
c*****************************************************
      if (cm.eq.'bl') then
        bfile = '    '
        kbstr=0
        kbpole=0
        if (make_blocks) then
          print *, 'Making blocks, Not reading blocks'       
        else

        if(nitems.eq.1) read (aline2, *) bname
        if(nitems.eq.2) read (aline2, *) bname, kbpole
        if(nitems.eq.3) read (aline2, *) 
     .     bname, kbpole, kbstr
        if(nitems.eq.4) read (aline2, *) 
     .   bname, kbpole, kbstr, bfile
     
        j=jblock+1
        call readblock(jblock, bname, a3, kbpole, kbstr, bfile) 
        write (*, '("Read block ",i4,1x,a4," Pole:",i4,
     .   " # points:", i5, " Centroid: ", 2f8.3, " Area", f10.4,
     .    " x10^6 sq km")') 
     .   j, bname, kbpole, nc_block(j), 
     .   block_centroid(j,1), block_centroid(j,2), block_area(j)/1.0d6
        block_flag(j) = .true.
        endif

c*****************************************************
c*** MO: - model name
c*****************************************************
      else if (cm.eq.'mo' ) then 
c       call clearchar (c4tmp, 200)
        c4tmp = ""
c       do ic=1,50
c        c4tmp(ic) = "    "
c        enddo
       read (aline2, *) (c4tmp(i),i=1,nitems)
        if ( modelin .and. c4tmp(1).eq.modelname) expname = c4tmp(1)
        if ( .not. modelin ) expname = c4tmp(1)
        nummo = nummo + 1

c*****************************************************
c*** EN: - end of input
c*****************************************************
      else if (cm.eq.'en') then
        go to 199

      endif

      enddo

c*****************************************************
 199  if (nummo.gt.1 .and. noem ) then
       print *, 'Error: Multiple MO: lines and no EM: line'
       goto 8888
      endif

      if( .not. make_blocks) 
     .    print *, 'Number of blocks read =', nblocks

      if ( expname.eq.blank4) then
       print *, 'Error: Model ', modelname, ' not found '
       goto 8888
      endif

c************************************************************************
c** make directory for output files 
c************************************************************************
c LMB Add null termination
c LMB system() returns INTEGER status
      call existfile( './'//expname, fexist, 0)
      if ( .not. fexist ) 
     .    status = system('mkdir ./'//expname//char(0))
      
c******************************************************
c** copy input control file to model directory
c******************************************************
      call dater(time_string)
      status = system ('cp -f '//infile//' '//
     . fnout('_control.'//time_string//'.'//runid//' ')//char(0) )

c******************************************************
c** read fault inputs FA: 
c******************************************************
      rewind (k10)
      nfault=0
      modskip = .false.
      moremods = modelin
      block_check = .true.
      inline = 0

      call clearlog (fflag, 5*MAX_f)
      print *, 'Pass 3 ..... reading faults FA '

      do jline = 1,90000

      read (k10, '(a256)', end=299) aline
      inline=inline+1
      call getcm (cm, a3, nitems)

c*****************************************************
c*** FA: - input fault data
c*****************************************************
      if (cm.eq.'fa' ) then 

       kf = 0
       c80 = '   '
       if(nitems.eq.2) read (aline2, *) fname, kf
       if(nitems.eq.3) read (aline2, *) fname, kf, c80

       if( kf.gt.MAX_f) then
         print *, '** Too many faults ', kf, fname
         call stop1
       endif
         
       if (kf.gt.0) then
        write (*, '("Reading fault ",a10, " Number ", i4)')
     .       fname, kf 
        call readfault(fname, kf, a3, c80, kerr)
c        write (*, '("Reading fault ",a10, " Number ", 3i4)')
c     .       fname, kf, nxf(kf), nzf(kf) 
       endif

        if ( .not. kerr .and. kf.gt.0) then
          nfault=max(nfault,kf)
        else
          write (*, '("*** Error reading fault ")' )
        endif


c*****************************************************
      else if (cm.eq.'en') then
        go to 299
      endif

      enddo
 299  continue


c clear removed vec file
      call fopen (kl, 1, '_removed.vec ' )
      write (kl, '(a15)') '# removed sites'
      k = kfclose(kl)

c************************************************************************
c** read inputs BC: ES:, FB:, BP:, CL: and PI:
c************************************************************************
      rewind (k10)
      modskip = .false.
      moremods = modelin
      block_check = .true.
      inline=0
      num_pole_invert = 0
      nrm_p = 0
      nrm_circ = 0
      nrm_site = 0
      nse_site = 0

      print *, 'Pass 4 ..... reading BC CL DR ES FB PI PM RC SE'

      do jline = 1,90000

      read (k10, '(a256)', end=299) aline
      inline=inline+1
      call getcm (cm, a3, nitems)

c*****************************************************
c*** BC: - block centroid and pole/strain assignment
c*****************************************************
      if (cm.eq.'bc' ) then 
       if(nitems.gt.2) then
       kbpole=0
       kstr=0
       if(nitems.eq.3) read (aline2, *) bname, xc, yc 
       if(nitems.eq.4) read (aline2, *) bname, xc, yc, kbpole 
       if(nitems.eq.5) read (aline2, *) bname, xc, yc, kbpole, kstr

c       print *, 'BC bname ', bname
       
c* see if it is the same block as one already read in
       call getplate (bname, j, no )
       
       if (j.eq.0) then
          nblocks = nblocks + 1
          jb = nblocks
       else
          jb = j
       endif
        
        block_centroid(jb,1) = fnlong(xc)
        block_centroid(jb,2) = yc
        b_name(jb) = bname
        npole_block(jb)=kbpole
        nstrain_block(jb)=kstr
c        block_flag(jb) = .false.
        
        
c        print *, b_name(jb), ' ', block_name(jb)
       else
        call badline(jline, 'BC')
       endif
        
c*****************************************************
c*** CL: - clear pole assignments already read in with bps
c*****************************************************
      elseif (cm.eq.'cl' ) then 
c        call clearchar (c3,30)
         c3 = ""
c        do ic=1,10
c        c3(ic) = "   "
c        enddo
        
        read (aline2, *) (c3(i), i=1,nitems)
       do i=1,nitems
        if ( c3(i).eq.'nse' ) nse_site = 0
        if ( c3(i).eq.'nrm' ) nrm_site = 0
        if ( c3(i).eq.'bps' ) then
         do ii=1,MAX_block
          npole_block(ii) = 0 
          nstrain_block(ii) = 0
         enddo
        endif


       enddo

c*****************************************************
c*** DR: - specify region in which to keep data
c****  dr: min_lon max_lon min_lat max_lat min_time max_time
c*****************************************************
      else if (cm.eq.'dr' ) then 
       call cleareal(dr_pos,6)
       if(nitems.eq.4) then
        read (aline2, *) (dr_pos(i),i=1,4)
        dr_pos(1) = fnlong(dr_pos(1))
        dr_pos(2) = fnlong(dr_pos(2))
       elseif(nitems.eq.6) then
        read (aline2, *) (dr_pos(i),i=1,6)
        dr_pos(1) = fnlong(dr_pos(1))
        dr_pos(2) = fnlong(dr_pos(2))
       else
        call badline(jline, 'DR')
       endif

       if ( dr_pos(1).gt.dr_pos(2) .or. dr_pos(3).gt.dr_pos(4) )
     .  print *, '*** DR: items not in correct order'
 
c*****************************************************
c*** ES: -  read transient sources
c*****************************************************

      else if ( cm.eq.'es' .and. time_dependence ) then 
             
       read (aline2, *) ns
        if ( fnilim(ns,1,MAX_srce) ) then
        call cleareal(v, 20)
         call parse(aline2, args, nargs)
       
        if(nitems.gt.6) then
        
         read (aline2, *) nt

c         sflag(nt) = .true.
c         do_trans  = .true.
     
         nps = int((nitems - 1)/2)
          do k=1,nps
           i1 = 2 +(k-1)*2  
           i2 = 3 +(k-1)*2  
           
           c2 = args(i1)
           call lcase(c2,2)
           
           if ( c2.eq.'fa' ) then
             info_source(nt,1) = c2i(args(i2))
           elseif ( c2.eq.'sp' ) then
             info_source(nt,2) = c2i(args(i2))
           elseif ( c2.eq.'sa' ) then
             info_source(nt,3) = c2i(args(i2))
           elseif ( c2.eq.'ts' ) then
             info_source(nt,4) = c2i(args(i2))
           elseif ( c2.eq.'mt' ) then
             info_source(nt,8) = c2i(args(i2))
           else          
             call gettparm(1, c2, np, ip)
             v1 = c2r(args(i2))
              if ( fnilim(ip,1,14) ) then
               v(ip) = v1
              endif

c-- 1D Boxcar/Gauss parameters
              do ix=1,MAX_x
c 1D Gauss
               if (ip.eq.17) twin(ns, ix, 1 ) = v1
               if (ip.eq.18) twin(ns, ix, 2 ) = v1
               if (ip.eq.19) twin(ns, ix, 3 ) = v1 
c 1D Boxcar
               if (ip.eq.20) twin(ns, ix, 1 ) = v1
               if (ip.eq.21) twin(ns, ix, 2 ) = v1
               if (ip.eq.22) twin(ns, ix, 3 ) = v1 
              enddo

           endif
          enddo
        endif
        v(1) = fnlong(v(1))
         do j=1,20
         transient(ns,j) = v(j)
        enddo
       else
        call badline(jline, 'ES')
       endif

c*****************************************************
c*** FB: - set faults to use for blocks
c**  if +, turn ON flag 4
c**  if -, turn OFF flags 1,3 and 4
c*****************************************************
      else if (cm.eq.'fb' ) then
       call clearint(iv, MAX_f)
       read (aline2, *) (iv(i),i=1,nitems)

c-- set all flags if 999
      if (abs(iv(1)).eq.999) then
        do i=1,MAX_f
         kf=i
         if (iv(1).eq.999) then
          fflag(kf,4) = .true.
         elseif (iv(1).eq.-999 ) then
          fflag(kf,1) = .false.
          fflag(kf,3) = .false.
          fflag(kf,4) = .false.
         endif
        enddo
       else
c-- set individual flags
        do i=1,nitems
         kf=abs(iv(i))
         if (iv(i).gt.0) then
          fflag(kf,4) = .true.
         elseif (iv(i).lt.0 ) then
          fflag(kf,1) = .false.
          fflag(kf,3) = .false.
          fflag(kf,4) = .false.
         endif
        enddo
       endif

c*****************************************************
c*** PI: - block poles to be adjusted
c*****************************************************
c      else if (cm.eq.'pi' ) then 
c        call clearint(iv, MAX_poles)
c        read (aline2, *) (iv(i),i=1,nitems)
c        num_pole_invert = 0
c        call clearint(npole_invert, MAX_poles)
c        do kk=1, nitems 
c          if ( iv(kk).gt.0 ) then 
c            num_pole_invert = num_pole_invert + 1 
c            npole_invert(num_pole_invert)=iv(kk)
c          endif
c        enddo

      else if (cm.eq.'pi' ) then 
       call clearint( iv, nitems)
       read (aline2, *) (iv(i),i=1,nitems)
       if (a3.ne.'c') call clearlog(pi_flag, MAX_poles)

        do kk=1, nitems 
         if ( abs(iv(kk)) .le. MAX_poles) then
          if (iv(kk).gt.0 ) pi_flag(iv(kk)) = .true.
          if (iv(kk).lt.0 ) pi_flag(abs(iv(kk))) = .false.
         endif
        enddo

        num_pole_invert = 0
        call clearint(npole_invert, MAX_poles)
         do kk=1, MAX_poles 
          if ( pi_flag(kk) ) then 
           num_pole_invert = num_pole_invert + 1 
           npole_invert(num_pole_invert)=kk
          endif
         enddo

c*****************************************************
c*** PM: - parameter min, max, change
c    use pm: i min max factor  where i is parameter number
c     or pmt: cc min max factor where cc is 2-letter parameter name
c*****************************************************
      else if (cm.eq.'pm') then
        pfac = 0.0d0

       if(nitems.eq.3 .or. nitems.eq.4) then
      
       if (a3.eq.'t' ) then 
        call parse(aline2, args, nargs)
        c2 = args(1)
        call gettparm(1, c2, np, ip)
        if ( fnilim(np,1,nparm_types) ) then
         psmin(np) = c2r(args(2))
         psmax(np) = c2r(args(3))
         if (nitems.eq.4) penalty_factor(np) = c2r(args(3))
c         print *, 'PMt ', c2, np, psmin(np), psmax(np)
        endif

       else
        if(nitems.eq.3) read (aline2, * ) np, p1, p2
        if(nitems.eq.4) read (aline2, * ) np, p1, p2, pfac
         if ( fnilim(np,1,nparm_types) ) then
          psmin(np) = p1
          psmax(np) = p2
          if(pfac.ne.0.0d0) penalty_factor(np) = pfac
         endif

       endif

      else
        call badline(jline, 'PM')
      endif
        
c*****************************************************
c*** RC: - remove GPS sites within a circular region
c****  rc: lon lat radius(km)
c*****************************************************
      else if (cm.eq.'rc' ) then 
       if (nitems.ge.3) then
        nrm_circ=nrm_circ + 1
        read (aline2, *) (rm_circ(nrm_circ,i),i=1,3)
         rm_circ(nrm_circ,1) = fnlong(rm_circ(nrm_circ,1))
        if(verbose) then
         print *, 'Remove circular region '
         write (*, '(i4,3f10.3)' ) nrm_circ,(rm_circ(nrm_circ,i),i=1,3)
        endif
       else
        call badline(jline, 'RC')
      endif
 

c*****************************************************
c*** RP: - remove GPS sites within a polygon region
c****  rp: N lon lat (1 to N)
c*****************************************************
      else if (cm.eq.'rp' ) then 
       if (nitems.ge.7) then
        nrm_p = nrm_p + 1
        read (aline2, *) n,(rm_poly(nrm_p,i,1),rm_poly(nrm_p,i,2),i=1,n)
        nrm_poly(nrm_p) = n
         do i=1,n
          rm_poly(nrm_p,i,1) = fnlong(rm_poly(nrm_p,i,1))
         enddo
         print *, 'Remove poly '
         write (*, '(2i4)' ) nrm_p, nrm_poly(nrm_p)

       else
        call badline(jline, 'RP')
      endif
 
c*****************************************************
c*** RM: - remove specific GPS sites listed from a file
c*****************************************************
      else if (cm.eq.'rm' ) then 
        nrm_site=nrm_site+1
        if(nrm_site.gt.MAX_rm) then
         print *, '** Number of RM lines exceeded ', MAX_rm
         call stop1
        endif
          rm_site(nrm_site) = aline2(1:175)
        if (a3.eq.'b') then
          rm_type(nrm_site) = 1
        elseif (a3.eq.'8') then
          rm_type(nrm_site) = 2
        else
          rm_type(nrm_site) = 0
        endif

c*****************************************************
c*** SE: - selsect only specific GPS sites listed 
c** take only these ones from a file
c*****************************************************
      else if (cm.eq.'se' ) then 
        nse_site=nse_site+1
          se_site(nse_site) = aline2(1:175)
        if (a3.eq.'b') then
          se_type(nse_site) = 1
        elseif (a3.eq.'8') then
          se_type(nse_site) = 2
        else
          se_type(nse_site) = 0
        endif
              
c*****************************************************
      else if (cm.eq.'en') then
        go to 2991
      endif

      enddo

 2991 continue

c****************************************************
c****************************************************
c**  make bocks from fault segments
c****************************************************
c****************************************************
c****************************************************
c*** write fault file
c****************************************************
      if ( make_blocks ) then

c-- first write faults that were read in
      fault_tmp = .false.
      if (verbose .and. fault_tmp ) then

      call fopen (kkk, 1, '_fault.tmp ' )
      ddx = 0.0d0
      do kf=1, nfault
       if (fflag(kf,4)) then
        write (kkk, '(a1, i5, 1x, a10)') '>', kf, fault_name(kf)
        do ix=1, nxf(kf)
          call getnodexy(xpt, ypt, kf, ix, 1, ddx)
          write(kkk,'(2f9.4, i6)' ) xpt, ypt, kf
        enddo   
       endif
      enddo
      ik = kfclose(kkk)

      call fopen (kkk, 1, '_block.tmp ' )
       do jb = 1,nblocks
c        if (block_flag(jb) ) then
         write (kkk, '(a4,2f9.3,2i3)') block_name(jb), 
     .   block_centroid(jb,1), block_centroid(jb,2),  
     .    npole_block(jb), nstrain_block(jb) 
c        endif
       enddo
      ik = kfclose(kkk)

      endif

c--- make the blocks
      call wrf_polygon (kstop)
      if (kstop) then
        print *, 'Error in fault configuration ****** '
        ik = kfclose(k10)
        goto 9999
      endif

      endif

      print *, 'Pass 5 ..... reading block pole/strain assignments BP'
      rewind (k10)
      modskip = .false.
      moremods = modelin
      block_check = .true.
      inline=0
      do jline = 1,90000

      read (k10, '(a256)', end=99) aline

      call getcm (cm, a3, nitems)

c*****************************************************
c*** BP: - assign block poles and strain rates, 
c**       overrides assignment in BL or BC input lines
c*****************************************************
      if (cm.eq.'bp' ) then
        call clearint(iv, 2)
       if(nitems.ge.2) then
        if (nitems.eq.2) read (aline2, * ) bname, iv(1)
        if (nitems.eq.3) read (aline2, * ) bname, iv(1), iv(2)
        call getplate (bname, nb, yes)
        if (nb.gt.0) then
         npole_block(nb)   = iv(1)
         nstrain_block(nb) = iv(2)
        endif
       else
        call badline(jline, 'BP')
       endif

c*****************************************************
c  IF - InSAR file flag
c  IF: N N N N -N    negative number to turn this file off
c  IF: -999          turn all files off
      else if (cm.eq.'if') then 
        call clearint(iv, 20)
        read (aline2, *) (iv(i),i=1,nitems)

       do i=1,nitems
        k = iv(i) 
        ik = abs(k)
c        print *, k 
        if (k.lt.0 .and. ik.gt.0 .and. ik.le.MAX_insar_files ) 
     .        insar_off(ik) = .true.
        if (k.gt.0 .and. ik.gt.0 .and. ik.le.MAX_insar_files ) 
     .        insar_off(ik) = .false.

        if(k.eq.-999) then
          do ik=1,MAX_insar_files 
            insar_off(ik) = .true.
          enddo
        endif

       enddo

c*****************************************************
      else if (cm.eq.'en') then
        go to 51
      endif

      enddo

 51   continue


c****************************************************
c** read through input file to get everything else
c****************************************************
       
c* clear variables
      num_sv = 0 
      num_sr = 0 
      num_gps = 0 
      num_gps_file = 0
      num_disp = 0
      num_ts = 0
      num_ss = 0
      num_ll = 0
      num_tilts = 0
      num_rot = 0
      num_invert_corn = 0
      num_poles=0
      num_gps_poles=0
      num_strain=0
      bum_strain_invert = 0
      nlines = 0
      num_fs=0
      num_fx_change = 0
      num_svfile = 1
      num_sr_file = 1
      num_pg = 0
      num_fixd_bl_pole = 0
      num_fixd_pg_pole = 0
      num_hc = 0
      nfmerges = 0
      nlayers = 0
      ndef_pts = 0
      num_insar = 0
      num_insar_file = 0
      num_fc = 0
      npars_eq = 0
      nodes_tr = 0
      num_tfilt = 0
      num_segs = 0

      call clearlog (gps_invert, MAX_gps_files)
c      call clearchar (c4tmp, 200)
      c4tmp = ""
c      do ic=1,50
c       c4tmp(ic)="    "
c       enddo

      print *, 'Pass 6 ..... reading remaining input'
      rewind (k10)
      modskip = .false.
      moremods = modelin
      inline=0

      do 5 jline = 1,90000

      read (k10, '(a256)', end=99) aline
      inline=inline+1

      call getcm (cm, a3, nitems)

c* clear the dummy input arrays
      n=MAX_nodes
      call clearint( iv, n)
      call clearint( iv2, n)
      call cleareal( v, n)
c      call clearchar (c3, 30)
      c3 = ""
c      do ic=1,10
c      c3(ic) = "   "
c      enddo
      call cleareal(gv, 12)

c** read and decipher input lines
      
c*****************************************************
c*** CF: - connect faults
c*****************************************************
      if (cm.eq.'cf' ) then 
       if(nitems.eq.2) then
        nfcncts=nfcncts+1
        read (aline2, *) (nfcnct(nfcncts,i),i=1,2)
       else
        call badline(jline, 'CF')
       endif

c*****************************************************
c*** TB: - break in time series
c*** tb: site network component T1 dT1 T2 dT2 ...
c*****************************************************
      elseif (cm.eq.'tb' .and. time_dependence ) then 
        call cleareal(v, 20)
       if(nitems.ge.5) then
        num_segs = num_segs + 1
        ns=num_segs
        n=0
        read (aline2, *) site_segs(ns,1), site_segs(ns,2), 
     .       i_segs(ns,1), (v(i),i=1,nitems-3)
         do k=1, MAX_tsegs
            i = 2*k-1
            if (v(i).gt.0.0d0 .and. v(i+1).gt.0.0d0) then
              n= n+1
              t_segs(ns,n,1) = v(i)
              t_segs(ns,n,2) = v(i+1)
             endif
          enddo
              i_segs(ns,2) = n

       else
        call badline(jline, 'TB')
       endif


c*****************************************************
c*** CL: - clear (remove) data or constraints already read in
c*****************************************************
      elseif (cm.eq.'cl' ) then 
c        call clearchar (c3,30)
      c3 = ""
c      do ic=1,10
c      c3(ic) = "   "
c      enddo
        read (aline2, *) (c3(i), i=1,nitems)

       do i=1,nitems

c* clear GPS data
        if ( c3(i).eq.'gps' ) then
          num_gps = 0 
          num_gps_file = 0
          num_ts = 0
          num_disp = 0 
           do j=1,MAX_gps
            ndx(j,1) = 0
            ndx(j,2) = 0
            gps_type(j)  = 0
           enddo
          
c* clear INSAR data
          elseif ( c3(i).eq.'ins' ) then
          num_insar = 0
          call clearlog(insar_flag, MAX_insar_files)

c* clear strain tensor centroids
        elseif ( c3(i).eq.'stc' ) then
         do j=1,MAX_strain
          do n=1,2
           strain2(j,n) = 0.0d0
          enddo
         enddo

c* clear transient free parameters
        elseif ( c3(i).eq.'efs' ) then
         do j=1,MAX_srce
          do n=1,30
           ntransient(j,n) = 0
          enddo
         enddo

c* clear slip rate data
        elseif ( c3(i).eq.'srs' ) then
          num_sr = 0
          no_srs = .true.
c* clear slip vector data
        elseif ( c3(i).eq.'svs' ) then
          num_sv = 0
          no_svs = .true.
c* clear removed sites
c        elseif ( c3(i).eq.'nrm' ) then
c          nrm_site = 0
c* clear selected sites
c        elseif ( c3(i).eq.'nse' ) then
c          nse_site = 0
c* clear surface strain rate data
        elseif ( c3(i).eq.'sss' ) then
          num_ss = 0
c* clear tilt data
        elseif ( c3(i).eq.'tlt' ) then
          num_tilts = 0
          no_tilts = .true.
c* clear rotation data
        elseif ( c3(i).eq.'rot' ) then
          num_rot = 0
          no_rot = .true.
c* clear hard constraints
        elseif ( c3(i).eq.'hcs' ) then
          num_hc = 0
          no_hcs = .true.
c* clear equated parameters
        elseif ( c3(i).eq.'eqs' ) then
          npars_eq = 0

        endif

       enddo

c*****************************************************
c*** DW: -  deformation during time window 
c***    window number, time1, time2 (time2>time1) in years
c***    optional list events to be included, default is all
c*****************************************************
      else if (cm.eq.'dw' .and. time_dependence ) then 
       call clearint( iv, n)

       if(nitems.eq.1) then
         read (aline2, *) k 
          if ( k.eq.0) then
           do i=1,MAX_srce
            do k=1, MAX_t_slice
             tsliceflag(k,i) = .false.
            enddo
           enddo
          endif
       elseif(nitems.eq.4) then
        read (aline2, *) k, kfslice(k), tslice(k,1), tslice(k,2)
         do i=1,MAX_srce
           tsliceflag(k,i) = .true.
         enddo
       elseif(nitems.gt.4) then
        read (aline2, *) k, kfslice(k), tslice(k,1), tslice(k,2),
     .   (iv(i), i=1, nitems-4)
          do i=1,nitems-4
           tsliceflag(k,iv(i)) = .true.
          enddo
       else
        call badline(jline, 'DW')
       endif

c*****************************************************
c*** EC: -  elastic constants at source; mu, lambda, Poisson Ratio 
c*****************************************************
      else if (cm.eq.'ec' ) then 
       call cleareal(v, 3)
       if(nitems.eq.1) read (aline2, *) v(1)
       if(nitems.eq.2) read (aline2, *) (v(j), j=1,2)
       if(nitems.eq.3) read (aline2, *) (v(j), j=1,3)
       if (v(1).gt.zero) xmu =     v(1)
       if (v(2).gt.zero) xlambda = v(2)
       if (v(3).gt.zero) poisrat = v(3)
       print *, 'Shear_modulus  Lambda  Poisson_ratio', 
     .      xmu, xlambda, poisrat

c*****************************************************
c*** EF: -  control for transient source parameters
c* ntransient() has flags for transients and parameter types
c*****************************************************
      else if (cm.eq.'ef' .and. time_dependence ) then

         read (aline2, *) ns
         call parse(aline2, args, nargs)
         nps = nargs-1
         do k=1,nps
          c2 = args(k+1)

c clear all events
            if(c2.eq.'cl' .and. ns.eq.999 ) then
              do n = 1, MAX_srce
                 srce_inv(n) = .false.
                do j=1,30
                  ntransient(n,j) = 0
                  iv(j) = 0
               enddo
              enddo
            endif

c clear just this event
          if ( c2.eq.'cl' .and. ns.ge.1 .and. ns.le.MAX_srce ) then
            srce_inv(ns) = .false.
           do j=1, 30
            ntransient(ns,j) = 0
            iv(j) = 0
           enddo
          endif

c set parameters to invert
         if ( c2.ne.'cl') then
           call gettparm(1, c2, np, ip)
           if (fnilim(ip,1,30)) iv(ip) = 1
         endif

         enddo

c reset flags     
        if ( fnilim(ns, 1, MAX_srce) ) then
          do j=1,30
           ntransient(ns,j) = 0
          enddo
        endif
c set all events 
        if(ns.eq.999 ) then
         do n = 1, MAX_srce
          srce_inv(n) = .false.
          do j=1,30
           ntransient(n,j) = iv(j)
           if(ntransient(n,j).gt.0) srce_inv(n) = .true.
          enddo
         enddo
       else
c set single event
          srce_inv(ns) = .false.
        do j=1,30
         ntransient(ns,j) = iv(j)
         if(ntransient(ns,j).gt.0) srce_inv(ns) = .true.
        enddo
       endif


c*****************************************************
c*** EI: -  flags for source parameters
c-- list events to be used
c*****************************************************
      else if (cm.eq.'ei' .and. time_dependence ) then 
      
        do j=1,MAX_srce
          sflag(j) = .false.
          isrce_order(j) = 0
        enddo

        call clearint(iv, nitems)
        
       read (aline2, *) (iv(j), j=1,nitems)

       call clearlog(sflag, MAX_srce)
       do_trans=.false.
     
        do j=1,nitems
          ii = iv(j)
          isrce_order(j) = ii
          if(fnilim(ii, 1, MAX_srce)) then
             sflag(ii) = .true.
             do_trans = .true.
          endif
        enddo

 
c*****************************************************
c*** EN: - end input
c*****************************************************
      else if (cm.eq.'en' ) then 
        print *, 'End of input file found at line ', inline
        goto 99

c*****************************************************
c*** EQ: - equate nodes
c*** EQ - equate phi's; EQp - same positions; EQt - equate transient slip
c*****************************************************
      else if (cm.eq.'eq' ) then 

        if (nitems.eq.3  .or. nitems.eq.6 ) then

c equate slip in transient
        if (a3.eq.'t' .and. nitems.eq.6 ) then
         npars_eq = npars_eq + 1
         read (aline2, *) (npar_eq(npars_eq,i),i=1,6)
         npar_eq_type(npars_eq) = 2

c equate other transient parameter
        elseif (a3.eq.'t' .and. nitems.eq.3 ) then
         npars_eq = npars_eq + 1
         read (aline2, *) (npar_eq(npars_eq,i),i=1,3)
         npar_eq_type(npars_eq) = 3

c equate node position
        elseif (a3.eq.'p') then
         npars_eq = npars_eq + 1
         read (aline2, *) (npar_eq(npars_eq,i),i=1,6)
         npar_eq_type(npars_eq) = 1

c equate node value
        else
         npars_eq = npars_eq + 1
         read (aline2, *) (npar_eq(npars_eq,i),i=1,6)
         npar_eq_type(npars_eq) = 0

        endif

       else
        call badline(jline, 'EQ')
       endif

c*****************************************************
c*** ER: -  set up for radial transient source
c*****************************************************
      else if (cm.eq.'er' .and. time_dependence ) then 
       read (aline2, *) nt, npoly, damp,
     .  (rpoly(nt,j), j=1,nitems-3)
     
       info_source(nt,6) = npoly
       damp_poly(nt) = damp
       
       
       
c*****************************************************
c*** ET: -  set up tau's for transient source
c*****************************************************
c          
c    srce Ntau  dtau  smooth atau1  atau2 ...
c et:  1    6   30.0    0.0  1000.0 1000.0

      else if (cm.eq.'et' .and. time_dependence ) then 
       if(nitems .ge. 5) then
        read (aline2, *) nt, ntau, dtau(nt),
     .   tsm, (atau(nt, k), k=1,nitems-4)
         tau_smooth(nt) = tsm
c         print *, aline2
c         print *, 'ET ', nt, ntau, dtau(nt),
c     .   tau_smooth(nt)

        info_source(nt,7) = ntau

        dtau(nt) = max(0.1d0 , dtau(nt))

       else
        call badline(jline, 'ET')
       endif

c*****************************************************
c*** EX: -  min/max for transient source parameters 1-30;
c          parm 30 is moment
c*****************************************************
c   constrain parameters for the sources, using parameter codes
c    source_#  parm  Min  Max   parm Min  Max    ..... 
c exc:  1        ln  67.0 68.0    lt   40   44    .....

      else if ( cm.eq.'ex' .and. time_dependence ) then 

       read (aline2, *) ns

        if ( fnilim(ns,1,MAX_srce) .or. ns.eq.99) then
         call parse(aline2, args, nargs)

c parameter and deviation
        if (a3.eq.'d') then

         nps = int(nitems/2)
         do k=1,nps
          c2 = args(2*k)
          call gettparm(1, c2, np, ip)
           dp = c2r(args(2*k+1))
           if ( fnilim(ip,1,20) ) then
            if (ns.eq.999) then
             do n=1,MAX_srce 
              tminmax(n,ip,1) = transient(n,ip) - dp
              tminmax(n,ip,2) = transient(n,ip) + dp
              mmparm(n,ip) = .true.
             enddo
            else
              tminmax(ns,ip,1) = transient(ns,ip) - dp
              tminmax(ns,ip,2) = transient(ns,ip) + dp
              mmparm(ns,ip) = .true.
            endif
           endif
         enddo

        else

c parameter and limits
         nps = int((nitems - 1)/3)
         do k=1,nps
          c2 = args(2+(k-1)*3)
          call gettparm(1, c2, np, ip)
           v1 = c2r(args(3+(k-1)*3))
           v2 = c2r(args(4+(k-1)*3))
           if ( fnilim(ip,1,30) ) then
            if (ns.eq.999) then
             do n=1,MAX_srce 
              tminmax(n,ip,1) = v1
              tminmax(n,ip,2) = v2
              mmparm(n,ip) = .true.
             enddo
            else
              tminmax(ns,ip,1) = v1
              tminmax(ns,ip,2) = v2
c               print *, ns,ip,v1,v2
              mmparm(ns,ip) = .true.
            endif
           endif
         enddo

          do n=1,MAX_srce 
           tminmax(n,1,1) = fnlong(tminmax(n,1,1))
           tminmax(n,1,2) = fnlong(tminmax(n,1,2))
          enddo

         endif

        endif

     
c*****************************************************
c*** FC: - set faults to have same phi
c*****************************************************
      else if (cm.eq.'fc' ) then
         print *, 'Reading FC: '
        call clearint(iv, MAX_fc)
       read (aline2, * ) (iv(i),i=1,nitems)
       num_fc = num_fc + 1
       if (num_fc.gt.MAX_fc) then
         print *, 'MAX_fc exceeded '
        call stop1
       endif
       do i= 1, MAX_fc
         kfc(num_fc,i) = iv(i)
       enddo

c*****************************************************
c*** FF: - reset fault flags, turn fault locking on and off
c**  if +, turn ON flag 1
c**  if -, turn OFF flags 1 and 3 
c*****************************************************
      else if (cm.eq.'ff' ) then
        call clearint(iv, MAX_f)
       read (aline2, * ) (iv(i),i=1,nitems)

c-- set all flags if 999
       if (abs(iv(1)).eq.999) then
        do i=1,MAX_f
         if (iv(1).eq.999) then
           fflag(i,1) = .true.
         elseif (iv(1).eq.-999 ) then
           fflag(i,1) = .false.
           fflag(i,3) = .false.
         endif
        enddo
       else
c-- set individual flags
        do i=1,nitems
         kf=abs(iv(i))
         if(kf.gt.0 .and. kf .le. MAX_f) then
          if (iv(i).gt.0) then
           fflag(kf,1) = .true.
          elseif (iv(i).lt.0 ) then
           fflag(kf,1) = .false.
           fflag(kf,3) = .false.
          endif
         endif
        enddo
       endif

c*****************************************************
c*** FS: - points to calculate fault slip
c*****************************************************
      else if (cm.eq.'fs' ) then
       call readfs(a3)

c*****************************************************
c*** FT: - fault fitting function for interseismic
c*** 2010.11.15 version - do PX: function here
c*** 2014.02.10 version - add skewed Gaussian type 5 with skew parameter
c*****************************************************
      else if (cm.eq.'ft' ) then
       call cleareal( v, 20)
       read (aline2, *) (v(i),i=1,nitems)
        nf = int(v(1))
        n2 = int(v(2))

c set individual fault
       if( nf.gt.0 .and. nf .le. MAX_f .and. n2 .le. 6) then
         fault_fit_type(nf)=n2

c 1D Gaussian skew, multiplied by updip spread
        if (n2.eq.5) Gskew(nf) = v(6)

        do i=1,3
         n=int(v(i+2))
         fprm_fixed(nf,i) = ( n.eq.0) 
        enddo

c-- set all faults if 999
       elseif ( nf.eq.999 .and. n2 .le. 6 ) then

        do j=1,MAX_f
         fault_fit_type(j)=n2
c 1D Gaussian skew, multiplied by updip spread
         if (n2.eq.5) Gskew(j) = v(6)
         do i=1,3
          n=int(v(i+2))
          fprm_fixed(j,i) = ( n.eq.0) 
         enddo
        enddo

       else
        call badline(jline, 'FT')
       endif

c*****************************************************
c*** FX: - change individual node position
c*****************************************************
      else if (cm.eq.'fx' ) then
        if(nitems.eq.5) then
        num_fx_change = num_fx_change + 1
        read (aline2, *) kfx_change(num_fx_change,1), 
     .    kfx_change(num_fx_change,2), kfx_change(num_fx_change,3),
     .    fx_change(num_fx_change,1), fx_change(num_fx_change,2)
       else
        call badline(jline, 'FX')
       endif


c*****************************************************
c*** GD: - Green's functions directory and interpolation intervals
c*****************************************************
      else if (cm.eq.'gd' ) then 
        call cleareal(v, 6)
       read (aline2, *) gfdir, (v(i),i=1,nitems-1)
        makeGF_all = .false.
        if (v(1).gt.0) GFx_interp =   v(1)
        if (v(2).gt.0) GFw_interp =   v(2)
        if (v(4).gt.0) dx_node_new =  v(4) 
        if (v(5).gt.0) gps_near =     v(5) 
        if (v(6).gt.0) gps_too_far =  v(6) 
        makeGF = .true.
        if (v(3).gt.0) makeGF_all = .true.

c*****************************************************
c*** GI: - list of GPS files to be rotated into reference frame
c*****************************************************
      else if (cm.eq.'gi' ) then 
        call clearlog(gps_invert, MAX_gps_files)
        call clearint(iv, MAX_gps_files)

        read (aline2, *) (iv(i),i=1,nitems)

        do i=1, nitems
          if ( iv(i).gt.izero .and. iv(i) .le. MAX_gps_files) 
     .       gps_invert( iv(i) )  = .true.
        enddo


c*****************************************************
c*** GR: - grid of points for calculated velocities
c*****************************************************
c save 2 spots for time

      else if (cm.eq.'gr' ) then 
       if(nitems.ge.7) then
        call cleareal(v, 15)
        read (aline2, * ) ngr, (v(i),i=1,nitems-1)
         make_grid= .true.
         do_dgt = .true.
         v(1)=fnlong(v(1))

      if ( ngr.gt.0 .and. ngr .le. MAX_grids ) then
       print *, 'Generate grid ', ngr
        do i=1,8
         pgrid(ngr,i) = v(i)
        enddo
       endif

       else
        call badline(jline, 'GR')
       endif
       
c*****************************************************
c*** GS: - grid search controls
c
c  GS:  M  dP  N  T  dStep
c  GS: 30 0.1  6  2   3.0
c
c  M = number of parameter increments (steps) in grid search
c  dP = starting step value for parameter in grid search
c  N = number of grid searches
c  T = search type
c  dStep = parameter step at each search is divided by dStep
c
c  search type 0 - search the full range
c              1 - search full range in random parameter order
c              2 - gradient search
c              3 - gradient search in random parameter order
c*****************************************************

      else if (cm.eq.'gs' ) then 
        call cleareal(v, 15)
        read (aline2, *) (v(i),i=1,nitems)
        do i=1,6
         gs_controls(i)=v(i)
        enddo

c*****************************************************
c*** HC: - hard constraints
c  hc = 1 for slip rate constraint
c  hc = 2 for slip direction constraint 
c  hc = 3 for rotation rate
c     (enter moving, then fixed blocks)
c*****************************************************
      else if (cm.eq.'hc' ) then 
       if(nitems.eq.7 .or. nitems.eq.8) then
        read (aline2, *) itype

       if (itype.ge. 1 .and. itype.le.3) then
        num_hc = num_hc + 1

        if(num_hc.gt.MAX_hc) then
          print *, 'MAX_hc exceeded '
         call stop1
        endif

        print *, 'HC ',num_hc,' ',aline2(1:80)

        if(nitems.eq.7) read (aline2, *) hc(num_hc), 
     .    x, hc_pos(num_hc,2), c4a, c4b, 
     .    hc_val(num_hc,1), hc_val(num_hc,2)

c for rate azimuth
        if(nitems.eq.8) read (aline2, *) hc(num_hc), 
     .    x, hc_pos(num_hc,2), c4a, c4b, 
     .    hc_val(num_hc,1), hc_val(num_hc,2),
     .    hc_val(num_hc,3) 

         c4a = block_rename(c4a)
         c4b = block_rename(c4b)

         hc_pos(num_hc,1) = fnlong(x)

         call getplate(c4a, k1, block_check)
         call getplate(c4b, k2, block_check)

c swap fixd and movg blocks if a3 = 1  
         if ( a3.eq.'s' ) call iswap (k1, k2)       

         hc_block(num_hc,1) = k1
         hc_block(num_hc,2) = k2
         call checkpoles( k1, k2, samepole, bothfixed)
         if ( samepole .or. bothfixed ) num_hc = num_hc-1

         if ( hc_val(num_hc,1).gt.hc_val(num_hc,2) ) then
            num_hc = num_hc-1
            print *, '**** Min value greater than max value'
         endif
       endif
         
       else
        call badline(jline, 'HC')
       endif

      
c*****************************************************
c*** IN: - fault interpolation lengths
c*****************************************************
      else if (cm.eq.'in' ) then 
       if(nitems.eq.2) then
         read (aline2, *) X_interp, W_interp
       elseif (nitems.eq.3 ) then
         read (aline2, *) X_interp, W_interp, sd_umin
       elseif (nitems.eq.4 ) then
         read (aline2, *) X_interp, W_interp, sd_umin, tr_umin
       else
        call badline(jline, 'IN')
       endif

c*****************************************************
c*** LL: - line length rate data file
c*****************************************************
      else if (cm.eq.'ll' ) then
        call readll  
      
c*****************************************************
c*** MF: - merge faults at T junction, project end of fault 1 onto fault 2
c*****************************************************
      elseif (cm.eq.'mf' ) then 
       if(nitems.eq.2) then
        nfmerges=nfmerges+1
        read (aline2, *), (nfmerge(nfmerges,i),i=1,2)
       else
        call badline(jline, 'MF')
       endif

c*****************************************************
c*** MM: - minimum / maximum moment, interseismic for a fault
c*****************************************************
      elseif (cm.eq.'mm' ) then 
        print *, '**** Use SM: option '

c*****************************************************
c*** MR: - mantle relaxation signals, read in GFs from visco1d, 
c***       divide by amplitude to get unit responses
c if flag=1 solve for amplitude
c*****************************************************
c MR: # Flag filename Amp Eq_lon Eq_lat Radius Q_Year V_year
      elseif (cm.eq.'mr' ) then
       call cleareal(v, 15)
       read (aline2, *) n, m, gps_file  
       if (n .ge. 1 .and. n .le. MAX_mrlx_files) then
         rlx_inv_flag(n) = (m.eq.1)
         rlx_file(n) = gps_file
c         rlxAmp(n) = v(1)
c         rlxDX(n) = v(2)


         np=0
         call fopen (kg, 0, rlx_file(n))
         
         read (kg, *) npts, (v(i),i=1,6)
          Amp = v(1)
c          rlxAmp(n) = v(1)
c          rlxDX(n) = v(6)
         do i=1,6
          rlxParms(n,i) = v(i)
         enddo
         rlxParms(n,2) = fnlong(rlxParms(n,2))
          do i=1, min(npts, MAX_mrlx_pts)
           read(kg,*, end = 113 ) (rlxGF(n,i,k),k=1,5)
            rlxGF(n,i,1) = fnlong(rlxGF(n,i,1))
            np=np+1
            do k=3,5
             rlxGF(n,i,k)=rlxGF(n,i,k)/Amp
            enddo
          enddo
 113   continue
        k = kfclose(kg)
        mrlx_pts(n) = np
        print *, 'Read ', rlx_file(n), np, ' points'
       endif
         
 
c*****************************************************
c*** MS: - merge time series of multiple sites
c*****************************************************
      elseif (cm.eq.'ms' .and. time_dependence ) then
        
       nsmerge=nsmerge+1
       read (aline2, *, err=112, end=112) 
     .    (merge_sites(nsmerge,k),k=1,7)
 112   write (*,*) 'Merging ', merge_sites(nsmerge,2)

c*****************************************************
c*** MT: - read moment tensors
c*****************************************************
      elseif (cm.eq.'mt' ) then 
       if(nitems.eq.2) then
        read (aline2, *) mtfile, qmtwt
        call readmts (mtfile) 
        if (qmtwt .le. zero) qmtwt = 1.0d0
       else
        call badline(jline, 'MT')
       endif

c*****************************************************
c*** NI: - controls the number of iterations
c*****************************************************
      else if (cm.eq.'ni' ) then 
       if(nitems.eq.1) then
        read (aline2, *) m
        kcalculate = ( m.gt.0)
        do i=1,m
         icontrol(2*i-1) = 2
         icontrol(2*i) = 1
        enddo
       else
        call badline(jline, 'NI')
       endif

c*****************************************************
c*** IC: - controls the iterations; 
c**     1 for simulated annealing; 2 for grid search
c*****************************************************
      else if (cm.eq.'ic' ) then
       call clearint(icontrol, 30)
       nit = min(nitmax,nitems)
       read (aline2, *) (icontrol(i),i=1,nit)

c*****************************************************
c*** NF: or NN: - node parameter numbers
c* with no a3 the index values are all on a line
c* if a3 = 0 or g then the input is in array form 
c* if a3 = t then the input is for a transient in 'i' format (program builds index grid)
c* the program can build the node index grid with a3='i' for inerseismic or a3='t' for transient
c*  kf Nx Nz Nx1 Nx2 Nxstep Nz1 Nz2 Nzstep  Amp
c*     from Nx1 to Nx2 and from Nz1 to Nz2 will be indexed and the 
c*      steps are how many nodes have the same index
c*      'Amp' is the amplitude given to the node
c*****************************************************
      else if (cm.eq.'nf' .or. cm.eq.'nn') then
      
      call cleareal( v,   MAX_nodes )
      call clearint( iv,  MAX_nodes )
      call clearint( iv2, MAX_nodes )

      nx1 = 0

      a3in = ( a3.eq.'g' .or. a3.eq.'i' .or. a3.eq.'t' )

c-- single in-line input       
      if (.not. a3in ) then
        read (aline2, *) nf1, (iv2(i),i=1,nitems-1)
      endif

      if ( a3in ) then
      
c--- read grid format
      if ( a3.eq.'g'  ) then
        read (aline2, *) nf1, nx, nz
        n=0
        do iz=1,nz
         read (k10, *), (iv2(n+i),i=1,nx)
         n=n+nx
        enddo
      endif

c build the indices
      if (a3.eq.'i' .or. a3.eq.'t' ) then
       read (aline2, *) nf1, nx, nz, 
     .    nx1, nx2, nxs, nw1, nw2, nws, amp

c check for bounds
c       if ( a3.eq.'i' ) then
c        call check_f_bounds (nf1, nx, nz)
c        call check_f_bounds (nf1, nx1, nw1)
c        call check_f_bounds (nf1, nx2, nw2)
c       endif

c   store transient boundaries
       if ( a3.eq.'t' .and. nf1 .ne. 999 ) then
        kev_grid(nf1,1) = nx1
        kev_grid(nf1,2) = nx2
        kev_grid(nf1,3) = nw1
        kev_grid(nf1,4) = nw2
       endif
       
c-- build node grid, store values used
      call make_index_grid (nx,nx1,nx2,nxs,nz,nw1,nw2,nws,iv2) 

c set starting values
        n=0
        call cleareal( v, MAX_nodes )
        do iz = 1, nz
          do ix = 1, nx
           n=n+1
           if (iv2(n).gt.0) v(iv2(n)) = amp
          enddo 
        enddo

       endif

      endif
c--------------------------------------------------------
c assign values

      if(nf1.eq.999) then
        k = iv2(1)
       do kf=1,MAX_f
         do i=1, MAX_nodes
           if ( a3.ne.'t' ) NN_in(kf,i) = k
         enddo
       enddo
       do kf=1,MAX_srce
         do i=1, MAX_nodes
           if ( a3.eq.'t' ) NN_tr(kf,i) = k
         enddo
       enddo

      else

       do i=1,MAX_nodes
         if ( a3.eq.'t' ) then
           NN_tr(nf1,i) = iv2(i)
           VN_tr(nf1,i) = v(i)
         endif
         if( a3.eq.'i' ) then
           NN_in(nf1,i) = iv2(i)
           VN_in(nf1,i) = v(i)
         endif
         if( a3.eq.'g' .or. (.not. a3in) ) then
           NN_in(nf1,i) = iv2(i)
         endif
       enddo
      endif

      if ( a3 .ne. 't' ) nn_read(nf1)= .true.

     

c*****************************************************
c*** NO: or NV: - starting coupling values for nodes
c* if a3 = 0 or = g then the input is in array form, else all values on a line
c* if a3 = t then the input is for a transient, in array form 
c*****************************************************

      else if (cm.eq.'no' .or. cm.eq.'nv') then 

c-- grid format
       if ( a3.eq.'0' .or. a3.eq.'g' .or. a3.eq.'t') then
        read (aline2, *) nf1, nx, nz
        n=0
         do iz=1,nz
          if ( a3.eq.'t' ) read (k10, *) (VN_tr(nf1,n+i),i=1,nx)
          if ( a3.ne.'t' ) read (k10, *) (VN_in(nf1,n+i),i=1,nx)
          n=n+nx
         enddo

      else  

c* read all values on a single line
       call cleareal( v, MAX_nodes )
       read (aline2, *, err=170,end=170) nf1, (v(i),i=1,nitems-1)

 170   do i=1,MAX_nodes
         VN_in(nf1,i)=v(i)
       enddo
c       do i=1,nitems-1
c         print *, nf1, VN_in(nf1,i)
c       enddo

c if 999 set all to same value read in on line
       if ( int(v(3)).eq.999) then
        vv=v(4)
         do i=1,MAX_nodes
           VN_in(nf1,i)=vv
         enddo
        endif

      if(nf1.eq.999) then
       do kf=1,nfault
         do i=1, MAX_nodes
           if ( a3 .ne. 't' ) VN_in(kf,i) = v(1)
         enddo
       enddo
      endif

c if 888 taper downdip from 1 to 0
       if ( int(v(3)).eq.888 ) then
        k=0
        nx=int(v(1))
        nz=int(v(2))
        rnz = v(2)
        do j=1,nz
         do i=1,nx
           k=k+1
           VN_in(nf1,k) = 1.0d0 - real(j-1)/rnz
         enddo
        enddo
      endif
        
      endif

      if ( a3 .ne. 't' ) nv_read(nf1)= .true.


c*****************************************************
c*** NX: - fixed nodes
c*****************************************************
      else if (cm.eq.'nx' ) then 
       call clearint( iv, MAX_nodes )
       read (aline2, *) nf1, (iv(i),i=1,nitems-1)

       if (nf1. eq. 999 ) then
        do kf = 1,MAX_f
         do i=1,MAX_nodes
          node_fix(kf,i)=iv(i)
         enddo
        enddo
       else
        do i=1,MAX_nodes
         node_fix(nf1,i)=iv(i)
        enddo
       endif
 

c*****************************************************
c*** PE: - penalty factor
c*****************************************************
      else if (cm.eq.'pe' ) then 
       if(nitems.eq.2) then
        read (aline2, *) kk, f
        if ( kk.gt.0 .and. kk .le. 6 ) then
         if ( f.gt.0.0d0) penalty_factor2(kk) = f
        endif
       else
        call badline(jline, 'PE')
       endif

c*****************************************************
c*** PF: - parameter file
c*****************************************************
      else if (cm.eq.'pf' ) then 
       c80 = ""
       if(nitems.eq.2) then
        read (aline2, *) c80, iparmread
        if ( .not. pio_on_cmd ) parmfile = c80
       else    
        call badline(jline, 'PF')
       endif

c*****************************************************
c*** PG: - GPS file pole
c*****************************************************
      else if (cm.eq.'pg' ) then 
       if(nitems.eq.4) then

       call cleareal( v, 3 )
       read (aline2, *) bname, (v(i), i=1,3)
        num_pg = num_pg + 1
        pg_name(num_pg) = bname

c** read in as lat, lon, omega 
       if ( a3.ne.'1' .and. a3.ne.'c' .and. a3.ne.'f' ) then
        call sphcar(v(1), v(2), v(3),w1,w2,w3)
        if (a3.eq.'g') call sphcar(v(2), v(1), v(3),w1,w2,w3)
        v(1)=w1 
        v(2)=w2 
        v(3)=w3 
       endif

       do i=1,3
         pg_pole(num_pg,i) = v(i)
       enddo

c*  fixed poles
      if (a3.eq.'f') then
        num_fixd_pg_pole = num_fixd_pg_pole + 1
        nfixd_pg_pole(num_fixd_pg_pole) = n
        do i=1,3
         fixd_pg_pole(num_fixd_pg_pole, i) = v(i)
        enddo
      endif

       else    
        call badline(jline, 'PG')
       endif


c*****************************************************
c*** PN: - coupling profile node flags
c*****************************************************
      else if (cm.eq.'pn') then 
       call clearint( iv2, nitems )
       read (aline2, *) nf1, (iv2(i),i=1,nitems-1)
       
       if(a3.eq.'t') then
        if (nf1.eq.99 ) then
         do j=1, MAX_srce
          do i=1,MAX_x
           tnode_prof(j,i) = iv2(i)
          enddo
         enddo
        else
          do i=1,MAX_x
           tnode_prof(nf1,i) = iv2(i)
          enddo
        endif
       else
        do i=1,MAX_x
         node_prof(nf1,i) = iv2(i)
        enddo
       endif

c*****************************************************
c*** PO: - poles and covariance
c 3rd char = c for Cartesian
c          = f to overwrite parameter file value (use Cartesian)
c          = b to use block name to specify pole (use Cartesian)
c          = g if input in longitude, latitude, omega
c for Cartesian read in Wx, Wy, Wz in degrees/Ma
c                  then Sx, Sy, Sz standard errors in deg/Ma
c                  then Sxy, Sxz, Syz the unitless covariances
c***
c*****************************************************
      else if (cm.eq.'po') then

        call cleareal(v, 20)

       if ( a3.eq.'b' .or. a3.eq.'g') then
         nb=0
         read (aline2, * ) bname, (v(i),i=1,nitems-1)
          call getplate (bname, nb, block_check)
         if ( nb.gt.0) nb = npole_block(nb)
       else
         read (aline2, * ) nb, (v(i),i=1,nitems-1)
       endif

       if ( nb.eq.0 ) then
         write (*,*)'Pole is zero for ', aline2
         goto 5
       endif

c** read in as lat, lon, omega (default), convert to Cartesian
       if ( a3.ne.'1' .and. a3.ne.'c' .and. a3.ne.'f' .and. 
     .    a3.ne.'b' ) then
        call sphcar(v(1),v(2),v(3),w1,w2,w3)
        if (a3.eq.'g') call sphcar(v(2),v(1),v(3),w1,w2,w3)
        v(1)=w1 
        v(2)=w2 
        v(3)=w3 
c* someday here convert lat.lon.omega covariance to cartesian
        do i=4,9
         v(i) = 0.0d0
        enddo
       endif

c* put in poles array
        do i= 1, 3
          poles(nb,i)=v(i)
          poles(nb,i+3)=v(i+3)
          poles(nb,i+6)=v(i+6)
        enddo
        num_poles= max(nb, num_poles)

c***  poles to override in parameter file
      if (a3.eq.'f' .or. a3.eq.'b') then
        num_fixd_bl_pole = num_fixd_bl_pole + 1
        nfixd_bl_pole(num_fixd_bl_pole) = nb
        do i=1,9
         fixd_bl_pole(num_fixd_bl_pole, i) = poles(nb,i)
        enddo
      endif

      if(verbose) write(*, '(a5,i3,9f10.5)') 
     .   'Pole ',nb, (poles(nb,i),i=1,9)

c*****************************************************
c*** PR: - profiles
c   Line number, start X, start Y, number of points, distance step, azimuth, width
c*****************************************************
      else if (cm.eq.'pr' ) then 
       call cleareal( v, 20)

       read (aline2, *) k
       c10 = '          '

      if (nitems .ge. 7) then

       if (nitems.eq.7) read (aline2, *) j, (v(i),i=1,6)
       if (nitems.eq.8) read (aline2, *) j, (v(i),i=1,6), c10
     
        print *, 'Reading Profile ', j, ' ', c10

        nlines=nlines+1
        if(nlines.gt.MAX_pr_lines ) then
          print *, 'MAX_pr_lines exceeded '
         call stop1
        endif

        prof_num(nlines)=j
        j=nlines
      
        xx=v(1)
        yy=v(2)
        x2=fnlong(xx) 
        y2=yy
        prof_n(j)= int(v(3))
        prof_dx(j)=v(4)
        prof_az(j) = v(5)
        prof_width(j)=v(6)
        prof_start(j,1)=fnlong(xx)
        prof_start(j,2)=yy
        prof_title(j) = c10

      elseif (nitems.ge.1 .and. nitems .lt.7) then
       read (aline2, *) j
         if(j.eq.0) then
          nlines=0
          print *, 'Resetting Profiles '
         endif
      else
        call badline(jline, 'PR')
      endif
  
      
c*****************************************************
c*** PT: - file of lon,lat points to compute displacements
c*        from fault slip
c*****************************************************
      else if (cm.eq.'pt' ) then 

       if(nitems.eq.1) then
        read (aline2, * ) ptfile
        print *, 'Reading ', ptfile
         call existfile( ptfile, fexist, 1)

         if (fexist) then
          k11=kfopen(11)
          open (k11,file = ptfile )
           do i=1,1000
           
            ndef_pts = ndef_pts+1
            if(ndef_pts .gt. 1000) goto 1891
            call i2c (ndef_pts-1, 3, c3a)
            bname = 'S'//c3a

            if (nitems .eq. 2) read(k11, *, end=1891) 
     .        xlon, xlat 
            if (nitems .eq. 3) read(k11, *, end=1891) 
     .        xlon, xlat, bname

            pdef_pt(ndef_pts,1) = fnlong(xlon)
            pdef_pt(ndef_pts,2) = xlat
            name_pt(ndef_pts)   = bname

           enddo
 1891     ik=kfclose(k11)
         else
           print *, 'File does not exist: ', ptfile
         endif
           
      else
        call badline(jline, 'PT')
      endif

  
c*****************************************************
c*** PV: - coupling profile values; 3rd = 't' for transient
c**  format pvt: 1 20 1 20 5
c*****************************************************
      else if (cm.eq.'pv') then 
       call cleareal( v, 20)
       read (aline2, *, end=197) nf1, nx, (v(i),i=1,nitems-2)
       
 197   if(a3.eq.'t') then
         if (v(1).gt.zero ) then
          do i=1,nx
           do j=1,3
            twin(nf1,i,j) = v(j)
           enddo
          enddo
         else
          do k=1,3
           read (k10, *) (twin(nf1,i,k),i=1,nx)
          enddo
         endif
       else
         if (v(1).gt.zero ) then
          do i=1,nx
           do j=1,3
            win(nf1,i,j) = v(j)
           enddo
          enddo
         else
          do k=1,3
           read (k10, *) (win(nf1,i,k),i=1,nx)
          enddo
         endif
       endif

c*****************************************************
      else if (cm.eq.'px') then 
       print *, '*** PX no longer used, use FT'


c*****************************************************
c*** RE: - reference block
c*****************************************************
      else if (cm.eq.'re' ) then 
        call cleareal(frame_pole,3)

c RM1107 - new read
        if(nitems.eq.4) read (aline2, *) bname, (frame_pole(i),i=1,3)
        if(nitems.eq.1) read (aline2, *) bname
        
       call getplate (bname, nblock_ref, block_check)
       write (*, *) 'Ref frame Block ', bname, ' #',nblock_ref

c*****************************************************
c*** RF: - rotated frame, output vectors in this frame also
c*****************************************************
      else if (cm.eq.'rf' ) then 
       if (nitems.eq.3) then
        read (aline2, *) (fpole(i),i=1,3)
        new_frame = .true.
       else
        call badline(jline, 'RF')
       endif

c*****************************************************
c*** RS: - reference site for GPS vectors
c*****************************************************
      else if (cm.eq.'rs' ) then 
        call cleareal( v, 10)
       if (nitems.eq.2) then
        read (aline2, *) jp, bname
        if (jp.gt.0) ref_site(jp) = bname
       elseif (nitems.eq.4) then
        read (aline2, *) jp, (v(i),i=1,3)
        if (jp.gt.0) then
         ref_site(jp) = '    '
         do i=1,3
           ref_vel(jp,i) = v(i)
         enddo
        endif
       else
        call badline(jline, 'RS')
       endif

c       print *, 'Ref site/vel ', ref_site(jp), 
c     . ref_vel(jp,1), ref_vel(jp,2), ref_vel(jp,3)


c*****************************************************
c*** RO: - rotation rate data file
c*****************************************************
      else if (cm.eq.'ro' ) then 
        call readrot (a3)

c*****************************************************
c*** SA: - simulated annealing controls
c in order in sa: line
c  temptr0  
c  num_iterations
c  number of sa iterations
c*****************************************************
      else if (cm.eq.'sa' ) then 
        call cleareal( v, 10)
        read (aline2, *) (v(i),i=1,nitems)
        sa_controls(1)=v(1)
        sa_controls(2)=v(2)
        sa_controls(3)=v(3)
        sa_controls(4)=v(4)
         
c*****************************************************
c*** SI: - list of strain tensors to be adjusted
c*****************************************************
      else if (cm.eq.'si' ) then 
       call clearint( iv, nitems)
       read (aline2, *) (iv(i),i=1,nitems)
       if (a3.ne.'c') call clearlog(si_flag, MAX_strain)

        do kk=1, nitems 
         if ( abs(iv(kk)) .le. MAX_strain) then
          if (iv(kk).gt.0 ) si_flag(iv(kk)) = .true.
          if (iv(kk).lt.0 ) si_flag(abs(iv(kk))) = .false.
         endif
        enddo

        num_strain_invert = 0
        call clearint(nstrain_invert, MAX_strain)
         do kk=1, MAX_strain 
          if ( si_flag(kk) ) then 
           num_strain_invert = num_strain_invert + 1 
           nstrain_invert(num_strain_invert)=kk
          endif
         enddo

c*****************************************************
c*** SM: - smoothing for fault or transient
c        1.  is fault number
c        2.  smoothing type 
c        3.  along strike smoothing factor
c        4.  downdip smoothing factor
c        5.  moment  smoothing factor
c        6.  min moment rate for fault
c        7.  max moment rate for fault
c
c      for transient use 'smt:' - third char is 't' for transient
c        1.  is transient number
c        2.  smoothing type 
c        3.  along strike smoothing factor
c        4.  downdip smoothing factor
c        5.  moment  smoothing factor
c        6.  min moment for event
c        7.  max moment for event
c*****************************************************
      else if (cm.eq.'sm' ) then
        call cleareal( v, 20)
       read (aline2, *) kf, (v(i),i=1,nitems-1)
        x1 = v(5)
        x2 = v(6)
c        print *, 'm1, m2', x1,x2
      
c-- for transients 
       if (a3.eq.'t' ) then

         if ( kf.eq.999) then
          do kk=1,MAX_srce
           do k=1,6
            trsmooth(kk,k) = v(k)
           enddo
            if(x1.ge.zero .and. x2.gt.zero .and. x2.gt.x1) then ! 11-13-15
             tminmax(kk,30,1) = x1
             tminmax(kk,30,2) = x2
             mmparm(kk,30) = .true.
            endif
          enddo
         
         else
    
          do k=1,6
           trsmooth(kf,k) = v(k)
          enddo
            if(x1.ge.zero .and. x2.gt.zero .and. x2.gt.x1) then ! 11-13-15
             tminmax(kf,30,1) = x1
             tminmax(kf,30,2) = x2
             mmparm(kf,30) = .true.
            endif
         endif
       else

c-- for faults 
       
         if ( kf.eq.999) then
          do kk=1,MAX_f
           do k=1,6
            smooth(kk,k) = v(k)
           enddo
          enddo
         
         else
    
          do k=1,6
           smooth(kf,k) = v(k)
          enddo
         endif
        
       endif
       

c*****************************************************
c*** SR: - slip rate data file
c*****************************************************
      else if (cm.eq.'sr' ) then
        call readsrs (a3)

c*****************************************************
c*** SS: - surface strain rate data file
c*****************************************************
      else if (cm.eq.'ss' ) then 
       read (aline2, *, err=260,end=260) ss_file, ss_wtfac 
  260  print *, 'Reading SS file ',ss_file
       call readss (ss_file, ss_wtfac)

c*****************************************************
c*** ST: - strain rate tensor: Exx, Eyy, Exy in nanostrain/yr
c*                            Cx, Cy - optional centroid for this strain rate
c*                            D - optional damper
c*****************************************************
      else if (cm.eq.'st') then
       if (nitems .ge. 4) then
        call cleareal( v,5)
        read (aline2, * ) n, (v(i),i=1,nitems-1)

c re-set all      
         if ( n.eq.999 ) then
          do ns = 1,MAX_strain
            do i= 1,3
             strain(ns,i)=v(i)/1.0d3
            enddo
            if (v(4) .ne. zero) strain2(ns,1) = fnlong(v(4))
            if (v(5) .ne. zero) strain2(ns,2) = v(5)
            if (v(6) .ne. zero) strain2(ns,3) = v(6)
            if (v(7) .ne. zero) strain2(ns,4) = v(7)
          enddo
         else

c re-set the one read in
          if(verbose) write (*,*)'Reading strain tensor ', n
            do i= 1,3
             strain(n,i)=v(i)/1.0d3
            enddo
            if (v(4) .ne. zero) strain2(n,1) = fnlong(v(4))
            if (v(5) .ne. zero) strain2(n,2) = v(5)
            if (v(6) .ne. zero) strain2(n,3) = v(6)
            if (v(7) .ne. zero) strain2(n,4) = v(7)
            num_strain= max(n, num_strain)
        endif

       else
        call badline(jline, 'ST')
       endif

c*****************************************************
c*** SV: - slip vector data file
c*****************************************************
      else if (cm.eq.'sv' ) then
        call readsvs (a3)

c*****************************************************
c*** TF: -  time series filter
c       common mode NAME lon lat radius to exclude
c       outlier filter
c*****************************************************
      else if (cm.eq.'tf' .and. time_dependence ) then 
       if(nitems.ge.4) then
        call cleareal(v, 10)
        read (aline2, *) c4, (v(j), j=1,nitems-1)
        num_tfilt = num_tfilt + 1
        tfilt_name(num_tfilt) = c4
         do j=1,6
          tfilt(num_tfilt,j) = v(j)
         enddo
       else
         call badline(jline, 'TF')
       endif

c*****************************************************
c*** TI: - read tilt data file
c*****************************************************
      else if (cm.eq.'ti' ) then 
       read (aline2, *, err=290,end=290) tilt_file, tilt_wtfac 
  290  print *, 'Reading tilt file ', tilt_file
       call readtilt( tilt_file, tilt_wtfac)


c*****************************************************
c*** UP: --- uplift rates data file
c*****************************************************
      else if (cm.eq.'up' ) then 
      
       print *, '*** Use DS: or GP: to input vertical data'
      call stop1


c****************************************************************************************
c   GP: - velocities data file
c   DS: - displacements data file
c   TS: - time series data file
c
c  items on line are:
c  GP: shortname, filename, pole number, wtfac, Wx, Wy, Wz, Tmin, Tmax, Sigmin, Sigmax, R/S_max, 3 component flags
c    format: Lon  Lat  Ve Vn Se Sn rho Site        (default) psvelo format
c            Lon  Lat  Ve Vn Vz Se Sn Sz Site      (if a3 = 1)
c            Lon  Lat  Ve Se Vn Sn Vz Sz Site      (if a3 = 2)
c            Lon  Lat  Ve Vn Se Sn rho Site Vz Sz  (if a3 = 3) - globk format
c            Lon  Lat  Vz Sz Site                  (if a3 = 4) - vertical only
c
c  TS: shortname, filename, pole_number, wtfac, Tdec, DT, sigmax, Tmin, Tmax, 3 Xo and 3 V flags, dtMin, 
c    header format: Lon  Lat  Ve Vn Se Sn rho Site        (default) psvelo format
c                   Lon  Lat  Ve Vn Vz Se Sn Sz Site      (if a3 = 1)
c    data format: Year Xe Se Xn Sn Xz Sz
c
c XX = unused
c
c  DS: shortname, filename, pole_number, wtfac, Wx, Wy, Wz, Tmin, Tmax, 3 component flags
c    format: Lon  Lat  De Se Dn Sn Dz Sz Site (default)
c            Lon  Lat  De Dn Dz Se Sn Sz Site (if a3 = 1)
c            Lon  Lat  De Dn Se Sn Site Dz Sz (if a3 = 2)
c            Lon  Lat  De Dn Se Sn rho Site   (if a3 = 3) horizontal only
c            Lon  Lat  Dz Sz Site             (if a3 = 4) - vertical only
c
c  component flags for E N U; 0 if not used, 1 if to be used
c
c****************************************************************************************
      else if (cm.eq.'gp' .or. cm.eq.'ts' .or. cm.eq.'ds') then 

       call cleareal(v, 20)
       read (aline2, *) bname, gps_file, (v(i),i=1,nitems-2)

c skip if no weighting
       if (v(2).gt.zero) then

c save for reading later
        num_gps_file = num_gps_file + 1 
        gpsline(num_gps_file) = aline2
        cmgps(num_gps_file) = cm
        a3gps(num_gps_file) = a3

        kp = abs(int(v(1)))
        num_gps_poles = max(kp, num_gps_poles)
        ngps_index(num_gps_file) = kp

        gps_filename(num_gps_file) = gps_file
        gps_fname(num_gps_file) = bname

       endif
      
c****************************************************************************************
c RT: file of times to remove from time series
c****************************************************************************************
      else if (cm.eq.'rt' ) then 
        num_f_rm = num_f_rm + 1
        krm_filename_type(num_f_rm) = 1
        read (aline2, *) rm_filename(num_f_rm)

c****************************************************************************************
c RX: file of offsets to correct in time series
c****************************************************************************************
      else if (cm.eq.'rx' ) then 
        num_f_off = num_f_off + 1
        off_sign(num_f_off) = 1.0
        if(nitems.eq.1) read (aline2, *) off_filename(num_f_off)
        if(nitems.ge.2) read (aline2, *) off_filename(num_f_off), 
     .       off_sign(num_f_off)

c*****************************************************
c*** FI: - set faults to adjust in inversion
c**  if +, turn ON flags 1 and 3
c**  if -, turn OFF flag 3
c*****************************************************
      else if (cm.eq.'fi' ) then
       call clearint(iv, MAX_f)
       read (aline2, *) (iv(i),i=1,nitems)

c-- set all flags if 999
      if (abs(iv(1)).eq.999) then
        do i=1,MAX_f
         kf=i
         if (iv(1).eq.999) then
          fflag(kf,1) = .true.
          fflag(kf,3) = .true.
         elseif (iv(1).eq.-999 ) then
          fflag(kf,3) = .false.
         endif
        enddo
       else
c-- set individual flags
        do i=1,nitems
         kf=abs(iv(i))
         if (iv(i).gt.0) then
          fflag(kf,1) = .true.
          fflag(kf,3) = .true.
         elseif (iv(i).lt.0 ) then
          fflag(kf,3) = .false.
         endif
        enddo
       endif

c*****************************************************
c  IS - InSAR data file for single event
c  IS: shortname, filename, source_number, wtfac, time1, time2, heading, inc_angle, OffsetFlag, XYSlopeFlag, TropoFlag, Ndec, Smax, Smin
c   Planar correction = Offset + Lon-slope * longitude + Lat-slope * Latitude
c   Ndec = decimation factor 
c   Smax - max sigma
c*****************************************************
c insar_info:
c 1 File number
c 2 Wtfac
c 3 Time1
c 4 Time2
c 5 Heading
c 6 Incidence angle
c    Input        Stored
c 7 Corr Flag  -> Offset
c 8 Tropo flag  - East slope
c 9 NU          - North slope
c 10 Ndec -     - Z slope
c 11 Smax
c 12 Smin
c 
c insar_info items:
c 1 Index
c 2 Weight factor
c 3 Time 1
c 4 Time 2
c 5 Heading
c 6 Incidence angle
c 7 Offset
c 8 Planar X slope
c 9 Planar Y slope
c 10 Decimation factor -> Tropospheric slope
c 11 Smax
c 12 Smin

      else if (cm.eq.'is') then 
       call cleareal(v, 20)
       read (aline2, *) bname, gps_file, (v(i),i=1,nitems-2)
        nf = abs(int(v(1)))
        call existfile( gps_file, fexist,1)

       if (fexist .and. (.not. insar_off(nf)) ) then 
        iflip = ( v(1).lt.0.0d0 )

        num_insar_file = num_insar_file + 1 
        if (num_insar_file .gt. MAX_insar_files) then
          print *, 'Too many InSAR files '
          call stop1
        endif
          
        insar_fname(nf) = bname
        insar_filename(nf) = gps_file
        insar_flag(nf) = .true.

       do j=1,12
        insar_info(nf,j) = v(j)
       enddo

c how many correction coefficients to get
       insar_corr(nf) = int(insar_info(nf,7))
       if (insar_tropo .or. insar_slope) insar_offset = .true.
       np = 0
       if (insar_offset ) np=np+1 
       if (insar_tropo )  np=np+1
       if (insar_slope )  np=np+2
       if (np.gt.0) insar_corr(nf)= np
       
      call readinsar (gps_file, bname, nf, a3, ngin, iflip)
        
      c180='No Name   WtFac  #in #read    Time1     Time2   Bearng   '//
     .  'Elev.  Corr. Flags Dec Filename'
        
        write (*,'(a100)') c180
        write (*,'(i2,1x,a4, f8.2,2i5,1x, 2f10.4, 2f8.2, 4i4, 1x,a80)') 
     .    nf, bname, insar_info(nf,2), num_insar, ngin,
     .    (insar_info(nf,k),k = 3,6), (int(insar_info(nf,k)),k = 7,10), 
     .    gps_file
      else
        if ( .not. fexist ) print *, 'Not found ', gps_file
      endif


      endif

   5  continue


  99  ik = kfclose (k10)


c*****************************************************
c*****  end of deciphering input lines *************** 
c*****************************************************
c** quit if input errors found
      if( quit_on_error .and. input_error) then
        print *, '**** ENDING prematurely - Input errors found '
        print *, '****  look at screen output '
       call stop1
      endif

c*****************************************************
c now read GPS data files
c*****************************************************
       ngfiles = num_gps_file
       num_gps_file = 0
       print *, 'Reading' , ngfiles, ' GPS files '

       do 333 nng = 1, ngfiles
         aline2 = gpsline(nng)
         cm = cmgps(nng)
         a3 = a3gps(nng)

       veldata  = (cm.eq.'gp')
       tsdata   = (cm.eq.'ts' .and. time_dependence) 
       dispdata = (cm.eq.'ds' .and. time_dependence) 

       if ( veldata .or. dispdata .or. tsdata ) then

       call cleareal(v, 20)
       call parse(aline2, args, nitems)

       read (aline2, *) bname, gps_file, (v(i),i=1,nitems-2)

       if (v(2).gt.zero) then

       num_gps_file = num_gps_file + 1 

       do j=1,16
         gps_info(num_gps_file,j) = v(j)
       enddo

       swapxy = ( v(1).lt. 0.0d0)
       kp = abs(int(v(1)))
       num_gps_poles = max(kp, num_gps_poles)
       ngps_index(num_gps_file) = kp
       tdec = v(3)

       gps_filename(num_gps_file) = gps_file
       gps_fname(num_gps_file)    = bname

c see if filter or common mode correction to be applied
         kfilt = 0
         do j = 1, num_tfilt
          if (bname.eq.tfilt_name(j) ) kfilt = j
         enddo
    
        print *, 'Reading ', gps_file

        call readgps (gps_file,bname,a3,ngin,ntsin,tdec,kfilt,
     .         kstop,swapxy)
        if ( kstop ) goto 9999
     
        if (tsdata) then
          write (*,*)'No Name     WtFac    MinSig    MaxSig   R/S max'//
     .        '   #TS  #pts  Pole'
          write (*, '(i2,2x,a4,4f10.3,3i7)') 
     .     num_gps_file, bname, 
     .     (gps_wt(num_gps_file,k),k=1,4), ngin, ntsin,
     .     ngps_index(num_gps_file) 
     
c common mode correction
        call commode (num_gps_file, bname, kfilt )
 
        elseif (veldata .or. dispdata ) then
      
         write (*,*)'No Name     WtFac    MinSig    MaxSig   Max_R/S'//
     .        ' #read  Pole  File'
         write (*, '(i2,2x,a4,4f10.2,2i7)') 
     .    num_gps_file, bname, 
     .    (gps_wt(num_gps_file,k),k=1,4), ngin,
     .    ngps_index(num_gps_file) 
        endif

       num_disp_used = num_disp

      endif

c for time dependence flag
      endif

 333  enddo
      print *, 'End reading GPS; # sites ', num_gps


c****************************************************
c** check for array overflows
c****************************************************
      yestop = .false.

      call dimcheck (nfault, MAX_f,        "MAX_f       ", yestop)
      call dimcheck (num_sv, MAX_sv,       "MAX_sv      ", yestop)
      call dimcheck (num_sr, MAX_sr,       "MAX_sr      ", yestop)
      call dimcheck (num_ll, MAX_ll,       "MAX_ll      ", yestop)
      call dimcheck (num_tilts, MAX_tilt,  "MAX_tilt    ", yestop)
      call dimcheck (num_rot,MAX_rot,      "MAX_rot     ", yestop)
      call dimcheck (num_hc, MAX_hc,       "MAX_hc      ", yestop)
      call dimcheck (num_poles, MAX_poles, "MAX_poles   ", yestop)
      call dimcheck (num_gps,MAX_gps,      "MAX_gps     ", yestop)
      call dimcheck (num_strain,MAX_strain,"MAX_strain  ", yestop)
      call dimcheck (nblocks,MAX_block,    "MAX_block   ", yestop)
      call dimcheck (nedits, MAX_edits,    "MAX_edits   ", yestop)
      call dimcheck (num_insar,MAX_insar_pts,"MAX_insar_pts",yestop)

c* grid points
c      do k=1, MAX_grids
c       call gridinfo(k, x0grid, nxgrid, dxgrid, 
c     .    y0grid, nygrid, dygrid, nxyg )
c       call dimcheck (nxyg,   MAX_gridpts,  "MAX_gridpts ", yestop)
c      enddo

       n=0
       do k=1,nlines
        n = n+prof_n(k)
       enddo
      call dimcheck ( n,      MAX_pr_pts,    "MAX_pr_pts   ", yestop)

c-- summary of input data
      ng = 0
      nd = 0
      do i=1,num_gps
        if ( gps_type(i).eq.1 ) ng = ng+1
        if ( gps_type(i).eq.2 ) nd = nd+1
      enddo

      print *, 'Data found:'
      print *, 'Number of InSAR             ', num_insar
      print *, 'Number of GPS velocities    ', ng
      print *, 'Number of GPS displacements ', nd
      print *, 'Number of slip rates        ', num_sr
      print *, 'Number of slip vectors      ', num_sv
      print *, 'Number of line lengths      ', num_ll
      print *, 'Number of rotations         ', num_rot
      print *, 'Number of strain rates      ', num_ss
      print *, 'Number of GPS time series   ', num_ts
      print *, 'Number of time series points', num_disp 
      print *, 'Number of tilts             ', num_tilts

      if ( yestop ) then
       print *, '**** ENDING prematurely - array dimensions exceeded'
       print *, '****  change dimensions in tdefcom1.h and recompile'
      call stop1
      endif

       call dater(dat)
c       call clearchar(SUMline,100)
       write (SUMline, 335) expname, dat 
  335  format('SUM:',1x, a4, 1x, a12)

c******* are there any profiles
       make_profile = (nlines.gt.0) 
      
c****************************************************
c* check for 'stop' file
      call defstop (expname, yestop, a1)
      if (yestop) then
       print *, 'Quit key detected'
       goto 9999
      endif



c****************************************************
c* check which faults will need GFs, set useGF flag
      call clearlog ( useGF, MAX_f)

c if all faults fixed by flags
      if (all_fault_0 ) then
        do kf = 1, nfault
          fflag(kf,1) = .false.
          fflag(kf,3) = .false.
          useGF(kf)   = .false.
        enddo
      endif

c all fixed at current locking values
      if (all_fault_fix) then
        do kf = 1, nfault
          fflag(kf,3) = .false.
          if (fflag(kf,1))  useGF(kf) = .true.
        enddo
      endif

c if using +ph1 set uniform slip on faults
      if (all_fault_1) then
        do kf = 1, nfault
          fflag(kf,3) = .false.
          if (fflag(kf,1)) then
            useGF(kf) = .true.
             do j = 1, nxf(kf)*nzf(kf)
              NN_in(kf,j) = 1 
             enddo
          endif
        enddo
      endif

c fflag(): 1 fault locking used            (FF: option)
c          2 3D slip on fault              (FA: option)
c          3 fault locking adjusted        (FI: option)
c          4 fault used to make blocks     (FB: option)
c          5 fault read in                 (FA: option)
c** if interseismic fault has all node indices = 0, turn it off

      if ( .not. all_fault_0 ) then
       do kf = 1, nfault
        if ( fflag(kf,1) ) then
         yesno = .false.

c check for any nodes with locking
         if( fault_fit_type(kf) .le. 1) then
          yesno = .false.
           do iz=1, nzf(kf)
            do ix=1, nxf(kf)
             do j = 1, nxf(kf)*nzf(kf)
              if ( NN_in(kf,j).gt.0 ) yesno = .true.
             enddo
            enddo
           enddo
          fflag(kf,1) = yesno
          useGF(kf) = yesno

         else

          useGF(kf) = .true.

         endif

        endif
       enddo
      endif


c** check transients to see if GFs needed
      do nt = 1, MAX_srce
       if ( sflag(nt) ) then
        kf = info_source(nt,1)
        if (kf.gt.0 ) then
         kq = info_source(nt,2)
         if (kq .le. 8 ) useGF(kf) = .true. 
        endif
       endif
      enddo
       
 
c check if any active faults, 
c  get MAX_xin, MAX_zin (actual max values)
       nofaults = .true.
         MAX_xin = 0
         MAX_zin = 0
       do i=1,MAX_f
         if (flock(kf)) nofaults = .false.
         MAX_xin = max (MAX_xin, nxf(i) )
         MAX_zin = max (MAX_zin, nzf(i) )
       enddo

c set inversion flags
      invert = .false.
      ksim   = .false.
      grid_search = .false.
      do i=1, nitmax
c       if(icontrol(i).gt.0) invert = .true.
c 7-28-17
       if(icontrol(i).ge.0) invert = .true.
       if(icontrol(i).eq.2) grid_search = .true.
       if(icontrol(i).eq.1) ksim = .true.
      enddo

c* don't make GFs unless inverting
c      if ( .not. invert ) makeGF = .false.

c write fault info
       call wf


c***************************************************
c** assign min/max for transients based on inputs
c***************************************************
c  assign default min/max        
      do ns=1, MAX_srce
        if (sflag(ns)) then
         do ip=1,30
           np=ip+20
           if ( .not. mmparm(ns,ip) ) then
            tminmax(ns,ip,1) = psmin(np)
            tminmax(ns,ip,2) = psmax(np)
           endif
         enddo
        endif
      enddo


c  write min/max        
      if (verbose) then
      print *, 'Transient source constraints'
      print *, 'NS IP NP Prm  Min          Max'
       do ns=1, MAX_srce
        if (sflag(ns)) then

         do ip=1,30
          np = ip+20
c          if ( mmparm(ns,ip) ) 
           write(*, '(3i3,1x,a2,1x,2e12.4)') 
     .      ns, ip, np, p2codes(np), tminmax(ns,ip,1), 
     .      tminmax(ns,ip,2) 
         enddo
        
        
        endif
       enddo
      endif

c****************************************************
c** find the block containing the Insar point
c******************************************************
      do j=1, nblocks
       if(block_flag(j)) then
         nb=nc_block(j) 

       do ib=1, nb 
         xcc(ib)=blockxy(ib,j,1)
         ycc(ib)=blockxy(ib,j,2)
       enddo
       
       do i=1, num_insar
c        nblock_insar(i)=0
        xpt = insar_pos(i,1)
        ypt = insar_pos(i,2)

c** check block for this site
        call inside ( xpt, ypt, xcc, ycc, nb, insde)

         if ( abs(insde).eq.1 ) then 
            if ( nblock_insar(i) .ne. 0 ) then
             print *, insar_sname(i), ' on multiple blocks'
             nblock_insar(i)=-99
            else
             nblock_insar(i)=j
            endif
         endif

         if ( abs(insde).eq.2 ) nblock_insar(i)=-99
        enddo
       endif
      enddo


c***************************************************
c** apply PG: option; GPS file rotation poles
c***************************************************
      do i=1, num_pg
        do j=1,num_gps_file
         if ( gps_fname(j).eq.pg_name(i) ) then
           jp = ngps_index(j)
           do k=1,3
            gps_pole(jp, k) = pg_pole(i,k)
           enddo
         endif
        enddo
      enddo

c****************************************************
c** apply CF - move subsurface nodes to connect faults at depth (end-to-end)
c****************************************************
      if ( nfcncts.gt.0) then
       print *, 'Connecting faults '

      do ii=1, nfcncts
       i1 = nfcnct(ii,1)
       i2 = nfcnct(ii,2)
       yesno = .false.

      print *, 'Connecting faults', i1, ' and',i2

      if ( nxf(i1).gt.1 .and. nxf(i2).gt.1 ) then

c* find intersection of 2 faults
        do ix1=1, nxf(i1)
         do ix2 = 1, nxf(i2)
          if ( xynode(1,ix1,1,i1).eq.xynode(1,ix2,1,i2) .and. 
     .         xynode(2,ix1,1,i1).eq.xynode(2,ix2,1,i2) .and. 
     .         (ix1.eq.1 .or. ix1.eq.nxf(i1))         .and.
     .         (ix2.eq.1 .or. ix2.eq.nxf(i2)) ) then

c node ix1 on fault i1 is the same as node ix2 on fault i2
c average the deeper nodes and reposition both

       nz = max(nzf(i1), nzf(i2))
       yesno = .true.

       do iz=2, nz
         if ( znode(iz,i1).eq.znode(iz,i2)) then
           x = (xynode(1,ix1,iz,i1) + xynode(1,ix2,iz,i2))/2.0d0
           y = (xynode(2,ix1,iz,i1) + xynode(2,ix2,iz,i2))/2.0d0
            xynode(1,ix1,iz,i1)=x
            xynode(1,ix2,iz,i2)=x
            xynode(2,ix1,iz,i1)=y
            xynode(2,ix2,iz,i2)=y
         endif
       enddo

         endif

         enddo
        enddo

       if ( .not. yesno) print *, 'CF: Faults', i1, ' and', i2, 
     .    ' do not connect at ends'

       else

       if (nxf(i1).eq.0) write(*,'("Fault ", i3, " not found")' ) i1
       if (nxf(i2).eq.0) write(*,'("Fault ", i3, " not found")' ) i2

       endif

       enddo
       endif


c****************************************************
c** apply MF - merge subsurface nodes to connect faults at depth (in T-junction)
c****************************************************

c* fault if1 is truncated against fault if2
c**
      if ( nfmerges.gt.0) then
       print *, 'Merging faults '

      do ii=1, nfmerges
       if1 = nfmerge(ii,1)
       if2 = nfmerge(ii,2)
       yesno = .false.

      if ( nxf(if1).gt.0 .and. nxf(if2).gt.0 ) then

c* find intersection of 2 faults
        do ix1=1, nxf(if1)
         do ix2 = 1, nxf(if2)
          if ( xynode(1,ix1,1,if1).eq.xynode(1,ix2,1,if2) .and. 
     .         xynode(2,ix1,1,if1).eq.xynode(2,ix2,1,if2) ) then

c surface node ix1 on fault if1 is the same as surface node ix2 on fault if2
c ix1 should be first or last node on fault if1

      write (*,'( "Merging: Flt ",i2," Node ",i2," with Flt ",i2 ,
     .   " Node ",i2, " at (",2f10.4,")" )' )
     .   if1, ix1, if2, ix2, xynode(1,ix1,1,if1),xynode(2,ix1,1,if1)
     
      yesno = .true.

c* intersecting node of fault 1 must be its first or last
c* node on fault 1 that touches fault 2
      if (ix1.eq.1) then
       nodef1_1 = 1
       nodef1_2 = 2
       newnode = 1
      elseif (ix1.eq.nxf(if1)) then
       nodef1_1 = ix1 -1
       nodef1_2 = ix1 
       newnode = ix1
      else
       write(*, '("Faults",2i3," Invalid MERGE, not T-junction")') 
     .    if1,if2
       newnode = 0
      endif

      if ( newnode.gt.0 ) then

c** nodes on fault 2, look 1 node to either side
      nodef2_1 = max( ix2-1, 1)
      nodef2_2 = min( ix2+1, nxf(if2) )

      do 255 iz = 2, nzf(if1)

      fexist = .false.

c* set up plane for fault 1 using top triangle
        xt1(2) = xynode(1,nodef1_1,iz-1,if1)
        yt1(2) = xynode(2,nodef1_1,iz-1,if1)
        zt1(2) = znode(iz-1,if1)
        xt1(1) = xynode(1,nodef1_2,iz-1,if1)
        yt1(1) = xynode(2,nodef1_2,iz-1,if1)
        zt1(1) = znode(iz-1,if1)
        xt1(3) = xynode(1,nodef1_1,iz,if1)
        yt1(3) = xynode(2,nodef1_1,iz,if1)
        zt1(3) = znode(iz,if1)
        zpt = zt1(3)

c** find intersection with fault 2
       do ixx = nodef2_1, nodef2_2 - 1
        do iz2 = 2, nzf(if2)

c* set up quadrilateral as search area
         xt3(1) = xynode(1,ixx,iz2-1,if2)
         yt3(1) = xynode(2,ixx,iz2-1,if2)
         xt3(2) = xynode(1,ixx+1,iz2-1,if2)
         yt3(2) = xynode(2,ixx+1,iz2-1,if2)
         xt3(3) = xynode(1,ixx+1,iz2,if2)
         yt3(3) = xynode(2,ixx+1,iz2,if2)
         xt3(4) = xynode(1,ixx,iz2,if2)
         yt3(4) = xynode(2,ixx,iz2,if2)

c* do upper and lower triangles
         do itri =1,2

c* set up plane for fault 2 using top triangle
       if (itri.eq.1) then
        xt2(1) = xt3(2)
        yt2(1) = yt3(2)
        zt2(1) = znode(iz2-1,if2)
        xt2(2) = xt3(1)
        yt2(2) = yt3(1)
        zt2(2) = znode(iz2-1,if2)
        xt2(3) = xt3(4)
        yt2(3) = yt3(4)
        zt2(3) = znode(iz2,if2)
       else
        xt2(1) = xt3(2)
        yt2(1) = yt3(2)
        zt2(1) = znode(iz2-1,if2)
        xt2(2) = xt3(3)
        yt2(2) = yt3(3)
        zt2(2) = znode(iz2,if2)
        xt2(3) = xt3(4)
        yt2(3) = yt3(4)
        zt2(3) = znode(iz2,if2)
       endif

        call tfaults ( xt1, yt1, zt1, xt2, yt2, zt2, xpt, ypt, zpt)
        n=4
        call inside ( xpt, ypt, xt3, yt3, n, insde)

        if ( xpt.ne.0.0d0 .and. ypt.ne.0.0d0 .and. 
     .        abs(insde).gt.0 ) then
         xynode(1,newnode,iz,if1) = xpt
         xynode(2,newnode,iz,if1) = ypt
         write(*,'("New node x,y,z ",3f10.4)' ) xpt, ypt, zpt
         fexist = .true.
         goto 255
        endif

        enddo
        enddo
       enddo
         if ( .not. fexist ) write(*, '( "Faults ",2i3,
     .     " New node not found at Z =", f10.3 )' )
     .       if1, if2, zpt

 255  continue

      endif

      endif

      enddo
      enddo

      if ( .not. yesno) write (*, '("Faults", 2i3,  
     .    " do not intersect")' ) if1, if2 

      else

       if (nxf(if1).eq.0) write(*,'("Fault ", i3, " not found")' ) if1
       if (nxf(if2).eq.0) write(*,'("Fault ", i3, " not found")' ) if2

      endif

      enddo
      endif

c****************************************************
c** apply FX - force node points to new positions
c****************************************************
      do i = 1, num_fx_change
       kf = kfx_change(i,1)
       ix = kfx_change(i,2)
       iz = kfx_change(i,3)
       xynode(1, ix, iz, kf) = fx_change(i,1)
       xynode(2, ix, iz, kf) = fx_change(i,2)
      enddo

c****************************************************
c** apply EQ with 'p' sub-option for node positions
c**    set node position to one from other fault
c**    second node listed takes position of first
c****************************************************
      if ( npars_eq.gt.0 ) then
       do i=1, npars_eq
        if ( npar_eq_type(i).eq.1 ) then
         kf1 = npar_eq(i,1)
         kf2 = npar_eq(i,4)
         ix1 = npar_eq(i,2)
         iz1 = npar_eq(i,3)
         ix2 = npar_eq(i,5)
         iz2 = npar_eq(i,6)
         xynode(1, ix2, iz2, kf2) = xynode(1, ix1, iz1, kf1) 
         xynode(2, ix2, iz2, kf2) = xynode(2, ix1, iz1, kf1) 
        endif
       enddo
      endif

      
c****************************************************
c****************************************************
c**  setup tf_parm profile lines for transients
c****************************************************
c****************************************************
      if (do_trans) then
      do nt=1,MAX_srce
       kf = info_source(nt,1)
       kq = info_source(nt,2)
c       print *, 'Srce dtau ', nt, dtau(nt)
        if ( kq.eq.3 .or. kq.eq.4) then
         do ix = 1, nxf(kf)
          n1 = tnode_prof(nt,ix)
          if (n1.eq.0 ) then
           do j=1,3
            tf_parm(nt, ix, j) = z
           enddo
          else
           do j=1,3
            tf_parm(nt, ix, j) = twin(nt,n1,j)
           enddo
          endif
         enddo
        endif
      enddo
      endif
          
c****************************************************
c*** check hard constraints - see that they are applied to separate blocks
c*** and that the blocks have poles assigned
c****************************************************
       do i=1,num_hc
        hc_flag(i) = .true.
         k1 = hc_block(i,1)
         k2 = hc_block(i,2)
         if (npole_block(k1).eq.npole_block(k2) .and. 
     .    nstrain_block(k1).eq.nstrain_block(k2) ) hc_flag(i) = .false.
         if (npole_block(k1).eq.0 .or. 
     .    npole_block(k2).eq.0 ) hc_flag(i) = .false.
        enddo

c****************************************************
c** find block that each node is under and use it as 
c**   the hanging wall block
c****************************************************
      print *,'Getting fault hangingwall (HW) and footwall (FW) blocks'
      print *,'  for nodes; see '//expname//'_fault.info file'

      call fopen (kkk,  1, '_fault.info ')
c"Fault name    F   X   Z   Long      Lat       Dep  HW   FW"
cCascadia      1   1   1 234.8000  40.5200    0.00 WCCR JdFa                               

        write (kkk, '("Fault name    F   X   Z   Long      Lat",
     .      "       Dep  HW   FW")')

      do kf=1, nfault
       if ( fflag(kf,4) .and. nzf(kf).gt.1 )  then

c-- original HW/FW assignments
       hw = fwname(kf,1)
       fw = fwname(kf,2)
       block_check = .true.
       call getplate (hw, khw1, block_check)
       call getplate (fw, kfw1, block_check)

c        khw1 = khw_block(1,1,kf)
c        kfw1 = kfw_block(1,1,kf)

       nx = nxf(kf)

c-- get footwall and hanging wall blocks from the middle of the fault
c     it is the same for all nodes on this fault 

c foot wall
          midx = nint(real(nxf(kf))/1.99d0)
          ddx=-0.005
          call getnodexy(xpt, ypt, kf, midx, 1, ddx)
           if ( nx.eq.2) then
            call getnodexy(xpt1, ypt1, kf, 1, 1, ddx)
            call getnodexy(xpt2, ypt2, kf, nx, 1, ddx)
            xpt = (xpt1+xpt2)/two
            ypt = (ypt1+ypt2)/two
           endif
          call nodeblock(xpt, ypt, kfw)

c hanging wall
          ddx= 0.005
           if ( nx.eq.2) then
            call getnodexy(xpt1, ypt1, kf, 1, 1, ddx)
            call getnodexy(xpt2, ypt2, kf, nx, 1, ddx)
            xpt = (xpt1+xpt2)/two
            ypt = (ypt1+ypt2)/two
           endif
          call getnodexy(xpt, ypt, kf, midx, 1, ddx)
          call nodeblock(xpt, ypt, khw)

c       hw = '    '
c       if (kfw.ne.kfw1 .or. khw.ne.khw1) msg = '****'
       
c       write(kkk,'(i3,1x,a10, 2(" HW ",a4," FW ",a4),1x,a4)') 
c     .    kf, fault_name(kf), 
c     .    block_name(khw1), block_name(kfw1),
c     .    block_name(khw),  block_name(kfw), hw

c-- don't change 
      if ( .not. adjust_hw) khw = khw1
      if ( .not. adjust_fw) kfw = kfw1
    
c-- get hangingwall block for each node      
       kfw_blk(1,1,kf) = kfw
       khw_blk(1,1,kf) = khw

c      if (nzf(kf).gt.1 ) then
       do ix=1, nxf(kf)
        do iz=1, nzf(kf)
          kfw_blk(ix,iz,kf)=kfw
c          call clearchar(msg,30)
          msg = ""

c-- get hangingwall block for node
         if ( adjust_hw ) then
          ddx=0.005
          call getnodexy(xpt, ypt, kf, ix, iz, ddx)
          call nodeblock(xpt, ypt, khw)
         endif

          khw_blk(ix,iz,kf)=khw

          if (khw.eq.kfw ) 
     .     msg = ' same FW and HW block: '//block_name(kfw)

          if (khw.eq.0 ) 
     .     msg = ' no HW block '

          khwp = npole_block(khw)
          kfwp = npole_block(kfw)
          khws = nstrain_block(khw)
          kfws = nstrain_block(kfw)

          if ( khwp.eq.kfwp .and. khws.eq.kfws )  
     .     msg = ' same FW / HW pole and strain'

          d=0.0d0
          call getnodexy(xpt, ypt, kf, ix, iz, d)
          write (kkk, '(a10,1x,3i4,2f9.4, f8.2, 2(1x,a4),1x,a30)') 
     .         fault_name(kf),kf,ix,iz,xpt,ypt,znode(iz,kf), 
     .         block_name(khw),block_name(kfw), 
     .         msg


        enddo
       enddo
c       endif
       endif
      enddo
      i = kfclose(kkk)

c****************************************************
c-- check GPS and InSAR for redundant locations
c****************************************************
      call parseGPS
      call parseINSAR

c****************************************************
c** parameter min/max
c****************************************************
      if(verbose) then
       print *, 'Parameter constraints'
       print *, 'No Parameter             Min            Max'
       do i=1,nparm_types
        write (*,'(i2,1x,a15, 2e15.4)') i, ptype(i), 
     .    psmin(i), psmax(i)
       enddo
      endif

       
c****************************************************
c** find blocks for grid points, flag if on fault
c******************************************************
      if (make_grid) then

c** first pt for each grid 
       ig = 0
       do kgg=1,MAX_grids
        call gridinfo(kgg, x0grid, nxgrid, dxgrid, 
     .    y0grid, nygrid, dygrid, nxyg )
         if(nxyg.gt.0) then
            grid1(kgg) = ig
            ig = ig + nxyg

       print *, 'Grid ',kgg,' first pt ', grid1(kgg)
            if ( ig. gt. MAX_gridpts) then
              print *, '**** MAX_gridpts exceeded '
              stop
            endif
         endif
       enddo


      do j=1, nblocks
       if(block_flag(j)) then
         nb=nc_block(j) 

       do i=1, nb 
         xcc(i)=blockxy(i,j,1)
         ycc(i)=blockxy(i,j,2)
       enddo

c-- loop through gridded surface points
       do kgg=1,MAX_grids

        call gridinfo(kgg, x0grid, nxgrid, dxgrid, 
     .    y0grid, nygrid, dygrid, nxyg )

        if(nxyg.gt.0) then
   
       do ky=1,nygrid
        do kx=1,nxgrid

        call gridxyk (kgg, kx, ky, xpt, ypt, kk, kb )

        call inside ( xpt, ypt, xcc, ycc, nb, ia)

        if ( ia.eq.1) jblock_grid(kk) = j

        if ( ia.eq.2) then
            on_fault(kk) = .true.
            print *, 'Grid pt ', kk, kgg, kx, ky, xpt, 
     .           ypt, ' on boundary', j
          call gridxyk (kgg, kx, ky, xpt, ypt, kk, kb )
          call inside ( xpt, ypt, xcc, ycc, nb, ia2)
          if ( ia2.eq.1) jblock_grid(kk) = j
          if ( ia2.eq.1) print *, 'Moved to block ', j
          if ( ia2.eq.2) print *, 'Moved, still on edge'
        endif

       enddo
      enddo
      endif
      enddo
      endif
c* end of checking grid pts      
      enddo

       endif

c******************************************************
c**  remove GPS points not in any block, those 
c**   requested to be removed by user, with too high sigmas
c**   or with no pole assigned
c******************************************************
      print *, 'Removing unwanted GPS .... '
      print *, nrm_site, ' RM: lines '

      nremoved = 0
      ntotal = num_gps
      call clearint(nrm,10)
      
       do  i = 1, num_gps
        kreason = rm_reason(i)
        if(kreason.gt.0 .and. kreason.le.10) then
          nrm(kreason)=nrm(kreason)+1
          nremoved=nremoved+1
        endif
      enddo

      num_gps_used = 0
      
      do i=1,num_gps
       if (gps_type(i).gt.0) num_gps_used=num_gps_used+1
      enddo

      print *, ntotal, ' Total GPS sites  '
      print *, nremoved, ' Number removed: see ', 
     .      fnout('_removed.vec ')
      print *, nrm(1), ' not in any block '
      print *, nrm(2), ' in removed block(s) '
      print *, nrm(3), ' by user request '
      print *, nrm(4), ' sigma too large '
      print *, nrm(5), ' on block boundary/multiple blocks '
      print *, nrm(6), ' in block w/o pole '
      print *, nrm(7), ' in removed region(s) '
      print *, nrm(8), ' equated '
      print *, nrm(9), ' outside data region '
      print *, nrm(10), ' sigma too small '
      
c      ik = kfclose (kl)


c******************************************************
c* assing GFs for mantle relaxation events
c******************************************************
c MR: # Flag filename Amp Eq_lon Eq_lat Radius Q_Year V_year
      do n = 1, MAX_mrlx_files
         np = mrlx_pts(n)
         tol = 5.0d0
       if(np.gt.0) then
         xe = rlxParms(n,2)
         ye = rlxParms(n,3)
         rade = rlxParms(n,6)
         do i=1,num_gps
          xg = gps_pos(i,1)
          yg = gps_pos(i,2)
          if (iclose(xg,yg,xe,ye,rade).eq.1) then
           do j=1,np
            if (iclose(xg,yg,rlxGF(n,j,1),rlxGF(n,j,2), tol).eq.1) 
     .        nGF_rlx(i,n) = j
           enddo
          endif
         enddo
       endif
      enddo


c******************************************************
c* find blocks for strain network points
c******************************************************
      if ( num_ss.gt.0) then
       do i=1,num_ss
        do k=1, 9
          call get_ss_pos (i, k, xpt, ypt)
          call getblock(xpt, ypt, kb)
          ss_block(i,k) = kb
          if (kb.eq.0) print *, 'Strain datum ', i, ' ',
     .       ss_name(i), ' point ', k,' is outside model'
        enddo
       enddo
      endif

c******************************************************
c* find blocks for line length points
c******************************************************
      if ( num_ll_sites.gt.0) then
       do i=1,num_ll_sites
         xpt = pos_ll(i,1)
         ypt = pos_ll(i,2)
          call getblock(xpt, ypt, kb)
          ll_block(i) = kb
          if (kb.eq.0) print *, 'Line length station ', 
     .       name_ll(i), ' is outside model'
       enddo
      endif

c******************************************************
c* write out the number of times each MOVE was applied
c******************************************************
      if ( nedits.gt.0) then
       print *, 'Number of ADDS/DELETES/MOVES'
       do i =1,nedits
        write (*, '(i4, 2f10.4, a3, 2f10.4)') edit_flag(i), 
     .     (ptedit(i,j), j=1,2), ' to',(ptedit(i,j), j=3,4)
       enddo
      endif

c******************************************************
c* find the site numbers for the requested offsets
c******************************************************
      if ( num_segs.gt.0 )  then
        do i=1,num_segs
          do ic= 1,num_gps
           if( site_segs(i,1).eq.gps_name(ic) .and. 
     .         site_segs(i,2).eq.gps_fname(gps_index(ic)) ) 
     .         i_segs(i,3) = ic
          enddo
        enddo
      endif
c******************************************************
c** points and pointers for profiles
c** profile points are held in 1-D array
c******************************************************

       kfirst_point(1)=1
       do i=2,nlines
         kfirst_point(i)=kfirst_point(i-1)+
     .         prof_n(i-1)
       enddo  

c* get profile points along great circle path

      if (nlines.gt.0) then
      print *, 'Setting up ',nlines,' profiles '
       k=0
       xpt = 0.0d0
       ypt = 0.0d0

       do 1310 kline=1, nlines

         xlon0 = prof_start(kline,1)
         xlat0 = prof_start(kline,2)
         del = prof_dx(kline)
         az =  prof_az(kline)

          C = az*d2r 
          d = del*d2r * dsin(C)
          dLo = - d
          xlat = xlat0*d2r

       do 1310 kdata= 1, prof_n(kline)

        if ( dabs(d).gt.0.0d0 ) then

          dLo = dLo + d
          ypt = datan ( ( dcos(C) * dsin(dLo) / dsin(C) + dsin(xlat) 
     .       * dcos(dLo) ) / dcos(xlat) ) 
          ypt = ypt*r2d
          xpt = xlon0 + dLo*r2d

        else

          xpt = xlon0
          r = real(kdata-1)
          if ( az.eq.0.0d0 )   ypt = xlat0 + r*del
          if ( az.eq.180.0d0 ) ypt = xlat0 - r*del

        endif

          if ( az.eq.90.0d0 ) ypt = xlat0 
          if ( az.eq.270.0d0 ) ypt = xlat0 

          k=k+1
c          write(*, '(2f10.4)') kline, k, xpt, ypt
          if(k.gt.MAX_pr_pts ) then
            print *, 'MAX_pr_pts exceeded '
           call stop1
          endif
          prof_pos(k,1)=xpt
          prof_pos(k,2)=ypt

 1310 continue

      endif


c******************************************************
c* read latest model from parameter file
c******************************************************
      parmsread = .false.
      if (iparmread.eq.1 .or. iparmread.eq.3) 
     .     call rwparms(1)

c****************************************************
c** write out initial blocks and poles  
c******************************************************
cBlock  Pole Free Strain Free  --------- Pole --------- ------ Strain Tensor -----
cNoAm      1 NO        0 NO    0.0000   0.0000   0.0000   0.0000   0.0000   0.0000
 
      if(verbose) then
       print *, 'Block  Pole Free Strain Free  '//
     .          '--------- Pole ---------  '//
     .          '------ Strain Tensor ---  '//
     .          ' --Lon--- --Lat--- --Damp-- ---Max-- '
       do j=1, nblocks
        if (block_flag(j)) then
        cp = 'NO '
        cs = 'NO '
        np = npole_block(j)
        ns = nstrain_block(j)

       do i = 1, num_pole_invert
        if ( npole_invert(i).eq.np ) cp = 'YES'
       enddo

       do i = 1, num_strain_invert
        if ( nstrain_invert(i).eq.ns ) cs = 'YES'
       enddo

        if ( block_name(j) .ne. 'xxxx' )
     .   write (*, '(1x,a4,i7,1x,a3,i8,1x,a3,10f9.4)') 
     .     block_name(j), npole_block(j), 
     .     cp, nstrain_block(j), cs,
     .     (poles(np,i),i=1,3),(strain(ns,i),i=1,3),
     .     (strain2(ns,i),i=1,4)
        endif
       enddo
      endif

c******************************************************
c* set amplitude of sources to area under curve
c******************************************************
      if (do_trans) call stfarea
      

c************************************************************************
c** set up phi and phi_free arrays
c* for main interseismic locking:
c** phi(ix,iz,kf) has node values
c** nphi(ix,iz,kf) has node indices
c** phi_free() flag indicates whether node is fixed or free in inversion
c** phi_err() will hold the node's uncertainty
c**
c** for transient sources
c** tphi(ix,iz,nt) has node values for transients
c************************************************************************
      print *, 'Setting up phi values '
      
    
      call initialize_phi


      if (do_trans) call set_tphi(0)

c      print *, '....... done Setting up phi values '

c******************************************************
c**** rotate ref frame   **********************
c******************************************************

        do i=1, num_gps_poles
         do j=1,3
          gps_pole(i,j) = gps_pole(i,j) - frame_pole(j)
         enddo
        enddo

        do i=1, num_poles
         do j=1,3
          poles(i,j) = poles(i,j) - frame_pole(j)
         enddo
        enddo
      
c******************************************************
c* override pole values read in from parameter file
c******************************************************

      do i=1,num_fixd_bl_pole
        n = nfixd_bl_pole(i)
        do j=1,9
         poles(n,j) = fixd_bl_pole(i,j)
        enddo
c        do j=4,10
c         poles(n,j) = zero
c        enddo
      enddo

      do i=1,num_fixd_pg_pole
        n = nfixd_pg_pole(i)
        do j=1,3
         gps_pole(n,j) = fixd_pg_pole(i,j)
        enddo
        do j=4,9
         gps_pole(n,j) = zero
        enddo
      enddo


c******************************************************
c* write model as is
c******************************************************
      call writemodel(izero)

c******************************************************
c***** solve ****************************
c******************************************************

c      if ( invert ) then
c          print *, 'Solve '
         call defsa(kend)
         if (kend) then
          print *, 'Premature end'
          goto 9999
         endif
c      endif

c******************************************************
c***** do forward problem at end **********************
c******************************************************

c* set flags so uncertainties are calculated
      if ( getcovariance) then
        vel_errors = .true.
        get_phi_err = .true.
      endif

c* flag indicating inversion was run
c      if ( invert ) 
        ksim_done = .true.
c      if ( invert ) 
        call update_phi

      print *, 'Forward run'
      call clearcalc  

      n = 9*MAX_pr_pts
      call cleareal( u_lines, n)

      n = 15*MAX_gridpts
      call cleareal( u_grid, n)

      print *, 'Getting node vectors '
      call nodevectors

      print *, 'Adding block rotation vectors '
      call addblocks

      print *, 'Adding network vectors '
      call network

      print *, 'Calculating elastic deformation ..... '
      call RECTCALC
c      print *, ' ..... done'

c get ref site velocity
      call refsitevel

      if (do_dgt .or. make_grid ) then
        print *, 'Calculating residual strain rates '
        call blockstrain
      endif

c* write out new parameter file
c      call dater(time_string)

      if (iparmread.eq.2 .or. iparmread.eq.3) then
        call rwparms(itwo)
        if ( pioflag ) call rwparms(5)
      endif

c**********************************************************************
c*** OUTPUT results to files
c**********************************************************************
      print *, 'Finished forward run, writing results'

      print *, 'Writing summary'
      call fopen (ksum,  1, '.sum ')
      
      write (ksum, '(5x,a90)') 'Modl YYYYMMDDHHMM    Misfit   DOF  '//
     .  'Ndata  Nparm    Prob    Variance   Data Chi2    Penalty'
      write (ksum, '(a100)') SUMline
      write (ksum, *) ' '
      write (ksum, *)'Indx Code   WtFac  SigMin  SigMax R/S_Max'//
     .  '  ---Reference Velocity-   File name'
      do i = 1, num_gps_file
        w = 0.0d0
        if ( gps_wt(i,1) .ne. 0.0d0 ) w = 1.0d0/(gps_wt(i,1)**2)

        nfno = gps_info(i,1)

        write (ksum,'(i3, 3x, a4, 7f8.2,2x,a80)') 
     .     nfno, gps_fname(i), 
     .     w, (gps_wt(i,j), j=2,4), (ref_vel(nfno,j), j=1,3), 
     .     adjustl(gps_filename(i))
   
      enddo

      write (ksum, *) ' '
      write(ksum,*)'Name   #obs      Nrms      Wrms  Prob     SumWt'//
     .  '     SSData      SSfit      SSres    Chi2/N   Filename'
c SR       60     0.892     0.636  87.4  1.178E+02  3.771E+04  3.679E+04  4.771E+01  7.951E-01                               
c
c      write(ksum,*)'Name   #obs    '//
c     . '    Nrms     Wrms    Prob SumWt       SSData      SSfit'//
c     . '      SSres  Chi2/N   Filename'

      print *, 'Writing outputs '
c* hard constraints
      call writehc
      call writetilts(tchi)
      call writesr(srchi)      
      call writesv(svchi)      
      call writess(sschi) 
      call writell(chill) 
      call writerot(rotchi) 
      call writeinsar
      if (num_ts.gt.0 )  call write_ts_data
      
      print *, 'Writing GPS '
      call writegps(gchi)
      
      ik = kfclose (ksum)

c write moment tensors
      call writemts

c** output poles
      print *, 'Writing poles and strain tensors'
      call pole_strain_out

c** write out model details
      print *, 'Writing model '
      call writemodel(izero)

c-- write penalties  
      print *, 'Writing penalties '
      call penalty(pe1, mp, 1)

c** slip at faults
      print *, 'Writing fault slip '
      call faultslip
      if (scec) call scecslip

c-- node file
      print *, 'Writing nodes '
      call nodevectors
      
c* transients
      if (do_trans) call writetrans 

c**  make profile files     
      if ( nlines.gt.0 .and. kcalculate ) call defprofiles

c-- output grid file for contouring and GMT vectors
      if (make_grid) call writegrids
      
c** write out misc points 
      if (ndef_pts.gt.0) then
      print *, 'Misc points '
       call fopen (k12, 1, '.pts ')
        do i=1, ndef_pts
        call getblock(pdef_pt(i,1), pdef_pt(i,2), kbl)
        if ( kbl.gt.0) then
        call relvel (3, kbl, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho)
       
c        call distkm(pdef_pt(i,1), pdef_pt(i,2),transient(1,1),
c     .   transient(1,2), d)
         write(k12, '(a4,1x,a4,5f10.3)') 
     .    name_pt(i), block_name(kbl),        
     .    (pdef_pt(i,j), j=1,2), Vx+u_pt(i,1), Vy+u_pt(i,2), u_pt(i,3)
        endif
        enddo
       ik = kfclose(k12)
      endif

c**********************************************************************
c* output fault information 
      call writefaultinfo

c*********************************************************************
      print *, 'Writing block files'
      call wblock

c* write common block boundaries to blk2 file
c      if (nblocks.gt.1) call wblock2
      if (nblocks.gt.1) call wblock3
      if (nblocks.gt.1) call wblock0
    
c* write blocks/faults to Mapinfo mid/mif files
      if (mapinfo) call wmapinfo

c* write faults to kml file
      if (makekml) call make_kml 

c* make .atr file of time slice of transients
      call get_t_slice


c*********************************************************************
c* write block strain rates at points in file, for NSHM2014
c*********************************************************************
      out_strain_pts = .true.

c* see if external file is to be read
       bfile = 'strain_pts.in'
       call existfile ( bfile, fexist, 0)
      
      if(myflag .and. fexist .and. out_strain_pts) then
       x3 = 1.0d3
       open (11, file = 'strain_pts.in' )
c       open (12, file = 'strain_pts.out' )
       call fopen (k12, 1, '_strain_pts.out ')

       do k=1, 100000
        read (11, *, end = 399 ) xpt, ypt
        xpt = fnlong(xpt)
        call getblock(xpt, ypt, kb)
        SExx = zero
        SEyy = zero
        SExy = zero
        Exx = zero
        Eyy = zero
        Exy = zero
        E1 = zero
        E2 = zero
        alph = zero
        ns = 0
        if (kb.gt.0 .and. kb .le. MAX_strain) then
          ns = nstrain_block(kb)
          Exx = strain(ns,1)*x3
          Eyy = strain(ns,2)*x3
          Exy = strain(ns,3)*x3
       call prinaxes ( Exx, SExx, Exy, SExy, Eyy, SEyy, 
     .   e1, sige1, e2, sige2, alph1, siga1, alph2, siga2)

        endif

          write (k12, '(2f10.4, 2i4, 6f10.2)') xpt, ypt, kb, ns, 
     .      Exx, Eyy, Exy, E1, E2, fn360(alph1)
       enddo
        
 399  close(11)
       ik = kfclose(k12)
      endif
c*********************************************************************

c close all files
     
      do  i=10,99
       if( fileopen(i) .and. verbose .and. myflag ) 
     .       print *, 'Unit ',i, ' open'
       jj = kfclose(i)
      enddo

9999  print *, 'DONE - NORMAL FINISH ', expname

c      print *, 'Ref site/vel ', ref_site(3), 
c     . ref_vel(3,1), ref_vel(3,2), ref_vel(3,3)

      if (interactive) goto 1111
      goto 777

8888  print *, 'DONE - ABORTED ' 
       
 777  continue
      end

c*********************************************************************
c** ALL DONE
c*********************************************************************
c
c*********************************************************************
c** Functions
c*********************************************************************

c* see if input line is a comment line
c*    i.e., if first character in line is  *, #, or '
      function fncomment(c) 
        character*1 c
        logical fncomment
        fncomment = ( c.eq.char(35) .or. c.eq.char(39) .or. 
     .   c.eq.char(42))
      return  
      end

c*********************************************************************
c** YES or NO
      function fnyesno(ic) 
        character*3 fnyesno
        logical ic
        fnyesno = ' NO'
        if (ic) fnyesno = 'YES'
      return  
      end

c******************************************************
c write bad entry line to screen
c* common for inpur error quit
      subroutine badline( jj, c2)
       character*2 c2
       integer jj
       logical quit_on_error, input_error
       common /er/ quit_on_error, input_error

       input_error = .true.
       write (*, '("*** Invalid ", a2, " input at line ", i5)') 
     .      c2, jj

        if(quit_on_error) call stop1

       return
       end

c******************************************************
      subroutine stop1
         print *, 'QUITTING PREMATURELY'
         stop
       return
       end
c******************************************************
      function kfopen (kin)
      
c* assign a file unit, kin is suggested unit number
      
      logical fileopen
      common /op/ fileopen(100)

       kfopen=kin
       k = kin

c-- open this unit       
       if ( k .ge. 10 .and. k .le. 99) then
         if ( .not. fileopen(k) ) then
          kfopen = k
          fileopen(k)= .true.
          return
         endif
       endif
         

c-- get a new unit number       

        do j=10, 99
         if ( .not. fileopen(j) ) then
          kfopen = j
          fileopen(j)= .true.
          return
         endif
        enddo

      return
      end

c******************************************************
      function kfclose (kin)

      logical fileopen
      common /op/ fileopen(100)
      
       k = kin
       kfclose = k

       if ( k .ge. 10 .and. k .le. 99) then
        if(fileopen(k)) then
         close (k)
         fileopen(k) = .false.
         kfclose = k
c         print *, 'Closed unit = ',k
        endif
      else
c        print *, 'Closed Unit = 0'
        kfclose = 0
      endif

      return
      end
      
c*********************************************************************
c* build and/or open out file, kf is unit number, cext is extension
c -- if k0 = 0, cext has file name
c -- if k0 = 1, build file name

      subroutine fopen(kf, k0, cext) 
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

c      character expname*4
c      common /co2/ expname, runid
      
      logical fileopen
      common /op/ fileopen(100)
      
      logical write_input
      common /cm3/ write_input

      character cext*80, cext2*81, fout*80, fnout*80
            
c      call clearchar(fout,80)
      fout = ""
      
       if ( kf.eq.0 ) kf = 50
       k=kf

c find an open unit number
       k = kfopen(k)
       kf = k

       cext2 = cext//' '
       k2 = index(cext2, ' ')
       fout = cext(1:k2-1)
       if (k0.eq.1) fout = fnout(cext)
       if(verbose) print *, 'Opening unit ', kf, ' as ', fout

       open (unit = kf, file = fout )

      return  
      end

c*********************************************************************
c* append to file, kf is unit number, cext is extension
c -- if k0 = 0, cext has file name
c -- if k0 = 1, build file name

      subroutine fappend(kf, k0, cext) 

      character expname*4
      common /co2/ expname, runid
      
      logical fileopen
      common /op/ fileopen(100)
      
      logical write_input
      common /cm3/ write_input

      character cext*80, cext2*81, fout*80, fnout*80
            
c      call clearchar(fout,80)
      fout = ""
      
       k=kf
       k = kfopen(k)
       kf = k
       cext2 = cext//' '
       k2 = index(cext2, ' ')
       fout = cext(1:k2-1)
       if (k0.eq.1) fout = fnout(cext)
c        print *, 'Opening unit ',kf, ' ', fout

       open (kf, file = fout, access = 'append', status = 'old' )


C  append to an external data file
c      OPEN (7, FILE = 'DATA01.TXT', ACCESS = 'APPEND',STATUS = 'OLD')
      
      return  
      end
c*********************************************************************
c* build the file name, cext is the extension
      function fnout(cext) 

      character expname*4
      common /co2/ expname, runid
      character cext*30, cext2*30, exp1*10, fnout*80
      
c      call clearchar(fnout,80)
      fnout = ""
      
      exp1 = expname//' '
      cext2 = cext//' '
      k  = index(exp1, ' ')
      k2 = index(cext2, ' ')
      if (k.gt.1) fnout = exp1(1:k-1)//'/'//exp1(1:k-1)//cext(1:k2-1)
      return  
      end

c*********************************************************************
c ** read end
      function fnend(a) 
        logical fnend
        character*3 a
        fnend = ( a.eq.'end' .or. a.eq.'END' )
      return  
      end

c*********************************************************************
c ** value in limits
      function fnilim(i1, i2, i3) 
        logical fnilim
        fnilim = ( i1 .ge. i2 .and. i1 .le. i3)
      return  
      end

c*********************************************************************
c ** value in limits
      function fnrlim(r1, r2, r3) 
        implicit real*8 (a-h,o-z)
        logical fnrlim
        fnrlim = ( r1 .ge. r2 .and. r1 .le. r3)
      return  
      end

c*********************************************************************
c* see if 2 lon/lat points are close, within tolerance tolr (in km)
      function iclose(x1i, y1i, x2i, y2i, tolr) 
       implicit real*8 (a-h,o-z)
       iclose = 0

        if ( x1i.eq.x2i .and. y1i.eq.y2i) then
         ddd = 0.0d0
        else
         call distkm (x1i, y1i, x2i, y2i, ddd)
        endif

        if ( ddd .le. tolr ) iclose = 1

      return  
      end

c*********************************************************************
c-- moment to Mw
      function fnMw(x)
      implicit real*8 (a-h,o-z)
       fnMw = -5.0d0
       if (x.gt.0.0d0) fnMw = 2.0d0/3.0d0 * (dlog10(x) - 9.1)
      return
      end

c*********************************************************************
c-- put longitude into 0 to 360 range, or -180 to 180 (if long_pos set)
      function fnlong(x)
       implicit real*8 (a-h,o-z)
       include "tdefcom1.h"
        fnlong= fn180(x)
        if ( long_pos ) fnlong= fn360(x)
      return
      end

c*********************************************************************
c-- put azimuth into 0 to 180 range
      function fn0to180(x)
      implicit real*8 (a-h,o-z)
       fn0to180=x
       if (x.lt.-180.0d0 ) fn0to180= x + 360.0d0
       if (x.lt.   0.0d0 ) fn0to180= x + 180.0d0
       if (x .gt.  360.0d0 ) fn0to180= x - 360.0d0
       if (x .gt.  180.0d0 ) fn0to180= x - 180.0d0
      return
      end

c*********************************************************************
c-- put longitude into -180 to 180 range
      function fn180(x)
      implicit real*8 (a-h,o-z)
       fn180=x
       if (x.lt.-180.0d0 ) fn180=x + 360.0d0
       if (x .gt.  180.0d0 ) fn180=x - 360.0d0
      return
      end
      
c*********************************************************************
c-- put longitude into 0 to 360 range
      function fn360(x)
      implicit real*8 (a-h,o-z)
       fn360=x
       if (x.lt.  0.0d0 ) fn360= x + 360.0d0
       if (x.gt.360.0d0 ) fn360= x - 360.0d0
      return
      end

c*********************************************************************
c-- put a real variable x into specified limits
      function rlimit(x, xmin, xmax)
      implicit real*8 (a-h,o-z)
       rlimit=x
       if (x.lt.xmin ) rlimit = xmin
       if (x.gt.xmax ) rlimit = xmax
      return
      end

c*********************************************************************
c-- get max strain of strain terms
      function air_strain(Exx, Eyy, Exy)
      implicit real*8 (a-h,o-z)
      sexx = 1.0d0
      sexy = 1.0d0
      seyy = 1.0d0
      call prinaxes ( Exx, SExx, Exy, SExy, Eyy, SEyy, 
     .    e1, se1, e2, se2, a1, sa1, a2, sa2)

       air_strain = max ( abs(E1), abs(E2) )
       air_strain = max ( air_strain, abs(E1+E2) )

      return
      end
c*********************************************************************
c-- maximum strain | 0.5 (Exx+Eyy) | + sqrt ( ((Exx-Eyy)/2)**2 +Exy**2 ) 
c-
      function str_max(Exx, Eyy, Exy)
      implicit real*8 (a-h,o-z)

       str_max = abs ( 0.5d0 * (Exx + Eyy) )
     . + dsqrt ( 0.25d0 * (Exx-Eyy)**2 + Exy**2 )

      return
      end


c*********************************************************************
c-- put an integer variable k into specified limits
      function ilimit(k, kmin, kmax)
      implicit real*8 (a-h,o-z)
       ilimit=k
       if (k.lt.kmin ) ilimit = kmin
       if (k.gt.kmax ) ilimit = kmax
      return
      end

c*********************************************************************
c* return grid parameters      
      subroutine gridinfo(ng, x0grid, nxgrid, dxgrid, 
     .  y0grid, nygrid, dygrid, nxyg )

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      x0grid = pgrid(ng,1)
      nxgrid = int(pgrid(ng,2))
      dxgrid = pgrid(ng,3)
      y0grid = pgrid(ng,4)
      nygrid = int(pgrid(ng,5))
      dygrid = pgrid(ng,6)
      nxyg = nxgrid*nygrid
      
      return 
      end

c*********************************************************************
c get grid point coordinates and index
      subroutine gridxyk (ng, kx, ky, xpt, ypt, kk, kb )
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      x0grid = pgrid(ng,1)
      nxgrid = int(pgrid(ng,2))
      dxgrid = pgrid(ng,3)
      y0grid = pgrid(ng,4)
      nygrid = int(pgrid(ng,5))
      dygrid = pgrid(ng,6)

      kk = grid1(ng) + kx + (ky-1)*nxgrid
      kb = jblock_grid(kk)

      xpt = x0grid + real(kx-1)*dxgrid
      ypt = y0grid + real(ky-1)*dygrid

      if (on_fault(kk) ) then
        xpt =xpt+1.0d-2
        ypt =ypt+1.0d-2
      endif

      return 
      end


c*********************************************************************
c*    geographic conversions
c*********************************************************************


c-- lon, lat to x,y; return position in km
      subroutine project( xlon, ylat, xl0, yl0, xx, yy)
      implicit real*8 (a-h,o-z)
      include "tdefcons.h"
      xx = (xlon-xl0)* d2x * cos(d2r*yl0) 
      yy = (ylat-yl0)* d2x
      return
      end

c-- lon, lat to x,y; return position in km
c-- get modified position for Okada, to apply spherical corrections per
c--   Segall (2010) section 8.4
c***********************************************************************

      subroutine ps_project( xlon, ylat, xl0, yl0, xx, yy)
      implicit real*8 (a-h,o-z)
      include "tdefcons.h"
      xx = (xlon-xl0)* d2x * cos(d2r*yl0) 
      yy = (ylat-yl0)* d2x
c      rp = dsqrt(xx*xx+yy*yy)
c      rfac = 1.0d0/6.0d0 * (rp/Erad)**2
c      xx = xx * (one - rfac)
c      yy = yy * (one - rfac)
      return
      end

c*********************************************************************
c-- X Y to lat lon
      subroutine unproject( xin, yin, xlon0, ylat0, xout, yout)
      implicit real*8 (a-h,o-z)
      include "tdefcons.h"
       xout = xin*x2d / dcos(d2r*ylat0)  + xlon0
       yout = yin*x2d + ylat0
      return
      end

c*********************************************************************
c-- get weight from sigma
      function fnwt(s)
      implicit real*8 (a-h,o-z)
      if (s.gt.1.0d-5) then 
        fnwt=1.0d0/(s*s)
      else
        fnwt = 1.0d-10
      endif

      return
      end

c*********************************************************************
c-- get dt from file
      function fndtsyn(isite)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
      dt = gps_info(gps_index(isite), 4)/365.25d0
      if (dt .le. 0.0d0) dt = dtsyn0
      fndtsyn = dt
       
      return
      end

c*********************************************************************
c** convert string of length n to lower case
      subroutine lcase(string, n)
      character*(*) string
       do i=1,n
         k=ichar(string(i:i))
         if ( k .ge. 65 .and. k .le. 90) k = k+32
         string(i:i) = char(k)
       enddo
       return
       end

c*********************************************************************
c** convert string of length n to upper case
      subroutine ucase(string, n)
      character*(*) string
       do i=1,n
         k=ichar(string(i:i))
         if ( k .ge. 97 .and. k .le. 122) k = k-32
         string(i:i) = char(k)
       enddo
       return
       end
c**********************************************************************
c** read in a block outline from file

      subroutine readblock(jblock, bname, a3, kbpole, kstr, bfile)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character*1 a3, c1
      character bname*4, bfile*80, aline3*150, aline4*150

      logical inlatlon, fexist, fncomment, no

      dimension blockx(MAX_corner), blocky(MAX_corner)

      zero = 0.0d0
      x=zero
      y=zero
      xc=zero
      yc=zero
      izero = 0
      fexist = .false.
      no = .false.

      jblock=nblocks+1

c* see if it is the same block as one already read in
       call getplate(bname, jb, no)
       if (jb.gt.0) jblock=jb

       if (jblock.eq.nblocks+1) nblocks=jblock

       b_name(jblock)=bname
       npole_block(jblock)=kbpole
       nstrain_block(jblock)=kstr

       j = jblock
       block_centroid(j,1) = zero
       block_centroid(j,2) = zero


c default input is lon,lat in degrees

       inlatlon = ( a3.eq.'1' )

c* see if external file is to be read
       call existfile ( bfile, fexist, 0)

       if (fexist) then 
         kfile=kfopen(0)
         open (kfile, file=bfile)
       else
         kfile=k10
       endif

       read (kfile, '(a150)') aline3
       read(aline3, *, err=11,end=11) nc, xc, yc
   11  continue

c-- centroid read in
       if (xc .ne. zero .or. yc .ne. zero ) then
         block_centroid(j,1) = fnlong(xc)
         block_centroid(j,2) = yc
       endif
       
c-- if making blocks from fault segments
       if (make_blocks) then
        if (fexist) close(kfile)
        return
       endif

       nc_block(j)=abs(nc)
       nc=nc_block(j)
       i=0
      
       do kc= 1,nc
        read (kfile, '(a150)') aline4
        c1=aline4(1:1)

        if (.not. fncomment(c1)) then

        read(aline4, *) xx, yy

        if ( (xx.eq.999 .and. yy.eq.999) .or.
     .    (xx.eq.9999 .and. yy.eq.9999) ) then
           nc_block(j) = i
           if(i.gt. MAX_corner) then
             print *, '*** Block ',bname, 'too many points'
            call stop1
           endif
           go to 20
        endif

        i=i+1
        if (inlatlon) call swap(xx,yy)

        blockxy(i,j,1)=fnlong(xx) 
        blockxy(i,j,2)=yy
      
        blockx(i)= blockxy(i,j,1)
        blocky(i)= blockxy(i,j,2)

       endif
      
      enddo
      
c-- close block by making last point = first point
  20    nc_block(j)=nc_block(j)+1
        n= nc_block(j)
        blockxy(n,j,1)=blockxy(1,j,1)
        blockxy(n,j,2)=blockxy(1,j,2)
        blockx(n)= blockxy(1,j,1)
        blocky(n)= blockxy(1,j,2)

c* do MV requests
        call domoves(j, 0)

c-- get centroid of block
      call centroid (n, blockx, blocky, Xc, Yc, area)
      block_area(j) = area

      if (block_centroid(j,1).eq.zero .and. 
     .     block_centroid(j,2).eq.zero) then
        block_centroid(j,1) = fnlong(Xc)
        block_centroid(j,2) = Yc
      endif

        if (fexist) ik = kfclose (kfile)

      return
      end


c*******************************************************
c see if site should be removed and for what reason
c if removed write to file      
c*******************************************************
      subroutine check_remove(isite, kreason, keep, kout, kswap)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
c* common for removing sites
      integer rm_type, rm_reason
      character*180 rm_site
      common /rm1/  rm_site(MAX_rm)
      common /rm2/  nrm_site, nrm_circ, nrm_p, nrm_poly(3),
     .   rm_type(MAX_rm), rm_reason(MAX_gps)
      common /rm3/  rm_circ(20,3), rm_poly(3,20,2)

      character*4 block_name, bname, fname, name_i, name_j   
      character*8 s8name, eqname

      dimension xcc(MAX_corner), ycc(MAX_corner)

      character*4 rm_file, c4tmp(50)
      character*8 c8tmp(50)
      logical name8, useENU, keep, nsame, psame, vsame, fsame

      character reason(10)*20

      data reason / ' not in any block   ', ' in removed block   ',
     .              ' by user request    ', ' sigma too large    ',
     .              ' on block boundary  ', ' block without pole ',
     .              ' in removed region  ', ' equated with       ',
     .              ' outside data region', ' sigma too small    '/


      if ( .not. keep ) return
      
      i = isite
      kr = 0
      kreason = 0
      keqsite=0
      rm_reason(isite) = 0
      ktype = gps_type(i)

c for equates swap names with site number kswap
      kswap = 0

c* remove site isite from GPS data if necessary

      call gpsinfo2(isite, xpt, ypt, xobs, yobs, zobs, 
     .     xcalc, ycalc, zcalc, sigx, sigy, sigz, s)

c****************************************************
c** find the block containing the GPS point
c******************************************************
      
      nblock_gps(isite)=0

      if(nblocks.gt.0) then
      do j=1, nblocks
       if(block_flag(j)) then
         nb=nc_block(j) 

       do ib=1, nb 
         xcc(ib)=blockxy(ib,j,1)
         ycc(ib)=blockxy(ib,j,2)
       enddo
       
c** check block for this site
        call inside ( xpt, ypt, xcc, ycc, nb, insde)

         if ( abs(insde).eq.1 ) then 
            if ( nblock_gps(i) .ne. 0 ) then
             print *, gps_name(i), ' on multiple blocks'
             nblock_gps(isite)=-99
            else
             nblock_gps(isite)=j
            endif
         endif

         if ( abs(insde).eq.2 ) nblock_gps(isite)=-99
       endif
      enddo

c (1) if site not on any block
         if (nblock_gps(isite).eq.0) then
           rm_reason(isite) = 1
          goto 11
         endif
      endif

c (2) in removed block RMb
c** if 3rd character is 'b', sites from the blocks listed are removed
c* if blocks to remove are listed, remove this site if on any of those blocks
       do j=1, nrm_site
c      call clearchar (c4tmp, 200)
        c4tmp = ""
c              do ic=1,50
c        c4tmp(ic) = "    "
c        enddo

        if (rm_type(j).eq.1 ) then
         read (rm_site(j), *, end=232) (c4tmp(k), k=1,50)
  232     do kk=1,50
           nbl = 0
           do k=1, nblocks
            if ( c4tmp(kk).eq.block_name(k) ) nbl=k
           enddo
           if (nblock_gps(isite).eq.nbl) then
            rm_reason(isite) = 2
            goto 11
           endif
         enddo
        endif
       enddo


c (3) from RM line, user request to remove listed sites
       do j=1, nrm_site
c        call clearchar (c4tmp, 200)
c        call clearchar (c8tmp, 400)
          c4tmp = ""
          c8tmp = ""
c        do ic=1,50
c        c4tmp(ic) = "    "
c        c8tmp(ic) = "        "
c        enddo

        name8 = .false.
        if ( rm_type(j).eq.2) name8 = .true.
        
c** first entry is GPS velocity file name, remove sites listed for file
        if (name8) then
          read (rm_site(j), *, end=234) rm_file, (c8tmp(k), k=1,50)
        else
          read (rm_site(j), *, end=234) rm_file, (c4tmp(k), k=1,50)
        endif

  234  do kk=1,50
        if ( rm_file.eq.gps_fname(gps_index(i)) .or. 
     .       rm_file.eq.'****') then
        l = len_trim(c8tmp(kk))
c       print *, l,c8tmp(kk),' ',gps_name(i)(1:l),' ',c8tmp(kk)(1:l) 
c        if ( gps_name(i)(1:l) .eq. c8tmp(kk)(1:l) ) then
         if ( (name8 .and. (gps_name(i).eq.c8tmp(kk)) ) .or. 
     .    ((.not. name8) .and. (gps_name(i)(1:4).eq.c4tmp(kk)))) then
           rm_reason(isite) = 3
           goto 11
         endif
        endif
       enddo

       enddo

c (4) sigma too large
c* remove large sigmas, but don't check time series
      if ( ktype.eq.1 .or. ktype.eq.2) then
       Ve = gps_obs(i,1)
       Vn = gps_obs(i,2) 
       Vz = gps_obs(i,6) 
       Se = gps_obs(i,3)
       Sn = gps_obs(i,4) 
       Sz = gps_obs(i,7) 
       rho = gps_obs(i,5)
       S = dsqrt(Se**2 + Sn**2)
       kf = gps_index(i)

c* maximum allowed sigma
       sigmin = abs(gps_wt(kf,2))
       sigmax = gps_wt(kf,3)
       ksig   = int(gps_info(kf,10))

c remove if either horizontal component exceeds sigmax
       if ( ksig.eq.0) then
        if ( (useENU(isite,1) .and. Se.gt.sigmax) .or. 
     .       (useENU(isite,2) .and. Sn.gt.sigmax) ) then
          rm_reason(isite) = 4
          goto 11
        endif
       endif
       if ( ksig.eq.0) then
        if ( (useENU(isite,1) .and. Se.lt.sigmin) .or. 
     .       (useENU(isite,2) .and. Sn.lt.sigmin) ) then
          rm_reason(isite) = 10
          goto 11
        endif
       endif

c* the followig checks the sigma in the direction of the vector
       if (ksig.eq.1) then
        Ue = 0.0d0
        Un = 0.0d0
        if (useENU(isite,1)) Ue = Ve
        if (useENU(isite,1)) Un = Vn
        if (Ue .ne. zero .or. Un .ne. zero) then
         call velsig (Ue, Un, Se, Sn, rho, Vel, Vsig )
          if (Vsig.gt.sigmax ) then
           rm_reason(isite) = 4
           goto 11
          endif
          if (Vsig.lt.sigmin ) then
           rm_reason(isite) = 10
           goto 11
          endif
         endif
        endif
     
c remove if any component exceeds sigmax
        if ( ksig.eq.2) then
         if ( ( useENU(isite,1) .and. Se.gt.sigmax) .or. 
     .       (useENU(isite,2)   .and. Sn.gt.sigmax) .or.
     .       (useENU(isite,3)   .and. Sz.gt.sigmax) ) then
          rm_reason(isite) = 4
          goto 11
         endif
         if ( ( useENU(isite,1) .and. Se.lt.sigmin) .or. 
     .       (useENU(isite,2)   .and. Sn.lt.sigmin) .or.
     .       (useENU(isite,3)   .and. Sz.lt.sigmin) ) then
          rm_reason(isite) = 10
          goto 11
         endif
        endif

      endif
      
c (5) on block boundary
c** if site on block boundary or multiple blocks
         if (nblock_gps(i).eq.-99) then
          rm_reason(isite) = 5
          goto 11
         endif

c (6) block without pole         
c** if site on block that has no pole 
c         print *, i, nblock_gps(i), npole_block(nblock_gps(i)) 
         if (npole_block(nblock_gps(i)).eq.0) then
          rm_reason(isite) = 6
          goto 11
         endif

c (7) in circular or polygon region (RC: and RP:)
c** site within circular removal area
        xg = gps_pos(i,1)
        yg = gps_pos(i,2)

       do j=1,nrm_circ
        xc = rm_circ(j,1)
        yc = rm_circ(j,2)
        call distkm (xg,yg,xc,yc,d)
        if (d .le. rm_circ(j,3) ) then
         rm_reason(isite) = 7
         goto 11
        endif
       enddo      

c** site within polygon area
       do j=1,3
        n = nrm_poly(j)
       if ( n.gt.0) then
        do ii=1,n
         xcc(ii) = rm_poly(j,ii,1)
         ycc(ii) = rm_poly(j,ii,2)
        enddo
         xcc(n+1) = rm_poly(j,1,1)
         ycc(n+1) = rm_poly(j,1,2)
         n=n+1

        call inside ( xg, yg, xcc, ycc, n, insde) 

        if (insde.eq.1 ) then
         rm_reason(isite) = 7
         goto 11
        endif

       endif
      enddo      

c (8) equated 
c*  equates are nearby sites assigned same velocities by GLOBK
c if check_name_equates remove those with same 4-char name
      if ( ( check_v_equates .or. check_name_equates )
     .    .and. i.gt.1 .and. gps_type(i).eq.1  .and. 
     .    (useENU(i,1) .or. useENU(i,2) .or. useENU(i,3)) ) then

      dv = 0.005d0
      dxkm = 1.0d0

       call gpsinfo2(i, xpti, ypti, xoi, yoi, zoi, xci, yci, zci, 
     .    sxi, syi, szi, s)

         name_i = gps_name(i)(1:4)

       do j = 1, isite-1

       call gpsinfo2(-j, xptj, yptj, xoj, yoj, zoj, xcj, ycj, zcj, 
     .    sxj, syj, szj, s)

         name_j = gps_name(j)(1:4)

c only if this site is still being used or was removed by RM:
       if ( use_gps ) then

       call distkm (xpti, ypti, xptj, yptj, d)
        fsame  = (gps_index(i).eq.gps_index(j))
        psame  = (  d.lt.dxkm )
        nsame  = (  name_i .eq. name_j )
        vsame  = ( abs(xoi-xoj).le.dv .and. abs(sxi-sxj).le.dv .and.
     .             abs(yoi-yoj).le.dv .and. abs(syi-syj).le.dv .and.
     .             abs(zoi-zoj).le.dv .and. abs(szi-szj).le.dv )

      if ( check_v_equates .and. psame .and. vsame .and. fsame) then
         rm_reason(isite) = 8
         keqsite = j
         goto 11
      endif

      if ( check_name_equates .and. nsame .and. psame .and. 
     .      vsame .and. fsame) then
         rm_reason(isite) = 8
         keqsite = j
         goto 11
      endif

c- check for primary site name to use
c - have a list of preferred site names, compare site keqsite to it
c  if its there, swap this isite with keqsite

        endif

       enddo
      endif


c (9) outside data region (DR:)
      call check_region (xpt, ypt, keep)
        if ( .not. keep ) then
          rm_reason(isite) = 9
          goto 11
        endif


 11   continue

      kt = rm_reason(isite)

      if ( kt.gt.0 .and. kt .le. 10) then

c** don't remove site but flag it
       if ( keep_bad .and. (kt.eq.3 .or. kt.eq.4 .or. kt.eq.7
     .                .or. kt.eq.8 .or. kt.eq.9 .or. kt.eq.10) ) then
        gps_keep(isite) = .false.
        gps_type(isite) = 0
       endif

c-- write
      bname = '    '
      eqname = '        '
      n = nblock_gps(isite)
      if (n.gt.0) bname = block_name(n)
      s8name = gps_name(isite)
      fname = gps_fname(gps_index(isite))
      if (keqsite.gt.0) eqname = gps_name(keqsite)

      write(kout,1) xpt, ypt, xobs, yobs, zobs, sigx, sigy, sigz,  
     .  s8name, fname, bname, reason(kt), eqname

  1   format(2f10.4, 6f8.2, 1x, a8, 2(1x, a4), a20, a8)

      gps_type(isite) = 0

      endif

       
      return
      end


c**********************************************************************
c******* see if this file/site has specific sites selected
c**********************************************************************
      subroutine check_selected (jmode, shortname, sitename, selected)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character*4 cname, shortname, sitename, sname(40) 
      character*180 a
      logical selected
      
c* common for selecting sites
      integer se_type
      character*180 se_site
      common /se1/  se_site(MAX_rm)
      common /se2/  nse_site, se_type(MAX_rm)

      selected = .false.
      if (nse_site.eq.0 ) return
      
c-- if mode=0 just see if this file has been selected      
      if( jmode.eq.0 ) then
       do k=1, nse_site 
        a = se_site(k)
c        print *, a
         read(a, *) cname 
        if ( cname.eq.shortname ) selected = .true.
       enddo 
      endif
          
c-- if mode=1 see if this site has been selected      
      if( jmode.eq.1 ) then
       do k=1, nse_site 
        a = se_site(k)
        read(a, *) cname 
        if ( cname.eq.shortname ) then
         read(a, *, end=1, err=1) (sname(j),j=1,40)
 1        do j=2,40
           if (sname(j).eq.sitename) selected = .true.
          enddo
        endif
       enddo
      endif 
        
      
      return
      end
      
      
c**********************************************************************
c******* see if this point x,y is within the DR: selected region 
c**********************************************************************
      subroutine check_region (x, y, selected)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

c* common for data region
      common /dr1/ dr_pos(6)
      
      logical selected
      
      selected = .true.
      x = fnlong(x)
      
      
       if ( x.lt.dr_pos(1) .or. x.gt.dr_pos(2) .or.
     .      y.lt.dr_pos(3) .or. y.gt.dr_pos(4) )
     .  selected = .false.
     
     
      return
      end

c**********************************************************************
c******* read a gps file, see GP: input for formats
c**********************************************************************

      subroutine readgps (gps_file, shortname, a3, ngpsin, ntsin, 
     .    Tdec0, kfilt, kerr,swapxy)

      implicit real*8 (a-h,o-z)
      parameter (MAX_tmp = 12000, MAX_rmtmp = 10000 )
      include "tdefcom1.h"
      include "tdefcons.h"

      character*1 a3
      character*4 cname, shortname, rmname(MAX_rmtmp), 
     .            offname(MAX_rmtmp), net, star4 
      character*8 gn 
      character*11 gn2 
      character*80 gps_file, a80, fnout
      character*120 gf, bline
      character*256 a, a1, a2, a256
      character*20 args(100)

      dimension iobs(MAX_gps, 40)
      dimension cwt(3), trm(MAX_rmtmp,2), toff(MAX_rmtmp)
      dimension ttmp(MAX_tmp), xtmp(MAX_tmp,3), stmp(MAX_tmp,3)
      dimension xoff(MAX_rmtmp,3)
      dimension xpre(3), xpost(3), xprewt(3), xpostwt(3)
      
      logical fncomment, kformat, f0, f1, f2, f3, f4, f5, f6, tdrop
      logical kerr, fnrlim, fnend, fexist
      logical file_selected, site_selected, keep_site, keeptx
      logical swapxy, write_data
      logical write_input
      common /cm3/ write_input

c* common for data region
      common /dr1/ dr_pos(6)

      k = num_gps
      ngpsin = 0
      ntsin = 0
      kformat = .false.
      ng = num_gps_file

      sigmin = 1.0d-3
      sigmax = 1.0d3
      call clearint(iobs, 40*MAX_gps)
      kerr = .false.
      cname = '    '
      star4 = '****'
      zero = 0.0d0
      ypd  = 1.0d0/365.25d0
      ypd2 = 0.5d0/365.25d0

c - which formats to use      
c      fa = (a3.eq.'a')
      f1 = (a3.eq.'1')
      f2 = (a3.eq.'2')
      f3 = (a3.eq.'3')
      f4 = (a3.eq.'4')
      f5 = (a3.eq.'5')
      f6 = (a3.eq.'6')
      f0 = ( .not. f1 .and. .not. f2 .and. .not. f3 .and. .not. f4 
     . .and. .not. f5 .and. .not. f6 )

c - skip file if wtfac=0      
      gps_wtfac = gps_info(ng,2)
      if ( gps_wtfac.eq.zero)  return

      gps_wt(num_gps_file,1) = gps_wtfac
      
c min and max of time window
      tmin = gps_info(ng,6)
      tmax = gps_info(ng,7)
c      tmin = max(tmin, dr_pos(5))
c      tmax = min(tmax, dr_pos(6))
c      gps_info(ng,6) = tmin
c      gps_info(ng,7) = tmax

      Ts_min = zero
      NP_min = 2

c - see if sites are selected from this file        
      call check_selected (0, shortname, cname, file_selected)

c component weighting
       do j=1,3
        cwt(j) = 1.0d0
       enddo

      if (veldata) then
       ktype = 1
       gps_file_type(ng) = ktype
       sigfloor = gps_info(ng, 8)
       sigmin = max( gps_info(ng, 8), sigmin)
       sigmax = gps_info(ng,9)
       if (sigmax .le. 0.0d0) sigmax = 1.0d3
       gps_wt(ng,2) = sigfloor
       gps_wt(ng,3) = sigmax
       gps_wt(ng,4) = gps_info(ng,10)
       gps_info(ng,8) = sigmin
       gps_info(ng,9) = sigmax
       tmin = gps_info(ng,6)
       tmax = gps_info(ng,7)
c       do j=1,3
c        cwt(j) = gps_info(ng,10+j)
c        if ( gps_info(ng,10+j).gt.0.0d0 ) gps_info(ng,10+j) = 1.0d0
c       enddo
      endif

      if (dispdata) then
        ktype = 2
        gps_file_type(ng) = ktype
        sigfloor = gps_info(ng, 3)
        sigmin = max( gps_info(ng, 3), sigmin)
        sigmax = gps_info(ng, 4)
        if (sigmax .le. 0.0d0) sigmax = 1.0d4
        gps_wt(ng,2) = sigfloor
        gps_wt(ng,3) = sigmax
        gps_wt(ng,4) = 0.0
        gps_info(ng,3) = sigmin
        gps_info(ng,4) = sigmax
c        do j=1,3
c         cwt(j) = gps_info(ng,7+j)
c         if ( gps_info(ng,7+j).gt.0.0d0 ) gps_info(ng,7+j) = 1.0d0
c        enddo
      endif
    
      if (tsdata) then
        ktype = 3
        gps_file_type(ng) = ktype
        sigmax = gps_info(ng,5)
        if (sigmax .le. 0.0d0) sigmax = 1.0d4
        gps_wt(ng,2) = 0.1
        gps_wt(ng,3) = sigmax
        gps_wt(ng,4) = 0.0
        Ts_min = gps_info(ng,14)
        NP_min = gps_info(ng,15)
c       do j=1,3
c        cwt(j) = gps_info(ng,7+j)
c        cwt(j) = 1.0d0
c        if ( gps_info(ng,7+j).gt.0.0d0 ) gps_info(ng,7+j) = 1.0d0
c       enddo

c times to be removed
       nrm = 0
       krm = 24
       do kk=1, num_f_rm
        if (krm_filename_type(kk).eq.1) then         
c         print *, 'RT File ',rm_filename(kk), shortname
         call existfile( rm_filename(kk), fexist, 1)
          if (fexist) then
          krm=kfopen(krm)
         open (krm, file = rm_filename(kk) )
  51    read (krm, '(a256)', end=91 ) a256
c         print *, a256
         if (fncomment(a256(1:1)) ) go to 51
         call parse(a256, args, nargs)
c         print *, nargs
         t2 = 0.0d0
         if (nargs.eq.3) read(a256, *, end=91) cname, net, t1
         if (nargs.gt.3) read(a256, *, end=91) cname, net, t1, t2
         if ( (net.eq.shortname .or. net.eq.star4) .or. 
     .        (net.eq.shortname .and. cname.eq.star4) ) then
           if (t2 .le. t1 ) t2 = t1 + ypd2
           nrm = nrm +1
           rmname(nrm) = cname
           trm(nrm,1) = t1-ypd2
           trm(nrm,2) = t2
c           print *, 'RT days ', net, rmname(nrm), trm(nrm,1), trm(nrm,2)
         endif
         goto 51
   91    ik = kfclose(krm)
        endif
        endif
       enddo

c offsets to fix
       call cleareal(xoff, MAX_rmtmp*3)
       noff = 0
       do kk=1, num_f_off
         call existfile( off_filename(kk), fexist, 1)
          if (fexist) then
c         print *, 'RX File ',off_filename(kk), shortname

          krm=kfopen(krm)
          offsign = 1.0d0
          if (off_sign(kk) .lt. 0.0d0 ) offsign = -1.0d0
         open (krm, file = off_filename(kk) )
   52    read (krm, '(a80)', end=92 ) a80
         if (fncomment(a80(1:1)) ) go to 52
         read(a80, *) cname, net, t, xo, yo, zo
         if ( net.eq.shortname  .or. net.eq.star4 ) then
           noff = noff +1
           offname(noff) = cname
           toff(noff) = t
           xoff(noff,1) = xo*offsign
           xoff(noff,2) = yo*offsign
           xoff(noff,3) = zo*offsign
c           print *, cname, t, xo, yo, zo
         endif
         goto 52
   92    ik = kfclose(krm)
        endif
       enddo


      endif


      write_data = ( veldata .and. myflag )
      if (write_data) call fopen (kf18, 1, '_'//shortname//'_vel.tmp ')

      call fappend (krem, 1, '_removed.vec ' )

      k12=kfopen(12)
      open (k12, file = gps_file)
      
c-- site reading loop      
      do 5 kk = 1,50000

c-- GPS headers
      read (k12, '(a256)', end=99, err=15) a1
      if (write_input) print *, a1
c      a = a1//' 0  0 '
      a = a1 

  15  if ( fnend(a(1:3)) ) go to 99
      if ( len(a).lt.10 .or. fncomment(a(1:1)) ) go to 5
      
c* see if format is specified
      if ( a1(1:1).eq.'(' .or. a1(1:2).eq.'#(' ) then
        if ( a1(1:1).eq.'('  ) gf = a1(1:100)
        if ( a1(1:2).eq.'#(' ) gf = a1(2:99)
        kformat = .true.
        go to 5
      endif
      
c Parse the string 'a' into arguments args(1), ..., args(nitems) 
c use a2 so a is not reformatted
        a2 = a
        call parse(a2, args, nitems)

c initialize
       cne = zero
       Ve = zero
       ere = zero
       Vn = zero
       ern = zero
       Vz = zero
       erz = zero
       t1 = zero
       t2 = zero
       pz=zero
       
c  GPS velocity
c  GP: shortname, filename, pole number, wtfac, Wx, Wy, Wz, Tmin, Tmax, Sigmin, Sigmax, R/S_max, 3 component flags
c    format: Lon  Lat  Ve Vn Se Sn rho Site   (default) psvelo format
c    format: Lon  Lat  Ve Vn Vz Se Sn Sz Site (if a3 = 1)
c    format: Lon  Lat  Ve Se Vn Sn Vz Sz Site (if a3 = 2)
c    format: Lon  Lat  Ve Vn Se Sn rho Site Vz Sz (if a3 = 3) - globk format
c    format: Lon  Lat  Vz Sz Site (if a3 = 4) - old UP format

      if ( veldata ) then
       if ( kformat ) then
        if (f0) read(a,gf) x, y, Ve, Vn, ere, ern, cne, gn
        if (f1) read(a,gf) x, y, Ve, Vn, Vz, ere, ern, erz, gn
        if (f2) read(a,gf) x, y, Ve, ere, Vn, ern, Vz, erz, gn
        if (f3) read(a,gf) x, y, Ve, Vn, 
     .                     ere, ern, cne, Vz, erz, gn
        if (f4) read(a,gf) x, y, Vz, erz, gn
        if (f5) read(a,gf) gn, x, y, pz, Ve, Vn, Vz,  
     .                     ere, ern, erz, cne
        if (f6) read(a,gf) gn, x, y, pz, Vz, erz
       else
        if (f0) read(a,*) x, y, Ve, Vn, ere, ern, cne, gn
        if (f1) read(a,*) x, y, Ve, Vn, Vz, ere, ern, erz, gn
        if (f2) read(a,*) x, y, Ve, ere, Vn, ern, Vz, erz, gn
        if (f3) read(a,*) x, y, Ve, Vn, r, r, 
     .                     ere, ern, cne, Vz, r, erz, gn
        if (f4) read(a,*) x, y, Vz, erz, gn
        if (f5) read(a,*) gn, x, y, pz, Ve, Vn, Vz,  
     .                     ere, ern, erz, cne
        if (f6) read(a,*) gn, x, y, pz, Vz, erz
       endif
        if(write_data .and. erz.gt.0.0d0) 
     .     write(kf18, '(a4,1x,2f9.4,6f8.2)') 
     .     gn, x, y, Ve, ere, Vn, ern, Vz, erz
        if(write_data .and. erz.eq.0.0d0) 
     .     write(kf18, '(a4,1x,2f9.4,4f8.2)') 
     .     gn, x, y, Ve, ere, Vn, ern 

c time of series
        tmin = gps_info(ng,6)
        tmax = gps_info(ng,7)

c check input
        if (.not. fnrlim(cne, -1.0d0, 1.0d0)) then
           print *, '*** GPS covariance incorrect, set to 0.0,',
     .       ' for site ', gn, ' in file ', gps_file, ' '
           cne=0.0d0
        endif

        if (ere.lt.zero .or. ern.lt.zero .or. erz.lt.zero) then
           print *, '*** Sigma(s) incorrect for site ', gn, 
     .      ' in file ', gps_file
           call stop1
        endif

c      if (myflag .and. (Ve.eq.zero .or. Vn.eq.zero) .and. 
c     .  ( .not. f4 .and. .not. f6 ) ) then
c         print *, '*** GPS zero found ', gn, Ve, Vn,  
c     .    ' in file ', gps_file
c         print *, a 
c        endif

        ere = ere*gps_wtfac
        ern = ern*gps_wtfac
        erz = erz*gps_wtfac
        
       if (sigfloor .gt. zero) then
        ere = max(ere, sigfloor)
        ern = max(ern, sigfloor)
        erz = max(erz, sigfloor)
       endif

      endif

c time series header       
       if (tsdata ) then
        Vz = zero
        erz = zero
        cne = zero
        if (f0) read(a, *) x, y, Ve, Vn, ere, ern, cne, gn
        if (f1) read(a, *) x, y, Ve, Vn, Vz, ere, ern, erz, gn
        if (f2) read(a, *) x, y, Ve, ere, Vn, ern, Vz, erz, gn
        if (f3) read(a, *) x, y, pz, gn
        if (f4) read(a, *) gn, x, y, pz, Ve, ere, Vn, ern, Vz, erz 
        if (f5) read(a, *) y, x, pz, gn
c fill out site name to 4 chars
        gn2 = trim(adjustl(gn))//'___'
        gn = gn2(1:4)
c        print *, shortname, gn
       endif

c displacements       
      if (dispdata) then
        t1 = zero
        t2 = zero
        ni = nitems

       if (kformat) then
        if (f0) read(a, gf) x, y, Ve, ere, Vn, ern, Vz, erz, gn, t1, t2
        if (f1) read(a, gf) x, y, Ve, Vn, Vz, ere, ern, erz, gn, t1, t2
        if (f2) read(a, gf) x, y, Ve, Vn, ere, ern, gn, Vz, erz, t1, t2
        if (f3) read(a, gf) x, y, Ve, Vn, ere, ern, cne, gn, t1, t2 
        if (f4) read(a, gf) x, y, Vz, erz, gn, t1, t2 
        if (f5) read(a, gf) x, y, Ve, Vn, ere, ern, gn, t1, t2 
       else
        if (f0 .and. ni .ge. 11) 
     .          read(a, *) x, y, Ve, ere, Vn, ern, Vz, erz, gn, t1, t2
        if (f0 .and. ni.eq.9) 
     .          read(a, *) x, y, Ve, ere, Vn, ern, Vz, erz, gn
        if (f1 .and. ni .ge. 11) 
     .          read(a, *) x, y, Ve, Vn, Vz, ere, ern, erz, gn, t1, t2
        if (f1 .and. ni.eq.9) 
     .          read(a, *) x, y, Ve, Vn, Vz, ere, ern, erz, gn 
        if (f2 .and. ni .ge. 11) 
     .          read(a, *) x, y, Ve, Vn, ere, ern, gn, Vz, erz, t1, t2 
        if (f2 .and. ni.eq.9) 
     .          read(a, *) x, y, Ve, Vn, ere, ern, gn, Vz, erz 
        if (f3 .and. ni .ge. 10) 
     .          read(a, *) x, y, Ve, Vn, ere, ern, cne, gn, t1, t2 
        if (f3 .and. ni.eq.8) 
     .          read(a, *) x, y, Ve, Vn, ere, ern, cne, gn 
        if (f4 .and. ni .ge. 7) 
     .          read(a, *) x, y, Vz, erz, gn, t1, t2
        if (f4 .and. ni.eq.5) 
     .          read(a, *) x, y, Vz, erz, gn 
        if (f5 .and. ni.ge.9) 
     .          read(a, *) x, y, Ve, Vn, ere, ern, gn, t1, t2 
        endif
        if (ere.lt.zero .or. ern.lt.zero .or. erz.lt.zero) then
           print *, '*** Sigma(s) incorrect for site ', gn, 
     .      ' in file ', gps_file
           call stop1
        endif

        ere = ere*gps_wtfac
        ern = ern*gps_wtfac
        erz = erz*gps_wtfac
        
       if (sigfloor .gt. zero) then
        ere = max(ere, sigfloor)
        ern = max(ern, sigfloor)
        erz = max(erz, sigfloor)
       endif
       
         tmin = gps_info(ng,6)
         tmax = gps_info(ng,7)
         if (t1.gt.zero ) tmin = t1
         if (t2.gt.zero ) tmax = t2
       endif

c-- increment the counters even if the data are not to be kept
c   this site can be removed later after reading in the time series data

      k=k+1
      if (k.gt.MAX_gps) then
        print *, '**** MAX_gps exceeded ****'
       call stop1
      endif

      ngpsin = ngpsin+1


c-- fill arrays  
      if(swapxy) call swap(x,y)
      x=fnlong(x)
      gps_pos(k,1) = x
      gps_pos(k,2) = y
      gps_pos(k,3) = pz
      gps_name(k)  = gn
      gps_obs(k,1) = Ve
      gps_obs(k,2) = Vn
      gps_obs(k,3) = ere
      gps_obs(k,4) = ern
      gps_obs(k,5) = cne
      gps_obs(k,6) = Vz
      gps_obs(k,7) = erz
      gps_index(k) = num_gps_file
      gps_type(k)  = ktype
      gps_keep(k)  = .true.
      timespan(k,1) = tmin
      timespan(k,2) = tmax
      num_gps      = k
     
 

c--------------------------------------------------------------------
c-- see if we will keep this site
      keep_site = .true.
      krm=0
       
      x=fnlong(x)
      if ( y.eq.0.0d0 .and. x.eq.0.0d0) keep_site = .false.

c take out
c      if ( Ve.eq.0.0d0 .or. Vn.eq.0.0d0) keep_site = .false.

c see if outside data window (DR:)
c       if (dispdata ) then
c         if ( timespan(k,1).gt.dr_pos(6) .or. 
c     .        timespan(k,1).lt.dr_pos(5) .or.
c     .        timespan(k,2).gt.dr_pos(6) .or. 
c     .        timespan(k,2).lt.dr_pos(5) ) keep_site = .false.
c       endif
 
c-- if selected to keep with SE:       
        site_selected = .true.
       if ( file_selected ) then
        call check_selected (1, shortname, gn, site_selected)
        if ( .not. site_selected ) keep_site = .false.
c        krm=9
c          write (krem, '(a4," not in SE")') gn
c          call removesite(krm, k, krem, 0)
       endif

       call check_remove (k, kreason, keep_site, krem, kswap)

       
c-- if keep_bad flag is set, don't remove site but flag it
      if ( keep_bad .and. .not. keep_site ) then
        gps_keep(k) = .false.
        keep_site = .true.
      endif
        
c-- remove site now unless it is a time series (set type = 0 )
      if( .not. keep_site .and. .not. tsdata ) then     
         gps_type(k)  = 0
         goto 5
      endif
      
c--------------------------------------------------------------------
         
        
c-- store time series in single array, flag the entries ndx(k), start/end entries for this site,
c-- data format: time (yrs), E-pos, E-sig, N-pos, N-sig, U=pos, U-sig; all in mm

c-- first read all data in requested time period into temporary array, then process

      if (tsdata) then
        
c-- read data        
        nd=num_disp
        ndp1 = nd + 1
        if(tmax.eq.0) tmax = 9999.0d0
        s1 = 3000.0
        s2 = 0.0

      n=0
      
c-- read in all data within time limits
      do 77 i = 1, MAX_tmp
       read(k12, '(a120)', end=99 ) bline
       if (write_input) print *, bline
       if (fncomment(bline(1:1))) goto 77

       read(bline, *) tx

c-- end of this site's data
      if( tx .ge. 3000.0 .or. tx.lt.1900.0 ) goto 8

c see if removed by user, by RT: line
       keeptx = .true.
       do krm=1, nrm
        if ( (tx.ge.trm(krm,1) .and. tx.le.trm(krm,2))
     .     .and. (gn.eq.rmname(krm) .or. star4.eq.rmname(krm)) ) then
          keeptx=.false. 
c          if(verbose) print *, 'Removing ', gn, ' ', tx
        endif
       enddo 


      if(tx.ge.tmin .and. tx.le.tmax .and. keep_site .and. keeptx) then
       
c-- how many obs each year       
       it = int(tx)-1989
       iobs(k,it) = iobs(k,it)+1
       n = n+1

c-- read data into temp arrays
       read(bline,*,end=99) ttmp(n),(xtmp(n,j), stmp(n,j),j=1,3)   
       
        
       if ( xe3 ) then
        do j=1,3
         xtmp(n,j) = xtmp(n,j)*1.0d3 
         stmp(n,j) = stmp(n,j)*1.0d3 
         enddo
         endif

c-- drop high sigmas, increase sigma for vertical
        UPsigmax = 3.0d0*sigmax
        tdrop = .false.
         do j=1,3
          stmp(n,j) = max(stmp(n,j), 0.1d0)
          stmp(n,j) = stmp(n,j)*gps_wtfac 
          if ( j.lt.3 .and. stmp(n,j).gt.sigmax )   tdrop = .true.
          if ( j.eq.3 .and. stmp(n,j).gt.UPsigmax ) tdrop = .true.
         enddo
         if ( tdrop ) n = n-1

       endif
c-- end of time check
     
  77  continue




c-- end of reading time series data
   8   npts = n 

      if ( keep_site ) then

c sort time series by time
      if (sort_ts) then
       call fopen (k33, 1, '_ts1_gps.tmp ')
       do i=1,npts
        write(k33, '(f12.5, 6f12.4)')ttmp(i),(xtmp(i,j),stmp(i,j),j=1,3)
       enddo
       ik = kfclose(k33)
c* sort the file and read back in
       status = system('sort -n '//fnout('_ts1_gps.tmp ')//' >  '
     .    //fnout('_ts2_gps.tmp ')//char(0) )
       call fopen (k33, 1, '_ts2_gps.tmp ')
       do i=1,npts
        read(k33, '(f12.5, 6f12.4)')ttmp(i),(xtmp(i,j),stmp(i,j),j=1,3)
       enddo
       ik = kfclose(k33)
      endif

c estimate offsets for times entered w/o specified offsets
      do krm = 1, noff
       if ( xoff(krm,1).eq.zero .and. xoff(krm,2).eq.zero .and. 
     .      xoff(krm,3).eq.zero .and. gn.eq.offname(krm) ) then
          ttoff = toff(krm)
          tdiff = 30.0d0/365.25d0
          npre = 0
          npost = 0
         do j=1,3
          xpre(j) = zero
          xpost(j) = zero
          xprewt(j) = zero
          xpostwt(j) = zero
         enddo

c get pre- and post- amplitudes within time tdiff
        do i=1,npts
         dt = abs(ttoff - ttmp(i))
         if ( ttmp(i).lt.ttoff  .and. dt.le.tdiff .and. dt.gt.ypd) then
            npre = npre+1
           do j=1,3
            w = fnwt(stmp(i,j))
            xpre(j) = xpre(j) + w*xtmp(i,j)
            xprewt(j) = xprewt(j) + w
           enddo
         endif
         if ( ttmp(i).gt.ttoff  .and. dt.le.tdiff .and. dt.gt.ypd) then
            npost = npost+1
           do j=1,3
            w = fnwt(stmp(i,j))
            xpost(j) = xpost(j) + w*xtmp(i,j)
            xpostwt(j) = xpostwt(j) + w
           enddo
         endif
        enddo
        if (npre .gt. 0 .and. npost .gt. 0 ) then
c        print *, offname(krm), npre, npost
          do j=1,3
           xoff(krm,j) = xpre(j)/xprewt(j) - xpost(j)/xpostwt(j)
          enddo
        endif
       endif
      enddo 

c fix offsets, RX: lines        
      do krm=1, noff
       if(gn.eq.offname(krm)) then
        write (*, '(a4,1x,a4,f10.4,3f8.1)') 
     .   offname(krm), shortname, toff(krm), 
     .   xoff(krm,1),xoff(krm,2),xoff(krm,3)
        do i=1,npts
         if ( ttmp(i) .ge. toff(krm) ) then
          do j=1,3
            xtmp(i,j) = xtmp(i,j) + xoff(krm,j)
          enddo
         endif
        enddo
       endif
      enddo 

c####################### filter the time series 
c remove outliers
      call tsfilter1 (kfilt, MAX_tmp, npts, xtmp, stmp, ttmp, gn)

c-- get wanted points, decimate by Tdec
      tx0 = 0.0d0
      Tdec = Tdec0/365.25d0
c      if  (Tdec .eq.zero .and. npts.gt.500 ) Tdec = 10.0d0/365.25d0   ! 11-13-15
c      if (npts .gt. 700 ) print *, gn
      nkept = 0

      do 88 i=1,npts

       tx = ttmp(i)

c-- decimation; skip until next point is > Tdec years after previous point 
c    and has sigmas below cutoff
   
       tdrop = .false.

c-- decimate
       if ( (tx - tx0).lt.Tdec ) tdrop = .true.

c-- keep last 10 points
c       if  ( myflag .and. i .ge. (npts-10) ) tdrop = .false.

c-- keep first and last point
       if  ( i.eq.npts .or. i.eq.1 ) tdrop = .false.
      
c-- drop high sigmas, increase sigma for vertical
        UPsigmax = 3.0d0*sigmax
         do j=1,3
          if ( j.lt.3 .and. stmp(i,j).gt.sigmax )   tdrop = .true.
          if ( j.eq.3 .and. stmp(i,j).gt.UPsigmax ) tdrop = .true.
         enddo

c-- keep this point
      if ( .not. tdrop ) then

c-- to get duration
       s1 = min(tx,s1)
       s2 = max(tx,s2)
       
       nd = nd + 1
       ntsin = ntsin + 1

c number kept for this site
       nkept = nkept + 1

       if ( nd.gt.MAX_disp ) then
         print *, '*** MAX_disp exceeded ***** '
         close(k12)
         close(krem)
        call stop1
       endif

       tx0 = tx
       
       t_disp(nd) = tx
       do j=1,3
          x_disp(nd,j) = xtmp(i,j)
          x_sig(nd,j)  = stmp(i,j)
       enddo
      endif


  88  continue

        ds = s2-s1  
      end if
c endif for keep_site

c###################################################

c short time or few points
c      if ( nkept.eq.1 ) print *, Ts_min, NP_min
      if ( ds .le. Ts_min .or. nkept .lt. NP_min) keep_site = .false. 
      if ( nkept.lt.2) keep_site = .false. 

      if ( keep_site ) then
        ndx(k,1) = ndp1
        num_ts = num_ts + 1
        num_disp = nd
        ndx(k,2) = nd

c set time series flags
         do j=1,3
          XVflag(k,j,1) = (gps_info(ng, 7+j))
          XVflag(k,j,2) = (gps_info(ng,10+j))
          if (XVflag(k,j,2).eq.4) XVflag(k,j,2) = 3
         enddo

c-- whether synthetics are for whole span or just the data time span 
      if (data_span) then
        dt = 0.01d0*(s2-s1)
        timespan(k,1) = s1 
        timespan(k,2) = s2 
      else
        timespan(k,1) = tmin
        timespan(k,2) = tmax
      endif

c- sigmas
        gps_obs(k,3)=1.0d0
        gps_obs(k,4)=1.0d0
        
c-- remove amplitude of first point
        x0 = x_disp(ndx(k,1),1)       
        y0 = x_disp(ndx(k,1),2)       
        z0 = x_disp(ndx(k,1),3)       
        do i = ndx(k,1), ndx(k,2)
          x_disp(i,1) = x_disp(i,1) - x0
          x_disp(i,2) = x_disp(i,2) - y0
          x_disp(i,3) = x_disp(i,3) - z0
        enddo
        
c* initialize Xo        
        do j=1,3
         GXo(k,j,1) = x_disp(ndx(k,1),j)
         GVo(k,j,2) = zero
        enddo

c* initialize Vo        
         GVo(k,1,2) = Ve
         GVo(k,2,2) = Vn
         GVo(k,3,2) = Vz
         
c-- if not to be kept, keep site name but remove ts data         
      else   
        gps_type(k) = 0
        ndx(k,1) = 0
        ndx(k,2) = 0
      endif
      
      endif
c end of tsdata if      

      
    5 continue
    
       go to 99

      continue

   99 ik = kfclose(k12)
      ik = kfclose(krem)
      if(write_data) ik = kfclose(kf18)

      num_gps = k

c-- write out obs history for time series     
      if (myflag .and. tsdata .and. verbose) then
       call fopen (k12,  1, 
     .       '_'//gps_fname(num_gps_file)//'_obs.history ')
       write(k12, '(5x,40i5)') ( j, j=1990,2020 )
        do i = 1, num_gps
        if (gps_type(i).eq.3 .and. gps_index(i).eq.num_gps_file )
     .    write (k12, '(a4, 1x, 40i5)') gps_name(i), 
     .    (iobs(i,j), j=1,31)    
        enddo
       ik = kfclose(k12)
      endif
       
       
      return
      end

c***************************************************************************
c --- outlier filter
c --- take weighted averages of 'nwindow' days and downweight outliers
c***************************************************************************
      subroutine tsfilter1(kfilt,MAX_tmp, npts, xtmp, stmp, ttmp, sname)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension xtmp(MAX_tmp,3), stmp(MAX_tmp,3), ttmp(MAX_tmp)
      character*8 sname

      if (npts.lt.11 .or. kfilt.eq.0 ) return

c controls
      scut = 4.0d0
      xcut = 10.0d0
      nwindow = 10

      scut    = tfilt(kfilt,4)
      xcut    = tfilt(kfilt,5)
      twindow = tfilt(kfilt,6)/365.25d0
      nwindow = int(tfilt(kfilt,6))

       if ( xcut.eq.0.0d0 .or. nwindow.eq.0 ) return

      print *, 'Filter ', sname

      nout = 0
 
      do j=1,3

       do i=1, npts 
      
        sumx = 0.0d0
        sumw = 0.0d0
        avx = 0.0d0

        ti = ttmp(i)
        nk = 0

        do k=1,npts
         n = i+k-1
         dt = ttmp(n)-ti

         if ( dt .le. twindow ) then
          w = fnwt(stmp(n,j))
          sumw = sumw + w
          sumx = sumx + w*xtmp(n,j)
          nk = nk +1
          if ( n.eq.npts ) goto 22
         else
          goto 22
         endif

        enddo

   22  if (nk.gt.5 .and. sumw.gt.0.0d0 ) then 
       avx = sumx/sumw

     
        do k = 1,nk
         n = i+k-1
         dx = abs(xtmp(n,j) - avx)
         if (dx.gt.scut*stmp(n,j) .or. dx.gt.xcut ) then
           nout = nout+1
           if (verbose) write ( *, '("RM ",a4, 2i4, f12.4, 4f8.2)') 
     .         sname, j, nk, ttmp(n), xtmp(n,j), avx, dx, dx/stmp(n,j)
           stmp(n,j) = 1.0d3
         endif
        enddo

       endif

       enddo

      enddo
c       if (nout.gt.0 ) print *, 'nout ',nout

      return
      end

c***************************************************************************
c --- common mode filter
c --- take weighted averages of days and subtract from time series 
c --- use sites outside radius r (km) from point x,y (lon,lat)
c***************************************************************************
      subroutine commode (ifile, bname, kfilt )

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      parameter ( ndays = 9000 )
      dimension xsum(ndays,3), wsum(ndays,3)
      dimension nsum(ndays,3) 
      character*4 bname

      if ( kfilt.eq.0 ) return

      nn = 3*ndays
      call cleareal(xsum, nn)
      call cleareal(wsum, nn)
      call clearint(nsum, nn)

      t0 = gps_info(ifile,6)
      zero = 0.0d0

      x = tfilt(kfilt,1)
      y = tfilt(kfilt,2)
      r = tfilt(kfilt,3)

      if ( r .le. zero ) return

      print *, 'Common mode for ', bname, ' t0 ', t0
      do i=1, num_gps
       npts = ndx(i,2) - ndx(i,1)

       if (gps_type(i).eq.3 .and. gps_index(i).eq.ifile .and. 
     .    npts.gt.0 ) then

         x1 = gps_pos(i,1)
         y1 = gps_pos(i,2)
         call distkm (x,y,x1,y1, xkm)

         if (xkm.gt.r) then
          do n = ndx(i,1), ndx(i,2)
            t = t_disp(n)
            itime = int( (t - t0) * 365.25d0 ) + 1
             do j=1,3
               w = fnwt(x_sig(n,j))
               xsum(itime,j) = xsum(itime,j) + x_disp(n,j)*w
               wsum(itime,j) = wsum(itime,j) + w
               nsum(itime,j) = nsum(itime,j) + 1
             enddo
          enddo
        endif

       endif
      enddo

c print *, 'Common mode 2 '

c compute weighted average by day
      do n = 1, ndays
        do j=1,3
          if (wsum(n,j).gt.zero .and. nsum(n,j).gt.5 )
     .         xsum(n,j) = xsum(n,j)/wsum(n,j)
        enddo
      enddo

c print weighted average by day
      if ( verbose ) then
       do n = 1, ndays
        if (nsum(n,1).gt.5)
     .  write (*, '(i5, 3(i4,1x,f8.2))') n, (nsum(n,j),xsum(n,j),j=1,3)
       enddo
      endif

c correct data
      do i=1, num_gps
       npts = ndx(i,2) - ndx(i,1)
       if (gps_type(i).eq.3 .and. gps_index(i).eq.ifile .and. 
     .    npts.gt.0 ) then
         do n = ndx(i,1), ndx(i,2)
           t = t_disp(n)
           itime = int( (t - t0) * 365.25 ) + 1
             do j=1,3
               x_disp(n,j) = x_disp(n,j) - xsum(itime,j) 
             enddo
         enddo
       endif
      enddo


      return
      end

      
c***************************************************************************
c* IS:  shortname, filename, source_number, wtfac, Tmin, Tmax, heading, inc_angle, dx, dy, offset
c***************************************************************************

      subroutine readinsar (insfile, bname, nf, a3, nin, iflip)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      dimension trm(1000,2)

      character*1 a3
      character*4 c4, bname, cname
      character*80 insfile, gf, a80
      character*256 a1, a2
      
      logical fncomment, kformat, iflip, keep, fnend, fexist
      logical f0, f1, f2, f3, f4, fg

      logical write_input
      common /cm3/ write_input

      kformat = .false.
      ten = 10.0d0
      nin = 0
      xsum = 0.0d0
      ysum = 0.0d0
      ndec = 0
      ng=0
      nread = 0
      nkept = 0

c insar_info items:
c 1 Index
c 2 Weight factor
c 3 Time 1
c 4 Time 2
c 5 Heading
c 6 Incidence angle
c 7 Offset
c 8 Planar X slope
c 9 Planar Y slope
c 10 Ndec - Tropospheric slope
c 11 Max sigma
c 12 Min sigma

c Third character   Format
c                   Lon  Lat  LOS sigma   (default) 
c  1                Lon  Lat  LOS sigma  Ux Uy Uz
c  2                Lon  Lat  LOS sigma  Elevation(meters)
c  3                Lon  Lat  Elevation(km)  LOS  sigma
c  4                Lon  Lat  Elevation(km)  LOS  sigma  Ux Uy Uz
c  g                GLOBK format 

c - which formats to use      
      fg = (a3.eq.'g')
      f1 = (a3.eq.'1')
      f2 = (a3.eq.'2')
      f3 = (a3.eq.'3')
      f4 = (a3.eq.'4')
      f0 = ( .not. f1 .and. .not. f2 .and. .not. f3 .and. .not. fg 
     .   .and. .not. f4 ) 

c skip file if wtfac=0      
      wtfac = insar_info(nf,2)
      if ( wtfac.eq.zero)  return

c-- max sigma
      smax = insar_info(nf,11)
      if (smax.eq.0.0d0 ) smax = 1.0d6
      
c-- min sigma
      smin = insar_info(nf,12)
      
c-- decimation factor      
      ndec = int(insar_info(nf,10)) + 1
      kr = ndec - 1

c-- file
      k12=kfopen(12)
      open (k12, file = insfile)

c data to be removed by RI: line
       nrm = 0
       krm = 14
       do kk=1, num_f_rm
c       print *, rm_filename(kk)
        if (krm_filename_type(kk).eq.2) then         
         call existfile( rm_filename(kk), fexist, 1)
          if (fexist) then
          krm=kfopen(krm)
         open (krm, file = rm_filename(kk) )
   51    read (krm, '(a80)', end=91 ) a80
c         print *, a80
         if (fncomment(a80(1:1)) ) go to 51
         read(a80, *) cname, rx, ry
         if ( cname .eq. bname ) then
           nrm = nrm +1
           trm(nrm,1) = fnlong(rx)
           trm(nrm,2) = ry
         endif
         goto 51
   91    ik = kfclose(krm)
        endif
        endif
       enddo

      
c-- reading insar loop      
c      do 5 kk = 1, 32000

  5   elev = 0.0d0
      x = 0.0d0
      y = 0.0d0
      Ve = 0.0d0
      ere = 0.0d0

c-- Insar data
      read (k12, '(a250)', end=99, err=15) a1
      if (write_input) print *, a1

  15  if ( fnend(a1(1:3)) ) go to 99

c-- see if format is specified
      if ( a1(1:1).eq.'(' .or. a1(1:2).eq.'#(' ) then
        if ( a1(1:1).eq.'('  ) gf = a1(1:80)
        if ( a1(1:2).eq.'#(' ) gf = a1(2:79)
        kformat = .true.
        go to 5
      endif
      if ( len(a1).lt.5 .or. fncomment(a1(1:1)) ) go to 5

c nitems      
      call count_items(a1, a2, nitems)
      nread = nread +1
c      print *, a1, nitems

c(2f11.5, f9.2, f8.2, 16x, 2f8.2, f7.3, f10.2, 8x, f8.2, 1x, a8)
c 183.43415  -43.95578   -37.79   52.58    0.88   -0.80    0.28    0.26 -0.160      2.02    1.15    0.48 CHAT 
c 204.54366   19.80136   -58.51   52.04    0.78   -0.43    0.29    0.28 -0.095     -3.17   -2.74    0.77 MKEA 
c-- read input line - globk format, read verticals only
      if (fg) then
        kformat = .true.
        gf = '(2f11.5, 56x, f10.2, 8x, f8.2)'
        Ux = 0.0d0
        Uy = 0.0d0 
        Uz = -1.0d0
       endif

       if ( kformat ) then
        if (fg) read(a1, gf ) x, y, Ve, ere     
        if (f0) read(a1, gf ) x, y, Ve, ere 
        if (f1) read(a1, gf ) x, y, Ve, ere, Ux, Uy, Uz 
        if (f2) read(a1, gf ) x, y, Ve, ere, elev 
        if (f3) read(a1, gf ) x, y, elev, Ve, ere 
        if (f4) read(a1, gf ) x, y, elev, Ve, ere, Ux, Uy, Uz
       else
        if (f0) read(a1, * ) x, y, Ve, ere 
        if (f1) read(a1, * ) x, y, Ve, ere, Ux, Uy, Uz 
        if (f2) read(a1, * ) x, y, Ve, ere, elev 
        if (f3) read(a1, * ) x, y, elev, Ve, ere 
        if (f4) read(a1, * ) x, y, elev, Ve, ere, Ux, Uy, Uz
       endif


       if (f2) elev = elev/1.0d3

       if(a3.eq.'9') then
         Ve = Ve * ten
         ere = ere * ten
        endif

       ere = max(ere, smin)

        if (iflip) Ve = -Ve

c-- unit vector to satellite, read from data if heading, incidence angle not given      
      if ( insar_info(nf,5) .ne. 0.0d0 ) then
       hd  = insar_info(nf,5)*d2r
       ang = insar_info(nf,6)*d2r
       Ux = -dcos(hd)*dsin(ang)
       Uy =  dsin(hd)*dsin(ang)
       Uz =  dcos(ang)
      endif

      x = fnlong(x)
      kr = kr + 1

c  outside data region (DR:)
      keep = .true.
c      call check_region (x, y, keep)

c check for points to remove by RI: within 10 meters
      do kkk=1,nrm
        call distkm (x, y, trm(kkk,1), trm(kkk,2), dx)
        if ( dx .lt. 1.0d-2 ) keep = .false.
      enddo
c*** 
c      if ( .not. ( ere .le. smax) ) print *, 'smax', smax
c      if ( .not. (kr .ge. ndec ) )  print *, 'kr', kr, ndec


      if ( ere .le. smax .and. keep  .and. kr .ge. ndec ) then

      num_insar = num_insar + 1

      if (num_insar.gt.MAX_insar_pts) then
        print *, '**** MAX_insar_pts exceeded ****'
       call stop1
      endif

      k = num_insar
      nin = nin+1
      nkept = nkept +1

      insar_pos(k,1) = x
      insar_pos(k,2) = y
      insar_pos(k,3) = elev 
      insar_obs(k,1) = Ve
      insar_obs(k,2) = ere*wtfac 
      insar_unit(k,1) = Ux 
      insar_unit(k,2) = Uy 
      insar_unit(k,3) = Uz 
      insar_file(k) = nf
      call i2c (nin, 4, c4)
      insar_sname(k) = c4
      
      xsum = xsum + x
      ysum = ysum + y
      kr = 0
      endif
      
      goto 5
        
c    5 continue

   99 ik = kfclose(k12)
   
c-- center of data   
      if (nin.gt.0 ) then
       insar_tmp(nf,1) = xsum/real(nin)
       insar_tmp(nf,2) = ysum/real(nin)
      endif

c      print *, 'NG Nr Nk', ng, nread, nkept

c-- unit vector to sattelite      
c      hd  = insar_info(nf,5)*d2r
c      ang = insar_info(nf,6)*d2r
c      insar_tmp(nf,3) = -dcos(hd)*dsin(ang)
c      insar_tmp(nf,4) =  dsin(hd)*dsin(ang)
c      insar_tmp(nf,5) =  dcos(ang)
      
c      insar_tmp(nf,3) = -0.606d0
c      insar_tmp(nf,4) = -0.129d0
c      insar_tmp(nf,5) =  0.785d0

      return
      end

c**********************************************************************

      subroutine getplate ( pname, nplate, block_check)
      
c* get the block number for block with name 'pname'

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character*4 pname, block_name 
      logical block_check

      nplate=0

      if ( nblocks.eq.0) return

      do j=1, nblocks
c       if ( block_flag(j) .and.
c     .     pname.eq.block_name(j) ) nplate = j
       if ( pname.eq.block_name(j) ) nplate = j
      enddo

      if ( nplate.eq.0 .and. block_check) 
     .    print *, 'Block '//pname//' is not defined '

      return
      end
c**********************************************************************

      subroutine getnodexy(xpt, ypt, inf, inx, inz, xoffset)

c get x and y of node 
c if xoffset > 0 move surface nodes slightly downdip so the hanging wall is found  
c if xoffset < 0 move surface nodes slightly updip so the footwall is found  

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dxx = 0.0d0
      dyy = 0.0d0
      kz=inz
      kx=inx
      kf=inf
      xpt=xynode(1, kx, kz, kf) 
      ypt=xynode(2, kx, kz, kf) 

      if(xoffset.eq.0.0d0 .or. kz.gt.1) return

c-- calculate offset positions
      if ( kz.eq.1) then

        dx = xynode(1, kx, 2, kf) - xynode(1, kx, 1, kf)
        dy = xynode(2, kx, 2, kf) - xynode(2, kx, 1, kf)
        s = dsqrt ( dx*dx + dy*dy )
        dxx = xoffset * dx/s
        dyy = xoffset * dy/s

       if ( kx.eq.1) then
        dx = xynode(1, 2, kz, kf) - xynode(1, 1, kz, kf)
        dy = xynode(2, 2, kz, kf) - xynode(2, 1, kz, kf)
        s = dsqrt ( dx*dx + dy*dy )
        dxx = dxx + xoffset * dx/s
        dyy = dyy + xoffset * dy/s
       elseif ( kx.eq.nxf(kf)) then
        dx = xynode(1, kx-1, kz, kf) - xynode(1, kx, kz, kf)
        dy = xynode(2, kx-1, kz, kf) - xynode(2, kx, kz, kf)
        s = dsqrt ( dx*dx + dy*dy )
        dxx = dxx + xoffset * dx/s
        dyy = dyy + xoffset * dy/s
       endif

      endif

        xpt=xynode(1, kx, kz, kf) + dxx
        ypt=xynode(2, kx, kz, kf) + dyy

      return
      end

c*********************************************************************
      subroutine closest_blocks(xpt, ypt, kp1, kp2, kb1,kb2,kf1, kch)

c** find two closest blocks and fault for a data point
c*  if different than input block pair kp1 and kp2, set kchange to true 

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      logical kch
      character*4 block_name      
      
      kch = .false.
      
      kb1=0
      kb2=0
      kf1=0
      dxmin = 1.0d9
      ymin = zero
      xmin = zero
      
      
c* first find min distance and point
      do j=1, nblocks
       if(block_flag(j)) then

       do i=1, nc_block(j)-1
         xc=blockxy(i,j,1)
         yc=blockxy(i,j,2)
         call distkm(xpt, ypt, xc, yc, d)
         if ( d.lt.dxmin) then
           dxmin = d
           xmin = xc
           ymin = yc
         endif
       enddo
      endif
      enddo
      
c* find two blocks that have this point   
      k=0   
      do j=1, nblocks
       if(block_flag(j)) then
       do i=1, nc_block(j) -1
         if (blockxy(i,j,1).eq.xmin .and. blockxy(i,j,2).eq.ymin) then
          k=k+1
          if ( k.eq.1) kb1= j
          if ( k.eq.2) kb2= j
         endif
       enddo
       endif
      enddo
      
       
c* first find min distance and point for faults
      dfxmin = 1.0d9
      kf1 = 0
      minix = 0

      do kf=1, nfault
       if(fflag(kf,4)) then
        do ix=1, nxf(kf)
          x1 = xynode(1,ix,1,kf)
          y1 = xynode(2,ix,1,kf)
          call distkm(xpt,ypt,x1,y1,d)
           if ( d.lt.dfxmin) then
             dfxmin = d
             kf1 = kf
             minix = ix
           endif
         enddo
       endif
      enddo

c       if ( (kp1.ne.kb1 .and. kp1.ne.kb2) .or. 
c     .   (kp2.ne.kb1 .and. kp2.ne.kb2) .and. chk_data ) then
        write (*, '( "Input blocks:",2(1x,a4),"  Nearest blocks:",
     .    2(1x,a4),"  Dist (km): ",f8.1, " Nearest fault",i4," Dist:",
     .    f8.1)') 
     .    block_name(kp1), block_name(kp2), block_name(kb1), 
     .    block_name(kb2), dxmin, kf1, dfxmin
        kch = .true.
c       endif

      return
      end

c*********************************************************************
c** perpendicular distance from a point (x3, y3) to a line wirh
c    endpoints (x1,y1) (x2,y2)
c      subroutine perpdist(x1, y1, x2, y2, x3, y3, dist)
c
c      implicit real*8 (a-h,o-z)
c      A = (y2-y1)/(x2-x1)
c      B = -1.0d0
c      C = y1 - A * x1
c      dist = abs((A*x3 + B*y3 + C) / sqrt(A*A + B*B))
cc      print *, dist
c      end
      
c*********************************************************************
      subroutine nodeblock(xpt, ypt, node_block)

c** which block is node at (xpt, ypt) under

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension xcc(MAX_corner), ycc(MAX_corner), kedge(MAX_block)

      node_block=0
      nedge = 0

      do 10 j=1, nblocks
       if(block_flag(j)) then
       nb = nc_block(j) 

       do i=1, nb 
         xcc(i)=blockxy(i,j,1)
         ycc(i)=blockxy(i,j,2)
       enddo

         call inside ( xpt, ypt, xcc, ycc, nb, insde)

         if ( abs(insde).eq.1 ) then 
            node_block = j 
            return
         endif

         if ( abs(insde).eq.2 ) then
           nedge = nedge + 1
           kedge(nedge) = j
           node_block = j
         endif
       endif
   10 continue

      if (nedge.gt.0 ) then
        write (*, 20) xpt,ypt, (kedge(i), i=1,nedge)
   20   format ('Node at ', 2f9.4, ' on edge of blocks', 20i4)
      endif

      return
      end
c**********************************************************************
      subroutine readfault(fname, kf, a3, c80, kerr)

c  read fault number kf

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      logical write_input
      common /cm3/ write_input

c      integer c2i

      logical inlatlon, kflip, block_check, kerr, nx999
      logical nflag(MAX_x), zflag(MAX_z), fncomment, very_verbose
      logical fexist

      character*1 a3
      character*2 c2
      character*3 fnumber
      character*4, hw, fw, fw2(MAX_x), block_rename
      character*10 fname
      character*20 args(100)
      character*80 c80
      character*256 fline, fline2

      dimension fxy(MAX_x, 2), fdip(MAX_x), ndip(MAX_x)
      dimension xkk(8), xznew(MAX_z,3)
      dimension polyf(1000,2), ztmp(MAX_z)
      dimension kfw2(MAX_x)


      ndim=MAX_x
      kflip = .false.
      kerr = .false.
      very_verbose =.false.
      pg = 0.0d0
      ks = 0
      kdd = 0
      block_check = .true.
      nx999 = .false.
      kfault = k10
      
c set flags to false
      fflag(kf,1) = .false.
      fflag(kf,3) = .false.


c fault in external file
      call existfile( c80, fexist, 0)
      if ( fexist ) then
        print *, 'Reading fault file ', kf, c80
        kfault=kfopen(33)
        open (kfault, file = c80 )
        read(kfault, '(a256)') fline
      endif

      if ( nxf(kf).gt.0) 
     .   print *, '*** Multiple faults with number: ', kf
      call clearlog(nflag, MAX_x)
      call clearlog(zflag, MAX_z)
     
      call i2c(kf, 3, fnumber)

      read(kfault, '(a256)') fline
       if(write_input) print *, fline

      read(fline,*,err=2,end=2) nxf1, nzf(kf), hw, fw, ks 
 2    continue
 
      fault_name(kf) = fname
      hw = block_rename(hw)
      fw = block_rename(fw)
      fwname(kf,1) = block_rename(hw)
      fwname(kf,2) = block_rename(fw)

c      if(expname.eq.'ecsz') nzf(kf) = min (3,nzf(kf) )

c-- if Nx=9999 read to end of input where lon=9999.
       if ( abs(nxf1).eq.9999) then
         nx999 = .true.
         nxf(kf) = MAX_x
       else
         nxf(kf) = abs(nxf1)
       endif
         
      if ( nxf1.lt.0 ) kflip = .true.

      if ( nxf(kf).gt.MAX_x ) then
       print *, 'Fault ',kf,' MAX_x exceeded '
       kerr = .true.
       if (fexist) ik = kfclose(kfault)
       return
      endif

      if ( nzf(kf).gt.MAX_z ) then
       print *, 'Fault ',kf,' MAX_z exceeded '
       kerr = .true.
       if (fexist) ik = kfclose(kfault)
       return
      endif

c set default fault flags
      fflag(kf,1) = .true.
      fflag(kf,2) = ( ks.eq.1 )
      fflag(kf,3) = .true.
      fflag(kf,4) = .true.
      fflag(kf,5) = .true.
      ksliptype(kf) =  ks

c pseudo fault
      if ( nzf(kf).eq.1) then
       fflag(kf,1) = .false.
       fflag(kf,3) = .false.
      endif


      fault_fit_type(kf) = 0

      call getplate (hw, khw, block_check)
      call getplate (fw, kfw, block_check)

c default input is lon,lat

       inlatlon = ( a3.eq.'1' )
       iz=0
       iztmp = 1
      
       do 555 izz1=1, nzf(kf)

          iz=iz+iztmp
          if (iz.gt.nzf(kf)) goto 666

          iztmp = 1

         read (kfault, '(a256)') fline

         if (write_input) print *, fline

         c2 = fline(1:2)
         call lcase(c2,2)
         l = index(fline, ':')
         fline2 = fline(l+1: len(fline)-l)

c* read ND: to add a new depth contour
         if ( c2.eq.'nd' .and. iz.gt.1) then
           read (fline2,*) ztmp(iz)
           zflag(iz) = .true.
           goto 555
         endif 

c* read MD: to add multiple new depth contours - working on this
c         if ( c2.eq.'md' .and. iz.gt.1) then
c           read (fline2,*) dz, nz
c           do k=1,nz
c            iz=iz+iztmp
c            ztmp(iz) = ztmp(iz-1) + dz
c            zflag(iz) = .true.
c           enddo
c           goto 555
c         endif 

c** read in DD: or ZD: lines and process
c*  fdepth is the depth increment from previous contour
c*  DD: reads the depth increment; ZD: reads the depth

         if ( (c2.eq.'dd' .or. c2.eq.'zd') .and. iz.gt.1) then

         if (iz.eq.2 ) call domoves(0, kf)

          dd_line(kf,iz-1) = fline
          call cleareal(fdip, MAX_x)
          call clearint(ndip, MAX_x)

          read (fline2,*, err=33,end=33) fdepth, 
     .      (fdip(i), ndip(i), i=1,MAX_x)

  33      continue

c         if (myflag) then
c***** SCEC test
c          if (scec .and. iz.eq.2 ) fdepth = 15.0
c         endif

          if ( c2.eq.'zd') fdepth = fdepth - znode(iz-1,kf)

          do ix=1,nxf(kf)
           fxy(ix,1) = xynode(1,ix, iz-1, kf)
           fxy(ix,2) = xynode(2,ix, iz-1, kf)
          enddo

          call faultz ( kf, fxy, ndim, nxf(kf), fdip, ndip, fdepth) 

          do ix=1,nxf(kf)
            xynode(1,ix, iz, kf) = fxy(ix,1)
            xynode(2,ix, iz, kf) = fxy(ix,2)
          enddo

          znode(iz,kf)=znode(iz-1,kf)+fdepth
          intX=1
          intZ=1

         else

          read (fline2,*) z
           if (nzf(kf).eq.1) z = 0.0d0
           znode(iz,kf)=z
           if (write_input) print *, 'Depth ', iz, z

c-- read X-values      
      nxf1=nxf(kf)

      do 22 ix = 1, nxf1

       call cleareal(xkk, 8)
       ifk = 0
       na = 0

c parse the x, y line 
   23  read (kfault, '(a256)') fline     
       if (fncomment (fline(1:1) ) ) goto 23
         
c       ix = ix+1
c      if(ix.gt.nxf1) goto 333
   
       if(write_input) print *, fline

       call parse(fline, args, na)
        if (na.gt.0) x=c2r(args(1))
        if (na.gt.1) y=c2r(args(2))

c ifk is flag for options
c   1 = set dip azimuth 
c   2 = add new node between this node and next along strike
c   3 = set footwall to next argument
c   4 = make polynomial node (z) function [not working]

       if (na.gt.2) then
         rfk = c2r(args(3))
         ifk = int(rfk)
       endif

       if ( ifk.eq.3 ) then
         fw2(ix) = args(4)
       else
         fw2(ix) = fw
       endif

       fw2(ix) = block_rename(fw2(ix))
       call getplate (fw2(ix), kfw2(ix), block_check)

c read rest of line
        if (na.gt.3 .and. ifk .ne. 3 ) then
         do k=1,na-3
          xkk(k) = c2r(args(k+3))
         enddo
        endif

       if ( nx999 .and. x.gt. 999.0) then
          nxf(kf)=ix-1
          goto 333
       endif

       fault_dip_az(kf,ix) = 0.0d0

c* check for special conditions
c** make fault dip in specified direction (use with dd: or zd:)
       if (iz.eq.1 .and. ifk.eq.1) fault_dip_az(kf,ix) = xkk(1)

c** add new node after this one
       if (iz.eq.1 .and. ifk.eq.2) nflag(ix) = .true.

       if (inlatlon) call swap(x,y)
       x = fnlong(x)

c* reverse order along strike if requested
       ixx=ix
       if ( kflip ) ixx = nxf(kf) - ix + 1

       xynode(1,ixx,iz,kf) = x
       xynode(2,ixx,iz,kf) = y


c***************************************************
c* make polynomial slab structure
c* as of 07.08.01 not working quite right
c*  read in starting point (lon, lat), dip angle at that point
       if ( iz.eq.1 .and. ifk.eq.4 ) then
        nopt = 1
        x2l = xkk(1)
        y2l = xkk(2)
        z2  = xkk(3)
        dip1 = xkk(4)
        dip2 = xkk(5)
        azimuth = xkk(6)
        dznew = xkk(7)
        nznew = int(xkk(8))

c        do ii = 1,8
c          print *, ixx,xkk(ii)
c        enddo

        call polyfault (nopt, x, y, z, x2l, y2l, z2, dip1, dip2, 
     .      azimuth, dznew, nznew, xznew)

        do inz = 1, nznew
c          print *, iz+inz,xznew(inz,1), xznew(inz,2), xznew(inz,3)
           xynode(1,ixx,iz+inz,kf) = xznew(inz,1)
           xynode(2,ixx,iz+inz,kf) = xznew(inz,2)
           znode(iz+inz,kf) = xznew(inz,3)
        enddo
           iztmp = nznew

       end if
c***************************************************

   22 continue

c apply snap
c  333 print *, 'snap fault ', snap, kf
  333 if (snap .gt. 0.0d0) then
c       print *, 'snap fault ', snap, kf
       do kkf = 1, MAX_f
         if (nxf(kkf).gt.0 .and. kkf.ne.kf) then
           iz = 1
           do ix=1,nxf(kkf)
             xf = xynode(1,ix,iz,kkf)
             yf = xynode(2,ix,iz,kkf) 
            do ixx =1,nxf(kf)
              call distkm(xf, yf, xynode(1,ixx,iz,kf), 
     .              xynode(2,ixx,iz,kf), dkm)
              if (dkm .le. snap) then
                xynode(1,ixx,iz,kf) = xf
                xynode(2,ixx,iz,kf) = yf
              endif
            enddo
          enddo
        endif
       enddo
      endif
c end of snap

c  333 continue 


      endif

  555 continue

  666 continue

c** move the points
      call domoves(0, kf)

c** add interpolated nodes between points
 777  n=nxf(kf)
      do ix=1,n-1
       if ( nflag(ix) ) then
c        kfw2(ix+1) = kfw2(ix)
        do iz = 1, nzf(kf)

        xnew = (xynode(1,ix,iz,kf) + xynode(1,ix+1,iz,kf))/2.0d0
        ynew = (xynode(2,ix,iz,kf) + xynode(2,ix+1,iz,kf))/2.0d0

           do i= 1, n-ix
            xynode(1,n-i+2,iz,kf) = xynode(1,n-i+1,iz,kf) 
            xynode(2,n-i+2,iz,kf) = xynode(2,n-i+1,iz,kf)
            if(iz.eq.1) nflag(n-i+2) = nflag(n-i+1)
           enddo
           
            xynode(1,ix+1,iz,kf) = xnew
            xynode(2,ix+1,iz,kf) = ynew
            nflag(ix+1) = .false.

        enddo
        nxf(kf) = nxf(kf)+1
        nflag(ix) = .false.
        goto 777
        
       endif
      enddo

      do iz=1,nzf(kf)
       do ix=1, nxf(kf)
        khw_blk(ix,iz,kf)=khw
c        kfw_blk(ix,iz,kf)=kfw2(ix)
        kfw_blk(ix,iz,kf)=kfw 
       enddo
      enddo
      depth_max(kf) = znode(nzf(kf),kf)
      psmax(8) = max( psmax(8), znode(nzf(kf),kf))

c** add new depth contours
      do  izn=1, nzf(kf)
       if ( zflag(izn) ) then
        znew = ztmp(izn)
        call new_contour (kf, izn, znew, zflag )
       endif
      enddo

c** get xwnode() array, the x and w coordinates on the fault surface, in kms
      do iz=1,nzf(kf)
       do ix=1, nxf(kf)
        do j=1, 2
         xwnode(j, ix, iz, kf) = 0.0d0
        enddo
       enddo
      enddo

c-- set NN_in if no nn: line
      if ( .not. nn_read(kf) ) then
       nn = 0
c       fflag(kf,1) = .false.
       do iz=1,nzf(kf)
        do ix=1, nxf(kf)
         nn = nn+1
         NN_in(kf, nn) = 1
         if( .not. nv_read(kf) ) VN_in(kf, nn) = 1.0d0
        enddo
       enddo
      endif

c-- distance along strike
      do iz=1,nzf(kf)
       do ix=2, nxf(kf)
        x1 = xynode(1,ix-1,iz,kf)
        y1 = xynode(2,ix-1,iz,kf)
        x2 = xynode(1,ix,iz,kf)
        y2 = xynode(2,ix,iz,kf)
        call distkm(x1,y1,x2,y2,dd)
        xwnode(1, ix, iz, kf) = xwnode(1, ix-1, iz, kf) + dd
       enddo
      enddo

c-- distance downdip
      do ix=1, nxf(kf)
       do iz=2,nzf(kf)
        x1 = xynode(1,ix,iz-1,kf)
        y1 = xynode(2,ix,iz-1,kf)
        x2 = xynode(1,ix,iz,kf)
        y2 = xynode(2,ix,iz,kf)
        call distkm(x1,y1,x2,y2,dd)
        dz = znode(iz,kf) - znode(iz-1,kf)
        d = dsqrt(dd*dd + dz*dz)
        xwnode(2, ix, iz, kf) = xwnode(2, ix, iz-1, kf) + d
       enddo
      enddo
      
c- get fault azimuth      
      ddx=0.0d0
      call getnodexy ( xn1, yn1, kf, 1,       1, ddx)
      call getnodexy ( xn2, yn2, kf, nxf(kf), 1, ddx)
      call delaz ( yn2, xn2, yn1, xn1, d, az )
      av_fault_strike(kf) = az

c-- make polygon from fault outline
      call makefaultpoly (kf, polyf, npf)
      nfault_poly(kf) = npf
      do i=1,npf
       do j=1,2
        fault_poly(i,j,kf) = polyf(i,j)
       enddo
      enddo
      
      if (very_verbose) then
      call fopen (kf18, 1, '_'//fnumber//'_fault.tmp ')
       write(kf18, '(a1,1x,a10)' ) '>',fname
       do iz = 1, nzf(kf)
        write(kf18, '(a1,1x,f10.3)' ) '>',znode(iz,kf)
        write(kf18, '(2f10.4, 2i6)' ) 
     .    (xynode(1,ix,iz,kf),  xynode(2,ix,iz,kf),
     .    khw_blk(ix,iz,kf), kfw_blk(ix,iz,kf),
     .    ix=1,nxf(kf) )
       enddo
      ik = kfclose (kf18)
      endif

      if (fexist) ik = kfclose(kfault)

      return
      end

c**********************************************************************

      subroutine polyfault (nopt, x1l, y1l, z1, x2l, y2l, z2, 
     .      dip1, dip2, azimuth, dznew, nznew, xznew)

c*  calculate downdip node positions using polynomial
c*
c* x1, y1, z1, dip1 - first point and its dip
c* x2, y2, z2, dip2 - last point and its dip
c* azimuth - azimuth of new line of points
c* dznew - depth increment for new points
c* nznew - number of new points
c* xznew - has new x, z points
c*
c* fits polynomial x = a z^2 + b z +c

      implicit real*8 (a-h, o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      dimension xznew(MAX_z, 3)
      two = 2.0d0
      one = 1.0d0
c      print *, nopt, x1l, y1l, z1, x2l, y2l, z2, 
c     .      dip1, dip2, azimuth, dznew, nznew

      s1 = one/tan(dip1*d2r)
      s2 = one/tan(dip2*d2r)

c* lon, lat to x,y
      call project( x2l, y2l, x1l, y1l, x2, y2)
      x1 = 0.0d0
      y1 = 0.0d0
      x2 = dsqrt(x2*x2 + y2*y2)
      call delaz(y2l, x2l, y1l, x1l, del, az)
      x2  = del*d2x
      nopt = 1
   
c* nopt 1 => 2 dips, start x,z, end z
      if ( nopt.eq.1 ) then
        a = (s1-s2)/(two*(z1-z2))
        b = s1 - two*a*z1
        c = x1 - a*z1*z1 - b*z1

c* nopt 2 => 2 positions and starting dip
      elseif ( nopt.eq.2 ) then
        dz2 = (z1*z1 - z2*z2)
        dz = z1 - z2
        dx = x1 - x2
        a = (dx - s1 * dz)/(dz2 - two*z1*dz)
        b = s1 - two*a*z1
        c = x1 - a*z1*z1 - b*z1

      endif

c       print *, az

      azz = az
      if ( azimuth .ne. 0.0d0) azz = azimuth

      do iz = 1, nznew
       z = z1 + real(iz)*dznew
       xx = a*z*z + b*z + c
       x = x1 + xx*dsin(azz*d2r)
       y = y1 + xx*dcos(azz*d2r)
       call unproject( x, y, x1l, y1l, xnew, ynew)

       xznew(iz,1) = xnew
       xznew(iz,2) = ynew
       xznew(iz,3) = z
c       print *, xnew, ynew, z
      enddo

      return
      end

c**********************************************************************
      subroutine faultz ( kf, fxy, ndim, n, fdip, ndip, fdepth)

c  calculate downdip node positions given dips and depths
c*  from previous nodes (updip)

      implicit real*8 (a-h, o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical nodecheck

      dimension fxy(ndim,2), fxy2(ndim,2), fdip(ndim), ndip(ndim),
     .  dip(ndim)

      nodecheck = .false.

      fd=fdepth

c* assign dip angles
      k=0
      if (ndip(1).eq.0) then
       do i=1,n
        dip(i) = fdip(1)
       enddo
      else
       do j=1,n
        m = ndip(j)
        if ( m.gt.0) then
         do i=1,m
          k=k+1
          dip(k) = fdip(j)
         enddo
        endif
       enddo
      endif
      
      do i=1,n

      td=dtan(dip(i)*d2r)
      ds = fd/td

      if (i.eq.1) then
        xlat1 = fxy(1,2)
        xlon1 = fxy(1,1)
        xlat2 = fxy(2,2)
        xlon2 = fxy(2,1)
      else if (i.eq.n) then
        xlat1 = fxy(n-1,2)
        xlon1 = fxy(n-1,1)
        xlat2 = fxy(n,2)
        xlon2 = fxy(n,1)
      else
        xlat1 = fxy(i-1,2)
        xlon1 = fxy(i-1,1)
        xlat2 = fxy(i+1,2)
        xlon2 = fxy(i+1,1)
      endif

      call delaz(xlat2, xlon2, xlat1, xlon1, del, az)

      a = (az+90.d0)*d2r
      dipaz = fault_dip_az(kf,i)
      if ( dipaz .ne. 0.0d0) a = dipaz*d2r

      dx = ds * dsin(a)
      dy = ds * dcos(a)

      xlon=fxy(i,1)
      xlat=fxy(i,2)

      fxy2(i,1) = xlon + dx*x2d / dcos(xlat*d2r)
      fxy2(i,2) = xlat + dy*x2d

      enddo

c** check for overlap of nodes by automatic node generation
c** turned off (nodecheck is false), does not work well
      if ( nodecheck ) then

        xlat1 = fxy2(1,2)
        xlon1 = fxy2(1,1)
        xlat2 = fxy2(2,2)
        xlon2 = fxy2(2,1)
        call delaz(xlat2, xlon2, xlat1, xlon1, del, az1)

      do i=2,n-1
        xlat1 = fxy2(i,2)
        xlon1 = fxy2(i,1)
        xlat2 = fxy2(i+1,2)
        xlon2 = fxy2(i+1,1)
        call delaz(xlat2, xlon2, xlat1, xlon1, del, az)
         if ( abs(az1-az).gt.90.0d0) then
           write(*, 1) i, fdepth, az, az1
           a = az*d2r
           ds=0.05
c           fxy2(i+1,1) = fxy2(i,1)+ ds * dsin(a)
c           fxy2(i+1,2) = fxy2(i,2)+ ds * dcos(a)
        endif
        az1=az
      enddo
  1   format("Overlap - fixing: ", i4, f6.2, 2f10.3)

      endif

      do i=1,n
       fxy(i,1)=fxy2(i,1)
       fxy(i,2)=fxy2(i,2)
      enddo

      return
      end

c**********************************************************************
      subroutine sumstrain(ix, iz, kf, amp, str)
c add up subsurface strains at nodes

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      dimension str(6)

      a = amp
      iix = ix
      iiz = iz  
      kkf = kf

      do i=1,6
        s = a*str(i)
c*st*       s_tensor(iix,iiz,kkf,i) = s_tensor(iix,iiz,kkf,i) + s
      enddo

      return
      end

c**********************************************************************
      subroutine RECTCALC
      
c performs forward calculation using rectangular dislocations

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
 
      character*4 info_lbl*200
      character*2 cm
      character*3 c3
      logical nonzero_slip, outatr, flock

      dimension Uc(3), u0(3,3)
      dimension xsub(4), ysub(4), zsub(4)
      dimension x(4), y(4), z(4), u(4), v(4), s(4), d(4), dd(4), e(4)
      dimension xww(4), www(4)
      dimension p(4)
      dimension tilt_tmp(MAX_tilt, 2)

      data xd4, x99, thou /1.0d4, 99.0d0, 1.0d3/

      outatr = fault_atr
      tmom = zero
      smom = zero
      pmom = zero

      info_lbl='Segment            Xf       Yf     Ztop     Zbot    
     . Width   strike     dip      Rake     U.ss     U.ds     U.te
     . U.tot  Length   d1/d2   Afac    Area'

      call uminmax (umin, umax) 
      call cleareal(tilt_tmp, 2*MAX_tilt)

c** open plot files if not making GFs
      if ( .not. makeGF .or. .not. readgflag ) then     

      if (use_crust2) call rdcrust2
c      if (myflag) print *, 'RECTCALC'
 

c* general info about fault
      if (write_info) then
       call fopen (kinfo, 1,  '_info.out ') 
       write (kinfo, '(a200)') info_lbl
      endif

c* plotting file for locking distributions
      call fopen (katr, 1,  '_flt_atr.gmt ') 

c moment by profile     
c      call fopen (k22, 1,  '_flt_Mo.gmt ')
      endif


      call cleareal (fault_sums, 7*MAX_f)


c************************************ start integrating
      do 50 kf=1, nfault

       if (flock(kf) ) then

c for individual faults
      if ( outatr ) then
        call i2c (kf, 3, c3)
        call fopen (katr2, 1,  '_flt_'//c3//'_atr.gmt ') 
      endif

c sum moments by profile
c      call cleareal (xmom_prof,  2*MAX_x)

      nx=nxf(kf)
      nz=nzf(kf)

      tmom = zero
      pmom = zero
      smom = zero
      tot_sliparea = zero
      tot_uA = zero

      if (.not. makeGF) print *, '  ',fault_name(kf)

c* integrate over fault  
c*  loop through elements, each bounded by 4 nodes
c*    * = nodes around quadrilateral element
c*
c*  updip    1 *---------------------* 2 --> strike direction
c*             |                     |
c*             |                     |
c*             |                     |
c*             |                     |
c*  downdip  4 *---------------------* 3
c*

  
      do 40 iz=1, nz-1
        do 30 ix=1, nx-1

c        print *, 'doing Kf, Ix, Iz ', kf, ix, iz
      
c-- corner nodes of this element, starting in +X direction
      call setxyz(ix,  iz,  kf,x(1),y(1),z(1),u(1),v(1),e(1),p(1))
      call setxyz(ix+1,iz,  kf,x(2),y(2),z(2),u(2),v(2),e(2),p(2))
      call setxyz(ix+1,iz+1,kf,x(3),y(3),z(3),u(3),v(3),e(3),p(3))
      call setxyz(ix,  iz+1,kf,x(4),y(4),z(4),u(4),v(4),e(4),p(4))

c      if (nonzero) then

c-- get dips d() at 4 corners of element
      call trislope(x(4),y(4),z(4),x(1),y(1),z(1),x(3),y(3),
     .   z(3), dp1, st)

      call trislope(x(4),y(4),z(4),x(2),y(2),z(2),x(3),y(3),
     .   z(3), dp2, st)

        d(1)=dp1 
        d(2)=dp2
        d(3)=dp2
        d(4)=dp1

        fault_dip(ix,iz,kf)     = dp1
        fault_dip(ix+1,iz,kf)   = dp2
        fault_dip(ix,iz+1,kf)   = dp1
        fault_dip(ix+1,iz+1,kf) = dp2
        sindip = dsin(d2r*(dp1+dp2)/2.0d0)
c        sindip = 1.0d0
      
c-- get strikes s() at 4 corners of element
       call delaz( y(2), x(2), y(1), x(1), del1, str1)
       call delaz( y(3), x(3), y(4), x(4), del2, str2)
       call fixaz(str1, str2)

        s(1)=str1
        s(2)=str1
        s(3)=str2
        s(4)=str2

        fault_strike(ix,iz,kf) = str1
        fault_strike(ix,iz+1,kf) = str2
        fault_strike(ix+1,iz,kf) = str1
        fault_strike(ix+1,iz+1,kf) = str2

c-- interpolation distances on grid in x and w
       if ( makeGF ) then
          X_int = GFx_interp
c          Z_int = GFw_interp*sindip
          Z_int = GFw_interp 
       else
          X_int = X_interp
c          Z_int = W_interp*sindip
          Z_int = W_interp 
       endif

       if ( X_int.eq.0.0d0) X_int = 5.0d0
       if ( Z_int.eq.0.0d0) Z_int = 2.0d0

       dx = max(X_int, 0.1d0)
       call distkm( x(2), y(2), x(1), y(1), del1)
       call distkm( x(3), y(3), x(4), y(4), del2)
       dx12 = dsqrt( del1**2 + (z(2)-z(1))**2 ) /dx
       dx34 = dsqrt( del2**2 + (z(4)-z(3))**2 ) /dx

       dz = max(Z_int, 0.1d0)
       call distkm( x(2), y(2), x(3), y(3), del23)
       call distkm( x(1), y(1), x(4), y(4), del14)
       dw23 = dsqrt( del23**2 + (z(2)-z(3))**2 ) /dz
       dw14 = dsqrt( del14**2 + (z(4)-z(1))**2 ) /dz

c number of subplanes for integration, minimum of 5
       nix = max(5, int( max ( dx12, dx34) ) )
       niy = max(5, int( max ( dw23, dw14) ) )

       MAX_nix = max(MAX_nix, nix)
       MAX_niy = max(MAX_niy, niy)

c** integrate over element      
c  get X,Y,Z of interior points
        rnix = real(nix)
        rniy = real(niy)

      do 60 i= 1, nix
       do 70 j = 1, niy

        ri = real(i)
        rj = real(j)

        dxc=(two*ri-one)/(two*rnix)
        dyc=(two*rj-one)/(two*rniy)
      
c-- get values at center of sub-box
        call bilinterp (x, dxc, dyc, xf, dd)
        call bilinterp (y, dxc, dyc, yf, dd)
        call bilinterp (z, dxc, dyc, zf, dd)
        call bilinterp (u, dxc, dyc, Ucent, dd)
        call bilinterp (v, dxc, dyc, Vcent, dd)
        call bilinterp (e, dxc, dyc, Ecent, dd)
        call bilinterp (p, dxc, dyc, Pcent, dd)
        call bilinterp (s, dxc, dyc, strike, dd)
        call bilinterp (d, dxc, dyc, dip, dd)

        dipr = dip*d2r
        
c* set max error in phi to 1.0        
        Ecent = min(one, Ecent)
      
c-- get xyz values at 4 corners of sub-box
        call interp4 (x, i, j, nix, niy, xsub )
        call interp4 (y, i, j, nix, niy, ysub )
        call interp4 (z, i, j, nix, niy, zsub )

c** slip deficit rates in horizontal x,y,z directions
        uc1 = Ucent * Pcent
        vc1 = Vcent * Pcent
        zc1 = zero

c** slip rates in horizontal x,y,z directions
        uc2 = Ucent  
        vc2 = Vcent 
        zc2 = zero

c* flag for shear or 3D slip vector
        k3d = 0
        if (fflag(kf,2)) k3d = 1
        k3d = ksliptype(kf)

c-- get s-s(U1), dip-slip(U2), and tensional (U3) components of locking 
        call getu1u2u3 (k3d, strike, dip, uc1, vc1, zc1, 
     .     U1, U2, U3, Uslip, frake )

c total slip deficit rate
c        Uslip = Uslip * Pcent

        tmu = xmu

c* get plane that approximates the quadrilateral
        xleft  = ( xsub(1) + xsub(4)) / two
        yleft  = ( ysub(1) + ysub(4)) / two
        xright = ( xsub(2) + xsub(3)) / two
        yright = ( ysub(2) + ysub(3)) / two

        call distkm(xleft, yleft, xright, yright, xlen2)
        zmax=(zsub(3)+zsub(4))/two 
        zmin=(zsub(1)+zsub(2))/two
        w=(zmax-zmin)/dsin(dipr)

        xlen=xlen2
        xff= xsub(4)
        yff= ysub(4)
        zff=zmax
        area_rec=w*xlen2
      
c* get actual area of quadrilateral patch
        call quadarea (xsub, ysub, area)
        area=area/dcos(dipr)

      if (uslip .ne. 0.0d0) tot_sliparea = tot_sliparea + area

c*  total slip x Area in m^3/year
      tot_uA = tot_uA + area * uslip * thou

c* total moment deficit in N-m/yr
      patch_Mom = area * uslip * tmu * thou
      patch_err = Ecent * area * tmu * thou

c      xmom_prof(ix,1) = xmom_prof(ix,1) + patch_Mom
c      xmom_prof(ix,2) = xmom_prof(ix,2) + patch_err

c*****************************************************************************      
c-- write to GMT fault attribute file
      if ( .not. makeGF .or. .not. readgflag ) then

c* PREM rigidity
        if (use_prem ) call prem ( zf, tmu, vpvs )

c** get CRUST2 rigidity
        if (use_crust2) call getcrust2 ( xf, yf, zf, tmu,vpvs )

       velo = dsqrt(Ucent*Ucent + Vcent*Vcent)
       rso=strike*d2r
       fpo = Ucent*dsin(rso) + Vcent*dcos(rso)
       fno = Ucent*dcos(rso) - Vcent*dsin(rso)
       ec=ecent
       pc=pcent

c-- write to file to make coupling plots
        if ( Uslip .ge. sd_umin ) then

c get along strike and downdip distances; XWW
        do kt=1,4
          call ll2xw(kf, xsub(kt),ysub(kt), xww(kt), www(kt))
         enddo

c-- get s-s(U1), dip-slip(U2), and tensional (U3) for relative slip
        call getu1u2u3 (k3d, strike, dip, uc2, vc2, zc2, 
     .     U1s, U2s, U3s, Uslip2, frake )

c-- make SDR negative if phi is negative
        if (pc .lt. 0.0d0) uslip = -uslip

c-- output fault to atr.gmt
        write(katr, 107 ) '> -Z',
     .    kf, uslip, pc, ec, U1s, U2s, U3s, frake, xf, yf, -zf,
     .    ix, iz, i, j, area, patch_Mom, patch_err, Ucent, Vcent

        write(katr, 106) (xsub(kt), ysub(kt), -zsub(kt), 
     .                     xww(kt), www(kt), kt=1,4)

c-- write file for this fault only
        if ( outatr ) then
         write(katr2, 107 ) '> -Z',
     .    kf, uslip, pc, ec, U1s, U2s, U3s, frake, xf, yf, -zf,
     .    ix, iz, i, j, area, patch_Mom, patch_err, Ucent, Vcent
         write(katr2, 106) (xsub(kt), ysub(kt), -zsub(kt), 
     .                     xww(kt), www(kt), kt=1,4)
        endif
       endif

      endif

 107  format(a4,i5,9f10.3,f7.2, 4i4, 1x, 3(1x,e12.4),2f10.3)

c*****************************************************************************      
  

      fault_sums(kf,1)= fault_sums(kf,1) + area
      fault_sums(kf,2)= fault_sums(kf,2) + area * uslip * thou
      fault_sums(kf,3)= fault_sums(kf,3) + patch_Mom
      fault_sums(kf,4)= fault_sums(kf,4) + patch_err*patch_err
      fault_sums(kf,5)= fault_sums(kf,5) + (zmin+zmax)/two * uslip*thou
      fault_sums(kf,6)= fault_sums(kf,6) + uslip*thou
      if (uslip.gt.0.05*umax) fault_sums(kf,7)= fault_sums(kf,7)+area

c -- correct for difference in true quadrilateral area vs. rectangular area
        area_fac = area/area_rec

      cm='GF'
      d1d2 = 0.0d0
      if (.not. makeGF .and. write_info) write (kinfo,104) cm,kf,ix,iz,
     .  i,j,xff,yff,zmin, zmax,w,strike,dip, frake,u1,u2,u3,uslip,
     .  xlen2, d1d2, area_fac, area

c*** -- loop through all points and calculate surface deformation

      if (kcalculate) then

      nonzero_slip =( U1 .ne. zero .or. U2 .ne. zero .or. U3 .ne. zero)

c** negative here is for backslip, GFs have backslip built in
      umult =  -area_fac
      if (nonzero_slip) then

c profile lines
      if (make_profile .and. .not. makeGF) then
          zpt = -1.0d-3
       do kline=1, nlines
         do kdata = 1, prof_n(kline)
          k = kdata - 1 + kfirst_point(kline)
          xpt=prof_pos(k,1)
          ypt=prof_pos(k,2)

c        if ( okada ) then
          call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .       u2, u3, xpt, ypt, Uc, U0)
c        else
c           call wangfin (strike, dip, frake, Xff, Yff, xlen, 
c     .       Zmin, Zmax, Uslip, Xpt, Ypt, Uc)
c        endif

c           call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
c     .       u2, u3, xpt, ypt, Uc, U0)

          do kk=1, 3
            u_lines(k,kk,2)=u_lines(k,kk,2)+ umult*Uc(kk)
          enddo

        enddo
       enddo
      endif
      
c loop through gridded surface points
      if ( make_grid  .and. .not. makeGF ) then
       do kg=1,MAX_grids
        call gridinfo(kg, x0grid, nxgrid, dxgrid, 
     .    y0grid, nygrid, dygrid, nxyg )
        if(nxyg.gt.0) then
        
      do kx=1,nxgrid
       do ky=1,nygrid

        call gridxyk (kg, kx, ky, xpt, ypt, k, kb )

        call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .    u2, u3, xpt, ypt, Uc, U0)

           u_grid(10,k)=u_grid(10,k)+umult*Uc(1)
           u_grid(11,k)=u_grid(11,k)+umult*Uc(2)
           u_grid(14,k)=u_grid(14,k)+umult*Uc(3)
           
           ee=0.0d0
           if ( Pcent .ne. 0.0d0 ) ee = Ecent/Pcent
           u_grid(12,k)=u_grid(12,k)+(umult*Uc(1)*ee)**2
           u_grid(13,k)=u_grid(13,k)+(umult*Uc(2)*ee)**2
           u_grid(15,k)=u_grid(15,k)+(umult*Uc(3)*ee)**2
           
       enddo
      enddo
      endif
      enddo
      endif
      
c loop through misc surface points
      if ( .not. makeGF ) then
       do kg=1,ndef_pts
       
        xpt=pdef_pt(kg,1)
        ypt=pdef_pt(kg,2)

        call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .    u2, u3, xpt, ypt, Uc, U0)

        do kk=1, 3
           u_pt(kg,kk)=u_pt(kg,kk)+umult*Uc(kk)
        enddo

       enddo
      endif
      
      
c-- loop through gps data locations
      if ( data_flag(1) .and. (.not. ksim_done) ) then
          zpt = 0.0d0
          nr = 0

       do iii = 1, num_gf_gps2
c        if (redo_site(iii)) then
c         nr = nr + 1
          xpt = ggf_pos2(iii,1)
          ypt = ggf_pos2(iii,2)
          zpt = ggf_pos2(iii,3)
          z1=zmin+zpt
          z2=zmax+zpt

          call OKADA85 (strike, dip, Xff, Yff, xlen, z1, z2, u1,
     .       u2, u3, xpt, ypt, Uc, U0)

          do kk=1, 3
           gps_ela(iii,kk)=gps_ela(iii,kk)+umult*Uc(kk)
          enddo

c        endif
       enddo
c       num_redo = nr
      endif
      
c-- loop through tilts, get uplift at both ends of line
      if(data_flag(2) .and. num_tilts.gt.0 .and. .not. ksim_done) then
       zpt = -1.0d-3
       do iii = 1, num_tilts
        do kt = 1, 2
          xpt= tilt_pos(iii,kt,1)
          ypt= tilt_pos(iii,kt,2)
           call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .       u2, u3, xpt, ypt, Uc, U0)
           tilt_tmp(iii,kt) = tilt_tmp(iii,kt) + umult*Uc(3)
        enddo
       enddo
      endif

c-- loop through surface strain rates
c   calculate displacements at 4 points around centroid of network
      if(data_flag(4) .and. num_ss.gt.0 .and. .not. ksim_done) then
          zpt = -1.0d-3
       do ii = 1, num_ss
        do kx = 1, 9
         call get_ss_pos( ii, kx, xpt, ypt )

         call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .       u2, u3, xpt, ypt, Uc, U0)

         do kk=1, 2
           ss_vel(ii,kx,kk) = ss_vel(ii,kx,kk) + umult*Uc(kk)
         enddo

        enddo

       enddo 
      endif
c* end of strain points
      
c-- loop through line length rates
c   calculate displacements at 2 points
      if(data_flag(5).and.num_ll_sites.gt.0 .and. .not. ksim_done) then
          zpt = -1.0d-3
       do ii = 1, num_ll_sites
         xpt = pos_ll(ii,1)
         ypt = pos_ll(ii,2)

         call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .       u2, u3, xpt, ypt, Uc, U0)

         do kk=1, 2
           vel_ll(ii,kk) = vel_ll(ii,kk) + umult*Uc(kk)
         enddo

       enddo 
      endif
c* end of line_length points

      endif
      endif

c** END OF CALC
      
   70 continue
   60 continue

c       endif

   30 continue 
   40 continue 

      endif

      if( outatr ) then
        ik = kfclose (katr2)
c        ik = kfclose (katr3)
      endif

c* write out moments by profile
c      do ix=1, nxf(kf)
c        xx1 = xynode(1,ix,1,kf)
c        yy1 = xynode(2,ix,1,kf)
c        xx2 = xynode(1,ix,nzf(kf),kf)
c        yy2 = xynode(2,ix,nzf(kf),kf)
c        call delaz ( yy2, xx2, yy1, xx1, dd, az)
c       write (k22, '(2i5,2f9.3,f7.1, 2e12.4)') kf, ix, xx1, yy1, az,
c     .    (xmom_prof(ix,j),j=1,2)
c      enddo


   50 continue 

c close moment by profile
c        ik = kfclose (k22)

c* calculate tilt rates
      if(data_flag(2) .and. num_tilts.gt.0 .and. .not. ksim_done) then
       do i = 1, num_tilts
         rmm = tilt_length(i)*1.0d6
         tilt_calc(i) = (tilt_tmp(i,2) - tilt_tmp(i,1))/rmm 
       enddo
       endif
    

      if (myflag .and. num_ss.gt.0 ) then
       call fopen (k37, 1, '_vel.tmp ')
        do i=1,num_ss
         write (k37,'(2f10.3)') ((ss_vel(i,k,j),j=1,2),k=1,9)
        enddo
       ik = kfclose(k37)
      endif


c* close files
      if ( .not. makeGF ) then
       if (write_info) ik = kfclose (kinfo)
       ik = kfclose (katr)
       if (sumflag)   ik = kfclose (k22)
      endif

c 101  format(13f12.5)
c 102  format(4f12.4)
c 103  format(f12.5)
 104  format(a2,1x,5i2,16f9.3)
c 105  format(a15)
 106  format(5f16.8)

      return
      end

c************************************************************************
c get lon/lat of points within strain network for velocity calculations
c first 4 are specified in input, 5th is center
c last 4 are between the central point and outside 4

      subroutine get_ss_pos (jc, k, xpt, ypt)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      z=0.0d0
      f=4.0d0
      xc=z
      yc=z

      if ( k .le. 4) then
        xpt = ss_pos(jc, k,1)
        ypt = ss_pos(jc, k,2)
        return
      endif

      do i=1,4
       xc = xc + ss_pos(jc, i,1)
       yc = yc + ss_pos(jc, i,2)
      enddo

       xc=xc/f
       yc=yc/f

       if ( k.eq.5 ) then
         xpt = xc
         ypt = yc 
         return
       endif

       if ( k.gt.5 ) then
         k1 = k - 5
         k2 = k1 + 1
         if (k2.eq.5) k2 = 1

         xpt = (ss_pos(jc,k1,1) + ss_pos(jc,k2,1) + xc)/3.0d0
         ypt = (ss_pos(jc,k1,2) + ss_pos(jc,k2,2) + yc)/3.0d0

       endif 

      return
      end

c************************************************************** 
      subroutine getss

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"


      dimension xtmp(9,2), vtmp(9,2), sigtmp(9,2), cortmp(9)
      logical pflag

c* calculate strain rates from 9 points within network

      xfac = 1.0d9
      pflag= .false.
      sig =1.0d0

      if( num_ss.gt.0 ) then

      if(pflag) call fopen(k37, 1, 'strain_sites.tmp ')

      do i=1, num_ss

c* get vels at 9 points
       nsites = 9
       ndim = 9
        do k=1, nsites
         call get_ss_pos (i, k, xpt, ypt)
           xtmp(k,1)=xpt
           xtmp(k,2)=ypt
           kmove = ss_block(i,k)
           kfixd = nblock_ref
           call relvel (3, kmove, kfixd, xpt, ypt, Ve, Se, Vn,Sn, rho)

           vtmp(k,1)=ss_vel(i,k,1)+ Ve
           vtmp(k,2)=ss_vel(i,k,2)+ Vn
           sigtmp(k,1) = 1.0d0
           sigtmp(k,2) = 1.0d0
           cortmp(k) = 0.0d0

          if(pflag) write (k37, 11) ss_name(i),i,k,kmove, xpt,ypt,Ve,Vn,
     .       (vtmp(k,j),j=1,2)
  11    format(a10,3i4,2f10.3,4f6.1)

        enddo
  
c* get strain rate tensor
      call get_ss_pos (i, 5, xpt0, ypt0)

      call getstrain (xpt0, ypt0, nsites, ndim, xtmp, vtmp, sigtmp, 
     .  cortmp, Exx, SExx, Exy, SExy, Eyy, SEyy, Cx, SCx, Cy, SCy, 
     .  Wr, SWr, chi)

      if(pflag) write(k37,12) Exx, SExx, Exy, SExy, Eyy, SEyy
  12  format(6e15.5)

      call prinaxes ( Exx, SExx, Exy, SExy, Eyy, SEyy,
     .    e1, se1, e2, se2, a1, sa1, a2, sa2)

      if(pflag) write(k37,12) E1, SE1, E2, SE2, a1, Sa1


      if ( ss_type(i).eq.0) then
       gam1 = E1
       gbeta = a1
       call svres (gbeta, ss_obs(i,2), sig, r, rs, 180.0d0)
       ss_calc(i,1) = gam1*xfac
       ss_calc(i,2) =  gbeta
       ss_calc(i,3) =  0.0d0

      elseif ( ss_type(i).eq.1) then
       gam1 = Exx-Eyy
       gam2 = 2.0d0 * Exy
       ar=1.0d6
       if (gam1 .ne. 0.0d0) ar = (-gam2/gam1)
       gbeta =  0.5d0 * datan(ar) *r2d
       call svres (gbeta, ss_obs(i,3), sig, r, rs, 180.0d0)
       ss_calc(i,1) = gam1*xfac
       ss_calc(i,2) = gam2*xfac
       ss_calc(i,3) =  gbeta

      elseif ( ss_type(i).eq.2) then
       call svres (a1, ss_obs(i,3), sig, r, rs, 180.0d0)
       ss_calc(i,1) = e2*xfac
       ss_calc(i,2) = e1*xfac
       ss_calc(i,3) = a1

      elseif ( ss_type(i).eq.3) then
       ss_calc(i,1) = Exx*xfac
       ss_calc(i,2) = Exy*xfac
       ss_calc(i,3) = Eyy*xfac

      endif


      enddo
      if(pflag) ik = kfclose(k37)
      endif

      return
      end

c************************************************************** 
      subroutine getll

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical pflag

c* calculate line length changes

      xfac = 1.0d3
      pflag= .false.

      if( num_ll.gt.0 ) then

      if(pflag) call fopen(k37, 1,  'll_sites.tmp ')

      do i=1, num_ll

c* get vels at 2 end points
       n1 = num_site_ll(i,1)
       n2 = num_site_ll(i,2)
       kfixd = nblock_ref

       xpt1 = pos_ll(n1,1)
       ypt1 = pos_ll(n1,2)
       kmove = ll_block(n1)
       call relvel (3, kmove, kfixd, xpt1, ypt1, Ve1, Se, Vn1, Sn, rho)
       Ve1 = Ve1 + vel_ll(n1,1)
       Vn1 = Vn1 + vel_ll(n1,2)

       xpt2 = pos_ll(n2,1)
       ypt2 = pos_ll(n2,2)
       kmove = ll_block(n2)
       call relvel (3, kmove, kfixd, xpt2, ypt2, Ve2, Se, Vn2, Sn, rho)
       Ve2 = Ve2 + vel_ll(n2,1)
       Vn2 = Vn2 + vel_ll(n2,2)

       az = daz_ll(i,2)*d2r
       sa = dsin(az)
       ca = dcos(az)

c* as a strain rate
c       dl = xfac * ( (Ve2-Ve1)*sa + (Vn2-Vn1)*ca ) / daz_ll(i,1)
c* as a velocity
       dl = (Ve2-Ve1)*sa + (Vn2-Vn1)*ca

       calc_ll(i) = dl

       if(pflag) write (k37, 11) name_ll(n1),name_ll(n2),i,kmove, 
     .   Ve1,Vn1, Ve2, Vn2, daz_ll(i,1), 
     .   daz_ll(i,2), dl,
     .   vel_ll(n1,1),vel_ll(n1,2),vel_ll(n2,1),vel_ll(n2,2)
  11    format(2(1x,a4),2i4,4f7.2, 3f10.3, 4f7.2 )

      enddo
      
        if(pflag) ik = kfclose(k37)
        
      endif

      return
      end

c************************************************************** 
c* get strike     
      function fnstriker (dx,dy)
      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

        z=0.0d0
        s=z
        if (dy .ne. z) then 
          s = datan(abs(dx/dy))
          if (dx.gt.z .and. dy.lt.z) s = pii-s
          if (dx.lt.z .and. dy.lt.z) s = pii+s
          if (dx.lt.z .and. dy.gt.z) s = -s
        else
          if (dx.gt.z) s =  pii/2
          if (dx.lt.z) s = -pii/2
        endif
          fnstriker=s*r2d
      return
      end
      
c************************************************************** 
c* get dip
      function fndipper (dx,dz) 
       implicit real*8 (a-h,o-z)
       include "tdefcons.h"

        fndipper=90.0d0*d2r
        if (dx .ne. 0.0d0) fndipper= atan(abs(dz/dx))
      return
      end
      
c************************************************************** 
      
      subroutine addblocks
c'-- get rotational velocities

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension xcc(MAX_corner), ycc(MAX_corner)
    
      do 10 j=1, nblocks
       if(block_flag(j)) then
      
       nb=nc_block(j) 
       
       do i=1, nb 
         xcc(i)=blockxy(i,j,1)
         ycc(i)=blockxy(i,j,2)
       enddo

c'-- loop through profiles
      if (make_profile) then
        do 30 kline=1, nlines
          do 30 kdata=1, prof_n(kline)
          k = kdata -1 + kfirst_point(kline)

          xpt=prof_pos(k,1)
          ypt=prof_pos(k,2)

          call inside ( xpt, ypt, xcc, ycc, nb, insde1)
      
          if ( abs(insde1).eq.2) then
           xpt=xpt + 1.0d-5
           ypt=ypt + 1.0d-5
           call inside ( xpt, ypt, xcc, ycc, nb, insde)
          else
           insde = insde1
          endif
      

          if ( abs(insde).eq.1 ) then

           call relvel (1, j, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho)
             u_lines(k,1,1) = Vx
             u_lines(k,2,1) = Vy

           call relvel (2, j, nblock_ref, xpt, ypt, Ux, Sx, Uy, Sy, rho)
             u_lines(k,1,3) = Ux
             u_lines(k,2,3) = Uy 

          endif

   30 continue
      endif
      
      
c'-- loop through gps data
      if (num_gps.gt.0 ) then
       do i = 1, num_gps

        if (j.eq.nblock_gps(i) ) then

         call gpsinfo2(i, xpt, ypt, xo, yo, zobs, xc, yc, zcalc,
     .     xs, ys, sigz, s)

         call relvel (1, j, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho)
            gps_rot(i,1)=Vx
            gps_rot(i,2)=Vy
            gps_rot(i,3)=Sx
            gps_rot(i,4)=Sy
            gps_rot(i,5)=rho

          call relvel (2, j, nblock_ref, xpt, ypt, Ux, Sx, Uy, Sy, rho)
            gps_str(i,1) = Ux
            gps_str(i,2) = Uy
         endif

       enddo
      endif
      endif
  10  continue
  
  
c-- loop through gridded surface points to get rotational and strain parts
      if (make_grid) then
      do kg=1,MAX_grids
        call gridinfo(kg, x0grid, nxgrid, dxgrid, 
     .    y0grid, nygrid, dygrid, nxyg )
       if(nxyg.gt.0) then
       
      do kx=1,nxgrid
       do ky=1,nygrid
        call gridxyk (kg, kx, ky, xpt, ypt, k, kbl )
      
c** block velocity
        call relvel (1, kbl, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho)
        u_grid(1,k)= Vx
        u_grid(2,k)= Vy
        u_grid(3,k)= Sx
        u_grid(4,k)= Sy
        u_grid(5,k)= rho
        
c** strain rate
        call relvel (2, kbl, nblock_ref, xpt, ypt, Ux, Sx, Uy, Sy, rho)
        u_grid(6,k)= Ux
        u_grid(7,k)= Uy
        u_grid(8,k)= Sx
        u_grid(9,k)= Sy

      enddo
      enddo
      endif
      enddo
      endif

      return
      end
      
c'------------------------------------------------------------
      subroutine writetilts(tchi)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character l*4, fname*80
c      call clearchar(fname, 80)
      fname = ""
      l='TILT'
      o2 =0.0d0
      npar = 0

      call stats (l,0,n,obs,o2,sig,calc, r,rs, sumwt, tchi, datavar,
     .  ssfit, fname, dn, dw, npar, 0)

      if (num_tilts.gt.0 ) then

      call fopen (k2, 1, '.tlt ')

      write (k2, '(a90)' ) 
     .'Line    lon1     lat1     lon2     lat2   Dist   Obs      sig'//
     .'    Calc     res    res/sig                                  '

        do 10 i=1, num_tilts
         s = tilt_length(i)
         sig = tilt_sig(i)
         tilt = tilt_calc(i)*1.0d9

       call stats (l,0, n, tilt_obs(i),o2,sig, tilt, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, 1)

        write (k2, 2) tilt_name(i), tilt_pos(i,1,1),  tilt_pos(i,1,2), 
     .     tilt_pos(i,2,1), tilt_pos(i,2,2), s, tilt_obs(i), sig, 
     .     tilt, r, rs
    2  format (a8, 4f9.3, 6f8.2)

   10   continue
      
      call stats (l,0,n, obs,o2, sig,calc, r, rs, sumwt, tchi, 
     .   datavar, ssfit, fname, dn, dw, npar, ksum)

      ik = kfclose (k2)
      endif

      return
      end
      
c'------------------------------------------------------------
c do data statistics

      subroutine stats (label, m, n, o1, o2, s, calc, res, rs, sumwt, 
     . SSres, SSdata, SSfit, filename, dat_nrms, dat_wrms, npar, kunit)

c* kunit controls what it does
c* kunit=0 zero the arrays
c* kunit=1 sum the stats
c* kunit > 6 write to file number kunit

c* m = 0 for normal data (o1 = mean, s = uncertainty)
c* m = 1 for uniform data (o1 = min, o2 = max, s=sigma)
c* m = 2 for angular data

c* if r and rs are non-zero, they are not calculated

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      character*4 label
      character*80 filename

      z = 0.0d0
      two = 2.0d0
      dat_nrms = z
      dat_wrms = z

c clear summation variables
      if (kunit. eq. 0) then
        SSres = z
        n = 0
        sumwt = z
        SSfit = z
        SSdata = z
        res = z
        rs = z
      endif

c do summation
      if (kunit.eq.1) then
        n=n+1
        w=fnwt(s)

c m=0 Gaussian data, m=1 min/max data, m=2 azimuth data
        if ( m.eq.0 ) then
         o=o1
         if(res.eq.z) res = o1 - calc
         if(rs.eq.z)  rs = res*dsqrt(w)

        elseif ( m.eq.1 ) then
         o = (o1+o2)/2.0d0
         if (res.eq.z) then
          if ( calc.lt.o1 ) res = o1 - calc
          if ( calc.gt.o2 ) res = o2 - calc
         endif
         if(rs.eq.z)  rs=res*dsqrt(w)

        elseif ( m.eq.2 ) then
          o = o1
          if(res.eq.z) res = o1 - calc
          if(rs.eq.z)  rs = two * dsin (res*d2r/two) / (s*d2r)
        endif

        sumwt  = sumwt  + w
        SSres  = SSres  + rs*rs
        SSfit  = SSfit  + w*calc*calc
        SSdata = SSdata + w*o*o
        
      endif

c write out to unit k3
      if (kunit.gt.6 .and. n.gt.0 ) then
       rn = real(n - npar)
       rn1 = real(n)
       rn = max(1.0d0, rn)
       xt = SSres/rn1
       dat_nrms = dsqrt(xt)
       if (sumwt.gt.z) dat_wrms = dsqrt( SSres / sumwt )
       Q = 1.0d2*gammq(rn1, SSres)

c       write (kunit,3) label, n, SSdata, SSfit, SSres, xt, 
c     .   dat_nrms, dat_wrms, sumwt, Q, filename
c    3  format(1x, a4, i7, 4(1x, 1pe10.3), f10.3, f9.2, 1pe10.3, 
c     .    f6.1, 1x, a30)

       write (kunit,3) label, n, dat_nrms, dat_wrms, Q,
     .   sumwt, SSdata, SSfit, SSres, xt, adjustl(filename)

    3  format(1x, a4, i7, 2f10.3, 
     .    f6.1, 5(1x, 1pe10.3),1x, a80)

      endif

      return
      end
      
c**********************************************************************

      subroutine gpsinfo2 (ksin, xlon, xlat, xob, yob, zob, 
     .                         xc, yc, zc, xs, ys, zs, sxy)

c-- get info for GPS site ks
      
      implicit real*8 (a-h,o-z)

      include "tdefcom1.h"
      
      logical dox, doy, doz, lgt0, useENU
      
      ks = abs(ksin)

c-- l is index for site ks in gps_ela (elastic strain component)
      l = loc_gps(ks)
      lgt0 = ( l.gt.0 )

      xlon = gps_pos(ks,1)
      xlat = gps_pos(ks,2)
      xht  = gps_pos(ks,3)

c-- what type of data     
      k = gps_type(ks)

c-- zero values      
      xd = 0.0d0
      yd = 0.0d0
      zd = 0.0d0
      xob = 0.0d0
      yob = 0.0d0
      zob = 0.0d0
      xc = 0.0d0
      yc = 0.0d0
      zc = 0.0d0
      xs = 0.0d0
      ys = 0.0d0
      zs = 0.0d0
      sxy = 0.0d0

      use_gps = ( gps_keep(ks) .and. k .ne. 0 )
      
c-- not being used      
      if ( k.eq.0 .and. ksin.gt.0 ) return

c-- see if each component is used      
      dox = useENU(ks,1)
      doy = useENU(ks,2)
      doz = useENU(ks,3)

c* mantle relaxation correction
      do j=1,3
       gps_rlx(ks,j) = 0.0d0
      enddo
      do n = 1, MAX_mrlx_files
       if(mrlx_pts(n).gt.0) then
         ng = nGF_rlx(ks,n)
         a = rlxParms(n,1)
          do j=1,3
           gps_rlx(ks,j) = gps_rlx(ks,j) + a*rlxGF(n,ng,j+2)
          enddo
       endif
      enddo

c-- get velocities
c-- if k < 3 correct data to RF, xc in RF of model
c-- if k=3 this is a time series, do not correct data to RF, xc is in RF of data

c-- East      
      if (dox) then
       if (lgt0) xd = gps_ela(l,1)
        xc = xd + gps_rot(ks,1) + gps_str(ks,1)-gps_rlx(ks,1)
       if (k .ne. 3) then
         xob = gps_obs(ks,1) - gps_net(ks,1)
         xs = gps_obs(ks,3)
       else
         xc=xd+gps_rot(ks,1)+gps_str(ks,1)-gps_rlx(ks,1)+gps_net(ks,1)
       endif
      endif
      
c-- North       
      if (doy) then
       if (lgt0) yd = gps_ela(l,2)
        yc = yd + gps_rot(ks,2) + gps_str(ks,2)-gps_rlx(ks,2)  
       if (k .ne. 3) then
         yob = gps_obs(ks,2) - gps_net(ks,2)
         ys = gps_obs(ks,4)
       else
         yc=yd+gps_rot(ks,2)+gps_str(ks,2)-gps_rlx(ks,2)+gps_net(ks,2) 
       endif
      endif

c-- Up      
      if (doz) then
       if (lgt0) zd = gps_ela(l,3)-gps_rlx(ks,3)
         zc = zd
       if (k .ne. 3) then
         zob =  gps_obs(ks,6)
         zs =   gps_obs(ks,7)
       endif
      endif

      if (dox .and. doy) sxy = gps_obs(ks,5)* xs * ys

c* reference vel/site correction
      jp = gps_info(gps_index(ks),1)
      if ( jp.gt.0 ) then

       if ( k.eq.1) then
        if (dox) xob = xob - ref_vel(jp,1)
        if (doy) yob = yob - ref_vel(jp,2)
        if (doz) zob = zob - ref_vel(jp,3)
       endif
      
       if ( k.eq.3) then
        if (dox) xc = xc + ref_vel(jp,1)
        if (doy) yc = yc + ref_vel(jp,2)
        if (doz) zc = zc + ref_vel(jp,3)
       endif

      endif

      return
      end

c**********************************************************************
      subroutine gpslongname (k)

c get longname for GPS site k

      implicit real*8 (a-h,o-z)
      character pname*4, c3*3, spc*1
      character*4 block_name      

      include "tdefcom1.h"

      spc = ' '
      
      longname = '                       '
      if (k.eq.0) return

      longname = gps_name(k)//spc//gps_fname(gps_index(k))//spc//
     .    '    '//spc//'    '

      n = max(0, nblock_gps(k))
      if ( n.eq.0 ) return

      m = npole_block(n)

      call i2c(m, 3, c3)
      pname='P'//c3

      longname = gps_name(k)//spc//gps_fname(gps_index(k))//spc//
     .    block_name(n)//spc//pname

      return
      end


c**********************************************************************
      subroutine velsigaz ( Vx, Vy, Sx, Sy, rho, Vel, Vsig, Vaz )

c* compute total V and its sigma in direction of V and azimuth
c* from RW King, 12.26.2003

      implicit real*8 (a-h,o-z)

      Vel = 0.0d0
      Vsig = 0.0d0
      Vaz = 0.0d0

      Vel = dsqrt ( Vx*Vx + Vy*Vy )

      s = rho*Sx*Sy

      if ( Vel.gt.0.0d0) Vsig = dsqrt( (Vx*Sx)**2 + (Vy*Sy)**2 +
     .  2.0d0 * s * Vx * Vy ) / Vel

      Vaz = fn360(fnstriker(Vx, Vy))
      
      return
      end
c**********************************************************************
      subroutine velsig ( Vx, Vy, Sx, Sy, rho, Vel, Vsig )

c* compute total V and its sigma in direction of V
c* from RW King, 12.26.2003

      implicit real*8 (a-h,o-z)

      Vel = 0.0d0
      Vsig = 0.0d0

      Vel = dsqrt ( Vx*Vx + Vy*Vy )

      s = rho*Sx*Sy

      if ( Vel.gt.0.0d0) Vsig = dsqrt( (Vx*Sx)**2 + (Vy*Sy)**2 +
     .  2.0d0 * s * Vx * Vy ) / Vel

      return
      end
c**********************************************************************
      subroutine gridres (a1, gxmin, gxmax, gymin, gymax, dx, dy)
c** weighted rms of residuals within cells

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
      character a1*1
      logical useGPS

      call fopen (k30, 1, '_res_'//a1//'.grd ')
      
      nnx = int((gxmax-gxmin)/dx) +1
      nny = int((gymax-gymin)/dy) +1

      do ix = 1, nnx
       do iy = 1, nny
       
       xx = gxmin + real(ix-1)*dx
       yy = gymin + real(iy-1)*dy

         sumwt = 0.0d0
         sumwr = 0.0d0
         sumwte = 0.0d0
         sumwtn = 0.0d0
         sumwre = 0.0d0
         sumwrn = 0.0d0
         sumwre2 = 0.0d0
         sumwrn2 = 0.0d0
         sumw2 = 0.0d0
         sumwt2 = 0.0d0

         n=0

         do i=1,num_gps
          if (gps_type(i).eq.1) then
           call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, 
     .       xcalc, ycalc, zcalc, sigx, sigy, sigz, s)

         if ( xpt .ge. xx .and. xpt.lt.xx+dx .and. ypt .ge. yy .and.
     .    ypt.lt.yy+dy .and. useGPS(i) ) then
           resx = xobs-xcalc
           resy = yobs-ycalc
           rho = gps_obs(i,5)
           call velsig(resx, resy, sigx, sigy, rho, r, rsig)
           wt  = fnwt(rsig)
           wte = fnwt(sigx)
           wtn = fnwt(sigy)
           sumwr = sumwr+wt*r
           sumwt = sumwt+wt
           sumwte = sumwte+wte
           sumwtn = sumwtn+wtn
           sumwre = sumwre+wte*resx
           sumwrn = sumwrn+wtn*resy
           sumwre2 = sumwre2+wte*resx*resx
           sumwrn2 = sumwrn2+wtn*resy*resy
           sumw2 = sumw2 + wtn*resy*resy + wte*resx*resx
           sumwt2 = sumwt2 + wtn + wte
           n=n+1
          endif
         endif
        enddo

           if (n.gt.0) then
            ss = dsqrt ( (sumwre/sumwte)**2 + (sumwrn/sumwtn)**2 )
            wrmsE = dsqrt(sumwre2/sumwte)
            wrmsN = dsqrt(sumwrn2/sumwtn)
            wrms =  dsqrt(sumw2/sumwt2)
            xnrms = dsqrt((sumwre2+sumwrn2)/real(2*n) )

c* output N, Residual vector weighted average, East weighted average, North weighted average,            
c*        vector magnitude of east and west WEs, total weighted rms, East weighted rms, North weighted rms,
c*        normalized rms

            write (k30, '(a5, i5, 8f10.4)') '> -Z ', n, sumwr/sumwt, 
     .       sumwre/sumwte, sumwrn/sumwtn, ss, wrms, wrmsE, wrmsN, xnrms
            write(k30, '(2f10.4)') xx,yy
            write(k30, '(2f10.4)') xx+dx,yy
            write(k30, '(2f10.4)') xx+dx,yy+dy
            write(k30, '(2f10.4)') xx,yy+dy
           endif

       enddo
       enddo
       ik = kfclose (k30)

      return
      end



c********************************************************************
c* write out INSAR

      subroutine writeinsar 

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

      character l*4, fname*80, sname*4
c      character h*124
      character*4 block_name      

      l='INSR'
      o2=0.0d0

      if ( num_insar.eq.0) return

      print *, 'Writing InSAR '
      call dater(time_string)     
      call fopen (k23,  1, '.isum ')
      
c      write(k23,*)'Name   #obs    SSData      SSfit      SSres  '//
c     . '  Chi2/N     Nrms     Wrms     SumWt    Q% Filename'

      write(k23,*)'Name   #obs      Nrms      Wrms  Prob     SumWt'//
     . '     SSData      SSfit      SSres    Chi2/N   Filename'

c-- stats by file      
      do nf = 1, MAX_insar_files
      
       if ( insar_flag(nf) )  then
       
         l = insar_fname(nf)
         fname = insar_filename(nf)
         
      call stats (l,0,n, obs,o2,sig, calc, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, izero)

      call fopen (k22,  1, '_'//insar_fname(nf)//'.insar ')
      if (insar_gmt) 
     .   call fopen (k24,  1, '_'//insar_fname(nf)//'.insar.gmt ')
     
      if ( add_rand ) then
       call fopen (kr, 1, '_'//insar_fname(nf)//'_rand.insar ')
      endif


c get min/max of lon and lat    
      xmin = 999.0
      xmax = -999.0
      ymin = 999.0
      ymax = -999.0  
      dx = 0.01d0
      dy = 0.008d0
      do i=1, num_insar
       if(insar_file(i).eq.nf ) then
        x = insar_pos(i,1)
        y = insar_pos(i,2)
        if (x.lt.xmin ) xmin = x
        if (x.gt.xmax ) xmax = x
        if (y.lt.ymin ) ymin = y
        if (y.gt.ymax ) ymax = y
       endif
      enddo

c write header
      write(k22, 1) insar_fname(nf), time_string,  
     .     nf, (insar_info(nf,i),i=3,4),
     .     (insar_info(nf,i),i=7,10), (insar_tmp(nf,i), i=1,2),
     .     xmin, xmax, ymin, ymax, insar_info(nf,11),
     .     (insar_info(nf,i),i=5,6)
      
  1   format ( "# ", 1x, a4, 1x, a12, i3, 15f12.3 )   
      
c      h='#    Long.      Lat.      Obs.      Sig.     Calc.      '//
c     .'Res.       DLOS     Dx        Dy        Dz      '//
c     .'Ux     Uy     Uz      N Name Filename    '
c      write (k22,'(a124)' ) h

      do i=1, num_insar

      call getinsarinfo (i, kf, xpt, ypt, zpt, o, xs, dlos, 
     .     rlos, cobs, corr, Ux, Uy, Uz, sname)

       if(kf.eq.nf ) then
         fname = insar_filename(kf)
         r = o-rlos

       call stats (l, 0, n, o, o2, xs, rlos,r, rs, sumwt, 
     .   tchi, datavar, ssfit, fname, dn, dw, npar, ione)

        xpt = insar_pos(i,1)
        ypt = insar_pos(i,2)
        zpt = insar_pos(i,3)

c corrections
        cor0 = insar_info(nf,7)
        corE = (xpt-insar_tmp(nf,1))*insar_info(nf,8)
        corN = (ypt-insar_tmp(nf,2))*insar_info(nf,9)
        corU =  zpt*insar_info(nf,10)

       write(k22, '(2f10.4, f7.3, 8f10.2, 3f8.4,2(1x,a4),11f10.2)') 
     .            xpt, ypt, zpt,
     .            o, xs, rlos, r, dlos, Ux, Uy, Uz, 
     .            (insar_unit(i,k),k=1,3), insar_sname(i),
     .            block_name (nblock_insar(i)),
     .            (ins_rot(i,j),j=1,2), (ins_str(i,j),j=1,2),
     .            (ins_ela(i,j),j=1,3), cor0, corE, corN, corU 
     
c'--- output calculated LOS with random errors, format 4
      if (add_rand) then
       xsr = xs
       call normal(xrand, zero, xsr)
       if ( add_rand0 ) then
          xrand = 0.0d0
          xsr = 0.1d0
       endif
          
       write(kr, '(2f10.4, f7.3, 2f10.3, 3f8.4,1x,a4)') 
     .            xpt, ypt, zpt,
     .            dlos+xrand, xsr, (insar_unit(i,k),k=1,3), 
     .            insar_sname(i)   
      endif


      if (insar_gmt) then
      write(k24, '("> -Z ",2f10.4,f7.3,9f10.2,3f8.4,2(1x,a4),11f10.2)') 
     .            xpt, ypt, zpt,
     .            o, xs, rlos, r, dlos, cobs, Ux, Uy, Uz, 
     .            (insar_unit(i,k),k=1,3), insar_sname(i),
     .            block_name (nblock_insar(i)),
     .            (ins_rot(i,j),j=1,2), (ins_str(i,j),j=1,2),
     .            (ins_ela(i,j),j=1,3), cor0, corE, corN, corU
       write(k24, '(2f10.4)' ) xpt-dx/two, ypt-dy/two
       write(k24, '(2f10.4)' ) xpt-dx/two, ypt+dy/two
       write(k24, '(2f10.4)' ) xpt+dx/two, ypt+dy/two
       write(k24, '(2f10.4)' ) xpt+dx/two, ypt-dy/two
      endif

       endif
      
      enddo
      
      call stats (l,0, n, o, o2,xs,calc, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, ksum)

      call stats (l,0, n, o, o2,xs,calc, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, k23)
      
      ik = kfclose (k22)
      if (insar_gmt) ik = kfclose (k24)
      if (add_rand) ik = kfclose (kr)
      
      endif
      
      enddo
      ik = kfclose (k23)



c-- total stats  
       call stats (l,0,n, obs,o2,sig, calc, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, izero)
     
        l='INSR'
c        call clearchar(fname,80)
        fname = ""
        fname = 'Total InSAR'
        
       do i=1, num_insar

         call getinsarinfo (i, nf, xpt, ypt, zpt, o, xs, dlos, 
     .     rlos, cobs, corr, Ux, Uy, Uz, sname)

         if ( insar_flag(nf) )  then
           r = o-rlos
           call stats (l, 0, n, o, o2, xs, rlos,r, rs, sumwt, 
     .       tchi, datavar, ssfit, fname, dn, dw, npar, ione)
         endif

       enddo

       call stats (l,0, n, obs, o2,sig,calc, r, rs, sumwt, tchi, 
     .   datavar, ssfit, fname, dn, dw, npar, ksum)
     

      return
      end

c**********************************************************************

      subroutine writesr(tchi)
c -- output slip rates

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      character l*4, stype*2, c3*3, c4*4, fname*80
      character*4 block_name 
      logical minmax
     
      l='SR  '
      z=0.0d0
      dvar = 0.0d0
      
c      call clearchar(fname, 80)
      fname = ""

      call stats (l,0,n, obs, o2,sig,calc, r, rs, sumwt, tchi, 
     . datavar, ssfit, fname, dn, dw, npar, 0)

      if (num_sr.gt.0) then 

       call fopen (k2, 1, '.srs ')

       if (myflag) then
        call fopen (k3, 1, '_srs.vec ')
        call fopen (k4, 1, '_srs.input ')
       endif

       if ( add_rand ) then
        call fopen (krS, 1, '_randS.srs ')
        call fopen (krM, 1, '_randM.srs ')
       endif

       p1 = 0.1d0

c itype - 0 for Gaussian, 1 - min/max
      do itype = 1,2

       if ( itype.eq.1) then
        write (k2, *) 
     .'   Long.     Lat.     Obs     ---     Sig    Calc     Res    '//
     .'R/S     FIXD MVNG   Az     Vx     Vy   Vtot Label/File'
       else
        write (k2, *)
     .'   Long.     Lat.     Min     Max     Sig    Calc     Res    '//
     .'R/S     FIXD MVNG   Az     Vx     Vy   Vtot Label/File'
       endif

       do 10 i=1, num_sr

        kfile = ksr_file(i)
        ktype = ksr_type(i)
        minmax = (ktype.eq.1 .or. ktype.eq.2 .or. ktype.eq.5)

        call i2c(kfile, 3, c3)
        c4 = 'F'//c3

        sig = sr_sig(i)
        call normal ( srrand, zero, sig)

        call getsr (i, Vx, Vy, Vtot, Vaz, Vz, r, rs)

        dvar=dvar+rs*rs

       if ( itype.eq.2 .and. minmax ) then

        stype = "HM"
        if(ktype.eq.5 ) stype = "VM"

        smin = sr_obs(i,1)
        smax = sr_obs(i,2)

        write (k2,2) sr_pos(i,1), sr_pos(i,2), smin, 
     .   smax, sig, sr_calc(i), r, rs, stype,
     .   block_name(kblk_sr(i,1)), 
     .   block_name(kblk_sr(i,2)), sr_az(i), Vx, Vy, Vtot, 
     .   adjustl(sr_label(i)(1:30))  
c     .   adjustl(sr_file(kfile)(1:30))

        if(myflag) write (k4,22) block_name(kblk_sr(i,1)),
     .   block_name(kblk_sr(i,2)),
     .   sr_pos(i,1), sr_pos(i,2), smin, 
     .   smax, sr_az(i), sr_sig(i), itype,
     .   sr_label(i)

  22  format("srd: ", 2(1x,a4), 2f8.3, 4f6.1, i3, 1x, a30)


c  srf: filename F Smin Type
c  format of file: FIXD MVNG Long Lat V1 V2 Azimuth Label
c  min/max data
       if (add_rand) then
        if (add_rand0) srrand = 0.0d0
        ds2 = (smax-smin)/two
        sn = sr_calc(i)
        smn = max(zero, sn - ds2 + srrand)
        smx = max(zero, sn + ds2 + srrand)
        write (krM,'(a4,1x,a4,2f9.3,3f8.1,1x,a40,f6.1)') 
     .    block_name(kblk_sr(i,1)), block_name(kblk_sr(i,2)), 
     .    sr_pos(i,1), sr_pos(i,2), min(smn,smx), max(smn,smx), 
     .    sr_az(i), sr_label(i), srrand
        endif

       elseif (itype.eq.1 .and. .not. minmax ) then

        stype = "HG"
        if(ktype.eq.4 ) stype = "VG"
        
       write (k2,2) sr_pos(i,1), sr_pos(i,2), sr_obs(i,1), z, sig,  
     .   sr_calc(i), r, rs, stype, block_name(kblk_sr(i,1)), 
     .   block_name(kblk_sr(i,2)), sr_az(i),Vx, Vy, Vtot, 
     .   adjustl(sr_label(i)(1:30))  
c     .   adjustl(sr_file(kfile)(1:30))

        if(myflag) write (k4,22) block_name(kblk_sr(i,1)),
     .   block_name(kblk_sr(i,2)),
     .   sr_pos(i,1), sr_pos(i,2), sr_obs(i,1), sig, 
     .   sr_az(i), p1, izero,
     .   sr_label(i)

c  srf: filename F Smin Type
c  format of file: FIXD MVNG Long Lat V1 V2 Azimuth Label
c  Gaussian data
       if (add_rand) then 
          srnew = sr_calc(i)+srrand
          write (krS, '(a4,1x,a4,2f9.3,4f8.2,i3,1x,a40,f7.2)') 
     .    block_name(kblk_sr(i,1)), block_name(kblk_sr(i,2)), 
     .    sr_pos(i,1), sr_pos(i,2), srnew, sig, 
     .    sr_az(i), zero, itype-1, sr_label(i), srrand
       endif

       endif

        call stats (l, 0, n, sr_obs(i,1), sr_obs(i,2), sig, sr_calc(i), 
     .    r, rs, sumwt, tchi, datavar, ssfit, fname, dn, dw, npar, 1)

        if (myflag) write (k3,3) sr_pos(i,1), sr_pos(i,2), Vx, Vy, Sx, 
     .    Sy, rho, sr_label(i) 

  10  continue

      enddo

      call stats (l, 0, n, obs, o2,sig,calc, r, rs, sumwt, tchi, 
     . datavar, ssfit, fname, dn, dw, npar, ksum)

      ik = kfclose(k2)
      if (myflag)   ik = kfclose(k3)
      if (myflag)   ik = kfclose(k4)
      if (add_rand) ik = kfclose(krS)
      if (add_rand) ik = kfclose(krM)
      endif

   2  format(2f9.3, 5f8.3, f7.2, 1x, a2, 1x, 2(1x, a4), f6.1, 
     .   3f7.2, 1x, a30, 1x, a30)
   3  format(2f9.3, 4f7.2, f8.4, 1x, a30)

c      print *, ' SR var = ', dvar

      return
      end


c*********************************************************************

      subroutine writesv(tchi)
c -- output slip vectors

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      character*4 block_name      

      character*4 l, c3*3, c4*4, fname*80
c      call clearchar(fname, 80)
      fname = ""
      l='SV  '
      a360 = 360.0d0
      two = 2.0d0
      zero = 0.0d0
      o2=0.0d0

      call stats (l,0,n, obs, o2,sig,calc, r, rs, sumwt, tchi, 
     . datavar, ssfit, fname, dn, dw, npar, izero)
 
      if (num_sv.gt.0) then 

       call fopen (k2, 1, '.svs ')

       if ( add_rand ) call fopen (kr, 1, '_rand.svs ')


       write (k2, *) '   Lon      Lat     Obs     Sig   Calc   '//
     .   'Res     R/S  FIXD MOVG Label           File'

      do 10 i=1, num_sv
        kfile = ksv_file(i)
        call i2c(kfile, 3, c3)
        c4 = 'F'//c3
        r2 = 0.0d0

        ypt=sv_pos(i,2)
        xpt=sv_pos(i,1)
        sig = sv_sig(i)

c slip vectors are the direction of the second block relative to the first
        nfx = kblk_sv(i,1)
        nmv = kblk_sv(i,2)

c rotations
      call relvel (3, nmv, nfx, xpt, ypt, Ve, Se, Vn, Sn, rho)

c get slip vector azimuth
      call svaz(ve, vn, az)

c get slip vector residual
      obs = sv_obs(i)
      call svres (az, obs, sig, r, rs, a360)
      sv_calc(i) = az
      
      call stats (l, 2, n, obs, o2,sig,az, r, rs, sumwt, 
     . tchi, datavar, ssfit, fname, dn, dw, npar, ione)

c* fitting function from NUVEL-1
c      rs = two * dsin (r2*d2r/two) / (sig*d2r)

        write (k2,2) sv_pos(i,1), sv_pos(i,2), sv_obs(i), sig, 
     .   sv_calc(i), r, rs,  block_name(kblk_sv(i,1)), 
     .   block_name(kblk_sv(i,2)), sv_label(i), 
     .   sv_file(ksv_file(i))(1:30)

   2  format(2f9.3, 4f7.1, f8.2, 2(1x, a4), 1x, a30, 1x, a30)


c read file with svf: option
       if ( add_rand) then
        call normal (rr, zero, sig)
         if (add_rand0) rr = 0.0d0
        svnew = sv_calc(i)+rr
        write (kr, '(a4,1x,a4,2f9.3, 2f7.1, 1x, a30, f7.1)' )
     .   block_name(kblk_sv(i,1)), block_name(kblk_sv(i,2)), 
     .   sv_pos(i,1), sv_pos(i,2), svnew, sig, sv_label(i), rr
       endif
     
  10  continue

      call stats (l,0,n, obs, o2,sig,calc, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, ksum)
     
      ik = kfclose (k2) 
      if ( add_rand ) ik = kfclose (kr)     
      
      endif
      
      return
      end

c**********************************************************************

      subroutine readtilt (tilt_file, tilt_wtfac)

c**  read in tilt data

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      character*80 tilt_file
      character tname*8, c3*3, bl8*8
      logical fncomment, fexist

      call existfile( tilt_file, fexist, 1)

      if ( .not. fexist ) then
        print *, 'File does not exist ', tilt_file
        return
      endif

      k12=kfopen(12)
      open (k12, file=tilt_file)

      k=num_tilts
      
      if (tilt_wtfac.eq.0.0)  tilt_wtfac=1

      do 5 i=1, 1000

      bl8 = '        '
      tname = bl8

      read (k12, '(a250)', end=99) aline

      if ( aline(1:3).eq.'end' ) goto 99
      if ( len(aline).lt.5 .or. fncomment(aline(1:1)) ) GOTO 5

      read (aline, *,end=3, err=3) xlon1,xlat1,xlon2,xlat2, o,s,tname

  3    k=k+1
        call i2c(k, 3, c3)
        if ( tname.eq.bl8) tname = 'Tilt_'//c3

       tilt_name(k)=tname 
       tilt_obs(k)=o
       tilt_sig(k)=s*tilt_wtfac
       tilt_pos(k,1,2)=xlat1
       tilt_pos(k,2,2)=xlat2 
       tilt_pos(k,1,1)=fnlong(xlon1)
       tilt_pos(k,2,1)=fnlong(xlon2)
      
       call distkm(xlon1, xlat1, xlon2, xlat2, del)
       tilt_length(k) = del
      
  5   continue

  99  ik = kfclose (k12)
      num_tilts=k
c      print *, 'Num tilts = ', num_tilts
      
      return
      end

c'-------------------------------------------------------------------------
      
      subroutine readss(ss_file, ss_wtfac)
      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      dimension xtmp(4,2)

      character*3 c3
      character*10 tname 
      character*80 ss_file
      logical fncomment
      
      two = 2.0d0

      k12=kfopen(12)
      open (k12, file=ss_file)

      k=num_ss
      
      if (ss_wtfac.eq.0.0)  ss_wtfac=1

      do 5 i=1, 1000

      tname = 'xxxxxxxxxx'

      read (k12, '(a250)', end=99) aline
      call count_items(aline, aline2, nitems)
c      print *, nitems, aline

      if ( aline(1:3).eq.'end' ) goto 99
      if ( fncomment(aline(1:1)) ) GOTO 5

c'-- read in name, lat, lon of network corners, radius of network in kilometers, 
c'   and type of data; 
c    ktype = 0 shear strain rates (Gamma, Beta)
c    ktype = 1 shear strain rates (Gamma1, Gamma2, Beta)
c    ktype = 2 principal strains (Emax, Emin, Az of Emin)
c    ktype = 3 def gradients (Exx, Exy, Eyy)

c'-- all in nanostrain/a (mm/a per 1000 km) and degrees
c      call count_items(aline, aline2, nitems)
c      print *, "2", nitems, aline

      call cleareal(xtmp,8)
      rad=0.0d0
      x=0.0d0
 
      read(aline,*,end=1,err=1) tname, ktype
c      print *, tname, ktype, nitems

      if ( ktype.gt.0) then
       if (nitems.eq.16) read(aline,*,end=1,err=1) tname, kt,
     .  e1, e1sig, e2, e2sig, e3, e3sig, ((xtmp(n,j),j=1,2),n=1,4)
       if (nitems.eq.11) read(aline,*,end=1,err=1) tname, kt,
     .  e1, e1sig, e2, e2sig, e3, e3sig, x,y,rad
      else
       if (nitems.eq.14) read(aline,*,end=1,err=1) tname, kt,
     .  e1, e1sig, e2, e2sig, ((xtmp(n,j),j=1,2),n=1,4)
       if (nitems.eq.16) read(aline,*,end=1,err=1) tname, kt,
     .  e1, e1sig, e2, e2sig, e3, e3sig, ((xtmp(n,j),j=1,2),n=1,4)
       if (nitems.eq.9) read(aline,*,end=1,err=1) tname, kt,
     .  e1, e1sig, e2, e2sig, x,y,rad
       if (nitems.eq.11) read(aline,*,end=1,err=1) tname, kt,
     .  e1, e1sig, e2, e2sig, e3, e3sig, x,y,rad
       e3=0.0d0
       e3sig=0.0d0
      endif

 1    k=k+1

       if ( k.gt.MAX_ss) then
          print *, '** MAX_ss exceeded '
          k= k-1
          goto 99
       endif

        call i2c(k, 3, c3)
        if ( tname.eq.'xxxxxxxxxx') tname = 'SS_'//c3

        ss_name(k)=tname
        x = fnlong(x)

c get corners of network 
        do n=1,4
         if ( rad. gt. 0.0d0) then
           ss_pos(k,n,1)=x + rad/111.2 * cos(real(n)*pii/two)
           ss_pos(k,n,2)=y + rad/111.2 * sin(real(n)*pii/two)
         else
           xlon=fnlong(xtmp(n,1))
           ss_pos(k,n,1)=xlon
           ss_pos(k,n,2)=xtmp(n,2)
         endif
        enddo

        SS_obs(k,1)=e1
        SS_sig(k,1)=e1sig*ss_wtfac
        SS_obs(k,2)=e2
        SS_sig(k,2)=e2sig*ss_wtfac
        SS_obs(k,3)=e3
        SS_sig(k,3)=e3sig*ss_wtfac
        SS_type(k)=ktype
      
   5  continue
  99  num_ss = k
      ik = kfclose (k12)
      
      print *, 'Num Strain Rates =', num_ss
      
      return
      end
c'==================================================================
      subroutine writess(tchi)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character*4 c4, cc(3,4), fname*80
      dimension rr(3), rss(3)

      data cc /'Gam ','Beta',' xx ',
     .         'Gam1','Gam2','Beta',
     .         'E22 ','E11 ','A11 ',
     .         'Exx ','Exy ','Eyy '  /

      if ( num_ss.eq.0 ) return

c      call clearchar(fname, 80)
      fname = ""
      c4 = 'STRN'
      izero=0
      one = 1.0d0
      o2=0.0d0

      call getss

      call stats (c4, 0,n, obs, o2, sig, calc, r, rs, sumwt, tchi, 
     . datavar, ssfit, fname, dn, dw, npar, izero)

      call fopen (k2, 1, '.sss ')

      do i=1, num_ss
       call get_ss_pos(i, 5, xpt, ypt)
       nt = ss_type(i)+1
       nph=3
       if (nt.eq.1) nph=2
       call cleareal(rr,3)
       call cleareal(rss,3)

       do j=1,nph
         ss = ss_calc(i,j)
         sig = ss_sig(i,j)
         sso = ss_obs(i,j)
         r  = 0.0d0
         rs = 0.0d0

         call stats (c4, 0, n, sso, o2, sig, ss, r, rs, sumwt, 
     .      tchi, datavar, ssfit, fname, dn, dw, npar, ione)

         rr(j) = r
         rss(j) = rs
       enddo

       call prinaxes (ss_calc(i,1), one, ss_calc(i,2), one, 
     .   ss_calc(i,3), one, e1, se1, e2, se2, a1, sa1, a2, sa2)

       call prinaxes (ss_obs(i,1), one, ss_obs(i,2), one, 
     .   ss_obs(i,3), one, e1o, se1, e2o, se2, a1o, sa1, a2, sa2)

       a1o = fnlong(a1o)
       a1 = fnlong(a1)

      write (k2, 4) ss_name(i), nt-1, xpt, ypt,
     .(cc(j,nt), ss_obs(i,j), ss_sig(i,j), ss_calc(i,j), rr(j), rss(j),
     .  j=1,3), e1o, e2o, a1o, e1, e2, a1

       enddo
      
      call stats (c4, 0,n, obs, o2,sig,ss, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, ksum)

      ik = kfclose (k2)

c    1 format('Net Comp Type   lon     lat     Obs     sig
c     .Calc    res    r/s')    
c    2 format (2(1x,a4), i3, 2f9.3, 5f8.2)
c    3 format (2(1x,a4),   21x, 5f8.2)
    4 format (a10, i3, 2f9.3, 3(1x,a4,5f8.2), 6f8.2 )

      return
      end

c**********************************************************************
      subroutine writell(tchi)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      character*4 c4, label*80, fname*80, block_name*4
      dimension Vsite(MAX_ll, 2)

      if ( num_ll.eq.0 ) return
c      call clearchar(fname, 80)
      fname = ""
      c4 = 'LL  '
      z=0.0d0
      o2=z

      one = 1.0d0

      call getll

      call stats (c4, 0,n, obs, o2,sig,calc, r, rs, sumwt, tchi, 
     . datavar, ssfit, fname, dn, dw, npar, 0)

      call fopen (k2, 1, '.lls ')

      label = 'Sta1 Sta2     Dist    Azim    Obs    sig   Calc'//
     . '    res    r/s'
      write (k2, '(a80)') label

      do i=1, num_ll

c* get vels at 2 end points
       n1 = num_site_ll(i,1)
       n2 = num_site_ll(i,2)
       kfixd = nblock_ref

       xpt1 = pos_ll(n1,1)
       ypt1 = pos_ll(n1,2)
       kmove = ll_block(n1)
       call relvel (3, kmove, kfixd, xpt1, ypt1, Ve1, Se, Vn1, Sn, rho)
       Ve1 = Ve1 + vel_ll(n1,1)
       Vn1 = Vn1 + vel_ll(n1,2)
       Vsite(n1,1) = Ve1
       Vsite(n1,2) = Vn1

       xpt2 = pos_ll(n2,1)
       ypt2 = pos_ll(n2,2)
       kmove = ll_block(n2)
       call relvel (3, kmove, kfixd, xpt2, ypt2, Ve2, Se, Vn2, Sn, rho)
       Ve2 = Ve2 + vel_ll(n2,1)
       Vn2 = Vn2 + vel_ll(n2,2)
       Vsite(n2,1) = Ve2
       Vsite(n2,2) = Vn2

       calc = calc_ll(i)
       obs = obs_ll(i)
       sig = sig_ll(i)
       r = obs-calc
       rs = r/sig

       call stats (c4, 0,n, obs, o2,sig,calc, r, rs, sumwt, 
     .      tchi, datavar, ssfit, fname, dn, dw, npar, 1)

       write (k2, 4) name_ll(n1), name_ll(n2), daz_ll(i,1),
     .  fn0to180(daz_ll(i,2)), obs, sig, calc, r, rs

      enddo

      call stats (c4, 0,n, obs,o2,sig, ss, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, ksum)

      write (k2, *) ' '
      write (k2, *) 'Site Block  Long.    Lat.'
      
      do i=1,num_ll_sites
       call getblock(pos_ll(i,1), pos_ll(i,2), kblock)
       write(k2,5) name_ll(i), block_name(kblock), 
     .    pos_ll(i,1), pos_ll(i,2)
      enddo

      ik = kfclose (k2)

    4 format (2(a4,1x), 2f8.2, 5f7.1)
    5 format (2(1x,a4),2f9.4)

      return
      end

c**********************************************************************

      subroutine nodevectors
c* get relative velocities at nodes

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical kverr 

      kverr = vel_errors
      n=MAX_x*MAX_z*MAX_f*5
      call cleareal(slip_n, n)
      n=MAX_x*MAX_z*MAX_srce*2
      call cleareal(slip_t, n)

c** do the faults
c** getting fault slip from poles for faults
      do  kf=1, nfault
       if (fflag(kf,1)) then

        do iz=1, nzf(kf)
         do ix=1, nxf(kf)

c-- get vector from poles
        xlat=xynode(2,ix,iz,kf)
        xlon=xynode(1,ix,iz,kf)
      
c-- slip vector is hanging wall relative to footwall 
      khw=khw_blk(ix,iz,kf)
      kfw=kfw_blk(ix,iz,kf)

      call relvel (3, khw, kfw, xlon, xlat, Ve, Se, Vn, Sn, rho)

      slip_n(ix,iz,kf,1) = Ve
      slip_n(ix,iz,kf,2) = Vn
      slip_n(ix,iz,kf,3) = Se
      slip_n(ix,iz,kf,4) = Sn
      slip_n(ix,iz,kf,5) = rho

         enddo
        enddo
       endif
      enddo

      
c** do transients, slip direction on fault
      if (do_trans) then
      do nt=1, MAX_srce
       kf =  info_source(nt,1)
       kq =  info_source(nt,2)
       kaz = info_source(nt,3)
       
       
c       if(kf.gt.0 .and. sflag(nt) .and. flock(kf) ) then
       if(kf.gt.0 .and. sflag(nt) ) then
        do iz=1, nzf(kf)
         do ix=1, nxf(kf)
           xlat=xynode(2,ix,iz,kf)
           xlon=xynode(1,ix,iz,kf)
           khw=khw_blk(ix,iz,kf)
           kfw=kfw_blk(ix,iz,kf)
           call relvel (3, khw, kfw, xlon, xlat, Ve, Se, Vn, Sn, rho)

c unit slip vector is used for transients; 
c   normalize components and make negative

          V = dsqrt(Ve*Ve+Vn*Vn)
          slip_t(ix,iz,nt,1) = -Ve/V
          slip_t(ix,iz,nt,2) = -Vn/V

c-- use 13th entry from transient for azimuth of slip on fault
c = slip of footwal rel to hanging wall

       if ( kaz.eq.1 ) then
          az = transient(nt,13)
          slip_t(ix,iz,nt,1) = dsin(az*d2r)
          slip_t(ix,iz,nt,2) = dcos(az*d2r)
       endif
       
        enddo
        enddo
      endif
      enddo
      endif

     
      return
      end

c*********************************************************************

      subroutine svaz (ve, vn, az)

c get slip vector azimuth from ve, vn
      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      z=0.0d0
      x90  = 90.d0*d2r
      x180 = 180.d0*d2r

       if (vn.eq.z) then
         az= x90 
         if (ve.lt.z) az = -x90
       else
         az = datan(ve/vn)
         if ( vn.gt.z ) goto 1
         if ( az.lt.z ) then
            az=az + x180 
         else
            az=az - x180
         endif
       endif 
      
 1    az = az*r2d
      return
      end


c*********************************************************************
      subroutine fixaz (az1, az2)

c fix az2 so its in same hemisphere as az1

      implicit real*8 (a-h,o-z)

         azstep = 360.0d0
         rmin=1.0d5
         azmin = az2

       do j = -3, 3

        x = j
         
        az = az2 + azstep * x 
        absaz = abs (az - az1)

       if (absaz.lt.rmin) then
         rmin = absaz
         azmin = az
       endif

       enddo

       az2 = azmin

      return
      end

c 
c*********************************************************************
      subroutine svres (calc, obs, sig, r, rs, ang)

c find calculated azimuth so that absolute value of residual
c is minimum

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      azstep = ang
      rmin=1.0d5
      azmin=calc
      r = obs - calc

       do j = -5, 5

        az = calc + azstep * real(j) 
        absaz = abs (az - obs)

       if (absaz.lt.rmin) then
         rmin = absaz
         r = obs - az
         azmin = az
       endif

       enddo

       rs = two * sin (r*d2r/two) / (sig*d2r)

       calc = azmin

      return
      end

c*********************************************************************

      subroutine write_ts_data 
       
c* write time series info

     
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      character d*12, c80*80

c-- date
            call dater(d)
      c80 = 'Time series data '

      call fopen (kkf, 1, '_ts_data.out ')

      write (kkf, '(a80)') c80

      do i=1, num_gps
       if (ndx(i,1).gt.0) then
        call gpsinfo2(i, xll, yll, xobs, yobs, zobs, ve, vn, zcalc,
     .    sx, sy, sigz, sxy)
c-- data range      
        n1 = ndx(i,1)
        n2 = ndx(i,2)
        nn = n2-n1+1
        dt = t_disp(n2)-t_disp(n1)

        if (use_gps) write(kkf, 1) 
     .   gps_name(i), gps_fname(gps_index(i)), xll, yll, 
     .   nn, dt,
     .   (GXo(i,j,1), j=1,3),
     .   ((GVo(i,j,m), j=1,3), m=2,6)
       endif
      enddo
      ik = kfclose (kkf)
 1    format(a8, 1x, a4, 2f12.4, i7, f9.2, 21f8.1)

      return
      end
         
c**************************************************************************
c* clearing arrays   
  
      subroutine clearlog (a, m)
c  makes logical array all false
      logical a(m)
      do 1 i=1,m
        a(i)= .false.
    1 continue
      return
      end

      subroutine clearint (n, m)
c  zeroes integer array
      integer n(m)
      do 1 i=1,m
        n(i)= 0
  1   continue
      return
      end

      subroutine cleareal (a, m)
c  zeroes real array
      real*8 a(m)
      do 1 i=1,m
        a(i)= 0.0d0
  1   continue
      return
      end

c      subroutine clearchar (a, m)
c  zeroes character array
c      character a
c      do i=1,m
c        a(i:i)= ' '
c      enddo
c      return
c      end

c      subroutine clearchar (a, m)
c  zeroes character array
c LMB Properly declare character string dummy argument
c      character a*(m)
c LMB Use chracter string assignment statement
c      a = ' '
c      return
c      end
      
c  zeroes character array
c Tom Herring suggestion 9-28-22
c      subroutine clearchar (a, m)
c      character*(*) a(m)
c      integer i
c      do i=1,m
c        a(i)= ''
c      enddo
c      return
c      end


c RM1104a - does the array contain all zeroes
      function allzero (a, m)
      real*8 a(m), tol
      logical allzero
      tol = 1.0d-40
      allzero = .true.
      do 1 i=1,m
        if ( dabs(a(i)).gt.tol ) then
          allzero = .false.
          return
        endif
  1   continue
      return
      end

c set to 0 if below tol
      function fnzero (a)
      real*8 a, tol, fnzero
        tol = 1.0d-40
        fnzero = a
        if ( dabs(a).lt.tol ) fnzero = 0.0d0
      return
      end

c**************************************************************************

      subroutine uminmax (umin, umax) 
c'--- get min and max of u for faults

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      logical flock

      umin= 10000.d0
      umax=-10000.d0
       do kf=1,nfault
        if (flock(kf)) then
        do 1 iz=1, nzf(kf)
          do 1 ix=1, nxf(kf)
           u = slip_n(ix,iz,kf,1)*phi(ix,iz,kf)
           v = slip_n(ix,iz,kf,2)*phi(ix,iz,kf)
           u = dsqrt (u*u + v*v)
           umin = min(u, umin)
           umax = max(u, umax)
  1   continue
      endif
      enddo

      return
      end 
c

c**************************************************************************
      function glat(hlat)
c------convert geographic latitude to geocentric latitude-------------
c        hlat (input) = geographic latitude in radians (north positive)
c        glat (output)= geocentric latitude in radians (north positive)
c---------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      halfpi = pii/2.0d0 

        if( (halfpi - dabs(hlat)) .ge. 0.05d0 ) then
          glat = datan (0.993277d0*dsin(hlat)/dcos(hlat))
        else
c------special formula near pole
          glat = hlat/0.993277d0 - sign(0.010632d0, hlat)
        endif

        return
        end
c**************************************************************************
      subroutine vomega( kerr, p, plon, plat, Ve, Se, Vn, Sn, rho)

c computes plate velocity in mm/a at (plon, plat) (in degrees) from Euler vector p()
c in degrees per million years

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      dimension p(9), w(3,3)
      logical kerr

      rpd2 = d2r*d2r
      Re   =  Erad

      Se = 0.0d0
      Sn = 0.0d0
      rho=0.0d0

      pt = plat*d2r
      pn = plon*d2r

      splt = dsin(pt)
      spln = dsin(pn)
      cplt = dcos(pt)
      cpln = dcos(pn)

c point vector
      Px = Re * cplt * cpln
      Py = Re * cplt * spln
      Pz = Re * splt

c Euler vector
      Wx = p(1)*d2r
      Wy = p(2)*d2r
      Wz = p(3)*d2r

c velocity by cross-product
      Vx = Wy*Pz - Wz*Py
      Vy = Wz*Px - Wx*Pz
      Vz = Wx*Py - Wy*Px

c dot product with (E,N) vector

      Tex = -spln
      Tey =  cpln
      Tez =  0.0d0
      Tnx = -splt * cpln
      Tny = -splt * spln
      Tnz =  cplt

      Ve = Tex * Vx + Tey * Vy + Tez * Vz
      Vn = Tnx * Vx + Tny * Vy + Tnz * Vz

      if (kerr) then

c* fill the covariance matrix W and convert to (radians/Ma)**2
      w(1,1) = p(4)*rpd2
      w(2,2) = p(5)*rpd2
      w(3,3) = p(6)*rpd2
      w(1,2) = p(7)*rpd2
      w(2,1) = w(1,2)
      w(1,3) = p(8)*rpd2
      w(3,1) = w(1,3)
      w(2,3) = p(9)*rpd2
      w(3,2) = w(2,3)

      call velerr (w, plat, plon, Se, Sn, rho, Az, Ax1, Ax2 )

      endif

      return
      end

c**********************************************************************

      subroutine getnodename(kf, ix, iz, co9)

c get 9 digit character for fault kfault and node 
c   eg kf=1, ix=2, iz=3 returns character co = '001002003'

      character co9*9, cf*3, cx*3, cz*3

      call i2c(kf, 3, cf)
      call i2c(ix, 3, cx)
      call i2c(iz, 3, cz)

      co9 = cf//cx//cz

      return
      end

c**********************************************************************

      subroutine network

c computes rotation adjustment for GPS velocity fields

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      logical kerr

      dimension p(9)

      kerr = vel_errors

      do 10 ifile = 1, num_gps_file

      jpole = ngps_index(ifile)

      if ( jpole.eq.0 ) goto 10

      do j=1,9
        p(j) = gps_pole(jpole,j)
      enddo

      do 20 i=1,num_gps

       if (gps_index(i).eq.ifile) then

        xpt = gps_pos(i,1)
        ypt = gps_pos(i,2)
     
        call vomega( kerr, p, xpt, ypt, Ve, Se, Vn, Sn, rho)

         gps_net(i,1)=Ve 
         gps_net(i,2)=Vn 
         gps_net(i,3)=Se 
         gps_net(i,4)=Sn 
         gps_net(i,5)=rho 

       endif

  20  continue
  10  continue

      return
      end

c**********************************************************************

      subroutine refsitevel

c computes corrections for reference site

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      character*4 sname

c     call cleareal (ref_vel, MAX_gps_files*3)

      do ifile = 1, num_gps_file

       sname = ref_site(ifile)
     
        do i=1, num_gps

        if ( gps_index(i).eq.ifile .and. 
     .      sname.eq.gps_name(i)(1:4) ) then

         call gpsinfo2(i, xpt, ypt, xo, yo, zobs, Ve, Vn, Vz,
     .    sigx, sigy, sigz, s)

         ref_vel(ifile,1)=Ve 
         ref_vel(ifile,2)=Vn 
         ref_vel(ifile,3)=Vz 
        endif

        enddo

      enddo


      return
      end

c*************************************************************
      subroutine defsa (kend)

c** does simulated annealing solution
      
      implicit real*8 (a-h,o-z)
      
c LMB system() returns INTEGER status
      integer   system, status

      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

      logical kend, no, kstop, flock
      logical loc_remove, fixed, new_gfs, more_data
      logical dofault, fexist

      dimension nphis(MAX_parms), nphist(MAX_parms)
      character inchar*1
      character Plabel(3)*3, Slabel(3)*3, Blabel(3)*3
c      character block_name*4

      character ptype(50)*15, p2codes(50)*2
      common /ptype1/ ptype, p2codes

      vel_errors = .false.
      no = .false.          
      kstop = .false.
      data Plabel / ' Wx', ' Wy', ' Wz' /
      data Blabel / ' Eb', ' Nb', ' Ub' /
      data Slabel / 'Exx', 'Eyy', 'Exy' /

c kparm_type  tells what kind orf parameter it is
c kparm_index tells which pole, strain or fault the parameter refers to

c -- write parameters out to file
      call fopen (kkprm, 1, '.prm ')

c--------------------------------------------
c -- setup free parameters in array parm()
      nparms=0

c*** GPS network adjustment parameters
c      print *, 'GPS network adjustment parameters'
      do k=1, MAX_gps_files
         if ( .not. get_gps_parm ) gps_invert(k) = .false.
         kindex = ngps_index(k)

c-- velocity field pole
       if ( gps_invert(k) ) then
        do j=1,3
         parm(nparms+j) = gps_pole(k,j)
         kparm_type(nparms+j) = 1
         kparm_index(nparms+j) = k
         write(kkprm,'("Parameter ",i4, " GPS pole  ",i4,1x,a3,f12.5)')
     .     nparms+j, k, Plabel(j), parm(nparms+j)
        enddo
         nparms=nparms+3
       endif

c-- velocity bias
       if (gps_file_type(k).eq.1) then
         do j=1,3
          if ( gps_info(k,j+2).gt.0) then
           nparms=nparms+1
           parm(nparms) = ref_vel(kindex,j)
           kparm_type(nparms) = 11
           kparm_index(nparms) = k
         write(kkprm,'("Parameter ",i4, " GPS bias  ",i4,1x,a3,f12.5)')
     .     nparms, k, Blabel(j), parm(nparms)
          endif
        enddo
       endif

       if (gps_file_type(k).eq.3) then
         do j=1,3
          if ( gps_info(k,j+10).eq.4) then
           nparms=nparms+1
           parm(nparms) = ref_vel(kindex,j)
           kparm_type(nparms) = 11
           kparm_index(nparms) = k
         write(kkprm,'("Parameter ",i4, " GPS bias  ",i4,1x,a3,f12.5)')
     .     nparms, k, Blabel(j), parm(nparms)
          endif
        enddo
       endif

       enddo
      
c*** Block pole parameters
      if ( .not. get_blk_parm ) num_pole_invert =0
      if (num_pole_invert.gt.0 ) then
       do k=1, num_pole_invert
        n = npole_invert(k)

       do j=1,3
        parm(nparms+j) = poles(n,j) 
        kparm_type(nparms+j)= 2
        kparm_index(nparms+j) = n
         write(kkprm,'("Parameter ",i4, " Block pole",i4,1x,a3,f12.5)')
     .    nparms+j, n, Plabel(j),parm(nparms+j)
       enddo

       nparms = nparms + 3

       enddo
      endif

c-- amplitudes for relaxation velocity fields
c-- kprlx(n) is the parameter number for this amplitude
      do n = 1, MAX_mrlx_files
       if(mrlx_pts(n).gt.0 .and. rlx_inv_flag(n) ) then
         nparms = nparms + 1
         parm(nparms) = rlxParms(n,1)
         kparm_type(nparms)= 10
         kparm_index(nparms) = n
         kprlx(n) = nparms
           write(kkprm,'("Parameter ",i4, " Rx amp    ",i4,f12.5)')
     .     nparms, n, parm(nparms)
       endif
      enddo
      
c*** Block strain rate parameters 
      if ( .not. get_str_parm ) num_strain_invert = 0

      if (num_strain_invert.gt.0 ) then
       first_strain_parm = nparms+1

       do k=1, num_strain_invert
        n = nstrain_invert(k)

        do j=1,3
         parm(nparms+j) = strain(n,j) 
         kparm_type(nparms+j)= 3
         kparm_index(nparms+j) = n
         write(kkprm,'("Parameter ",i4, " Strain    ",i4,1x,a3,f12.5)')
     .    nparms+j, n,  Slabel(j), parm(nparms+j)
        enddo

        nparms = nparms + 3

       enddo
      endif

c-- interseismic fault node parameters
c--  first node in parameter list
      first_node_parm = nparms+1
c      num_node_parms = 0

c** check if interseismic fault needs GFs
       do kf = 1, nfault
        if (fflag(kf,1)) then
         if(fault_fit_type(kf) .le. 1) then
           do iz=1, nzf(kf)
            do ix=1, nxf(kf)
              if ( phi (ix,iz,kf) .ne. 0.0d0 ) useGF(kf) = .true.
            enddo
           enddo
         endif
        endif
       enddo
      
c-- loop through nodes to find free nodes and assign to parm() array
      if ( get_flt_parm ) then
      
      do 33 kf = 1, nfault
        
      kfft = fault_fit_type(kf)
      if (all_fault_1 .or. all_fault_fix) kfft = 0

      dofault = ( flock(kf) .and. (.not. all_fault_0) .and. 
     .   fflag(kf,3) )

c     process fault
      if (dofault) then

c** fault has independent nodes      
      if (kfft .le. 1 ) then

       do iz = 1, nzf(kf)
        do ix = 1, nxf(kf)
         nph = nphi(ix,iz,kf) 

c** if node is free      
         if ( phi_free(ix,iz,kf) .and. nph.gt.0 ) then

          useGF(kf) = .true.

c** check for node with same parameter number on this fault 
c**  nflag is an index to provide a unique identifier for this fault node
        nflag = kf*1000 + nph 

c****************************************************
c** see if a prior node on this fault has the same index
c****************************************************
        isamenode=0
        do inc=1, nparms
          if ( nflag.eq.nphis(inc)) isamenode=inc
        enddo

c****************************************************
c** see if a prior fault has the same flag, from FC: option
c****************************************************
       isamefault = 0
       if (kfft .le. 1) then
        do k=1, MAX_fc
         do jf = 1, MAX_fc
          if ( kfc(k,jf).eq.kf ) then
c       print *, 'Equating faults', kf
           do kk = 1, MAX_fc
            if ( kfc(k,kk).lt.kf .and. kfc(k,kk).gt.0) then
             np = npp ( 1, 1, kfc(k,kk) )
             if(np.gt.0) isamefault = np
c            print *, 'Equating fault phis', kf, kfc(k,kk)
            endif
           enddo
          endif
         enddo
        enddo
       endif
c       if (isamefault.gt.0) print *, 'Equating faults'

c****************************************************
c** apply EQ: equating node phis
c****************************************************
      isequated = 0
      if ( npars_eq.gt.0 ) then
       do i=1, npars_eq
        if ( npar_eq_type(i).eq.0 ) then
        npe1 = 0
        npe2 = 0
         if (kf.eq.npar_eq(i,1) .and. ix.eq.npar_eq(i,2) .and.
     .       iz.eq.npar_eq(i,3) ) then
             npe1=npp(ix, iz, kf) 
             npe2=npp(npar_eq(i,5), npar_eq(i,6), npar_eq(i,4)) 
         endif
         if (kf.eq.npar_eq(i,4) .and. ix.eq.npar_eq(i,5) .and.
     .       iz.eq.npar_eq(i,6) ) then
             npe1=npp(ix, iz, kf) 
             npe2=npp(npar_eq(i,2), npar_eq(i,3), npar_eq(i,1)) 
         endif
        if (npe1.gt.0 .and. npe2.eq.0) isequated = npe1
        if (npe1.eq.0 .and. npe2.gt.0) isequated = npe2
       endif
       enddo
      endif

c****************************************************
c** if no prior node with same flag found, make a new parameter
c****************************************************
      if (isamenode.eq.0 .and. isamefault.eq.0 .and. 
     .    isequated.eq.0) then
        nparms=nparms+1
        nphis(nparms)=nflag
        kparm_index(nparms) = kf
        np = nparms
c        print *, 'Nparms', np
      endif

c****************************************************
c** if prior node with same flag found, assign it same parameter number
c****************************************************
      if (isequated .gt.0) np = isequated
      if (isamenode .gt.0) np = isamenode
      if (isamefault.gt.0) np = isamefault

c** npp() holds parameter number for the node
          npp(ix, iz, kf) = np
          parm(np) = phi(ix,iz,kf)
          kparm_type(np)=4

        write (kkprm,'("Parameter ",i4," Node (X,Z,F,NP):",4i4,f8.4)') 
     .   np, ix, iz, kf, nphi(ix,iz,kf), parm(np)

         endif
        enddo
       enddo

c-- now, process down-dip functions
      elseif (kfft .ge. 2 .and. kfft .le. 6 ) then
      
c-- fix gamma at 5.0      
       if (gamma5 .and. kfft.eq.2 ) then
        do ix = 1, nxf(kf)
         f_parm(kf,ix,1) = 5.0d0
        enddo
        fprm_fixed(kf,1) = .true.
       endif

c* Wang, boxcar, or Gaussian parameters
      do ix = 1, nxf(kf)
        n1 = node_prof(kf,ix)

c** see if this profile is fixed by user (NX:)
        fixed  = .false.
        do i=1,nxf(kf)
         if (node_fix(kf,i).eq.n1) fixed = .true.
        enddo

        do ipar=1,3

        npp(ix, ipar, kf)=0
c        if(fprm_fixed(kf, ipar)) print *, ipar, 'fprm fixed'
c        if(fixed) print *, ipar, 'fixed'

        if ( .not. fprm_fixed(kf, ipar) .and. .not. fixed) then

          useGF(kf) = .true.

        v  = f_parm(kf,ix,ipar)
        nflag = kf*1000 + n1*10 + ipar

c** see if a prior node on this fault has the same flag
        isame=0
        do i=1, nparms
          if ( nflag.eq.nphis(i)) isame=i
        enddo

c** if no prior node with same flag found, make a new parameter
c** if prior node with same flag found, assign it same parameter number
        if ( isame.eq.0) then
          nparms=nparms+1
          nphis(nparms)=nflag
          kparm_index(nparms) = kf
          np = nparms

        do iz=1,nzf(kf)
           phi_free(ix,iz,kf) = .true.
        enddo

        else
          np= isame
        endif

c** npp() holds parameter numbers
        npp(ix, ipar, kf)=np
        parm(np) = v
        kparm_type(np)= ipar+4
        if (ipar.eq.1 .and. kfft.eq.3) kparm_type(np)=4
         if ( kfft.eq.4 .or. kfft.eq.6 ) then
          if (ipar.eq.1) kparm_type(np)=4
          if (ipar.eq.2) kparm_type(np)=8
          if (ipar.eq.3) kparm_type(np)=9
         endif

        endif
       enddo
      enddo

      endif
      
      endif
  33  continue
      endif

c** end of interseismic faults
c      num_node_parms = nparms - first_node_parm + 1

c***********************************************************
c**** process transient parameters
c***********************************************************
c 
c  ntransient(nt,k) flag = 1 to adjust 
c  transient(nt,k)  has parameter values

c  k  parameter     ktype
c  1  longitude     21
c  2  latitude      22
c  3  depth         23
c  4  free node     24 if sp = 1
c  4  W-width       24 if sp = 6,7,9,11
c  4  Semi-major    24 if sp = 12
c  4  R1            24 if sp = 10 and ts = 8
c  5  Amplitude     25
c  6  X-width       26 if sp = 6,7,9,11
c  6  Axis ratio    26 if sp = 12
c  4  R2            26 if sp = 10 and ts = 8
c  7  Origin time   27
c  8  Time constant 28
c  9  Migr rate     29
c 10  Migr rate 2   30
c 11  Strike        31
c 12  Dip           32
c 13  Rake          33
c 13  Slip azimuth  33 if sp not 9,11
c 14  Az Gauss 2D   34 if sp = 6
c 14  Polygon flag  34 if sp = 8

c Spatial source types (sp)  
c  1 Independent nodes (use smoothing)<br>
c  2 not used
c  3 1D Boxcar phi(z)
c  4 1D Gaussian phi(z)             
c  5 not used
c  6 2D Gaussian slip source     
c  7 2D Boxcar slip source  
c  8 Polygon, uniform slip source  
c  9 Earthquake slip source (double couple not on fault)
c 10 Mogi point slip source 
c 11 Planar expansion source  
c 12 Prolate spheroid  

      call clearint (nsource_parm, 20*MAX_srce)

      if ( time_dependence ) then

       do 34 nt2 = 1, MAX_srce
        nt = isrce_order(nt2)

       if(sflag(nt) .and. srce_inv(nt) ) then
c       print *, 'Transient parameters ', nt
       
        kf   = info_source(nt,1)
        kq   = info_source(nt,2)
        kft  = info_source(nt,3)
        kt   = info_source(nt,4)
        ntau = info_source(nt,7)

c       print *, 'Transient parameters nt kf kq kt ', nt, kf, kq, kt
       
c ***********************************************************      
c-- do time dependence
c ***********************************************************      
c-- Amplitude of Impulse, Gaussian or boxcar time function         
c ***********************************************************      
      if (kt.ne.2 .and. kt.ne.5) then
       if (ntransient(nt,5).gt.0 ) then
         call newtparm (nt, 5, 25 )
c Mogi amplitude
c         if (kq.eq.10) kparm_type(nparms) = 25
       endif
      endif
      
c***********************************************************
c-- triangular STF amplitude parameters               
c***********************************************************
       if((kt.eq.2 .or. kt.eq.5) .and. ntau.gt.0 ) then
        if (ntransient(nt,16).gt.0 ) then
          do kk=1,ntau 
            nparms=nparms+1
            parm(nparms) = atau(nt, kk)
            ntau_parm(nt, kk) = nparms
            kparm_type(nparms) = 36
c            kparm_type(nparms) = 25
            kparm_index(nparms) = nt
          enddo
        endif  
       endif

c***********************************************************
c-- longitude, latitude, depth
c***********************************************************
       do k=1,3
        if (ntransient(nt,k).gt.0 ) then
         nn = k+20
         call newtparm (nt, k, nn )
        endif
       enddo

c***********************************************************
c-- origin time and duration parameters
c***********************************************************
       do k=7,8
        if (ntransient(nt,k).gt.0 ) then
          nn=k+20
          call newtparm (nt, k, nn )
        endif
       enddo
      
c***********************************************************
c-- migration parameters 
c***********************************************************
       do k=9,10
        if (ntransient(nt,k).gt.0 ) then
          nn=k+20
         call newtparm (nt, k, nn )
        endif
       enddo
       
      
c***********************************************************
c* 1D Boxcar function downdip parameters, # 40, 41, 42
c***********************************************************
      if ( kq.eq.3 ) then
       np0 = nparms
       call clearint(nphist, MAX_parms)

       do ix = 1, nxf(kf)
        n1 = tnode_prof(nt,ix)
        
        if (n1.gt.0) then

        do ipar=1,3

        nppt(ix, ipar, nt)=0

        if (ntransient(nt,ipar+19).gt.0 )  then


c** see if a prior node on this fault has the same flag
        nflag = nt*1000 + n1*10 + ipar
        isame=0
        do i=np0, nparms
          if ( nflag.eq.nphist(i) ) isame=i
        enddo

c** if no prior node with same flag found, make a new parameter
c** if prior node with same flag found, assign it same parameter number
        if ( isame.eq.0) then
          nparms=nparms+1
          nphist(nparms)=nflag
          np = nparms
          kparm_index(nparms) = nt
          v  = tf_parm(nt,ix,ipar)
c          num_node_parms = num_node_parms +1
          useGF(kf) = .true.

c          print *, 'nt ix ipar nparms n1 ', nt,ix,ipar,nparms,n1
        else
          np = isame
          v = parm(np)
        endif

c-- npp() holds parameter numbers
        nppt(ix, ipar, nt)=np
        parm(np) = v
        kparm_type(np)=ipar+39

        endif
       enddo
       endif
      enddo
      endif


c***********************************************************
c* 1D Gaussian function downdip parameters, # 37, 38, 39
c***********************************************************
      if ( kq.eq.4 ) then
       np0 = nparms
       call clearint(nphist, MAX_parms)

       do ix = 1, nxf(kf)
        n1 = tnode_prof(nt,ix)
c        print *, ' nt ix n1 ', nt,ix, n1
        
        if (n1.gt.0) then

        do ipar=1,3

         nppt(ix, ipar, nt)=0
c         print *, 'ip ntrans ', ipar, ntransient(nt,ipar+16)
        if (ntransient(nt,ipar+16).gt.0 )  then


c** see if a prior node on this fault has the same flag
        nflag = nt*1000 + n1*10 + ipar
        isame=0
        do i=np0, nparms
          if ( nflag.eq.nphist(i) ) isame=i
        enddo

c** if no prior node with same flag found, make a new parameter
c** if prior node with same flag found, assign it same parameter number
        if ( isame.eq.0) then
          nparms=nparms+1
          nphist(nparms)=nflag
          np = nparms
          kparm_index(nparms) = nt
          v  = tf_parm(nt,ix,ipar)
c          num_node_parms = num_node_parms +1
          useGF(kf) = .true.

c          print *, 'nt ix ipar nparms n1 ', nt,ix,ipar,nparms,n1
        else
          np = isame
          v = parm(np)
        endif

c-- npp() holds parameter numbers
        nppt(ix, ipar, nt)=np
        parm(np) = v
        kparm_type(np)=ipar+36

        endif
       enddo
       endif
      enddo
      endif


c***********************************************************
c-- planar earthquake not on a modeled fault
c***********************************************************
       if ( kq.eq.9 ) then

c strike
        k = 11
        kp = 31
        if (ntransient(nt,k).gt.0 ) call newtparm (nt, k, kp )

c dip
        k = 12
        kp = 32
        if (ntransient(nt,k).gt.0 ) call newtparm (nt, k, kp )
c rake
        k = 13
        kp = 33
        if (ntransient(nt,k).gt.0 ) call newtparm (nt, k, kp )

      endif
     
c***********************************************************
c-- expansion crack or spheroid, only strike and dip
c***********************************************************
       if ( kq.eq.11 .or. kq.eq.12 ) then

c strike
        k = 11
        kp = 31
        if (ntransient(nt,k).gt.0 ) call newtparm (nt, k, kp )

c dip
        k = 12
        kp = 32
        if (ntransient(nt,k).gt.0 ) call newtparm (nt, k, kp )

      endif
     
c***********************************************************
c* slip direction       
c***********************************************************
      if ( kq.ne.9 .and. kq.ne.11 .and. kq.ne.12 ) then
        if (ntransient(nt,13).gt.0 ) then
          kp=33
          k=13
         call newtparm (nt, k, kp )
        endif
      endif
         
c***********************************************************
c* 2D Gaussian spatial source, 2D boxcar, planar quake, expansion crack
c***********************************************************

      if (kq.eq.6 .or. kq.eq.7 .or. kq.eq.9 .or. kq.eq.11 .or. 
     .        kq.eq.12 .or. (kq.eq.10 .and. kt.eq.8) ) then
      
c down-dip width
      k = 4
      kp = 24
      if(ntransient(nt,k).gt.0) then
         call newtparm (nt, k, kp )
         if (kq.eq.6 .or. kq.eq.7 ) useGF(kf) = .true.
c         num_node_parms = num_node_parms +1
      endif

c along strike width
      k = 6
      kp = 26
      if(ntransient(nt,k).gt.0) then
         call newtparm (nt, k, kp )
         if (kq.eq.6 .or. kq.eq.7 ) useGF(kf) = .true.
c         num_node_parms = num_node_parms +1
      endif

c azimuth of Gaussian  
      k = 14
      kp = 34
      if ( kq.eq.6 .and. ntransient(nt,k).gt.0 ) then
         call newtparm (nt, k, kp )
      endif
           

      endif
       
c***********************************************************
c*** independent nodes
c***********************************************************
      if (kq.eq.1 ) then

       call clearint(nphist, MAX_parms)

       do iz = 1, nzf(kf)
        do ix = 1, nxf(kf)
         if ( nodefree(ix,iz,nt).eq.1 ) then

c** check for node with same parameter number on this fault 
c**  nflag is a counter to provide a unique identifier for the fault node
        nflag = nt*1000 + ntphi(ix,iz,nt) 

c** see if a prior node on this fault has the same flag
        isamenode=0
        do inc=1, nparms
          if ( nflag.eq.nphist(inc)) isamenode=inc
        enddo

c****************************************************
c** apply EQ: equating node slip
c****************************************************
c      isequated = 0
c      if ( nodes_tr.gt.0 ) then
c       do i=1, nodes_tr
c        npe1 = 0
c        npe2 = 0
c         if (kf.eq.node_tr(i,1) .and. ix.eq.node_tr(i,2) .and.
c     .       iz.eq.node_tr(i,3) ) then
c             npe1=npp(ix, iz, kf) 
c             npe2=npp(node_tr(i,5), node_tr(i,6), node_tr(i,4)) 
c         endif
c         if (kf.eq.node_tr(i,4) .and. ix.eq.node_tr(i,5) .and.
c     .       iz.eq.node_tr(i,6) ) then
c             npe1=npp(ix, iz, kf) 
c             npe2=npp(node_tr(i,2), node_tr(i,3), node_tr(i,1)) 
c         endif
c        if (npe1.gt.0 .and. npe2.eq.0) isequated = npe1
c        if (npe1.eq.0 .and. npe2.gt.0) isequated = npe2
c       enddo
c      endif

c** if no prior node with same flag found, make a new parameter
c** if prior node with same flag found, assign it same parameter number
        if ( isamenode.eq.0) then
          nparms=nparms+1
          nphist(nparms)=nflag
          kparm_index(nparms) = nt
          np = nparms
c          num_node_parms = num_node_parms +1
          useGF(kf) = .true.
        else
          np = isamenode
        endif

c** npp() holds parameter number for the node
          nppt(ix, iz, nt) = np
          parm(np) = tphi(ix,iz,nt)

c*** change type
          kparm_type(np) = 25
          kparm_type(np) = 4

         endif
        enddo
       enddo
      endif
       
c***********************************************************
c--  polygon spatial source
c***********************************************************
      if ( kq.eq.8 ) then

c-- polygon radii
      if(ntransient(nt,14).gt.0 ) then
        np = info_source(nt,6)
        do k = 1, np
         nparms=nparms+1
         parm(nparms) = rpoly(nt, k)
         nrad_parm(nt, k) = nparms
         kparm_type(nparms) = 35
c         num_node_parms = num_node_parms +1
         useGF(kf) = .true.
         kparm_index(nparms) = nt
        enddo
       endif

       
      endif       
c*******************************************       
       
      endif

  34  continue
  
        print *, 'Done assigning parameters '

       if (nparms.gt.MAX_parms) then
         print *, '*** Too many free parameters - finishing'
         print *, '    Increase MAX_parms'
         stop
       endif


      endif
c time dependence

c***********************************************************
       ik = kfclose (kkprm)

       call fopen (kkprm, 1, '_prm.in ')
        write(kkprm, *) 'No.  Type   Parameter     Error        Type '
        do i=1,nparms
         write (kkprm, 13) i, kparm_type(i), 
     .     parm(i), parm_err(i), ptype(kparm_type(i))
        enddo
 13   format(i4,i5,1x,2(2x,1pe12.5),2x,a15)

c       write (kkprm, 41) (k, parm(k), kparm_type(k), kparm_index(k),
c     .        k=1,nparms)
c 41    format(i5, f14.4, 2i4)


c       write (kkprm, *)'Fault Nodes'
c       write (kkprm, *)
c     .  'Flt  iZ  iX Flg Prm   HW   FW       Val.   Free'//
c     .  '     X          Y          Z'

      do kf = 1, nfault
      if (flock(kf)) then
c        print *, 'Fault ', kf
       do 44 iz = 1, nzf(kf)
        dnode = 0.0d0

        do 44 ix = 1, nxf(kf)

        dwidth = 0.0d0

c*** slip vector at node is hanging wall relative to footwall 
      khw=khw_blk(ix,iz,kf)
      kfw=kfw_blk(ix,iz,kf)
      
      ipf=0
      if (phi_free(ix,iz,kf)) ipf=1
      
c       write (kkprm, 42) kf, iz, ix, nphi(ix,iz,kf),
c     .  npp(ix,iz,kf), block_name(khw), block_name(kfw), 
c     .  phi(ix,iz,kf), ipf,  
c     .  xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf)
c  42  format(5i4, 2(1x,a4), f12.4, i4, 3f10.4)
      
  44  continue
      endif
      enddo

      ik = kfclose (kkprm)

c*******************************************************************
c*** added 3/19/04
c** before reading GFs, remove unused site locations
c*******************************************************************

      loc_remove = .true.
      if ( loc_remove) then
c      print *, 'Removing unused GPS locations'
c      print *, 'Old number of locations = ', num_gf_gps

 100  n = num_gf_gps

      do 101 loc = 1, n

c* see if this location is used
        do j=1,num_gps
         if ( loc_gps(j).eq.loc ) goto 101
        enddo

c* remove this location and reassign locs to GPS data
        do j=loc,n-1
         ggf_pos(j,1) = ggf_pos(j+1,1)
         ggf_pos(j,2) = ggf_pos(j+1,2)
         ggf_pos(j,3) = zero
         if (ggf_pos(j+1,3) .ne. zero) ggf_pos(j,3) = ggf_pos(j+1,3)
        enddo

        do j=1,num_gps
         if ( loc_gps(j) .ge. loc) loc_gps(j) = loc_gps(j)-1
        enddo

        num_gf_gps = num_gf_gps - 1
        goto 100

 101  continue

c      print *, 'New number of locations = ', num_gf_gps
      endif

c******************************************************
c***** read/write Green's functions *******************
c******************************************************

c* how many GFs for each data type
      ndat_gf(1) = num_gf_gps
      ndat_gf(2) = num_tilts
      ndat_gf(3) = 0
      ndat_gf(4) = num_ss
      ndat_gf(5) = num_ll
      ndat_gf(6) = num_insar

c      print *, 'Number of node parms =', num_node_parms 

      call getdof (kdof,nd)

      write (*, 53) expname,nd,nparms,kdof
  53  format('Model ',a4,' Ndata=',i8,' Npar=',i5,' DOF=',i8)

      if (nd.gt.0 ) then
c* make GF directory if necessary
        
c LMB Add null termination
c LMB system() returns INTEGER status
      call existfile( './'//gfdir, fexist, 0)
      if (.not. fexist) then
        print *, 'Making directory ', gfdir
        status = system('mkdir ./'//gfdir//char(0))
      endif
      new_gfs = .false.
c*** read GFs or find out what GFs are needed
      if (readgflag) then
        print *, 'Reading GFs from ', gfdir
        call readgfs (1, new_gfs, more_data, kend)
        if (kend) return
      endif

c* make new Green's functions if necessary
      if ( new_gfs ) then
        print *, 'New GFs needed '
        kend = .false.
        call writegfs(kend)
        if (kend) return
        call readgfs (2, new_gfs, more_data, kend)
       endif
      endif


        
c********************************************************************
      if ( .not. kend) then

       if ( Nd.eq.0 .and. invert ) then
         print *, '*** No data to invert '
       endif

       if ( nparms.eq.0 .and. invert ) then
         print *, '*** No free parameters '
       endif

c       if (nitmax.eq.0) then
c         print *, '*** No iterations - finishing'
c         goto 31
c       endif

       print *, 'Starting inversion; nparms=', nparms

       do itnum = 1, nitmax-1

        itertype = icontrol(itnum)

        if ( itertype.eq.0 ) then
         last_iter = .true.
         call solve1(inchar, itnum, itertype)
         last_iter = .false.
         goto 31
        endif


        if ( itertype.gt.0 ) then
         call solve1(inchar, itnum, itertype)
         if (inchar.eq.'q' .or. inchar.eq.'Q') goto 31
        endif

       enddo

  31   call update_phi
       call updatestrain
       call updatepoles 
       call updateRx 


      endif

      return      
      end
      
c************************************************************************
c** WRITEGFS ************************************************************
c************************************************************************
c* generate Green's functions *******************************************
c************************************************************************
      subroutine writegfs (yestop)

c* yestop is flag for error

      implicit real*8 (a-h,o-z)

      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

      character*1 a1
      character gf*80
      character datemade*12
c      character dline*110

      dimension phi_tmp(MAX_x,MAX_z,MAX_f), 
     . slip_n_tmp(MAX_x,MAX_z,MAX_f,2)

      real*8 ins_gf
      common /gf1/ gps_gf(MAX_gps,2,3), tilt_gf(MAX_tilt,2), 
     . ss_gf(MAX_ss,9,2,2), xll_gf(MAX_ll,2,2),
     . ins_gf(MAX_insar_pts,2,3)

      logical recalc, yestop 

      vel_errors = .false.
      kcalculate = .true.
      makeGF = .true.
      yestop = .false.
      
      call defstop (expname, yestop, a1)
      if (yestop) return

c* store current settings and set all phi to 1.0
      do 5 kf=1,nfault
       do 5 iz= 1,nzf(kf)
        do 5 ix = 1,nxf(kf)

          phi_tmp(ix, iz, kf) = phi(ix, iz, kf)
          phi(ix, iz, kf) = 1.0d0

          slip_n_tmp(ix, iz, kf,1) = slip_n(ix, iz, kf,1)
          slip_n_tmp(ix, iz, kf,2) = slip_n(ix, iz, kf,2)

    5 continue

c** make Green's functions as needed
      ncall = 0
      do 10 kf = 1,nfault
       if ( useGF(kf) ) then
        do 20 iz = 1,nzf(kf)
         do 30 ix = 1,nxf(kf)

c* see if any at this node need re-doing
         call clearlog(data_flag, 6)
         recalc = .false.
         do ig=1,6
          if ( gf_code(kf,ix,iz,ig).gt.1 ) then
            data_flag(ig) = .true.
            recalc = .true.
          endif
         enddo

c check for quit
      call defstop (expname, yestop, a1)
      if (yestop) return

        if (recalc) then

        n = 2*MAX_f*MAX_z*MAX_x
        call cleareal(slip_n, n)
        ncall = ncall+1

c- GPS
c - arrays:
c     num_gps     gps_pos()   GPS positions for all sites
c     num_gf_gps  ggf_pos()    GPS positions for unique positions
c     num_gf_gps2 ggf_pos2()   GPS positions to do GFs (near this node)

c- do sites near this node only

       num_gf_gps2 = 0
       do i=1,num_gf_gps
        k = iclose ( xynode(1,ix,iz,kf), xynode(2,ix,iz,kf),
     .               ggf_pos(i,1), ggf_pos(i,2), gps_too_far )
        if ( k.eq.1) then
         num_gf_gps2 = num_gf_gps2+1
         ggf_pos2(num_gf_gps2,1) = ggf_pos(i,1)
         ggf_pos2(num_gf_gps2,2) = ggf_pos(i,2)
         ggf_pos2(num_gf_gps2,3) = zero
         if(ggf_pos(i,3).ne.zero) ggf_pos2(num_gf_gps2,3) = ggf_pos(i,3)
        endif
       enddo

c- InSAR
c - arrays:
c     num_insar     insar_pos()  InSAR positions for all sites
c     num_gf_ins    igf_pos()    InSAR positions for unique positions
c     num_gf_ins2   igf_pos2()   InSAR positions to do GFs (near this node)

       num_gf_ins2 = 0
       do i=1,num_gf_ins
        k = iclose ( xynode(1,ix,iz,kf), xynode(2,ix,iz,kf),
     .               igf_pos(i,1), igf_pos(i,2), gps_too_far )
        if ( k.eq.1) then
         num_gf_ins2 = num_gf_ins2+1
         igf_pos2(num_gf_ins2,1) = igf_pos(i,1)
         igf_pos2(num_gf_ins2,2) = igf_pos(i,2)
         igf_pos2(num_gf_ins2,3) = igf_pos(i,3)
        endif
       enddo
c - 

        write (*, '("Re-doing GF: F",i3," X",i3," Z",i3)') 
     .       kf, ix, iz
        
        call okgfcalc (kf, ix, iz, tmom)

c write GF files

      kish=0
      if (fflag(kf,2)) kish = 1
      kish = ksliptype(kf)

      call dater(datemade)
      xmom = zero
      nlayers=0
 
c* GPS
      if ( data_flag(1)  ) then

       call checkgf (ione, ix, iz, kf, kcode, gf, xm)

       kg=kfopen(22)
       open (kg, file=gf)
        write (*, '("Writing ",a16, " # GPS = ",i5)') 
     .      adjustl(gf),num_gf_gps2

        write (kg, 1) kf, ix, iz, num_gf_gps2, 
     .   xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf),
     .   GFx_interp, GFw_interp, kish,nlayers,gf_version, 
     .   datemade, tmom, gps_near

c put in array after 
c        inde = index_gf(ix,iz,kf)
c        g_gf_date(inde) = datemade
c        ggf_pos(kg,1)= xynode(1,ix,iz,kf)
c        ggf_pos(kg,2)=xynode(2,ix,iz,kf)
c        do j=1,6
c          g_gf(inde,kg,j) = gftmp(j)
c        enddo

        do i=1,num_gf_gps2
         write (kg, 13) ggf_pos2(i,1), ggf_pos2(i,2),
     .    ((gps_gf(i,j,k), j=1,2), k=1,3)
        enddo
       ik = kfclose (kg)
       gf_code(kf,ix,iz,1) = 0

      endif

   1  format('A',3i4,i5,3f12.5,2f8.2,2i5,a5, 1x, 
     .   a12,d14.7,f10.4)
  13  format('G', 2f10.4, 1x, (6d20.13))

c* INSAR
      if ( data_flag(6) ) then
       call checkgf (6, ix, iz, kf, kcode, gf,xm)
        write (*, '("Writing ",a16, " # INSAR = ",i5)') 
     .      adjustl(gf), num_gf_ins2
        kg=kfopen(22)
        open (kg, file=gf)

      write (kg, 1) kf, ix, iz, num_gf_ins2, 
     . xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf),
     . GFx_interp, GFw_interp, kish, nlayers, gf_version, datemade,
     . xmom, gps_near

       do i = 1, num_gf_ins2
         write (kg, 34) igf_pos2(i,1),igf_pos2(i,2),
     .    ((ins_gf(i,j,k), j=1,2), k=1,3)
       enddo
   34 format('I', 2f10.4, 1x, 6(d20.13))

       ik = kfclose(kg)
      endif

c* tilts
      if ( data_flag(2) ) then
       call checkgf (2, ix, iz, kf, kcode, gf, xm)
        write (*, '("Writing ",a16, " # TILT = ",i5)') 
     .      adjustl(gf),num_tilts
c        print *, 'Writing ',gf, ' TILT num= ',num_tilts
       kg=kfopen(22)
       open (kg, file=gf)

      write (kg, 1) kf, ix, iz, num_tilts, 
     . xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf),
     . GFx_interp, GFw_interp, kish, nlayers, gf_version, 
     . datemade, xmom, gps_near

      do 15 i = 1, num_tilts
        tx = (tilt_pos(i,1,1) + tilt_pos(i,2,1))/2
        ty = (tilt_pos(i,1,2) + tilt_pos(i,2,2))/2
        write (kg, 16) tx, ty,
     .      tilt_gf(i,1), tilt_gf(i,2), tilt_name(i) 
  15  continue
  16  format('T',2f10.4, 2d20.13, 1x, a8 )

       ik = kfclose(kg)
      endif


c* surface strain
      if ( data_flag(4) ) then
       call checkgf (4, ix, iz, kf, kcode, gf,xm)
        write (*, '("Writing ",a16, " number SS = ",i5)') 
     .      adjustl(gf),num_ss
c        print *, 'Writing ',gf, ' SS num= ',num_ss
       kg=kfopen(22)
       open (kg, file=gf)

      write (kg, 1) kf, ix, iz, num_ss, 
     . xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf),
     . GFx_interp, GFw_interp, kish, nlayers, gf_version, datemade,
     . xmom, gps_near

c** strain rates, store 3 components Exx, Exy, Eyy for 2 input displacements
      do i = 1, num_ss
        call get_ss_pos(i,5, x0, y0)

        write(kg, 23) x0, y0, ss_type(i), ss_name(i) 
        do j=1,9
          call get_ss_pos(i, j, x, y)
          write(kg,24) x,y,((ss_gf(i,j,k,l), l=1,2), k=1,2)
        enddo

      enddo

   23 format('S', 2f10.4, 1x, i4, 1x, a10 )
   24 format(2f10.4, 4(d20.13) )

       ik = kfclose(kg)
      endif


c* line lengths
      if ( data_flag(5) .and. num_ll_sites .gt.0) then
       call checkgf (5, ix, iz, kf, kcode, gf,xm)
        write (*, '("Writing ",a16, " number LL = ",i5)') 
     .      adjustl(gf),num_ll_sites
c        print *, 'Writing ',gf, ' LL num= ',num_ll_sites
        kg=kfopen(22)
        open (kg, file=gf)

      write (kg, 1) kf, ix, iz, num_ll_sites, 
     . xynode(1,ix,iz,kf), xynode(2,ix,iz,kf), znode(iz,kf),
     . GFx_interp, GFw_interp, kish, nlayers, gf_version, datemade,
     . xmom, gps_near

       do i = 1, num_ll_sites
        x0 = pos_ll(i,1)
        y0 = pos_ll(i,2)
        write(kg, 33) x0, y0, ((xll_gf(i,j,k),j=1,2),k=1,2),name_ll(i) 
       enddo
   33 format('L', 2f10.4, 1x, 4(d20.13), 1x, a4 )

       ik = kfclose(kg)
      endif

c** end of output

      endif

   30 continue
   20 continue

      endif
   10 continue


c* re-set the arrays
      do 7 kf=1,nfault
       do 7 iz=1,nzf(kf)
        do 7 ix=1,nxf(kf)
          phi(ix, iz, kf) = phi_tmp(ix, iz, kf)
          slip_n(ix, iz, kf,1) = slip_n_tmp(ix, iz, kf,1)
          slip_n(ix, iz, kf,2) = slip_n_tmp(ix, iz, kf,2)
    7 continue

      do ig=1,6
       data_flag(ig) = .true.
      enddo
 
      makeGF = .false.

      return
      end

c********************************************************************
c*** READGFS ********************************************************
c********************************************************************
c* read GF files and check that they are correct for current model **
c********************************************************************
      subroutine readgfs (ncall, new_gfs, more_data, yestop)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

      logical kgflag(MAX_gps), ktflag(MAX_tilt),
     .  ksflag(MAX_ss), klflag(MAX_ll), kiflag(MAX_insar_pts)

      character cname*4, c8name*8, dtype*1, gf*80
      character a1*1, hdr*122, gfdate*12
      dimension gftmp(6), gf_line(MAX_gps,8)

      logical write_err, new_gfs, yestop, more_data, newmeth
      logical kstop, missing_gf, read_gf_in

      character*1 gfcomp(6)

      data gfcomp / 'g', 't', 'u', 's', 'l', 'i' /

      write_err = .false.
      new_gfs = .false.
      more_data = .false.
      yestop = .false.
      call defstop (expname, yestop, a1)
      if (yestop) return

      call clearlog(kgflag, MAX_gps)
      call clearlog(kiflag, MAX_insar_pts)
      call clearint(gf_code, MAX_f*MAX_x*MAX_z*6)

c* create GFs with a function instead of reading them
c  +fgf flag set
      if ( funcgfs ) then
       indx = 0
       xcordist = 100.0d0
       do kf = 1, nfault
        do iz = 1, nzf(kf)
         do ix = 1, nxf(kf)
          xn = xynode(1,ix,iz,kf)
          yn = xynode(2,ix,iz,kf)
          inde = index_gf(ix,iz,kf)
          if (inde.eq.0 ) then
           indx = indx+1
           index_gf(ix,iz,kf) = indx
           inde = indx
          endif

        do  kg = 1, num_gf_gps
          px = ggf_pos(kg,1)
          py = ggf_pos(kg,2)
          call distkm (xn,yn,xg,yg,xkm)
             do j=1,6
              g_gf(inde,kg,j) = exp ( - (xkm/xcordist)**2 )
             enddo
        enddo

          enddo
         enddo
        enddo
 
      return
      endif

c* first read through and check for new GFs needed
      if (ncall.eq.1) then
      do kf = 1, nfault
       if ( useGF(kf) ) then
       do iz = 1, nzf(kf)
        do ix = 1, nxf(kf)
         do ig = 1,6
          call checkgf (ig, ix, iz, kf, kcode, gf, xm)
          gf_code(kf,ix,iz,ig) = kcode
          if ( gf_code(kf,ix,iz,ig).gt.1 ) new_gfs = .true.
          if ( gf_code(kf,ix,iz,ig).eq.1 ) more_data = .true.
         enddo
        enddo
       enddo
       endif
      enddo

c set flag if GFs for adjacent nodes needs to be redone
      if ( new_gfs) then
       do kf=1,nfault
        if ( useGF(kf) ) then
        do iz = 1,nzf(kf)
         do ix = 1,nxf(kf)
          do ig = 1,6
           if ( gf_code(kf,ix,iz,ig).eq.2 ) then
            ix1=max(1,ix-1)
            ix2=min(nxf(kf), ix+1)
            iz1=max(1,iz-1)
            iz2=min(nzf(kf), iz+1)

            do ixx = ix1, ix2
             do izz = iz1, iz2
              gf_code(kf,ixx,izz,ig)= 9
             enddo
            enddo

            endif

          enddo
         enddo
        enddo
        endif
       enddo

       if ( ncall.eq.1) print *, 'Error in GFs on first read'
       return
      endif
      endif

c******** start reading in GFs
      indx = 0
      
c-- clear GF array
c      do 1 i= 1, MAX_nodes
c       do 1 j= 1, MAX_gps
c        do 1 k = 1,6
c  1      g_gf(i, j, k ) = 0.0d0 
  
  
      do kf = 1, nfault
c       if(fflag(kf,4)) print *, fault_name(kf)
       if ( useGF(kf) ) then
       
c** index for GF       
c         first_gf(kf) = inde+1

       write(*, '("Reading GFs for fault #",i3, 2x, a10)' )  kf, 
     .  adjustl(fault_name(kf))

c*** loop through nodes to read GFs
 
       do 50 iz = 1, nzf(kf)
        do 50 ix = 1, nxf(kf)

c index for this node's GF 
         inde = index_gf(ix,iz,kf)
         if (inde.eq.0 ) then
           indx = indx+1
           index_gf(ix,iz,kf) = indx
           inde = indx
         endif
         
         call clearlog(kgflag, MAX_gps)
           
c******** read GPS GFs, both lists are sorted for faster reading

      if ( num_gf_gps.gt.0 ) then

        call checkgf (1, ix, iz, kf, kcode, gf, xm)
        if(verbose) write(*, '("Reading ", a80)') adjustl(gf)

        tmom_node(ix,iz,kf) = xm

c* first check for sites too distant
       xn = xynode(1,ix,iz,kf)
       yn = xynode(2,ix,iz,kf)
      do kg = 1, num_gf_gps
       xgp = ggf_pos(kg,1)
       ygp = ggf_pos(kg,2)
       kkk = iclose ( xgp, ygp, xn, yn, gps_too_far )
       if (kkk .ne. 1) kgflag(kg)=.true.
      enddo

      newmeth=.false.

      if ( newmeth) then

c** new method, read all GFs in
      k28=kfopen(38)
      open (k28, file=gf)
      read(k28, '(a122)') hdr
      read(hdr, '(86x,a12)') gfdate
      if(write_hdr) print *, k28, hdr
      read(hdr, '(13x,i5)') ngf
      do k=1,ngf
       read(k28, 13) dtype, (gf_line(k,j), j=1,6)
      enddo
      ik = kfclose(k28)
      m1=1

      do 65 k=1,ngf
         gx=gf_line(k,1)
         gy=gf_line(k,2)

        do 66 kg = m1, num_gf_gps
         if (.not. kgflag(kg))  then
          px = ggf_pos(kg,1)
          py = ggf_pos(kg,2)

            if (iclose(px,py,gx,gy,gps_near).eq.1 ) then
             do j=1,6
              g_gf(inde,kg,j) = gf_line(k,j+2)
             enddo
              kgflag(kg)= .true.
              m1 = kg+1
              goto 66
            elseif(px.gt.gx) then
              m1 = kg-1
              goto 65
            else
              goto 66
            endif

         endif
 66   continue
 65   continue


      else

c** old method, read 1 GF at a time, and compare to all data
c-- check the date of the GF to see if its already read in

      k28=kfopen(38)
      open (k28, file=gf)
      read(k28, '(a122)') hdr
      read(hdr, '(86x,a12)') gfdate
      read_gf_in = (gfdate .ne. g_gf_date(inde))
      read(hdr, '(13x,i5)') ngf
      if (write_hdr) print *, hdr

      if ( read_gf_in .and. ngf.gt.0 ) then
   
      n1=1
      g_gf_date(inde) = gfdate

 11   read(k28, 13, end=12) dtype, xgf, ygf, (gftmp(j), j=1,6)

c 15   m1 = n1
      m1 = 1

      do 14 kg = m1, num_gf_gps

       if ( .not. kgflag(kg) ) then

        xgp = ggf_pos(kg,1)
        ygp = ggf_pos(kg,2)

        if (iclose(xgf,ygf,xgp,ygp,gps_near).eq.1) then

           do j=1,6
             g_gf(inde,kg,j) = gftmp(j)
           enddo

           kgflag(kg)= .true.

c           n1 = max(1,kg-2)
c           goto 15

c         else

c           if ( xgp.gt.xgf .or. ygp.gt.ygf) then
c             n1 = max(1,kg-2)
c             goto 11
c           endif

         endif
        endif

 14    continue
        goto 11

      endif

 12    ik = kfclose(k28)

      endif


 13   format(a1, 2f10.4, 1x, (6d20.13) )

c* check for missing GFs  

      if ( gf_code(kf, ix, iz, 1) .le. 1 .and. read_gf_in ) then
      missing_gf = .false.
       do i=1, num_gf_gps
        if ( .not. kgflag(i) ) then 
         do kk=1,num_gps
          if (gps_type(kk).gt.0) then
           if ( loc_gps(kk).eq.i )  then
            px = ggf_pos(i,1)
            py = ggf_pos(i,2)
            kc = iclose(px,py,xynode(1,ix,iz,kf),xynode(2,ix,iz,kf),
     .          gps_too_far)

            if (kc.eq.1) then
               gf_code(kf, ix, iz, 1) = 8
               if(inde.eq.1) write(*,27) gps_name(kk),gf
               missing_gf = .true.
            endif

           endif
          endif
         enddo
        endif
       enddo
c       if(ncall.eq.1 .and. missing_gf) then
c        return
c       endif
      endif
  27  format('GF not found for site ',a8,' in GF File ', a80)

      endif
c*** end of GPS read

c***********  read INSAR GFs ***************************

      if ( num_insar.gt.0 ) then
       xinear = gps_near
       call clearlog(kiflag, MAX_insar_pts)

       call checkgf (6, ix, iz, kf, kcode, gf, xm)

cA   1   1   1 1236   234.80000    40.52000     0.00000    4.80    2.50    0    010.13 201206121339 0.0000000D+00    0.1000
       if ( kcode.eq.0 ) then
        if(verbose) write(*, '("Reading ", a80)') adjustl(gf)
        ki=kfopen(38)
        open (ki, file=gf)
        read(ki, '(a122)') hdr
        read(hdr, '(13x,i5)') ngf
        read(hdr, '(86x,a12)') gfdate
        read_gf_in = (gfdate .ne. i_gf_date(inde))

      if ( read_gf_in .and. ngf.gt.0 ) then

        i1 = 1

c insar obs points are not sorted, but GFs are

       do k = 1, ngf 
       read (ki, 39, end=38) dtype, xg, yg, (gftmp(j), j=1,6)

c find data that use this GF
c set kiflag() if GF found
c nrd is first site not already read in
c      nrd = 0
c            do i = i1, num_insar
            do i = 1, num_insar

             if ( .not. kiflag(i) ) then
              xu = insar_pos(i,1)
              yu = insar_pos(i,2)
               if ( iclose(xu,yu,xg,yg, xinear).eq.1 ) then
c                print *, 'Insar GF i inde ', i, inde
                do j=1,6
                  i_gf(inde, i, j) = gftmp(j)
                enddo
                kiflag(i)= .true.
c                if ( nrd.eq.0 ) nrd = i
               endif
             endif

            enddo
c          i1 = nrd
        enddo
       endif
  38    ik = kfclose(ki)


       do i=1, num_insar
        if ( .not. kiflag(i) ) then 
         if ( write_err) print *, i,' I  GF not found'
         kstop = .true.
         gf_code(kf, ix, iz, 6) = 9
        endif
       enddo

       endif
      endif
  39  format(a1, 2f10.4, 1x,(6d20.13))

c********* read tilt GFs ***************************
      if ( num_tilts.gt.0 ) then
       call checkgf (2, ix, iz, kf, kcode, gf,xm)
       if ( kcode.eq.0 ) then
       
        ki=kfopen(28)
        open (ki, file=gf)
        read(ki, '(a117)') hdr

  21   read (ki, 23, end=22) dtype, x, y, (gftmp(j), j=1,2), c8name 
         do i=1,num_tilts
           if (c8name.eq.tilt_name(i)) then
             do j=1,2
               t_gf(inde,i,j) = gftmp(j)
             enddo
             ktflag(i)= .true.
            endif
          enddo
          goto 21
  22  ik = kfclose(ki)

       do  i=1, num_tilts
        if ( .not. ktflag(i) ) then 
         if ( write_err) print *, tilt_name(i),' T  not found'
         kstop = .true.
         gf_code(kf, ix, iz, 2) = 9
        endif
       enddo

       endif
      endif
  23  format(a1, 2f10.4,(2d20.13),1x,a8)
      
      

c************* read strain GFs **********************
      if ( num_ss.gt.0 ) then
       call checkgf (4, ix, iz, kf, kcode, gf,xm)
       if ( kcode.eq.0 ) then

        ki=kfopen(38)
        open (ki, file=gf)

      do 45 i=1, num_ss
         call get_ss_pos(i, 5, xp, yp)
         rewind (ki)
         read(ki, '(a117)') hdr

c        write(kg, 23) x0, y0, ss_type(i), ss_name(i) 
c   23 format('S', 2f10.4, 1x, i4, 1x, a10 )

   41  read (ki,43,end=45) dtype, xs, ys, ktp, cname

       if ( (iclose( xs, ys, xp, yp, gps_near ).eq.1) .or. 
     .    (cname.eq.ss_name(i))) then

            do j=1,9
             read(ki,44,end=45) x,y,(gftmp(n), n=1,4)
             n=0
              do k=1,2
               do l=1,2
                n=n+1
                s_gf(inde, i, j, k, l) = gftmp(n)
               enddo
              enddo
             enddo
             ksflag(i)= .true.
             goto 45

         else

           do j=1,9
             read(ki, '(a120)', end=45) c80
           enddo
           goto 41

         endif

   45 continue

      ik = kfclose(ki)

       do i=1, num_ss
        if ( .not. ksflag(i) ) then 
         if ( write_err) print *, ss_name(i),' S  not found'
         kstop = .true.
         gf_code(kf, ix, iz, 4) = 9
        endif
       enddo

      endif
      endif

   43 format(a1, 2f10.4, 1x, i4, 1x, a10)
   44 format(2f10.4,4d20.13)

c************* read line length GFs *************************
      if ( num_ll_sites.gt.0 ) then
       call checkgf (5, ix, iz, kf, kcode, gf,xm)
       if ( kcode.eq.0 ) then

       ki=kfopen(38)
       open (ki, file=gf)
       read(ki, '(a117)') hdr

 51   read(ki, 53, end=52) dtype, x, y, (gftmp(j), j=1,4)

      do kg=1, num_ll_sites
       if (iclose(x,y,pos_ll(kg,1),pos_ll(kg,2),gps_near).eq.1 ) then
        do j=1,4
         x_gf(inde,kg,j) = gftmp(j)
        enddo
         klflag(kg)= .true.
       endif
      enddo

      goto 51

 52    ik = kfclose(ki)
 

       do i=1, num_ll_sites
        if ( .not. klflag(i) ) then 
         if ( write_err) print *, name_ll(num_site_ll(i,1)),' ',
     .       name_ll(num_site_ll(i,1)),' LL  not found'
         kstop = .true.
         gf_code(kf, ix, iz, 5) = 9
        endif
       enddo


       endif
      endif
 53   format(a1, 2f10.4, 1x, (4d20.13))

c****************************************************
c* end of reading gfs
c****************************************************

   50 continue
      endif
      enddo

      makeGF = new_gfs

      return
      end
      

c'=============================================================================================
      subroutine checkgf (kdin, ix, iz, kf, kcode, gfile, xmom)
c
c check GF file, returns kcode 
c  = -1 GF not needed (no data)
c  =  0 file ok as is
c  =  1 does not contain enough data
c  =  2 node position or node index changed
c  =  3 interpolation intervals too big
c  =  4 file does not exist
c  =  5 earlier defnode version (formats may have changed)
c  =  6 all GFs to be redone (request in GD: option)
c  =  7 wrong shear type

c gfile is file name (generated here)
c kdat - data type (1,2,3,4,5,6) --> (g,t,u,s,l,i)
c ix,iz,kf for node

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

      character gfile*80, cf*9, gfc*1, datemade*12, dnv*5
      logical fexist, rep 

      character gfcomp*6

      kdat = kdin
      gfcomp = 'gtusli'
      gfc = gfcomp(kdat:kdat)

      kcode = -1
      rep = .true.
 
      ndatexp = ndat_gf(kdat)
      
      call getnodename(kf, ix,iz, cf)
      gfile = gfdir//'/gf'//cf//gfc
c      print *, 'Looking for ', gfile
      
      if ( ndatexp.eq.0) then
       return
      endif
      
      if ( makeGF_all ) then
       kcode = 6
       return
      endif


      call existfile( gfile, fexist, 0)
      if ( .not. fexist ) then
        kcode = 4
        if (rep) print *, 'Not exist ', adjustl(gfile)
        return
      else
        kcode = 0
      endif

      ki = kfopen(12)
      open (ki, file=gfile)

      read(ki, 1) ikf, iix, iiz, nin, xn, yn, zn, Xi, Zi, ishear, nl, 
     .  dnv, datemade, xmom, gpsn

 1    format(1x,3i4,i5, 3f12.5, 2f8.2,2i5, a5, 1x, a12, d14.7,f10.4)


cc shear type
      if ( ksliptype(ikf) .ne. ishear ) then
       kcode = 7
       if (rep) print *, 'Wrong fault slip type '  
       goto 999
      endif

c* wrong version of file
      if ( dnv .ne. gf_version) then
       kcode = 5
       print *, 'Old ',dnv,' New ', gf_version
       if (rep) print *, 'Wrong version ', gfile
       goto 999
      endif

c* interpolation intervals too large
      if ( Xi.gt.GFx_interp .or. Zi.gt.GFw_interp) then
       kcode = 3
       if (rep) print *, 'Interp too big ', adjustl(gfile)
       goto 999
      endif

c* not enough data in GF
      if ( ndatexp.gt.nin ) then
        kcode = 0
c        kcode = 1
c         if (rep) then
c           print *, 'Not enough data ', gfile
c           print *, 'Expeced:',ndatexp,' Found:', nin
c         endif 
c        goto 999
      endif

c  incorrect node position or index in file
       xnr = dx_node_new
       isnear = iclose(xynode(1,ix, iz, kf),
     .  xynode(2,ix, iz, kf), xn, yn, xnr)

      if ( (isnear.eq.0) .or. abs(znode(iz, kf) - zn).gt.1.0d-2 .or.
     .    ikf .ne. kf .or. iix .ne. ix .or. iiz .ne. iz )then
        kcode = 2
        if (rep) print *, 'Node position wrong ', adjustl(gfile)
        goto 999
      endif

 999  ik = kfclose (ki)

      return
      end
c'=============================================================================================
      subroutine solve2 (prms, total_misfit, datachi2, pensum, maxpen)
c      
c
c-- get misfit and penalty for input set of parameters prms()
c
c
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

      dimension prms(MAX_parms)
      dimension elaG(MAX_gps, 3), elaI(MAX_insar_pts,3)

      logical fitpoles, ppr, flock, wela, useGPS

      character c3*3

      total_misfit = 0.0d0
      datachi2 = 0.0d0
      pensum = 0.0d0
      maxpen = 0

      fitpoles = .false.
      ppr = .false.

c      print *, 'Solve2'

c write elastic component by fault
      wela = ( last_iter .and. ela_by_fault )

c      call cleareal(parm, MAX_parms)

c clear all arrays
      call clearcalc  

      call uminmax (umin, umax) 

c-- fill the common array holding the model parameters
      do i=1, nparms
        parm(i)=prms(i)
      enddo
      


c update the phi array
      call update_phi

c add rotation to GPS
      call updatepoles 

c update relaxation
      call updateRx 

c add strain to GPS
      call updatestrain 

c time functions
      call stfarea
      
c GPS rotation and strain vectors      
      do i = 1, num_gps
         n = nblock_gps(i)
         call gpsinfo2(i, xpt, ypt, xo, yo, zobs, xc, yc, zcalc,
     .    xs, ys, sigz, s)
         call relvel (1, n, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho) 
         gps_rot(i,1)=Vx
         gps_rot(i,2)=Vy
         call relvel (2, n, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho) 
         gps_str(i,1)=Vx
         gps_str(i,2)=Vy
      enddo

c InSAR rotation and strain vectors      
      do i = 1, num_insar
         n = nblock_insar(i)
         xpt = insar_pos(i,1)
         ypt = insar_pos(i,2)
         call relvel (1, n, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho) 
         ins_rot(i,1)=Vx
         ins_rot(i,2)=Vy
         ins_rot(i,3)=0.0d0
         call relvel (2, n, nblock_ref, xpt, ypt, Vx, Sx, Vy, Sy, rho) 
         ins_str(i,1)=Vx
         ins_str(i,2)=Vy
         ins_str(i,3)=0.0d0
      enddo

c  add network adjustment
      call network

c  get node vectors
      call nodevectors

c-- loop through nodes to convolve GFs
c      num_gf =0
 
      do kf = 1, nfault
       if ( flock(kf) .and. useGF(kf) ) then
       fault_sums(kf,3) = 0.0d0
       fault_sums(kf,4) = 0.0d0
        if (wela) call cleareal(elaG, 3*MAX_gps)
        if (wela) call cleareal(elaI, 3*MAX_insar_pts)

       do 20 iz = 1, nzf(kf)
         do 20 ix = 1, nxf(kf)
      
c        num_gf = num_gf +1
        num_gf = index_gf(ix,iz,kf)
     
        a = phi(ix, iz, kf)
        
c- loop through sites and convolve Green's functions
      if ( a .ne. 0.0d0 ) then
       
       sX = a * slip_n(ix, iz, kf, 1)
       sY = a * slip_n(ix, iz, kf, 2) 

       fault_sums(kf,3) = fault_sums(kf,3) + a * tmom_node(ix,iz,kf)*xmu
       fault_sums(kf,5) = fault_sums(kf,5) + a * znode(iz,kf)*1.0d3
       fault_sums(kf,6) = fault_sums(kf,6) + a * 1.0d3
 
c--- GPS
        do  k=1, num_gf_gps
         U1 = sX * g_gf (num_gf, k, 1) + sY * g_gf (num_gf, k, 2)
         U2 = sX * g_gf (num_gf, k, 3) + sY * g_gf (num_gf, k, 4)
         U3 = sX * g_gf (num_gf, k, 5) + sY * g_gf (num_gf, k, 6)
         gps_ela(k,1) =  gps_ela(k,1) + U1
         gps_ela(k,2) =  gps_ela(k,2) + U2
         gps_ela(k,3) =  gps_ela(k,3) + U3

c for this fault
        if (wela) then
         elaG(k,1) =  elaG(k,1) + U1
         elaG(k,2) =  elaG(k,2) + U2
         elaG(k,3) =  elaG(k,3) + U3
        endif

        enddo

c-- Insar
        do  k=1, num_insar
         U1 = sX * i_gf (num_gf, k, 1) + sY * i_gf (num_gf, k, 2)
         U2 = sX * i_gf (num_gf, k, 3) + sY * i_gf (num_gf, k, 4)
         U3 = sX * i_gf (num_gf, k, 5) + sY * i_gf (num_gf, k, 6)
         
         ins_ela(k,1) =  ins_ela(k,1) + U1 
         ins_ela(k,2) =  ins_ela(k,2) + U2
         ins_ela(k,3) =  ins_ela(k,3) + U3
     
        if (wela) then
c         print *, 'ins ', k, num_gf
         elaI(k,1) =  elaI(k,1) + U1
         elaI(k,2) =  elaI(k,2) + U2
         elaI(k,3) =  elaI(k,3) + U3
        endif

        enddo


c-- tilt rates
       if (num_tilts.gt.0) then
        do k = 1, num_tilts
          tilt_calc(k) = tilt_calc(k) + sX * t_gf(num_gf,k,1)  
     .                                + sY * t_gf(num_gf,k,2)
        enddo
       endif
 
c-- strain rates
      if (num_ss.gt.0) then
        do ii = 1, num_ss
          do j = 1, 9

           ss_vel(ii,j,1) = ss_vel(ii,j,1)  + 
     .       sX * s_gf(num_gf, ii, j, 1, 1) +
     .       sY * s_gf(num_gf, ii, j, 1, 2)

           ss_vel(ii,j,2) = ss_vel(ii,j,2)  + 
     .       sX * s_gf(num_gf, ii, j, 2, 1) +
     .       sY * s_gf(num_gf, ii, j, 2, 2)

          enddo
        enddo  
      endif

c-- line length change rates
       if (num_ll_sites.gt.0) then
        do k=1, num_ll_sites
         vel_ll(k,1) =  vel_ll(k,1) + sX * x_gf (num_gf, k, 1)  
     .                              + sY * x_gf (num_gf, k, 2)

         vel_ll(k,2) =  vel_ll(k,2) + sX * x_gf (num_gf, k, 3)  
     .                              + sY * x_gf (num_gf, k, 4)
        enddo
       endif
      
      endif
      
      
 20   continue

c -- write GPS elastic velocities for this fault
      if ( wela .and. num_gps.gt.0 ) then
       call i2c (kf, 3, c3)
       call fopen (kkp, 1, '_flt_'//c3//'.elaG ')
        do i=1, num_gps
        if ( useGPS(i) ) then
          l = loc_gps(i) 
          if(l.gt.0) write (kkp, '(2f9.3, 3f8.2,3f4.1,1x,a8)') 
     .     (gps_pos(i,j),j=1,2), (elaG(l,j),j=1,3), zero,zero,zero,
     .      gps_name(i)
         endif
       enddo
       ik = kfclose(kkp)
      endif
      
c InSAR elastic LOS and vectors
      if ( wela .and. num_insar.gt.0 ) then
       call fopen (kkp, 1, '_flt_'//c3//'.elaI ')
        do i=1, num_insar
          l = loc_ins(i) 
          if(l.gt.0) then
            dlos = -insar_unit(i,1)*elaI(l,1) - 
     .      - insar_unit(i,2)*elaI(l,2)
     .      - insar_unit(i,3)*elaI(l,3)
          write (kkp, '(2f9.3, 4f8.2, i6)') 
     .     (insar_pos(i,j),j=1,2), dlos, (elaI(l,j),j=1,3), l
          endif
        enddo
       ik = kfclose(kkp)
      endif


       endif
      enddo


c-- get ref site velocity
      call refsitevel

c-- compare data to calculations
      call misfit(sumch2, ndata1, kdof)

c-- data reduced chi**2 misfit
      datachi2 = sumch2/real(kdof)

c-- apply penalties   
c      print *, 'call penalty 1', tminmax(nt,30,1), tminmax(nt,30,2)
      call penalty(pensum, maxpen, 0)
c      print *, 'call penalty 2', tminmax(nt,30,1), tminmax(nt,30,2)

c-- total misfit
      total_misfit = datachi2 + pensum
c        write(*, '(30f10.4)' ) (parm(i),i=1,nparms), total_misfit

      return
      end
      
c'======================================================================
      subroutine penalty(pensum, nmaxpen, kwrite)

c--- get penalties for constraints
c    pensum is accumulated penalty
c    nmaxpen is the index of the maximum penalty

      implicit real*8 (a-h,o-z)

      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

      character ptype(50)*15, p2codes(50)*2
      common /ptype1/ ptype, p2codes

      logical wwpen, check_fault, flock
      
      character*2 c2
      character*4 pname(MAX_pen)
      
      dimension p(9), pfac(40), tpen(20)
     
      dimension ppX(200), ppW(200), pfX(500), pfY(500),
     .  penF(MAX_f,6), penT(MAX_srce,10)

c penalties for transient parms
      data tpen / 1.0d1, 1.0d1, 1.0d0, 1.0d0, 0.1d0, 
     .            1.0d0, 1.0d1, 1.0d0, 1.0d0, 1.0d0, 
     .            1.0d0, 1.0d0, 0.1d0, 1.0d0, 1.0d0, 
     .            1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0 /

      call cleareal(p,9)
      call cleareal(pentype, MAX_pen)
      call cleareal(penF, 6*MAX_f)
      call cleareal(penT, 10*MAX_srce)

      zero = 0.0d0
      pensum = 0.0d0
      nmaxpen = 0
      c2='  '
      wwpen = ( kwrite.gt.0 )


      if ( .not. apply_penalty ) return

c* penalties listed in order
c* (101)moment, (102)node values, (103)depths, 
c* (104)downdip constraints, (105)smoothing, (106)hard constraints

c* penalty slopes
      xmom_pen_fac   = penalty_factor2(1)
      xnode_pen_fac  = penalty_factor2(2)
      wang_pen_fac   = penalty_factor2(2)
      smooth_pen_fac = penalty_factor2(3)
      z_pen_fac      = penalty_factor2(4)
      dd_pen_fac     = penalty_factor2(5)
      hc_pen_fac     = penalty_factor2(6)


      do i=1,40
        pfac(i) = penalty_factor(i)
      enddo

      xf1 = 1.0d0
      xf2 = xf1
      
c** node values
      phi_min=psmin(4)
      phi_max=psmax(4)
      if ( phi_max.eq.0.0 ) phi_max=1.0d0

c min, max depths
      zmin6=psmin(6)
      zmax6=psmax(6)
      zmin7=psmin(7)
      zmax7=psmax(7)

c min, max depths for Gaussian
      zmin8=psmin(8)
      zmax8=psmax(8)
      zmin9=psmin(9)
      zmax9=psmax(9)

c* Penalty type 95
c** bounds on interseismic moment rate
       np=95
       do kf=1, nfault
        if ( flock(kf) .and. fflag(kf,3) ) then
         cm = fault_sums(kf,3)
         omin = smooth(kf,5)
         omax = smooth(kf,6)

          if ( omin.gt.zero .and. cm.lt.omin ) then
           pens = abs( dlog10(cm) - dlog10(omin) ) * xmom_pen_fac
           pentype(np)=pentype(np)+pens
          endif

          if ( omax.gt.zero .and. cm.gt.omax ) then
           pens = abs( dlog10(cm) - dlog10(omax) ) * xmom_pen_fac
           pentype(np)=pentype(np)+pens
          endif

        endif
       enddo

c* Penalty on block strain rates; type 89
c-- block strain penalty
      if (damp_strain) then
       do kn=1, nparms
        kpt = kparm_type(kn)
        if (kpt.eq.3 ) then
           pkn = parm(kn) * 1.0d3  
           kk = kparm_index(kn)
           pf = 1.0d-3
           stmax = 1.0d8
           if (strain2(kk,3).gt.zero ) pf    = strain2(kk,3)
           if (strain2(kk,4).gt.zero ) stmax = strain2(kk,4)
           pens = abs(pkn) * pf
           ss = abs(pkn)
           if ( ss.gt.stmax ) pens = pens + (ss-stmax)/10.0d0
           pentype(89)=pentype(89)+pens
        endif
       enddo
      endif

c* Penalty type 97
c* Zmax must be greater than Zmin
       do kn=1, nparms
        pkn = parm(kn)
        kpt = kparm_type(kn)
        pknp1 = parm(kn+1)
        kptp1 = kparm_type(kn+1)

        if (kpt.eq.6 .and. kptp1.eq.7) then
          if  ( pkn.gt.pknp1 ) then
           pens = abs(pkn-pknp1) * z_pen_fac
           pentype(97)=pentype(97)+pens
          endif
        endif

       enddo

c** force z2 > z1
      do kf = 1, nfault
       if (flock(kf) .and. fflag(kf,3) ) then
        kfft = fault_fit_type(kf)
        if ( kfft.eq.2 .or. kfft.eq.3  ) then
         do ix = 1, nxf(kf)
          dz = f_parm(kf,ix,3) - f_parm(kf,ix,2)

          if ( dz.lt.zero ) then
           pens = abs(dz) * z_pen_fac
           pentype(97)=pentype(97)+pens
           penF(kf,2) = penF(kf,2) + pens
          endif

         enddo
        endif
       endif
      enddo


c* Penalty type 94
c*** force nodes to not increase in value downdip
c    by applying penalty if updip node has a smaller phi
      
      do kf = 1, nfault
       if ( flock(kf) .and. 
     .    (fault_fit_type(kf).eq.1) .and. fflag(kf,3) ) then

        do 20 iz = 2, nzf(kf)
        do 20 ix = 1, nxf(kf)

          np0 = npp (ix, iz-1, kf) 
          np1 = npp (ix, iz,   kf)

        if (np0.gt.0 .and. np1.gt.0 .and. np1 .ne. np0) then

c         vp1 = parm(np1)
c         vp0 = parm(np0)
c    applying penalty if updip node has smaller Vphi
         v1 = dsqrt(slip_n(ix,iz,  kf,1)**2+slip_n(ix,iz,  kf,2)**2)
         v0 = dsqrt(slip_n(ix,iz-1,kf,1)**2+slip_n(ix,iz-1,kf,2)**2)
         vp1 = phi(ix, iz, kf) * v1
         vp0 = phi(ix, iz-1, kf) * v0

           if (vp1.gt.vp0 ) then
             pens = abs(vp1-vp0) * dd_pen_fac
             pentype(94)=pentype(94)+pens
             penF(kf,1) = penF(kf,1) + pens
           endif

        endif
 20   continue

      endif
      enddo

c*** apply INTERSEISMIC along strike and down-dip smoothing 
      do kf = 1, nfault
       if ( flock(kf) .and. fflag(kf,3) ) then
       ksm = int(smooth(kf,1))
       smX = smooth(kf,2) 
       smW = smooth(kf,3) 
       sm0 = smooth(kf,4) 
       np = 80+ksm
c       rn = real(nxf(kf)*nzf(kf))
       rn = zero
       pens = zero

c how many free nodes
       do iz = 1, nzf(kf)  
        do ix = 1, nxf(kf)  
         if ( phi_free(ix, iz, kf) ) rn = rn + 1.0d0
        enddo
       enddo

c*** damp sum of average phi; penalty 80
      if ( sm0.gt.zero .and. rn.gt.zero) then
       suma=zero
       do iz = 1, nzf(kf)  
        do ix = 1, nxf(kf)  
         if ( phi_free(ix, iz, kf) ) 
     .       suma = suma + abs (phi(ix, iz, kf))
        enddo
       enddo

       pens = suma * sm0 / rn
       pentype(80)=pentype(80)+pens
       penF(kf,4)=penF(kf,4)+pens
      endif

c - damp gradients in phi  Penalty type 81

      if ( ksm.eq.1 .and. (smX.gt.zero .or. smW.gt.zero )
     .          .and. rn.gt.zero ) then

        do iz = 1, nzf(kf) - 1
         do ix = 1, nxf(kf) - 1
         if ( phi_free(ix, iz, kf) ) then
         
          dXp = abs ( phi(ix+1, iz, kf) - phi(ix, iz, kf) )
          dX =  xwnode( 1, ix+1, iz, kf ) - xwnode( 1, ix, iz, kf )
          
          dWp = abs ( phi(ix, iz+1, kf) - phi(ix, iz, kf) )
          dW =  xwnode( 2, ix, iz+1, kf ) - xwnode( 2, ix, iz, kf )

c* dphi is change in phi per km
          dWphi = 0.0d0
          dXphi = 0.0d0
          if ( dX.gt.zero ) dXphi = dXp/dX
          if ( dW.gt.zero ) dWphi = dWp/dW 

         pentype(np)=pentype(np)+abs(dXphi) * smX/rn          
         pentype(np)=pentype(np)+abs(dWphi) * smW/rn
         penF(kf,3) = penF(kf,3) + abs(dXphi)*smX/rn + abs(dWphi)*smW/rn
        endif
         enddo
        enddo
       endif

c damp spread of phi Penalty type 82
      if ( ksm.eq.2 .and. (smX.gt.zero .or. smW.gt.zero )       
     .          .and. rn.gt.zero ) then


c- get centroid of slip distribution
       sumxs = zero
       sumws = zero
       sums = zero
      
        do iz = 1, nzf(kf)  
         do ix = 1, nxf(kf)   
           s = phi(ix, iz, kf)
           x = xwnode( 1, ix, iz, kf )
           w = xwnode( 2, ix, iz, kf )
           sumxs = sumxs + x*s
           sums  = sums + s
           sumws = sumws + w*s
         enddo
        enddo
          if(sums.gt.zero) then
           xc = sumxs/sums
           wc = sumws/sums
          endif

c-- get variance of slip distribution
       sumxs = zero
       sumws = zero
      
        do iz = 1, nzf(kf)  
         do ix = 1, nxf(kf)   
           s = phi(ix, iz, kf)
           x = xwnode( 1, ix, iz, kf ) - xc
           w = xwnode( 2, ix, iz, kf ) - wc
           sumxs = sumxs + x*x*s*s*smX
           sumws = sumws + w*w*s*s*smW
c          endif
         enddo
        enddo
        
       pentype(np) = pentype(np) + (sumxs + sumws)/rn
       penF(kf,5) = penF(kf,5) + (sumxs + sumws)/rn

      endif

c*** apply LaPlacian smoothing Interseismic, Penalty type 83

      if ( ksm.eq.3 .and. (smX.gt.zero .or. smW.gt.zero) 
     .     .and. rn.gt.zero ) then

        do iz = 1, nzf(kf) - 2
         do ix = 1, nxf(kf) - 2

          d2x2=0.0d0
           x1 = xwnode( 1, ix,   iz, kf )
           x2 = xwnode( 1, ix+1, iz, kf )
           x3 = xwnode( 1, ix+2, iz, kf )
           p1 = phi(ix, iz, kf)
           p2 = phi(ix+1, iz, kf)
           p3 = phi(ix+2, iz, kf)
           d2x2 = ((p3-p2)/(x3-x2)-(p2-p1)/(x2-x1))/((x3-x1)/two)

          d2w2=0.0d0
           w1 = xwnode( 2, ix, iz, kf )
           w2 = xwnode( 2, ix, iz+1, kf )
           w3 = xwnode( 2, ix, iz+2, kf )
           p1 = phi(ix, iz, kf)
           p2 = phi(ix, iz+1, kf)
           p3 = phi(ix, iz+2, kf)
           d2w2 = ((p3-p2)/(w3-w2)-(p2-p1)/(w2-w1))/((w3-w1)/two)

           pens  = smX*d2x2**2 + smW*d2w2**2
           pentype(np) = pentype(np) + pens/rn
           penF(kf,6) = penF(kf,6) + pens/rn
         enddo
        enddo
        
       endif
       endif
      enddo


c**********************************************************
c*** apply transient parameter constraints 
c**********************************************************
      do nt = 1, MAX_srce
       if ( sflag(nt) .and. srce_inv(nt) ) then
       
        kf = info_source(nt,1)
        kq = info_source(nt,2)
        kt = info_source(nt,4)
        ntau = info_source(nt,7)
        zp = 1.0d0

c-------------------------------------------------------------
c* check that source lon/lat is on fault, penalty EE56
c-------------------------------------------------------------
      check_fault = .true.
      if (check_fault) then
         pf = 10.0d0
         npt = 100*nt + 56
        
c 8-24-16        if ( (kf.ge.6 .and. kq.le.8) .and.
        if ( (kf.ge.1 .and. kq.le.8) .and.
     .     (ntransient(nt,1).eq.1 .or. ntransient(nt,2).eq.1) ) then
     
          x0 = transient(nt,1)
          y0 = transient(nt,2)
          call ll2xw (kf, x0, y0, x, w)
         
c-- get fault polygon       
        npf = nfault_poly(kf)
       do i=1,npf
        pfX(i) = fault_poly(i,1,kf)
        pfY(i) = fault_poly(i,2,kf)
       enddo
             
         call inside ( x, w, pfX, pfY, npf, insde) 
         if(insde.eq.0) pentype(npt) = pf
            
        endif

      endif


c-------------------------------------------------------------
c transient parameters       EE21 to EE34
c-------------------------------------------------------------
        do ip=1,14
         nsp = nsource_parm (nt,ip)
         if ( nsp .gt.0 ) then
           kpt = kparm_type(nsp)
           kpt = 20+ip
           zp = tpen(ip)
           call gettparm(2, c2, kpt, ip)
           np = 100*nt+kpt
           dt = tminmax(nt,ip,1) - transient(nt,ip) 
           if ( dt.gt.zero) pentype(np) = pentype(np)+dt*zp
           dt = transient(nt,ip) - tminmax(nt,ip,2)  
           if ( dt.gt.zero) pentype(np) = pentype(np)+dt*zp
         endif
        enddo 

c-------------------------------------------------------------
c-- Spheroid dip and A/B
c-------------------------------------------------------------
      if (kq.eq.12) then
c A/B > 1.0
        if ( nsource_parm(nt,6).gt.0 ) then
         np=100*nt+26
         if (transient(nt,6).lt.1.0d0) 
     .    pentype(np)=pentype(np)+abs(transient(nt,6)-1.0d0)*10.0d0
        endif
c dip 1.0 to 89.9
        if ( nsource_parm(nt,12).gt.0 ) then
         np=100*nt+32
         if (transient(nt,12).gt. 89.9d0) 
     .    pentype(np)=pentype(np)+abs(transient(nt,12)-89.9d0)*1.0d0
         if (transient(nt,12).lt. 1.0d0) 
     .    pentype(np)=pentype(np)+abs(transient(nt,12)-1.0d0)*1.0d0
        endif

      endif

c-------------------------------------------------------------
c-- VE Mogi, R2 > R1
c-------------------------------------------------------------
      if (kq.eq.10 .and. kt.eq.8 ) then
       if ( nsource_parm(nt,4).gt.0 .or. nsource_parm(nt,6).gt.0) then
        np=100*nt+26
        dr = transient(nt,6) - transient(nt,4)
        if (dr.lt.0.001d0 ) 
     .    pentype(np)=pentype(np) + abs(dr*10.0d0)
       endif
      endif

c-------------------------------------------------------------
c-- make sure planar fault does not extend above surface, EE51
c-------------------------------------------------------------
      if (kq.eq.9 .or. kq.eq.11) then
        nsp=0
        if ( nsource_parm(nt, 3).gt.0 ) nsp=51
        if ( nsource_parm(nt, 4).gt.0 ) nsp=51
        if ( nsource_parm(nt,12).gt.0 ) nsp=51
        if (  nsp.gt.0 ) then
          dz = transient(nt,4)/two*dsin(transient(nt,12)*d2r) - 
     .            transient(nt,3)
          np=100*nt+nsp
          if(dz.gt.zero) pentype (np) = pentype(np)+dz*zp
        endif
      endif

c-------------------------------------------------------------
c-- 1D Boxcar parameters EE40 to EE42
c-- nppt() holds parameter numbers
c-------------------------------------------------------------
       if (kq.eq.3) then
        do ip=20,22
          ipp = ip-19 
         do ix = 1, nxf(kf)
          nsp = nppt(ix, ipp, nt) 
          if ( nsp .gt.0 ) then
           kpt = ip+20
           np = 100*nt+kpt
           dt = tminmax(nt,ip,1) - tf_parm(nt,ix,ipp)  
           if ( dt.gt.zero) pentype (np) = pentype(np)+dt*zp
           dt = tf_parm(nt,ix,ipp)  - tminmax(nt,ip,2)  
           if ( dt.gt.zero) pentype (np) = pentype(np)+dt*zp
          endif
         enddo
        enddo 

c-------------------------------------------------------------
c* Zmax must be greater than Zmin; Penalty type EE97
c-------------------------------------------------------------
       np = 100*nt+97
       do ix = 1, nxf(kf)
        nsp1 = nppt(ix, 2, nt) 
        nsp2 = nppt(ix, 3, nt) 
        if ( nsp1.gt.0 .or. nsp2.gt.0 ) then
         dz = tf_parm(nt,ix,2) - tf_parm(nt,ix,3)
         if (dz.gt.0.0d0 ) pentype(np)=pentype(np)+abs(dz)*1.0d0
        endif
       enddo

      endif

c-------------------------------------------------------------
c-- 1D Gauss parameters
c-- nppt() holds parameter numbers
c-------------------------------------------------------------
       if (kq.eq.4) then
        do ip=17,19
          ipp = ip-16 
         do ix = 1, nxf(kf)
          nsp = nppt(ix, ipp, nt) 
          if ( nsp .gt.0 ) then
           kpt = ip+20
           np = 100*nt+kpt
           dt = tminmax(nt,ip,1) - tf_parm(nt,ix,ipp)  
           if ( dt.gt.zero) pentype (np) = pentype(np)+dt*zp
           dt = tf_parm(nt,ix,ipp)  - tminmax(nt,ip,2)  
           if ( dt.gt.zero) pentype (np) = pentype(np)+dt*zp
          endif
         enddo
        enddo 
       endif

c-------------------------------------------------------------
c-- penalty on tau amplitudes PE = EE36
c-------------------------------------------------------------
       if ((kt.eq.2 .or. kt.eq.5) .and. ntau.gt.0) then
c         sumt = 0.0d0
           kpt = 36
           ip = 16
           np = 100*nt+kpt
           taupen = 1.0d-7
         do j=1,ntau
c           sumt = sumt+abs(atau(nt,j))
c-- penalty outside bounds
           dt = tminmax(nt,ip,1) - atau(nt,j) 
           if ( dt.gt.zero) pentype(np) = pentype(np)+dt*1.0d0
           dt = atau(nt,j) - tminmax(nt,ip,2)  
           if ( dt.gt.zero) pentype(np) = pentype(np)+dt*1.0d0
           pentype(np) = pentype(np) + abs(atau(nt,j))*taupen
         enddo
c damp total tau
c            pentype(np) = pentype(np) + sumt/real(ntau)*1.0d-3
          
c-- tau smoothing by variance PE = EE54
c-- damps the wiggles in the STF
        tsm = tau_smooth(nt) 
        if ( tsm.gt.zero ) then
          dt = dtau(nt)/dpy
          d2 = 0.0d0
          np = 100*nt+54
         do j=2,ntau
           d2 = d2 + (( atau(nt,j) - atau(nt,j-1) )/dt)**2
         enddo
           pentype(np) = pentype(np) + tsm*dsqrt(d2)/real(ntau) 
        endif
       endif
          
c-------------------------------------------------------------
c** transient moment, penalty EE50      
c-------------------------------------------------------------
         if ( mmparm(nt,30) ) then
          ip=30
          np = 100*nt + 50
c          print *, 'tr_mom', tminmax(nt,ip,1), tminmax(nt,ip,2)
          call tr_moment(nt)
          call M0_penalty (nt, pp, dmin)
c          pp = pp * 100.0d0
c          print *, 'penalty', tminmax(nt,ip,1), tminmax(nt,ip,2),pp
          pentype (np) = pentype(np) + pp
          penT(nt,1) = penT(nt,1) + pp
         endif
     
c-------------------------------------------------------------
c-- damp source polygons, penalty 100*nt+55
c-------------------------------------------------------------
       if(ntransient(nt,15).gt.0 .and. 
     .    kq.eq.8) then
     
        kf = info_source(nt,1)
        npl = info_source(nt,6)
        np = 100*nt+55
        d2 = zero
        pf = damp_poly(nt) 

        rpoly(nt,npl+1) = rpoly(nt,npl)
        
        do i=1,npl
         d2 = d2 + (rpoly(nt,i+1)-rpoly(nt,i))**2 
        enddo
        pentype(np)=pentype(np)+d2*pf        

c-- vertex should be on fault surface     
c-- get fault polygon       
        npf = nfault_poly(kf)
       do i=1,npf
        pfX(i) = fault_poly(i,1,kf)
        pfY(i) = fault_poly(i,2,kf)
       enddo
c-- build source polygon      
       call makepoly (nt, ppX, ppW, np1, 0)
       do i=1,np1
         call inside ( ppX(i), ppW(i), pfX, pfY, npf, insde) 
         if(insde.eq.0) pentype(np)= pentype(np) + 1.0d2
       enddo
       
       endif

c-------------------      
       endif
      enddo
     
c**********************************************************
c*** apply smoothing of TRANSIENTS 
c**********************************************************
      do nt = 1, MAX_srce

       if ( sflag(nt) .and. srce_inv(nt)) then

       kf = info_source(nt,1)
       kq = info_source(nt,2)
       ksm = int(trsmooth(nt,1))
       smX = trsmooth(nt,2) 
       smW = trsmooth(nt,3) 
       sm0 = trsmooth(nt,4) 
       rn = real(nxf(kf)*nzf(kf))

c UNDER CONSTRUCTION
c*** damp changes between 2 slip events, events have to be same type
c      if ( kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. kq.eq.6 .or. kq.eq.7 .or. 
c     .            kq.eq.8 ) then 
c       do k=1,10
c         ke1 = kdamp2(k,1)
c         ke2 = kdamp2(k,2)
c        if ( ke1.eq.nt .and. ke2 .ne. 0 ) then
c          kq2 = info_source(ke2,2)
c          if (kq2.eq.kq ) then
c          endif
c        endif
c       enddo
c      endif

c*** smoothing TRANSIENTS, damp amplitude; penalty EE60
      if ( sm0.gt.0.0d0) then

       np = 100*nt+60
       amp = fnTamp(nt)

        suma=0.0d0
        sm0=sm0/1.0d6

       if ( kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. 
     .      kq.eq.6 .or. kq.eq.7 .or. kq.eq.8) then

        do iz = 1, nzf(kf)  
         do ix = 1, nxf(kf)  
c          n1 = nodefree(ix, iz, nt)
c         if ( n1.ne.0 ) suma = suma + abs (tphi(ix, iz, nt))
          suma = suma + abs (tphi(ix, iz, nt))
         enddo
        enddo
        pens = abs(amp) * suma * sm0/rn
       endif

       if (kq.eq.9 .or. kq.eq.10 .or. kq.eq.11) pens = abs(amp)*sm0 

        pentype(np)=pentype(np)+pens
        penT(nt,4) = penT(nt,4) + pens
       
      endif

c*** apply along strike and down-dip smoothing TRANSIENTS, penalty EE61
c** damp gradient
      if ( (kq.eq.1 .or. kq.eq.3 .or. kq.eq.4) .and. ksm.eq.1) then

       np = 100*nt+61
       amp = fnTamp(nt)

       if ( smX.gt.zero .or. smW.gt.zero ) then

        do iz = 1, nzf(kf) - 1
         do ix = 1, nxf(kf) - 1
         
          n1 = nodefree(ix,   iz,   nt)
          n2 = nodefree(ix+1, iz,   nt)
          n3 = nodefree(ix,   iz+1, nt)
         if ( n1.ne.0 .and. n2.ne.0 .and. n3.ne.0 ) then
     
          dXp = abs ( tphi(ix+1, iz, nt) - tphi(ix, iz, nt) )
          dX =  xwnode( 1, ix+1, iz, kf ) - xwnode( 1, ix, iz, kf )
          
          dWp = abs ( tphi(ix, iz+1, nt) - tphi(ix, iz, nt) )
          dW =  xwnode( 2, ix, iz+1, kf ) - xwnode( 2, ix, iz, kf )

c* dphi is change in phi per km
          dWphi = 0.0d0
          dXphi = 0.0d0
          if ( dX.gt.zero ) dXphi = amp*dXp/dX
          if ( dW.gt.zero ) dWphi = amp*dWp/dW 

           pens = abs(dXphi) * smX
           pentype(np)=pentype(np)+pens/rn
          
           pens = abs(dWphi) * smW
           pentype(np)=pentype(np)+pens/rn
        
         endif
         enddo
        enddo
       endif
       endif

c*** apply spread smoothing TRANSIENTS, penalty EE62
c  damp spread of slip
      if ( (kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. kq.eq.8) .and. 
     .   ksm.eq.2 ) then

       np = 100*nt+62

c- get centroid of slip distribution
       sumxs = zero
       sumws = zero
       sums = zero
      
        do iz = 1, nzf(kf)  
         do ix = 1, nxf(kf)   
          n0 = nodefree(ix, iz, nt)
          if(n0.gt.0) then
           s = tphi(ix, iz, nt)
           x = xwnode( 1, ix, iz, kf )
           w = xwnode( 2, ix, iz, kf )
           sumxs = sumxs + x*s
           sums  = sums + s
           sumws = sumws + w*s
          endif
         enddo
        enddo
          if(sums.gt.zero) then
           xc = sumxs/sums
           wc = sumws/sums
          endif

c-- get variance of slip distribution
       sumxs = zero
       sumws = zero
      
        do iz = 1, nzf(kf)  
         do ix = 1, nxf(kf)   
          n0 = nodefree(ix, iz, nt)
          if(n0.gt.0) then
           s = tphi(ix, iz, nt)
           x = xwnode( 1, ix, iz, kf ) - xc
           w = xwnode( 2, ix, iz, kf ) - wc
           sumxs = sumxs + x*x*s*s*smX
           sumws = sumws + w*w*s*s*smW
          endif
         enddo
        enddo
        
       pens = (sumxs + sumws)/rn
       pentype(np) = pentype(np) + pens
        penT(nt,2) = penT(nt,2) + pens

      endif

c*** apply LaPlacian smoothing TRANSIENTS, penalty EE63
      if ( (kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. kq.eq.8) .and. 
     .         ksm.eq.3 ) then

       np = 100*nt+63       
       pens = 0.0d0
       
        do iz = 1, nzf(kf) - 2
         do ix = 1, nxf(kf) - 2
          n1 = nodefree(ix,   iz,   nt)
          n2 = nodefree(ix+1, iz,   nt)
          n3 = nodefree(ix+2, iz,   nt)

          d2x2=0.0d0
          if ( n1.ne.0 .or. n2.ne.0 .or. n3.ne.0) then
           x1 = xwnode( 1, ix,   iz, kf )
           x2 = xwnode( 1, ix+1, iz, kf )
           x3 = xwnode( 1, ix+2, iz, kf )
           p1 = tphi(ix, iz, nt)
           p2 = tphi(ix+1, iz, nt)
           p3 = tphi(ix+2, iz, nt)
           d2x2 = ((p3-p2)/(x3-x2)-(p2-p1)/(x2-x1))/((x3-x1)/two)
          endif

          n2 = nodefree(ix, iz+1,   nt)
          n3 = nodefree(ix, iz+2,   nt)
          d2w2=0.0d0
          if ( n1.ne.0 .or. n2.ne.0 .or. n3.ne.0) then
           w1 = xwnode( 2, ix, iz, kf )
           w2 = xwnode( 2, ix, iz+1, kf )
           w3 = xwnode( 2, ix, iz+2, kf )
           p1 = tphi(ix, iz, nt)
           p2 = tphi(ix, iz+1, nt)
           p3 = tphi(ix, iz+2, nt)
           d2w2 = ((p3-p2)/(w3-w2)-(p2-p1)/(w2-w1))/((w3-w1)/two)
          endif

           pens  = pens  + (smX*d2x2**2 + smW*d2w2**2)/rn
         
         enddo
        enddo
        
       pentype(np) = pentype(np) + pens
        penT(nt,6) = penT(nt,6) + pens

      endif

      endif
      enddo


c************************************************************
c*** hard constraints; Penalty type 96
c************************************************************

      do i=1, num_hc

      if (hc_flag(i) ) then

       hcp = hc_pen_fac

c* slip rate hc
      if (hc(i).eq.1) then

       call relvel (3, hc_block(i,1), hc_block(i,2), hc_pos(i,1), 
     .   hc_pos(i,2), Vx, Sx, Vy, Sy, rho)

       calc = dsqrt( Vx*Vx + Vy*Vy)

c at azimuth
c       if (hc_val(i,3) .ne. zero ) then
c get azimuth
c       endif


c* slip azimuth hc
      else if (hc(i).eq.2) then

       call relvel (3, hc_block(i,1), hc_block(i,2), hc_pos(i,1), 
     .   hc_pos(i,2), Vx, Sx, Vy, Sy, rho)
       call svaz(Vx, Vy, calc)
       obs = (hc_val(i,1) + hc_val(i,2))/2.0d0
       a360=360.0d0
       sig = 1.0d0
       call svres (calc, obs, sig, r, rs, a360)
       hcp = hc_pen_fac/10.0d0

c* rotations
      else if (hc(i).eq.3) then
       kmv = hc_block(i,1)
       kfx = hc_block(i,2)
       xpt = hc_pos(i,1)
       ypt = hc_pos(i,2)
       nmv = npole_block(kmv)
       nfx = npole_block(kfx)

       do k=1,3
        p(k) = poles(nmv,k) - poles(nfx,k)
       enddo
       call vertaxrot (xpt, ypt, p, calc)

c** end of rot


      endif
     
      pens = 0.0d0

      if ( calc.lt.hc_val(i,1))  then 
        pens = (hc_val(i,1) - calc ) * hcp
        pentype(96)=pentype(96)+pens
      endif

      if ( calc.gt.hc_val(i,2))  then 
        pens = (calc - hc_val(i,2) ) * hcp
        pentype(96)=pentype(96)+pens
      endif

      hc_pen(i) =  pens

      endif

      enddo

c-------------------------------------------------------------
c--  all non-transient parameters
      pf = 100.0d0
      do kn=1, nparms
       kpt = kparm_type(kn)
       
        if ( kpt .le. 19  ) then
         pkn = parm(kn)
         pens = 0.0d0
         pf = pfac(kpt)
         if (pkn.lt.psmin(kpt)) pens = abs(psmin(kpt)-pkn)*pf
         if (pkn.gt.psmax(kpt)) pens = abs(psmax(kpt)-pkn)*pf
         pentype(kpt)=pentype(kpt)+pens
        endif
      enddo

c****************************************
     
c sum penalties and find max penalty
      pensum = 0.0d0
      nmaxpen=0
      pmax = 0.0d0

      do i=1, MAX_pen
       pensum=pensum+pentype(i)
       if (pentype(i).gt.pmax) then
        pmax = pentype(i)
        nmaxpen = i
       endif
      enddo


c* writing out penalties to file
      if (wwpen) then

      do i=1,MAX_pen
       pname(i) = 'Misc'
      enddo
      pname(81) = 'Grad'
      pname(82) = 'Var '
      pname(83) = 'LaPl'
      pname(84) = 'Phi '
      pname(89) = 'Strn'
      pname(94) = 'DDc '
      pname(95) = 'Momt'
      pname(96) = 'HCon'
      pname(97) = 'Zmax'

       call fopen (k33, 1, '.penalty ')

       write(k33, *) 'Type  Penalty '
       do i=1, MAX_pen
        if (pentype(i).gt.zero) then
c         if (i.gt. 100) then
c           kpt = i - int(i/100)*100 
c           if (kpt.gt.0 .and. kpt.lt.41) pname(i) = p2codes(kpt)
c         endif
         write (k33, '(i5, 1pe12.4,1x,a4)' ) i, pentype(i), pname(i)
        endif
       enddo

c faults
       write(k33, *) 'Flt    Downdip      Z2 > Z1     Slip_Grad'//
     .    '   Slip_Total  Slip_Varnce    LaPlacian'
       do i=1, MAX_f
        sump = 0.0d0
        do j=1,6
          sump = sump+penF(i,j)
        enddo
        if (sump.gt.zero) then
         write (k33, '(i4, 6(1x,1pe12.4))' ) i, (penF(i,j),j=1,6)
        endif
       enddo

c sources
       write(k33, *) 'Srce   Moment      Z2 > Z1     Slip_Grad'//
     .    '   Slip_Total  Slip_Varnce    LaPlacian'
       do i=1, MAX_srce
        sump = 0.0d0
        do j=1,6
          sump = sump+penT(i,j)
        enddo
        if (sump.gt.zero) then
         write (k33, '(i4, 10(1x,1pe12.4))' ) i, (penT(i,j),j=1,10)
        endif
       enddo
        ik = kfclose(k33)
      endif

      return
      end
      
c******************************************************
c** parseGPS
c** get distances from GPS sites to nodes, don't make GFs for any that are too far away 
c**   (ie distance > toofar degrees) from any fault node
c** get locations of GPS sites that need GFs
c**   if 2 sites are within gps_near (in km) of each other, they share a GF
c************************************************************
      subroutine parseGPS

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      integer system, status
c      logical flock
      dimension nused(MAX_gps), nsite(MAX_gps, 100) 
      character fnout*80, dash*1
      character*80 fm1, fm2

      dash = '_'
      fm1 = '( 2f10.4, 200(i6) ) '

c- GPS
c - arrays:
c     num_gps     gps_pos()    GPS positions for all sites
c     num_gf_gps  ggf_pos()    GPS positions for unique positions
c     num_gf_gps2 ggf_pos2()   GPS positions to do GFs (near this node)

      if(num_gps.eq.0) return

      print *, 'Checking for redundant GPS locations'

c-- assign GF indices

      toofar = gps_too_far
      num_gf_gps = 0
      call cleareal(ggf_pos, 3*MAX_gps)
      ggf_pos(1,1) = 500.0d0

      do 1201 i=1, num_gps

      loc_gps(i) = 0
      
      if (gps_type(i).gt.0. or. do_all_gf ) then

       xg = gps_pos(i,1)
       yg = gps_pos(i,2)
       zg = gps_pos(i,3)

c* see if site is too far away from all nodes to need GFs
      do kf=1, nfault
       if ( useGF(kf) ) then
        do iz=1, nzf(kf)
         do ix=1, nxf(kf)
          xf= xynode(1,ix,iz,kf)
          yf= xynode(2,ix,iz,kf)
          if (iclose(xg, yg, xf, yf, toofar).eq.1) goto 1200
         enddo
        enddo
       endif
       enddo

       goto 1201

c* see if it is near enough to another site to use same GF
 1200 do j=1,num_gf_gps
      if(iclose(xg, yg, ggf_pos(j,1),ggf_pos(j,2),gps_near).eq.1) then
         loc_gps(i) = j
         nused(j)=nused(j)+1
         nsite(j, nused(j)) = i
       endif
      enddo

c* use unique GF
      if (loc_gps(i).eq.0) then
       num_gf_gps = num_gf_gps + 1
       ggf_pos(num_gf_gps,1) = xg 
       ggf_pos(num_gf_gps,2) = yg
       ggf_pos(num_gf_gps,3) = zero
       if(zg.ne.zero) ggf_pos(num_gf_gps,3) = zg
       loc_gps(i) = num_gf_gps
       nused(num_gf_gps) = 1
       nsite(num_gf_gps,1) = i
      endif

      endif
 1201 continue

      write(*, '("Number of original GPS locations ",i4)') num_gps
      write(*, '("Number of unique GPS locations   ",i4)') num_gf_gps
c* sort the sites by longitude for faster reading of GFs back in
c*  write tmp file, sort with Unix sort, then read back in
       call fopen (k33, 1, '_gf1_gps.tmp ')
       do i=1,num_gf_gps
c        write (fm1, '( "(2f10.4, ",i4,"(i6))" )'  ) nused(i)+2
        write (k33, fm1 ) (ggf_pos(i,j),j=1,2), i, 
     .   nused(i), (nsite(i, j), j=1,nused(i))
       enddo
       ik=kfclose(k33)

c-- write list of sites that use each GF
       call fopen (k33, 1, '_gf3_gps.tmp ')
       do i=1,num_gf_gps
        write (fm2, '( "(2f10.4, 2i5",i4,"(1x,a4,a1,a4))" )'  ) nused(i)
        write (k33, fm2 ) 
     .    (ggf_pos(i,j),j=1,2), i, nused(i), 
     .    (gps_name(nsite(i, j)),dash, 
     .     gps_fname(gps_index(nsite(i,j))), 
     .      j=1,nused(i))
       enddo
       ik = kfclose(k33)

c* sort the file gf1_gps.tmp and put in gf2_gps.tmp
       status = system('sort -n '//fnout('_gf1_gps.tmp ')//' >  '
     .    //fnout('_gf2_gps.tmp ')//char(0) )

       call fopen (k33, 1, '_gf2_gps.tmp ')
       do i=1,num_gf_gps
c        write (fm1, '( "(2f10.4, ",i4,"(i6))" )'  ) nused(i)+2
        read (k33, fm1 ) (ggf_pos(i,j),j=1,2), k, n, 
     .    (nused(j), j=1,n )
         do j=1,n
          loc_gps(nused(j))= i
         enddo
        enddo
       ik = kfclose(k33)

      return
      end

      
c******************************************************
c** parseINSAR - see notes for parseGPS
c************************************************************
      subroutine parseINSAR

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      integer system, status
c      logical flock
      dimension nused(MAX_insar_pts), nsite(MAX_insar_pts, 100) 
      character*80 fnout, fm1, fm2

c- InSAR
c - arrays:
c     num_insar   insar_pos()  InSAR positions for all sites
c     num_gf_ins  igf_pos()    InSAR positions for unique positions
c     num_gf_ins2 igf_pos2()   InSAR positions to do GFs (near this node)

      if(num_insar.eq.0) return
      print *, 'Checking for redundant InSAR locations'

c-- assign GF indices

      toofar = gps_too_far
      num_gf_ins = 0
      call cleareal(igf_pos, 3*MAX_insar_pts)
      igf_pos(1,1) = 500.0d0
      fm1 = '(2f10.4, 100i6)'

      do 1201 i=1, num_insar

      loc_ins(i) = 0
      
       xg = insar_pos(i,1)
       yg = insar_pos(i,2)
       zg = insar_pos(i,3)
c       print *, xg, yg

c* see if site is too far away from all nodes to need GFs
      do kf=1, nfault
       if ( useGF(kf) ) then
        do iz=1, nzf(kf)
         do ix=1, nxf(kf)
          xf= xynode(1,ix,iz,kf)
          yf= xynode(2,ix,iz,kf)
          if (iclose(xg, yg, xf, yf, toofar).eq.1) goto 1200
         enddo
        enddo
       endif
       enddo

       goto 1201

c* see if it is near enough to another point to use same GF
 1200 if ( num_gf_ins.gt.0 ) then
       do j=1,num_gf_ins
        if(iclose(xg, yg, igf_pos(j,1),igf_pos(j,2),gps_near).eq.1) then
         loc_ins(i) = j
         nused(j)=nused(j)+1
         nsite(j, nused(j)) = i
        endif
       enddo
      endif

c* use unique GF
      if (loc_ins(i).eq.0) then
       num_gf_ins = num_gf_ins + 1
       igf_pos(num_gf_ins,1) = xg 
       igf_pos(num_gf_ins,2) = yg
       igf_pos(num_gf_ins,3) = zg
       loc_ins(i) = num_gf_ins
       nused(num_gf_ins) = 1
       nsite(num_gf_ins,1) = i
      endif

 1201 continue

      write(*, '("Number of original InSAR locations ",i5)') num_insar
      write(*, '("Number of unique InSAR locations   ",i5)') num_gf_ins

c* sort the sites by longitude for faster reading of GFs back in
c*  write tmp file, sort with Unix sort, then read back in
       call fopen (k33, 1, '_gf1_ins.tmp ')
       do i=1,num_gf_ins
        write (fm2, '( "(2f10.4, ",i4,"(i6))" )'  ) nused(i)+2
        write (k33, fm2 ) (igf_pos(i,j),j=1,2), i, 
     .   nused(i), (nsite(i, j), j=1,nused(i))
       enddo
       ik=kfclose(k33)

c* sort the file gf1_ins.tmp and put in gf2_ins.tmp
       status = system('sort -n '//fnout('_gf1_ins.tmp ')//' >  '
     .    //fnout('_gf2_ins.tmp ')//char(0) )

       call fopen (k33, 1, '_gf2_ins.tmp ')
       do i=1,num_gf_ins
c        write (fm1, '( "(2f10.4, ",i4,"(i6))" )'  ) nused(i)+2
        read (k33, fm1 ) (igf_pos(i,j),j=1,2), k, n, 
     .    (nused(j), j=1,n )
         do j=1,n
          loc_ins(nused(j))= i
         enddo
        enddo
       ik = kfclose(k33)

      return
      end

      
c****************************************
      subroutine findiffders (ls_inv, padjust, ndat)
      
c'-- finite difference derivatives and uncertainties

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

      character*2 dtype(ndat), tstype(3), dstype(3)
      character fmt1*80
      logical sing, sing1, sing3, no, write_sumder, useENU
      logical usegp, usesv, usesr

      dimension vtmp(ndat,2), cov(ndat), der(ndat,nparms), 
     .  wa(ndat,nparms), cov2(nparms,nparms), bb(nparms,1)

      dimension sumder(ndat)

      dimension par2(nparms), pp(nparms), cpp(nparms,nparms)
      
c      dimension V(nparms,nparms), w(nparms), 
c     .  cov3(nparms,nparms),

      dimension kblk(ndat)
      dimension dp(40)

c-- inversion arrays
      logical  ls_inv, useGPS 
      dimension B6(nparms,1),   resp(ndat,1)
      dimension Wtp(ndat,ndat), ppp(nparms,1)
      dimension AtWp(nparms,ndat),AtWAp(nparms,nparms),AtWOp(nparms,1)  
      dimension padjust(MAX_parms)

      data tstype / 'tE', 'tN', 'tU' /
      data dstype / 'dE', 'dN', 'dU' /

c-- finite diff steps for various parameter types
      data dp / 1.0d-4, 1.0d-3, 1.0d-3, 1.0d-3, 1.0d-2, 
     .          3.0d0,  3.0d0,  1.0d-2, 1.0d-1, 1.0d-1,
     .          1.0d-1, 1.0d-1, 1.0d-1, 1.0d-1, 1.0d-3,
     .          1.0d-1, 1.0d-1, 1.0d-1, 1.0d-1, 2.0d0,
     .          1.0d-2, 1.0d-2, 1.0d-1, 1.0d0,  1.0d-1,
     .          1.0d0,  1.0d-1, 1.0d-1, 1.0d-1, 1.0d-1,
     .          1.0d-1, 1.0d-1, 1.0d-1, 1.0d-1, 1.0d-1,
     .          1.0d-1, 1.0d-1, 2.0d1,  1.0d-1, 1.0d-1/

      no = .false.
      apply_penalty = .false.
      write_sumder = .false.
      usegp = ( .not. no_wt_gps )
      usesv = ( .not. no_wt_svs )
      usesr = ( .not. no_wt_srs )
      a360 = 360.0d0
      
      call cleareal(resp, ndat)
      call cleareal(parm_err, nparms)
      call cleareal(vtmp, 2*ndat)
      call cleareal(cov, ndat)
      call cleareal(der, ndat*nparms)
      call cleareal(wa,  ndat*nparms)
      call cleareal(cov2, nparms*nparms)
      call cleareal(cpp, nparms*nparms)
      call cleareal(bb, nparms)
      call cleareal(sumder, ndat)

c-- save the current parameters to par2(), and use temp parameter array pp()
      do i=1,nparms
       pp(i)=parm(i)
       par2(i)=parm(i)
      enddo
      
      print *, 'Getting derivatives'
      print *, 'Number of parameters = ', nparms
      print *, 'Number of data = ', ndat
c      print *, 'jpar,j,kk,kpt,jeqn,U,o,vtmp,resp,cov'
c        write (*, *)  ' PARAMETER    ERROR --- before '
c        do i=1,nparms
c          write (*, '(2f15.6)') parm(i), parm_err(i)
c        enddo

c-- loop through the parameters, getting finite difference derivatives
c      print *, 'Parms'
      do jpar = 1, nparms
       kpt = kparm_type(jpar)
c       print *, kpt, pp(jpar), dp(kpt),psmin(kpt),psmax(kpt)
      enddo

      do 10 jpar = 1, nparms
    
       kpt = kparm_type(jpar)
       onedp = dp(kpt)
       twodp=2.0d0*onedp
      
c  loop through thrice, setting parameter onedp smaller and then onedp larger
c     then get the difference in calculated values
c  3rd loop through is to get residuals (resp) at current model
c  some resp are set to 0 as place holders

      pp1 = pp(jpar)-onedp
      pp2 = pp(jpar)+onedp
      pp3 = pp(jpar)

      if ( pp1 .le. psmin(kpt)) then
        pp1 = psmin(kpt)
        pp2 = pp1+twodp
      endif

      if ( pp2 .ge. psmax(kpt)) then
        pp2 = psmax(kpt)
        pp1 = pp2-twodp
      endif

      do 15 kk=1, 3

      if (kk.eq.1) pp(jpar)=pp1
      if (kk.eq.2) pp(jpar)=pp2
      if (kk.eq.3) pp(jpar)=pp3

      call solve2(pp, xx, dchi2, pensum,mp)

      jeqn=0
      
       do k = 1, num_tilts
          jeqn= jeqn+1 
          if(kk.eq.3) resp(jeqn,1) = 0.0
          if(kk.lt.3) vtmp(jeqn,kk)=tilt_calc(k)*1.0d9
          cov(jeqn)= fnwt(tilt_sig(k))
          dtype(jeqn)='tl'
          kblk(jeqn) = 0
       enddo
      
       do k = 1, num_sv
         if (usesv) then
          jeqn= jeqn+1
          sig=1.0d0
          call svres (sv_calc(k), sv_obs(k), sig, r, rs, a360)
          if(kk.eq.3) resp(jeqn,1) = r
          if(kk.lt.3) vtmp(jeqn,kk)=sv_calc(k)
          cov(jeqn)= fnwt(sv_sig(k))
          dtype(jeqn)='sv'
          kblk(jeqn) = 0
         endif
       enddo
      
       do k = 1, num_sr
         if (usesr) then
          jeqn= jeqn+1
        if(kk.eq.3) then 
          call getsr (k, Vx, Vy, Vtot, Vaz, Vz, r, rs)
          resp(jeqn,1) = r
        endif
        if(kk.lt.3) vtmp(jeqn,kk)=sr_calc(k)
          cov(jeqn)= fnwt(sr_sig(k))
          dtype(jeqn)='sr'
          kblk(jeqn) = 0
        endif
       enddo
      
        do i=1, num_ss
         do k=1, 3
          jeqn= jeqn+1
          if(kk.eq.3) resp(jeqn,1) = 0.0
          if(kk.lt.3) vtmp(jeqn,kk) = ss_calc(i,k)
          cov(jeqn)= fnwt(ss_sig(i,k))
          dtype(jeqn)='ss'
          kblk(jeqn) = 0
         enddo
        enddo

        do k = 1, num_insar
          jeqn= jeqn+1
          if(kk.eq.3) resp(jeqn,1) = 0.0
          if(kk.lt.3) vtmp(jeqn,kk)=insar_los(k)
          cov(jeqn)= fnwt(insar_obs(k,2))
          dtype(jeqn)='in'
          kblk(jeqn) = 0
        enddo
        
      do i = 1, num_gps

       call gpsinfo2(i, x, y, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)

       if (useGPS(i) .and. usegp ) then

        if (kpt.eq.1 ) then
         xcalc=-gps_net(i,1)
         ycalc=-gps_net(i,2)
         zcalc= 0.0d0
        endif
        
c*** use GPS vector    
      if (gps_type(i).eq.1 ) then
        
         if (useENU(i,1)) then
          jeqn= jeqn+1
          if(kk.eq.3) resp(jeqn,1) = xobs-xcalc
          if(kk.lt.3) vtmp(jeqn,kk) = xcalc
          cov(jeqn)= fnwt(sigx)
          dtype(jeqn)='gE'
          kblk(jeqn)=nblock_gps(i)
         endif
          
         if (useENU(i,2)) then
          jeqn= jeqn+1
          if(kk.eq.3) resp(jeqn,1) = yobs-ycalc
          if(kk.lt.3) vtmp(jeqn,kk) = ycalc
          cov(jeqn)= fnwt(sigy)
          dtype(jeqn)='gN'
          kblk(jeqn)=nblock_gps(i)
         endif
          
         if (useENU(i,3)) then
          jeqn= jeqn+1
          if(kk.eq.3) resp(jeqn,1) = zobs - zcalc
          if(kk.lt.3) vtmp(jeqn,kk) = zcalc
          cov(jeqn)= fnwt(sigz)
          dtype(jeqn)='gU'
          kblk(jeqn)=nblock_gps(i)
         endif
         
      endif
      
c*** use displacements, not working    
      if (gps_type(i).eq.2 ) then

       ksyn=0     
       call ts_calc (0, i, ksyn)
         do j=1,3
          if( useENU(i,j) ) then
           jeqn= jeqn+1
           U = gps_calc(i, 2, j) - gps_calc(i, 1, j)
           o = gps_obs(i,j)
           xs = gps_obs(i,j+2)
            if (j.eq.3) then
             o  = gps_obs(i,6)
             xs = gps_obs(i,7)
            endif
           if(kk.lt.3) vtmp(jeqn,kk) = U
           if(kk.eq.3) resp(jeqn,1) =  o - U
           cov(jeqn)= fnwt(xs)
           dtype(jeqn)=dstype(j)
           kblk(jeqn)=nblock_gps(i)
c           print *, gps_name(i),jpar,i,j,kk,kpt,jeqn,U,o,
c     .            vtmp(jeqn,kk),resp(jeqn,1),cov(jeqn)
          endif
         enddo
     
      endif

c*** use time series displacement data 
      if (gps_type(i).eq.3 ) then
       ksyn=0     
       call ts_calc (0, i, ksyn)
       
       n1 = ndx(i,1)
       n2 = ndx(i,2)
       
       do n = n1, n2
        do j=1,3
         if( useENU(i,j) ) then
          jeqn= jeqn+1
          if(kk.eq.3) resp(jeqn,1) = 0.0
          if(kk.lt.3) vtmp(jeqn,kk) = x_calc(n,j)
          cov(jeqn)= fnwt(x_sig(n,j))
          dtype(jeqn)=tstype(j)
          kblk(jeqn)=nblock_gps(i)
         endif
        enddo
       enddo
      
      endif

       endif
      enddo
      
 15   continue 

c      dermin = 1.0d20
      do i=1, jeqn
        der(i,jpar) = (vtmp(i,2)-vtmp(i,1))/twodp 
c        if (der(i,jpar).eq.0.0d0 ) der(i,jpar) = dermin
        sumder(i) = sumder(i) + der(i,jpar)*cov(i)
c        print *, i,jpar,der(i,jpar)
      enddo
      
c      endif
      
 10   continue

c write .vtmp
c       call fopen (ks,  1, '.vtmp ' )
c      do jpar = 1,nparms
c       do i=1, jeqn
c       write(ks, '(2i5, 1x,a2,1x, 4f12.4)') 
c     .  jpar, i, dtype(i), vtmp(i,1), vtmp(i,2), der(i,jpar), resp(i,1)
c       enddo
c      enddo
c       ik = kfclose (ks)

c-- reset parameters
      do 17 i=1,nparms
  17    parm(i)=par2(i)

      apply_penalty = .true.
      call solve2(par2, x, dchi2, pensum,mp)

      print *, 'Number of weighted observations = ', jeqn

c*********************************

      if (write_sumder) then
       call fopen (ks,  1, '.wtder ' )

       write(ks, *) 'Derivatives * wt'
       do i=1,jeqn
        write (ks, '(a2,i3,d14.6)') dtype(i), kblk(i), sumder(i)
       enddo
       ik = kfclose (ks)
      endif

      if (write_der) then
       call fopen (ks, 1,  '.der ' )

        write (fmt1, '( "(a2, i3, ", I4, "(d14.6))" )' )  nparms

       write(ks, *) 'Derivatives'
       do i=1,jeqn
        write (ks, fmt1 ) dtype(i), kblk(i), 
     .    (der(i,j), j=1,nparms)
       enddo
       ik = kfclose (ks)
      endif

      print *, 'Checking derivatives '
c  check derivatives for zero column
      sing = .false.
      do j=1, nparms
       sing1 = .true.
       do i=1,jeqn
         if ( der(i,j) .ne. 0.0d0) sing1 =.false.
       enddo
       if (sing1) then
         write(*,'("Parameter #",i4," has all zero derivatives")') j
         sing = .true.
       endif
      enddo

      if (sing) print *, '*** Check .prm file for parameter numbers'

c get uncertainties 
c der = matrix of derivatives

      if ( .not. ls_inv .and. .not. sing) then

       print *, 'Building matrices; size =',ndat*nparms

       call rowscl(der,cov,wa,ndat,nparms,ione)
       call atransb(der,wa,cov2,ndat,nparms,nparms)

c****************************
c* build the a priori parameter covariance
c* experimental at present

      if (parm_cov) then
       print *, 'Applying parameter covariance'

      do i=1, nparms
       do j=1, nparms

        if ( i.eq.j ) then 
          cpp(i,j) = 1.0d5
          if ( kparm_type(i).eq.4 ) cpp(i,j) = 1.0d0
        endif

       enddo
      enddo

      n=1
      call gaussj( cpp, nparms, nparms, bb, n, n)

       do i=1,nparms
        do j=1,nparms
         cov2(i,j) = cov2(i,j) + cpp(i,j)
        enddo
       enddo

      endif
      
c****************************
c invert matrix with G-J
      print *, 'Inverting matrix '
       n=1
       call gaussj( cov2, nparms, nparms, bb, n, n)

c-- assign uncertainties to parameters and reset parm()
       do i=1,nparms
        parm_err(i) = dsqrt(cov2(i,i))
        parm(i) =  par2(i)
       enddo

      if (write_cov) then
       call fopen (ks, 1,  '.cov ' )
        write (fmt1, '( "(",I4, "(d14.6))" )' )  nparms

        write (ks, *) 'Covariance'
        do j=1, nparms
          write (ks, fmt1 ) (cov2(j,k), k=1,nparms)
        enddo

        write (ks, *)  ' PARAMETER    ERROR'
        do i=1,nparms
          write (ks, '(2f12.4)') parm(i), parm_err(i)
        enddo

       ik = kfclose (ks)

      endif
      
    

c**********************************************************************
c get pole/strain uncertainties and put in arrays

      np=0

      do k=1,num_gps_poles
       if (gps_invert(k) .and. get_gps_parm ) then
        l1=np+1
        l2=np+2
        l3=np+3
        gps_pole(k,4)= cov2(l1,l1)
        gps_pole(k,5)= cov2(l2,l2)
        gps_pole(k,6)= cov2(l3,l3)
        gps_pole(k,7)= cov2(l1,l2)
        gps_pole(k,8)= cov2(l1,l3)
        gps_pole(k,9)= cov2(l2,l3)
        np=np+3
       endif
      enddo

       do kjj = 1, num_pole_invert
        njj=npole_invert(kjj)

        l1 = 3*(kjj-1) + 1 + np
        l2 = 3*(kjj-1) + 2 + np
        l3 = 3*(kjj-1) + 3 + np

        poles(njj,4) = cov2(l1,l1)
        poles(njj,5) = cov2(l2,l2)
        poles(njj,6) = cov2(l3,l3)
        poles(njj,7) = cov2(l1,l2)
        poles(njj,8) = cov2(l1,l3)
        poles(njj,9) = cov2(l2,l3)

       enddo

       do kjj = 1, num_strain_invert
        njj = nstrain_invert(kjj)
        np =  first_strain_parm

        l1 = 3*(kjj-1) + np 
        l2 = 3*(kjj-1) + np + 1
        l3 = 3*(kjj-1) + np + 2 

        strain(njj,4) = dsqrt(cov2(l1,l1))
        strain(njj,5) = dsqrt(cov2(l2,l2))
        strain(njj,6) = dsqrt(cov2(l3,l3))
        strain(njj,7) = cov2(l1,l2)
        strain(njj,8) = cov2(l1,l3)
        strain(njj,9) = cov2(l2,l3)

       enddo

      endif

c--------------------------------------------------------------
c-- adjust parameters by least squares, put in padjust() array
c--------------------------------------------------------------
      if (ls_inv .and. .not. sing) then

        npar = nparms
        itwo = 2
        ione = 1
        sing3 = .false.

        print *, 'Inverting'
        call cleareal(padjust, MAX_parms)
        call cleareal(wtp, ndat*ndat)
        call cleareal(b6, npar)
        call cleareal(ppp, npar)
        call cleareal(AtWp, npar*ndat)
        call cleareal(AtWAp, npar*npar)
        call cleareal(AtWOp, npar)


      do i=1,ndat
        wtp(i,i) = cov(i)
      enddo

c-- make AtW
      call atransb(der, Wtp, AtWp, ndat, npar, ndat)

c-- make AtWA
      call amultb(AtWp, der, AtWAp, npar, ndat, npar)

c-- make AtWO
      call amultb(AtWp, resp, AtWOp, npar, ndat, ione)


c-- invert matrix
      call gaussj2(AtWAp, npar, npar, B6, ione, ione, sing3)
      if (sing3) then
        print *, 'Singular matrix'
        return
      endif

c -- get solution
      call amultb(AtWAp, AtWOp, ppp, npar, npar, ione)

      do k=1,nparms
c        print *, k, pp(k,1)
        padjust(k) = ppp(k,1)
      enddo

      endif

      return
      end
      
c**********************************************************************
      subroutine rowscl(a,b,c,nr1,nc1,iopt)
c
c...  subroutine to scale the rows of a by the vector b or its
c     reciprocal.  This is equivalent to pre-multiplying a matrix by a
c     diagonal matrix.
c
      implicit real*8 (a-h,o-z)
      dimension a(nr1,nc1),b(nr1),c(nr1,nc1)
      do j=1,nr1
        rm=b(j)
        if(iopt.eq.2) rm=1.0d0/b(j)
        do i=1,nc1
          c(j,i)=a(j,i)*rm
        enddo
      enddo
      return
      end

c**********************************************************************

      subroutine updatepoles 
c -- get poles from inversion and update gps_poles() and poles() arrays

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

      dimension pout(3)

      np=0

c update GPS file rotations
      do k=1, MAX_gps_files
         kindex = ngps_index(k)
c      do k=1,num_gps_poles
       if (gps_invert(k)) then
        do j=1,3
         gps_pole(k,j)=parm(np+j) 
        enddo
         np=np+3
       endif
           
c-- velocity bias
       if (gps_file_type(k).eq.1) then
         do j=1,3
          if ( gps_info(k,j+2).gt.0) then
           np=np+1
           ref_vel(kindex,j) = parm(np) 
          endif
         enddo
       endif

       if (gps_file_type(k).eq.3) then
         do j=1,3
          if ( gps_info(k,j+10).eq.4) then
           np=np+1
           ref_vel(kindex,j) = parm(np) 
          endif
        enddo
       endif

      enddo

c update block poles
      if (adjust_pole) then
        call fopen (kf8, 1, 'polefit.out ')
      endif

      do kjj = 1, num_pole_invert
       njj=npole_invert(kjj)
       npc = np+(kjj-1)*3

       if (adjust_pole) then
        call fitpole(njj, pout, nsite)
        print *, 'Adjusting pole ', njj,nsite,pout(1),pout(2),pout(3)
        do i=1,3
          parm(npc+i) = parm(npc+i) + pout(i)
        enddo
       endif

       do i=1,3
         poles(njj,i)=parm(npc+i)
       enddo

      enddo

      if (adjust_pole) ik = kfclose (kf8)
      
      return
      end
      
c**********************************************************************
      subroutine updatestrain
c -- get strain from inversion and update strain() array

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

      np = first_strain_parm

c -- update block strains
      do kjj = 1, num_strain_invert
       njj=nstrain_invert(kjj)
       npc = np + (kjj-1) *3

       do i=1,3
         strain(njj,i)=parm(npc+i-1)
       enddo

      enddo
      
      return
      end
      
c**********************************************************************
      subroutine updateRx
c -- get relaxation amplitude from inversion and update array

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

c-- update amplitudes for relaxation velocity fields
c-- kprlx(n) is the parameter number for this amplitude
      do n = 1, MAX_mrlx_files
       np = kprlx(n)
       if(np.gt.0 .and. rlx_inv_flag(n) ) rlxParms(n,1) = parm(np) 
      enddo
      
      return
      end
      
c**********************************************************************
      subroutine tdelaynode (nt, kt, ix, iz, kf, delayfac )
c-- get delay in years for transient migration at node
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

         delayfac = 0.0d0
         Vx  = transient(nt, 9)
         Vw  = transient(nt,10)

         if ( kt.gt.0 .and. (Vx.ne.0.0d0 .or. Vw.ne.0.0d0) ) then
          Xo  = transient(nt, 1)
          Yo  = transient(nt, 2)
          pX  = xwnode(1, ix, iz, kf)
          pW  = xwnode(2, ix, iz, kf)
          call ll2xw (kf, Xo, Yo, x, w)
          dx = x-pX
          dw = w-pW
          Tx = 0.0d0
          Tw = 0.0d0

c mtype=0 unilateral, = 1 circular Vx=Vw
          mtype = info_source(nt,8)
          if (mtype.eq.1) then
            Vw = Vx
            transient(nt,10) = Vx
          endif

c elliptical wavefront
c
c  (x/a)^2 + (y/b)^2 = 1
c  a = Vx*t,  b = Vy*t, so
c  (x/Vx)^2 + (y/Vy)^2 = t^2
c
c  t = sqrt( (x/Vx)^2 + (y/Vy)^2 )
 
          if (Vx .ne. 0.0d0 ) Tx = dx/Vx
          if (Vw .ne. 0.0d0 ) Tw = dw/Vw
          T = dsqrt( Tx*Tx + Tw*Tw )
          delayfac = T / dpy

         endif

       return
       end
c**********************************************************************
      subroutine tdelaysite (nt, kt, Xsite, Ysite, delayfac )
c-- get delay in years for transient migration at site
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

         delayfac = 0.0d0
         Vx  = transient(nt, 9)
         Vw  = transient(nt,10)
         kf = info_source(nt,1)
         mtype = info_source(nt,8)

         if ( kt.gt.0 .and. (Vx .ne. 0.0d0 .or. Vw .ne. 0.0d0 )) then
          Xo  = transient(nt, 1)
          Yo  = transient(nt, 2)
          sx = 1.0d-10
          sw = 1.0d-10

c mtype=0 unilateral, = 1 bilateral
          if (mtype.eq.1) then
            Vw = Vx
            transient(nt,10) = Vw
          endif

c elliptical wavefront
c
c  (x/a)^2 + (y/b)^2 = 1
c  a = Vx*t,  b = Vy*t, so
c  (x/Vx)^2 + (y/Vy)^2 = t^2
c
c  t = sqrt( (x/Vx)^2 + (y/Vy)^2 )
 
c distance to station and migration vector
           if (Vx .ne. 0.0d0 ) sx = 1.0d0/Vx
           if (Vw .ne. 0.0d0 ) sw = 1.0d0/Vw
c azs is from source to site 
           call delaz( Ysite, Xsite, Yo, Xo, del, azs)
c set Vx to be along strike
           azs = azs - av_fault_strike(kf)
c adjust by strike of fault so de is along strike, dn downdip
           dkm = del*d2x
           dx = dkm*dcos(azs*d2r)
           dw = dkm*dsin(azs*d2r)
           Tx = dx*sx
           Tw = dw*sw
           T = dsqrt( Tx*Tx + Tw*Tw )
           delayfac = T / dpy
         endif

       return
       end
c**********************************************************************      
      subroutine ts_calc (ntype, isite, ksyn)

c get predicted displacement time series for site isite
c done by adding incremental displacements 
c isite is the site number
c ntype = 0 for GPS, = 1 for INSAR
c set ksyn > 0 at final step to make and output a synthetic time series at increment dtsyn

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"
      
      common /hs1/ gcurve_dt(MAX_srce), gcurve(MAX_srce, MAX_ngc)
      common /hs2/ num_gc(MAX_srce)
      common /hs3/ xsyn(11000,3)
      
      dimension vv(3), gf(3), dU(3,3), v(3), t2(2)

c      dimension scurve(MAX_ngc)
      logical wr, wr2, doGPS, doInSAR, getgf, node_migrate 
      
      wr = .false.
      wr2 = .false.

      n1 = 0 
      n2 = 0
      
      half = one/two
      sq2pii = dsqrt(two*pii)
      doGPS   = (ntype.eq.0)
      doInSAR = (ntype.eq.1)

c** do INSAR at 2 times 
c-- 4/27/12 added secular velocities
      if ( doInSAR ) then
       xsite = insar_pos(isite,1)
       ysite = insar_pos(isite,2)
       zsite = insar_pos(isite,3)
c       ksite = loc_ins(isite)
       ksite = isite
       nf = insar_file(isite)
       dt = insar_info(nf,4)-insar_info(nf,3)
        do j=1,3
         Vs = ins_rot(isite,j) + ins_str(isite,j) + ins_ela(isite,j)
c         print *, 'Vs ',isite, Vs
         insar_calc(isite, 1, j) = 0.0d0
         insar_calc(isite, 2, j) = dt * Vs
        enddo
      endif
      
c** GPS data
      if ( doGPS ) then
      
      call gpsinfo2(isite, xsite, ysite, xobs, yobs, zobs, 
     .      vv(1), vv(2), vv(3), sx, sy, sz, sxy)
      call gpslongname(isite)
      zsite = gps_pos(isite,3)

c-- v() is the velocity of the site in the same ref frame as the data      
      do j = 1,3
       v(j) = vv(j)
      enddo 
     
      ksite = loc_gps(isite)
      n1 = ndx(isite,1) 
      n2 = ndx(isite,2)
 
c-- for displacements, get GPS position at 2 times
c-- secular velocity from model
      if ( gps_type(isite).eq.2) then
       dt = timespan(isite,2) - timespan(isite,1)
        do j=1,3
         gps_calc(isite, 1, j) = 0.0d0
         gps_calc(isite, 2, j) = dt * v(j)
        enddo
      endif

c-- for time series      
      if (n1 .gt.0) then

c offset flags (Eo No Uo) control what to do with the offsets and seasonal signals for the 3 components 
c Flag = 0 don't use this component 
c        1 fix at current value
c        2 solve for offset by regression
c        3 solve for offset and one periodic signal (yearly period) 
c        4 solve for offset and two periodic signals (yearly and 6-month periods) 
        
c* get baseline offset, and set first value to it      
        do j=1,3
         x0 = 0.0d0
         if (XVflag(isite, j, 1).eq.0 ) x0 = x_disp(n1,j)
         if (XVflag(isite, j, 1).eq.1 ) x0 = GXo(isite,j,1)
         if (XVflag(isite, j, 1).eq.2 ) x0 = GXo(isite,j,1)
         if (XVflag(isite, j, 1).eq.3 ) x0 = GXo(isite,j,1)
         if (XVflag(isite, j, 1).eq.4 ) x0 = GXo(isite,j,1)
            
         x_calc(n1,j) = x0
         GXo(isite,j,1) = x0
        enddo
        
c  slope flags (Ev Nv Uv):
c Flag = 0 don't use slope (set slope to 0)
c        1 fix at current value
c        2 solve for slope by regression
c        3 use slope (velocity) from block model

c* get slope of time series
       do j=1,3
        if (XVflag(isite, j, 2).eq.0 ) v(j) = 0.0d0
        if (XVflag(isite, j, 2).eq.1 ) v(j) = GVo(isite,j,2)
        if (XVflag(isite, j, 2).eq.2 ) v(j) = GVo(isite,j,2)
        if (XVflag(isite, j, 2).eq.3 ) v(j) = vv(j)
        GVo(isite,j,2) = v(j)
       enddo

c* add displacements due to linear, secular velocity and sinusoidal 
c -- done in ref frame of data
        ts1 = t_disp(n1)
       do n = n1, n2
        dt = t_disp(n) - t_disp(n-1)
        tt = t_disp(n)
        do j=1,3
         x_calc(n,j) =  GXo(isite,j,1) + (tt-ts1) * v(j) +
     .      fsine(tt,GVo(isite,j,3),GVo(isite,j,4),GVo(isite,j,5),
     .        GVo(isite,j,6))
        enddo
       enddo

c**************************************************
c** synthetic time series       
c**************************************************
       if ( ksyn.gt.zero) then
       
        do j=1,3
          xsyn(1,j) = GXo(isite,j,1) 
        enddo
        
        ts1 = timespan(isite,1)
        ts2 = timespan(isite,2)
        dtsyn  = fndtsyn(isite)
        nk = int((ts2-ts1)/dtsyn) +1
        
c-- add secular velocity        
        do k=1,nk
          tt = ts1 + real(k-1)*dtsyn
         do j=1,3
           xsyn(k,j) =  GXo(isite,j,1) + (tt-ts1) * v(j) +
     .      fsine(tt,GVo(isite,j,3),GVo(isite,j,4),GVo(isite,j,5),
     .      GVo(isite,j,6))
         enddo
        enddo
        
       endif

       endif
      endif

       
c* if time dependent, get displacements at sites from the transient sources 
c* tr_def has the GPS displacements due to each transient
c* tr_ins has the Insar displacements due to each transient
      
      do nt =1, MAX_srce
      
c fault
        kf = info_source(nt,1)
        
c kq = spatial type
        kq = info_source(nt,2)
        
c-- if GFs are needed or not        
        getgf = ( (kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. kq.eq.6 .or. 
     .            kq.eq.7 .or. kq.eq.8) .and. useGF(kf) )
        
c time dependence type        
c* kt = none(0), Gaussian(1), triangles(2), exponential(3), boxcar(4),  ...
        kt = info_source(nt,4)
         
       if (kq.gt.0 .and. sflag(nt)) then

         Xo = transient(nt,1)
         Yo = transient(nt,2)
         To = transient(nt,7)
         Tarrive(nt,isite) = To
         stf = fnTamp(nt)
         amp = one
         Amp5 = transient(nt,5)
         node_migrate = ( use_node_delay .and. 
     .    (transient(nt,9).ne.0.0d0 .or. transient(nt,10).ne.0.0d0) )

c         if (kq. eq. 1) amp = amp5
         
c** get site displacements for this transient from stored GFs
      if ( getgf ) then

       call cleareal(gf, 3)
       tmom = 0.0d0
 
c       ngf = first_gf(kf)-1

c** calculate centroid of nodes
       cxnode = 0.0d0
       cynode = 0.0d0
       sumtwt = 0.0d0

c** get correct Green's function for this fault using the index ngf
       do 22 iz = 1, nzf(kf)
        do 22 ix = 1, nxf(kf)
         ngf = index_gf(ix,iz,kf)

c- get delay in years for this node from migration parameters; store in dtnode( )
         xn = xynode(1,ix,iz,kf)
         yn = xynode(2,ix,iz,kf)

         if (node_migrate) then
            call tdelaynode (nt, kt, ix, iz, kf, delayfac )
         else
            call tdelaysite (nt, kt, Xn, Yn, delayfac )
         endif
      
         dtnode(ix, iz, nt) = delayfac

c  slip_t is unit vector for slip direction, tphi has slip amplitude
       sX = amp * tphi(ix, iz, nt) * slip_t(ix, iz, nt, 1)
       sY = amp * tphi(ix, iz, nt) * slip_t(ix, iz, nt, 2)

crm1209 
       S = dsqrt(sX*sX+sY*sY)

       if (s.gt.0.0d0) then

       if(kf.ne.0) then 
            tmom = tmom + S*stf*tmom_node(ix, iz, kf)
c-- weighted average of moment to get centroid of source
            cxnode = cxnode + xynode(1,ix,iz,kf)*tmom
            cynode = cynode + xynode(2,ix,iz,kf)*tmom
            sumtwt = sumtwt + tmom
       endif

c gf is the sum over all nodes for GPS or InSAR
c danode is the node contribution for this site

       if (doGPS .and. .not. node_migrate) then
         gf(1) =  gf(1) + sX * g_gf (ngf, ksite, 1)  
     .                  + sY * g_gf (ngf, ksite, 2)
         gf(2) =  gf(2) + sX * g_gf (ngf, ksite, 3)  
     .                  + sY * g_gf (ngf, ksite, 4)
         gf(3) =  gf(3) + sX * g_gf (ngf, ksite, 5)  
     .                  + sY * g_gf (ngf, ksite, 6)
       endif

       if (doInSAR  .and. .not. node_migrate) then
         gf(1) =  gf(1) + sX * i_gf (ngf, ksite, 1)  
     .                  + sY * i_gf (ngf, ksite, 2)
         gf(2) =  gf(2) + sX * i_gf (ngf, ksite, 3)  
     .                  + sY * i_gf (ngf, ksite, 4)
         gf(3) =  gf(3) + sX * i_gf (ngf, ksite, 5)  
     .                  + sY * i_gf (ngf, ksite, 6)
       endif

c store displacements at site from this node 
       if (doGPS .and. node_migrate) then
         danode(ix,iz,nt,1) = sX * g_gf (ngf, ksite, 1)  
     .                      + sY * g_gf (ngf, ksite, 2)
         danode(ix,iz,nt,2) = sX * g_gf (ngf, ksite, 3)  
     .                      + sY * g_gf (ngf, ksite, 4)
         danode(ix,iz,nt,3) = sX * g_gf (ngf, ksite, 5)  
     .                      + sY * g_gf (ngf, ksite, 6)
       endif

       if (doInSAR  .and. node_migrate) then
         danode(ix,iz,nt,1) = sX * i_gf (ngf, ksite, 1)  
     .                      + sY * i_gf (ngf, ksite, 2)
         danode(ix,iz,nt,2) = sX * i_gf (ngf, ksite, 3)  
     .                      + sY * i_gf (ngf, ksite, 4)
         danode(ix,iz,nt,3) = sX * i_gf (ngf, ksite, 5)  
     .                      + sY * i_gf (ngf, ksite, 6)
       endif

      endif

  22  continue

c moment      
      trans_sums(nt,3) = tmom*xmu

c displacements by site for this transient
        do j=1,3
          if(doGPS)   tr_def(nt,ksite,j) = gf(j)*stf 
          if(doInSAR) tr_ins(nt,ksite,j) = gf(j)*stf
        enddo
      endif
         
c** Mogi source        
         if ( kq.eq.10 ) then
          Pm = 1.0d0
          Zm = transient(nt,3)

c      call MOGI1 (Xo,Yo,Zm, Pm, Xsite, Ysite, Zsite,gf(1),gf(2),gf(3))
c      call MOGI2(Xo,Yo,Zm, Pm, Xsite, Ysite, Zsite,gf(1),gf(2),gf(3))
      call MOGI3(Xo,Yo,Zm, Pm, Xsite, Ysite, Zsite,gf(1),gf(2),gf(3))

           do j=1,3
            if(doGPS)   tr_def(nt,ksite,j) = gf(j)*stf
            if(doInSAR) tr_ins(nt,ksite,j) = gf(j)*stf
           enddo
         endif
         
c** finite-plane earthquake         
         if ( kq.eq.9 ) then
          W = transient(nt,4)
          aqk = fnTamp(nt)
          aqk = one
          flength = transient(nt,6)
          strike =  transient(nt,11) 
          dip  =    transient(nt,12) 
          rake =    fn180(transient(nt,13))
          ddep = W*dsin(dip*d2r) 
          Zmax = transient(nt,3) + ddep/two + zsite
          u1 = aqk*dcos(rake*d2r)
          u2 = aqk*dsin(rake*d2r)
          u3 = 0.0d0
          call okadaquake (strike, dip, Xo, Yo, flength,
     .      W, Zmax, u1, u2, u3, xsite, ysite, gf, dU )
           do j=1,3
            if(doGPS)   tr_def(nt,ksite,j) = gf(j)*stf
            if(doInSAR) tr_ins(nt,ksite,j) = gf(j)*stf
           enddo
          if(wr) write(*,'(i3,1x,a8,3f8.1)') nt, gps_name(isite), 
     .      (gf(j),j=1,3)
         endif
    
c** finite-plane expansion not on modeled fault        
         if ( kq.eq.11 ) then
          W = transient(nt,4)
          aqk = fnTamp(nt)
          aqk = one
          flength = transient(nt,6)
          strike = transient(nt,11) 
          dip = transient(nt,12) 
          ddep = W*dsin(dip*d2r)  
          Zmax = transient(nt,3) + ddep/two + zsite
          u1 = 0.0d0
          u2 = 0.0d0
          u3 = aqk
          call okadaquake (strike, dip, Xo, Yo, flength,
     .      W, Zmax, u1, u2, u3, xsite, ysite, gf, dU )
          do j=1,3
            if(doGPS)   tr_def(nt,ksite,j) = gf(j)*stf
            if(doInSAR) tr_ins(nt,ksite,j) = gf(j)*stf
          enddo
         endif
    
c Prolate spheroid
         if ( kq.eq.12 ) then
          Xo  = transient(nt,1)
          Yo  = transient(nt,2)
          Zo  = transient(nt,3)
          Pwr = transient(nt,5)
          Pwr = 1.0d0
          Ap   = transient(nt,4)
          ABp  = transient(nt,6)
          strike = transient(nt,11)
          dip = transient(nt,12)

c      if( DIP .gt. 90.0d0 ) then 
c       DIP = 180.0d0-dip
c       strike = strike+180.0d0
c      endif
c      if( DIP.lt.0.0d0 ) then 
c       DIP = abs(dip)
c       strike = strike+180.0d0
c      endif
c      if (ABp .le. 1.0d0 ) ABp = 1.001d0

c       transient(nt,6) = ABp
c       transient(nt,11) = strike
c       transient(nt,12) = dip

          call PROLATE(Dx,Dy,Dz, Xsite,Ysite,Zsite, Xo,Yo,Zo, 
     .          strike, dip, Ap, ABp, Pwr)

          gf(1) = Dx
          gf(2) = Dy 
          gf(3) = Dz 

c          write(*,'(14f12.4)') Xsite,Ysite,Zsite, Xo,Yo,Zo,
c     .          strike, dip, Ap, ABp, Pwr, Dx,Dy,Dz

           do j=1,3
            if(doGPS)   tr_def(nt,ksite,j) = gf(j)*stf
            if(doInSAR) tr_ins(nt,ksite,j) = gf(j)*stf
           enddo
         endif

c** migration
c  directivity vel (km/yr) gives apparent position for this day
c  advance or delay the time series
       delayfac = 0.0d0
       call tdelaysite (nt, kt, Xsite, Ysite, delayfac )
       
       ngc = num_gc(nt)
       gdt = gcurve_dt(nt)
       tmax = real(ngc-1)*gdt
       Tst = To + delayfac
       Tarrive(nt,isite) = Tst

c INSAR: total displacements at both times
      if ( doInSAR ) then
       nf = insar_file(isite)
       t2(1) = insar_info(nf,3)
       t2(2) = insar_info(nf,4)

       do n=1,2
        dt = t2(n) - Tst
        if (dt .le. zero) then
          gc = zero
        elseif (dt .ge. tmax) then
          gc = gcurve(nt,ngc-1)
        else
          idt = int(dt/gdt)+1
          ddt = dt - real(idt-1)*gdt
          dgc = gcurve(nt,idt+1) - gcurve(nt,idt)
          gc = gcurve(nt,idt) + dgc*ddt/gdt
        endif

       do j=1,3
        insar_calc(isite, n, j) = insar_calc(isite, n, j) + gf(j)*gc 
       enddo
      enddo
      endif
c end of INSAR


c GPS displacement: total displacements at both times
      if ( doGPS .and. gps_type(isite).eq.2) then
       t2(1) = timespan(isite,1)
       t2(2) = timespan(isite,2) 

       do n=1,2
        dt = t2(n) - Tst
        if (dt .le. zero) then
          gc = zero
        elseif (dt .ge. tmax) then
          gc = gcurve(nt,ngc-1)
        else
          idt = int(dt/gdt)+1
          ddt = dt - real(idt-1)*gdt
          dgc = gcurve(nt,idt+1) - gcurve(nt,idt)
          gc = gcurve(nt,idt) + dgc*ddt/gdt
        endif

       do j=1,3
        gps_calc(isite, n, j) = gps_calc(isite, n, j) + gf(j)*gc 
       enddo

      enddo
      endif
c end of GPS displacement

c-- GPS time series
c interpolate the displacement history to get this time
      if (n1.gt. 0 .and. doGPS) then
        Tarrive(nt,isite) =  To + delayfac
        ngc = num_gc(nt)
        gdt = gcurve_dt(nt)
        tmax = real(ngc-1)*gdt

cc new part to migrate along nodes, instead of along GPS sites
      if ( node_migrate ) then

       do iz = 1, nzf(kf)
        do ix = 1, nxf(kf)

        Tst = To + dtnode(ix,iz,nt)
       
       do n = n1, n2
        dt = t_disp(n) - Tst
        if (dt.lt.zero) then
          gc = zero
        elseif (dt .ge. tmax) then
          gc = gcurve(nt,ngc-1)
        else
          idt = int(dt/gdt)+1
          ddt = dt - real(idt-1)*gdt
          dgc = gcurve(nt,idt+1) - gcurve(nt,idt)
          gc = gcurve(nt,idt) + dgc*ddt/gdt
        endif

c convolve with gf (displacement)
        do j=1,3
          x_calc(n, j) =  x_calc(n, j) + danode(ix,iz,nt,j)*gc 
        enddo

       enddo

       enddo
       enddo

      else
       
c migrate along surface sites

       ngc = num_gc(nt)
       gdt = gcurve_dt(nt)
       tmax = real(ngc-1)*gdt
       Tst = To + delayfac
       Tarrive(nt,isite) = Tst
       
       do n = n1, n2
        dt = t_disp(n) - Tst
        if (dt.lt.zero) then
          gc = zero
        elseif (dt .ge. tmax) then
          gc = gcurve(nt,ngc-1)
        else
          idt = int(dt/gdt)+1
          ddt = dt - real(idt-1)*gdt
          dgc = gcurve(nt,idt+1) - gcurve(nt,idt)
          gc = gcurve(nt,idt) + dgc*ddt/gdt
        endif

c convolve with gf (displacement)
        do j=1,3
         x_calc(n, j) =  x_calc(n, j) + gf(j)*gc 
        enddo

       enddo

      endif
 
c********************************
c** synthetic time series       
c********************************
      if ( ksyn.gt.zero) then
        ts1 = timespan(isite,1)
        ts2 = timespan(isite,2)
        dtsyn  = fndtsyn(isite)
        nk = int((ts2-ts1)/dtsyn) +1
        Tst = To + delayfac


      if ( use_node_delay ) then

       do iz = 1, nzf(kf)
        do ix = 1, nxf(kf)

        Tst = To + dtnode(ix,iz,nt)
       
        do k=1,nk
          t = ts1 + real(k-1)*dtsyn
          dt = t - Tst

        if (dt.lt.zero) then
          gc = zero
        elseif (dt .ge. tmax) then
          gc = gcurve(nt,ngc-1)
        else
          idt = int(dt/gdt)+1
          ddt = dt - real(idt-1)*gdt
          dgc = gcurve(nt,idt+1) - gcurve(nt,idt)
          gc = gcurve(nt,idt) + dgc*ddt/gdt
        endif

c convolve with gf (displacement)
        do j=1,3
          xsyn(k, j) =  xsyn(k, j) + danode(ix,iz,nt,j)*gc 
        enddo

       enddo

       enddo
       enddo



      else

        do k=1,nk
          t = ts1 + real(k-1)*dtsyn
          dt = t - Tst
        
        if (dt.lt.zero) then
          gc = zero
        elseif (dt .ge. tmax) then
          gc = gcurve(nt,ngc)
        else
          idt = int(dt/gdt)+1
          ddt = dt - real(idt-1)*gdt
          dgc = gcurve(nt,idt+1) - gcurve(nt,idt)
          gc = gcurve(nt,idt) + dgc*ddt/gdt
        endif

        do j=1,3
         xsyn(k, j) =  xsyn(k, j) + gf(j)*gc 
        enddo

       enddo

      endif
       nsyn = nk
      endif
      
c*****************************  

       endif      
        endif
       enddo
       
       return
       end


c**********************************************************************     
      subroutine stfarea
      
c-- find area under STF and duration for each source

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      dimension tnorm(MAX_tau)

c* common for data region
      common /dr1/ dr_pos(6)
      
c time dependence type        
c kt = none(0), Gaussian(1), triangles(2), exponential(3), boxcar(4), neg boxcar(5)
c dur1 is duration from fit, dur2 is duration of model

      end_time = 2050.0
      if( dr_pos(6) .ne. zero) end_time = dr_pos(6)

      do nt = 1, MAX_srce
      
        kt   = info_source(nt,4)
        ntau = info_source(nt,7)

        To = transient(nt,7)
        Ts = max(1.0d-10,transient(nt,8))/dpy
        dur_max = end_time - To

        area = 0.d0
        dur1 = 0.0d0
        dur2 = 0.0d0
        
        
      if ( sflag(nt) ) then

c** impulse      
       if ( kt.eq.0 .or. kt.eq.8 ) then
        area = transient(nt,5)
        dur1 = 0.0d0
        dur2 = 1.0d-4
      
c** Gaussian
       elseif ( kt.eq.1 ) then
        Ts = max(0.1d0,transient(nt,8))/dpy
        a = transient(nt,5)
        area = a * Ts * dsqrt(pii)
        dur1 = min(four*Ts, dur_max)
        dur2 = dur1
        
c** exponential
       elseif ( kt.eq.3 ) then
        Ts = max(0.1d0,transient(nt,8))/dpy
        a = transient(nt,5)
        area = a * Ts 
        dur1 = min(three*Ts, dur_max)
        dur2 = dur1
        
c** boxcar
       elseif ( kt.eq.4) then
        Ts = max(0.1d0,transient(nt,8))/dpy
        a = transient(nt,5)
        area = a * Ts 
        dur1 =  min(Ts, dur_max)
        dur2 = dur1

c** Omori s(t) = A /(t + D)
       elseif ( kt.eq.6 ) then
        Ts = max(0.1d0,transient(nt,8))/dpy
        a = transient(nt,5)
        t1 = Ts+three
        area = a
        if( t1.gt.zero .and. Ts.gt.zero) then
         area = a*(dlog(t1) - dlog(Ts) )
        endif
        dur1 = min(Ts, dur_max)
        dur2 = dur1
        
c   Shen: A / ( log(10.0)*(Ts+t-to) )
c  its the derivative of A log10( 1 + (t-to)/Ts) )
       elseif ( kt.eq.7 ) then
        Ts = max(1.0d-10,transient(nt,8))/dpy
        a = transient(nt,5)
        t1 = Ts+three
        area = a
        if( t1.gt.zero .and. Ts.gt.zero) then
         area = a*(dlog10 (one + t1/Ts) )
        endif
        dur1 = min(Ts, dur_max)
        dur2 = dur1

c** triangles       
       elseif ( kt.eq.2 .and. ntau.gt.0 ) then
       
c-- tau is the half-width of the triangle
c          print *, 'stfarea dtau', nt,dtau(nt)
        tau = dtau(nt)/dpy
        dur2 = tau*real(ntau+1)
        dur2 = min(dur2, dur_max)

c** area under stf and duration
        tmax = zero
        acent = zero
        sum = zero
        
        do k=1,ntau
         sum = sum + dabs(atau(nt,k) )
        enddo
        
        do k=1,ntau
         tnorm(k) = dabs(atau(nt,k)) / sum
        enddo
        
        do k=1,ntau
c         atau(nt,k) = max(atau(nt,k), 0.0d0 ) 
         tmax = max(tmax, abs(atau(nt,k)) ) 
c1202         area = area + dabs(atau(nt,k)) * tau 
         area = area + atau(nt,k) * tau 
        enddo
        
        transient(nt,5) = area
        
c** centroid time        
        do k=1,ntau
         acent = acent + tnorm(k)*real(k)
        enddo
         tcent = acent*tau

c--- 95% duration calculated by second moment method
c         (2-sigma for gaussian STF):
c    t95 = 2 * sqrt ( (second moment)/(zeroth moment))  

        st = zero
        do k=1,ntau
         st=st + tnorm(k) * (real(k*k) + one/six)
        enddo
        
       sss= max(zero, (st - (tcent/tau)**2) )
       dur1 = (four * tau * sqrt (sss))
       dur1 = min(dur1, dur_max)
c       endif

c** boxcars       
       elseif ( kt.eq.5 .and. ntau.gt.0 ) then
       
c-- tau is the width of the boxcar
        tau = dtau(nt)/dpy
        dur2 = tau*real(ntau+1)
        dur2 = min(dur2, dur_max)
        dur1 = 0.0d0

c** area under stf and duration
        do k=1,ntau
          area = area + atau(nt,k) * tau 
          if(abs(atau(nt,k)).gt. 0.01 ) dur1=dur1+tau
        enddo
        
        transient(nt,5) = area
       endif

       endif
       
c-- area undef stf      
c**       stf_area(nt) = area
       
c-- duration   
       tdur2(nt) = dur2*dpy    
c-- duration       
       tdur(nt)  = dur1*dpy
       
        
      enddo
      
      return
      end
      
c**********************************************************************     
      subroutine buildhistory (kk)
      
c-- build displacement history curve gcurve(t) for all sources
c-- scurve(t) is the slip history 
c-- time units are years, displacement in mm

c1202 make gcurve true amplitude

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      common /hs1/ gcurve_dt(MAX_srce), gcurve(MAX_srce, MAX_ngc)
      common /hs2/ num_gc(MAX_srce)

c* common for data region
      common /dr1/ dr_pos(6)

      dimension scurve(MAX_ngc), ttmp(MAX_tau+3)
      character c3*3
      logical wr
      
      wr = (kk.eq.1)
      ngc = 0
      gdt = 1.0d0
      half = one/two
      qrtr = one/four
      ypd = one/dpy
      sq2pii = dsqrt(two*pii)
      one = 1.0d0
      end_time = today
      if( dr_pos(6) .ne. zero) end_time = dr_pos(6)

c      n = MAX_ngc*MAX_srce
c      call cleareal(gcurve,n)

      call stfarea
      
      do nt = 1, MAX_srce
       if (sflag(nt)) then
       
       kf = info_source(nt,1)
       kq = info_source(nt,2)
       kt = info_source(nt,4)
       ntau = info_source(nt,7)


c  transient() =  Ln Lt Zh d1 am d2 to tc xr wr st dp rk az              

       To = transient(nt,7)
       Ts = max(1.0d-5, transient(nt,8))/dpy
       Amp5 = transient(nt,5)
       Amp_stf = fnTamp(nt)
       dur_max = end_time - To

      call cleareal(scurve, MAX_ngc)
      
      if (wr) then
       call i2c(nt, 3, c3)
       call fopen (k55, 1, '_src_'//c3//'.stf ')
      endif

c-- s(t) is the STF velocity      
c-- scurve() is the slip rate history, area under it is unity  
c-- gcurve() is the displacement history 

c-- 0: impulse in rate, step in displacement
      if ( kt.eq.0) then
       gdt = 1.0d-4/dpy
       scurve(1) = Amp5
       gcurve(nt,1) = zero
       ngc = 20
       do i=2,ngc
         gcurve(nt,i) = Amp5
       enddo
      endif

      
c** 1: Gaussian s(t) - use -3sigma as the starting time, make it 6sigma long
      if ( kt.eq.1) then
        gdt = max(1.0d-3, Ts/1.0d2)
        ngc = int(10.0d0*Ts/gdt)
        ngc = int(dur_max/gdt)
        ngc = min(ngc,MAX_ngc)
c        ngc=MAX_ngc
        scurve(1) = zero

c start time is 3 Ts before peak
       do i=2,ngc
        t = real(i-1)*gdt 
        x = (t - three*Ts)/Ts
        scurve(i) = Amp5*exp(-half*x*x)/(Ts*sq2pii)  
       enddo

       gcurve(nt,1) = scurve(1)
       do i=2,ngc
         gcurve(nt,i) = gcurve(nt,i-1) + scurve(i)*gdt
       enddo
      endif


c** 2: s(t) for triangular elements
c1202 remove normalization
       if (kt.eq.2) then
        tau = dtau(nt)/dpy
        gdt = tau/10.0d0
        ntau = info_source(nt,7)
        call cleareal(ttmp, MAX_tau+3)

c-- temporary array of  tau to integrate        
        amp = fnTamp(nt)
        do k=1,ntau
c1202         ttmp(k+1)=atau(nt,k)/amp 
         ttmp(k+1)=atau(nt,k) 
        enddo
        
        ngc = min( int(real(ntau+4)*tau/gdt), int(dur_max/gdt) )
        ngc = min(ngc,MAX_ngc)

        do k=1,ntau+2
         amp1 = ttmp(k) 
         amp2 = ttmp(k+1) 
         dadt = (amp2-amp1)/tau
         t1 = real(k-1)*tau
         t2 = real(k)*tau
         do i=1,ngc
           tgc = real(i-1)*gdt
           if (tgc.ge.t1 .and. tgc .le. t2) then
             a = amp1 + dadt*(tgc-t1)
             scurve(i) = a
           endif
         enddo
        enddo
        
       gcurve(nt,1) = scurve(1)*gdt
       do i=2,ngc
         gcurve(nt,i) = gcurve(nt,i-1)+scurve(i)*gdt 
       enddo
      endif
      
c** 3: exponential d(t) = -A exp ( -(t-To) /Ts )
c**    velocity    s(t) = A/Ts exp ( -(t-To) /Ts )
c  
      if (kt.eq.3) then
        scurve(1) = zero
        gdt = ypd/2.0d0
        ngc = min(int(dur_max/gdt),MAX_ngc)
       do i=2,ngc
        x = (real(i-1)*gdt)/Ts
        scurve(i) = Amp5/Ts * exp( -x )
c        scurve(i) = Amp5  * exp( -x )
       enddo
       gcurve(nt,1) = scurve(1)*gdt
       do i=2,ngc
         gcurve(nt,i) = gcurve(nt,i-1)+scurve(i)*gdt
       enddo
      endif
      
      
c** 4: boxcar, To is start, Ts is duration
      if (kt.eq.4) then
        amp = transient(nt,5)
c        gdt = ypd/2.0d0
c        ngc = min( int(Ts/gdt), int(dur_max/gdt) )
c        ngc = max(ngc,1)
c        ngc = min(ngc,MAX_ngc)
c old
        gdt = min(Ts/1.0d2,ypd/2.0d0)
        ngc = min( int(two*Ts/gdt), int(dur_max/gdt) )
        ngc = min(ngc,MAX_ngc)


        scurve(1) = zero

        
       do i=2,ngc
        t=real(i-1)*gdt
        scurve(i) = Amp5/Ts
        if (t.gt.Ts) scurve(i) = zero
       enddo
       gcurve(nt,1) = scurve(1)*gdt
       do i=2,ngc
         gcurve(nt,i) = gcurve(nt,i-1) + scurve(i)*gdt
       enddo
      endif
      
c** 5: s(t) for multiple boxcar elements
       if (kt.eq.5) then
        tau = dtau(nt)/dpy
        gdt = tau/10.0d0
        ntau = info_source(nt,7)

        ngc = min(int(real(ntau+4)*tau/gdt), int(dur_max/gdt) )
        ngc = min(ngc,MAX_ngc)

        scurve(1) = zero
        do k=1,ntau + 1 
         amp1 = atau(nt,k) 
         t1 = real(k-1)*tau
         t2 = real(k)*tau
         do i=2,ngc
           tgc = real(i-1)*gdt
           if (tgc.ge.t1 .and. tgc .le. t2) then
             scurve(i) = amp1
           endif
         enddo
        enddo
        
       gcurve(nt,1) = scurve(1)*gdt
       do i=2,ngc
         gcurve(nt,i) = gcurve(nt,i-1)+scurve(i)*gdt 
       enddo
      endif
      

c  6:
c** Omori  d(t) =  A / (t + D)
c** Omori  s(t) = -A / (t + D)**2

      if (kt.eq.6) then
        gdt = ypd/2.0d0
        ngc = min(int(dur_max/gdt),MAX_ngc)

        scurve(1) = zero
       do i=2,ngc
c** old
c         t = (real(i-1)*gdt) + Ts
c         scurve(i) = Amp5 / t
c new
          t = (real(i-1)*gdt) + Ts
          scurve(i) = Amp5 / t**2

       enddo
       gcurve(nt,1) = scurve(1)*gdt
       do i=2,ngc
         gcurve(nt,i) = gcurve(nt,i-1)+scurve(i)*gdt
       enddo
      endif
      
c  7:
c   Shen: displacement = A log10( 1 + (t-To)/Ts) )
c         velocity =  A / ( log(10)*(Ts + t - To) )
      if (kt.eq.7) then
c        gdt = max(Ts/50.0d0,ypd)
        gdt = ypd/2.0d0
        gdt = ypd 
c        ngc = min( int((5.0d4*Ts)/gdt), int(dur_max/gdt))
        ngc = min(int(dur_max/gdt),MAX_ngc)
c        ngc = MAX_ngc
        scurve(1) = zero
        dl = dlog(10.0d0)
       do i=2,ngc
         t = (real(i-1)*gdt) + Ts
         scurve(i) = Amp5 / (dl*t)
       enddo
        gcurve(nt,1) = scurve(1)*gdt
       do i=2,ngc
         gcurve(nt,i) = gcurve(nt,i-1)+scurve(i)*gdt
       enddo
      endif

c  8:
c  Viscoelastic relaxation at magma chamber, Mogi
c  spatial type is Mogi
c  Segall (2010) eqn 7.105 is displacement, velocity is derivative
c  displacement =   A [ exp (-t/Tr) + (R2/R1)**3 * (1-exp(-t/Tr) ) ]
c  velocity     = - A / Tr * [ exp(-t/Tr) - (R2/R1)**3 * exp(-t/Tr) ]
c  A (Amp5) is amplitude (volome change) from Mogi
c  Ts = 3 visc (1-PR) R2^3 / [ mu*(1+PR) R1^3 ]

      if (kt.eq.8) then
        R1 = transient(nt,4)
        R2 = transient(nt,6)
        r3 = max(one, (R2/R1)**3 )
        vexp = max(one,transient(nt,8))
        vexp = min(30.0,transient(nt,8))
        visc = 10.0d0**vexp
c get Ts in years, Segall eqn 7.98 
        Ts = 3.0d0*r3*visc*(one-poisrat) / ( xmu*(one+poisrat) )
        Ts = max(0.1, Ts/secperyr)

        gdt = max(Ts/1.0d2,ypd)
        ngc = min( int((300.0d0*Ts)/gdt), int(dur_max/gdt) )
        ngc = min(ngc,MAX_ngc)
c       print *, transient(nt,8),Ts, ngc
        e2 = one/Ts  
       do i=1,ngc
        t = real(i-1)*gdt  
        e1 = exp (-t/Ts) 
        scurve(i) = -Amp5 * e2 * ( e1 - r3 * e1 )
        e3 = r3 * ( 1.0d0 - e1 )
        gcurve(nt,i) = Amp5 * (e1+e3)
       enddo

c         gcurve(nt,1) = scurve(1)*gdt
c       do i=2,ngc
c         gcurve(nt,i) = gcurve(nt,i-1)+scurve(i)*gdt
c       enddo

c this works for displacement
c        e1 = exp (-t/Ts) 
c        e2 = (R2/R1)**3 * ( 1.0d0 - e1 )
c        scurve(i) = Amp5 * (e1+e2)
c        gcurve(nt,i) = scurve(i)

      endif

c-- normalize
c       gmax = gcurve(nt,ngc)
c       do i=2,ngc
c         gcurve(nt,i) = gcurve(nt,i)/gmax
c       enddo
       
c store info      
       gcurve_dt(nt) = gdt
       num_gc(nt) = ngc

c total amplitude of event, can run off end of time series
       if(kt.eq.8) then
         stf_area(nt) = Amp5
       else
         stf_area(nt)=gcurve(nt,ngc)
       endif
      
c write out      
      if (wr) then 
       do i=1,ngc
        stf = scurve(i)
        if (kt.eq.2) stf = scurve(i)
        if (kt.eq.4) stf = scurve(i)*
     .        max(0.1d0,transient(nt,8))/dpy

         smax = 1.0
         amp = fnTamp(nt)
         if( kq.le.8 .and. kf.gt.0 ) then
           smax = 0.0d0
           do ix=1,nxf(kf)
            do iz=1,nzf(kf)
              smax = max(smax, abs(tphi(ix,iz,nt)))
            enddo
           enddo
         endif

        write(k55,'(i5, 6f15.5)') i, real(i-1)*gdt, To+real(i-1)*gdt,
     .    scurve(i)*smax, gcurve(nt,i), gcurve(nt,i)/gcurve(nt,ngc), 
     .    real(i-1)*gdt*dpy

       enddo
      endif


      endif
      
      if (wr) ik = kfclose(k55)
      
      enddo
      

      return
      end
      
c********************************************************************** 
c check if this site is to be used
      function useGPS (ista)
       implicit real*8 (a-h,o-z)
       include "tdefcom1.h"
       logical useGPS
       useGPS = .false.
       useGPS = ( gps_keep(ista) .and. gps_type(ista) .ne. 0 )
   
       return
       end
c********************************************************************** 
c check if this component is to be used
c this checks the Eo No Uo entries on input lines
      function useENU (ista, icomp)
       implicit real*8 (a-h,o-z)
       include "tdefcom1.h"
       logical useENU
       useENU = .false.
       itype = gps_type(ista)
       if ( itype.eq.1 .and. 
     .      int (gps_info(gps_index(ista), icomp+10) ).gt.0) 
     .      useENU = .true.
       if ( itype.eq.2 .and. 
     .      int (gps_info(gps_index(ista), icomp+7 ) ).gt.0) 
     .      useENU = .true.
       if ( itype.eq.3 .and. 
     .      int (gps_info(gps_index(ista), icomp+7 ) ).gt.0) 
     .      useENU = .true.
       
       return
       end
c********************************************************************** 
c check if fault locking is turned ON and it is a real fault
      function flock (kf)
       implicit real*8 (a-h,o-z)
       include "tdefcom1.h"
       logical flock
       
       flock = .false.

       if( kf.gt.0 .and. kf .le. MAX_f )
     .    flock = ( fflag(kf,1) .and. (nzf(kf).gt.1) )
      
       return
       end


c**********************************************************************      
      subroutine misfit(tchi, nwdat, kdof)

c--  compute total chi**2 misfit to all data

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

      dimension p(9)
      dimension rtmp(MAX_insar_pts), rsig(MAX_insar_pts), 
     .          aa(MAX_insar_pts,4), pp(4),  pper(4)
             
c      dimension grs(MAX_gps,3), wrs(MAX_gps,3), av0(3), wt0(3)
      
      logical use_vel, useENU, useg, useGPS
      character sname*4

      z   = 0.0d0
      one = 1.0d0
      two = 2.0d0
      nwdat=0
      use_vel = .false.

c data chi**2
      tchi = 0.0d0
 
      ksyn=0
      call buildhistory (ksyn)
      
c------------------------------------------------------------
c GPS velocities


c remove average residuals by file index
c 1-31-15 replaced by  parameter 'gb'
c      do j=1, num_gps_file
c       if ( gps_file_type(j).eq.1 .and. 
c    .    (gps_info(j,3).gt.0 .or. gps_info(j,4).gt.0 .or. 
c    .      gps_info(j,5).gt.0 ) ) then

c      ii=0
c      jindex = ngps_index(j)

c      do i=1, num_gps
c       jp = gps_info(gps_index(i),1)
c       if (gps_type(i).eq.1 .and. jp.eq.jindex ) then
c        call gpsinfo2(i, xx, yy, xobs, yobs, zobs, xcalc, ycalc, zcalc,
c    .    sx, sy, sz, sxy)
       
c-- check for large misfit

c      useg = ( useGPS(i) .and. .not. no_wt_gps )

c        if ( useg ) then
c         ii = ii+1
c         grs(ii,1) = xobs - xcalc
c         grs(ii,2) = yobs - ycalc
c         grs(ii,3) = zobs - zcalc
c         wrs(ii,1) = fnwt(sx)
c         wrs(ii,2) = fnwt(sy)
c         wrs(ii,3) = fnwt(sz)
c        endif
c       endif
c      enddo

c      ni = ii

c     call cleareal(av0,3)
c     call cleareal(wt0,3)

c      do i=1, ni
c       do k=1,3
c        av0(k) = av0(k) + grs(i,k)*wrs(i,k)
c        wt0(k) = wt0(k) + wrs(i,k)
c       enddo
c      enddo

c       do k=1,3
c         if(wt0(k).gt.zero ) then
c          av0(k) = av0(k)/wt0(k)
c         else
c          av0(k) = 0.0d0
c         endif
c         if (gps_info(j,k+2).gt.0 ) 
c     .         ref_vel(jindex,k)=ref_vel(jindex,k)+av0(k)
c       enddo

c       endif      
c      enddo

c---------------------------
c process
      do i=1, num_gps
       if (gps_type(i).eq.1) then

        kfile = gps_index(i)

        call gpsinfo2(i, xx, yy, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sx, sy, sz, sxy)
       
c-- check for large misfit

      useg = ( useGPS(i) .and. .not. no_wt_gps )

       if (useg .and. gps_type(i).eq.1 ) then

        Rx = xobs - xcalc
        Ry = yobs - ycalc
        Rz = zobs - zcalc
        x2 = sx*sx
        y2 = sy*sy
        z2 = sz*sz
        dc = zero

        do j=1,3
         if (useENU(i,j)) nwdat = nwdat+1
        enddo

c-- vertical        
        if ( useENU(i,3) ) dc = dc + Rz*Rz*fnwt(sz)

        if ( sxy.eq.zero .or. .not. gps_cov ) then
          if ( useENU(i,1) ) dc = dc + Rx*Rx*fnwt(sx)
          if ( useENU(i,2) ) dc = dc + Ry*Ry*fnwt(sy)
        else
         if (useENU(i,1) .and. useENU(i,2)) then
          s2 = sxy*sxy
          a = (one + s2/ ( -s2 + x2 * y2 )) / x2
          b = sxy / (s2 - x2 * y2)
          c = b
          d = one / (-s2 / x2 + y2)
          dc = Rx * (a*Rx + c*Ry) + Ry * (b*Rx + d*Ry)
         endif
        endif

        tchi = tchi +  dc

       endif
       endif
      enddo
      
     
c------------------------------------------------------------
c-- time series displacement data 
c--  estimate new residual average, slope and seasonal signals

      do i=1, num_gps

       call gpsinfo2(i, xx, yy, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sx, sy, sz, sxy)

c       print *, i, gps_type(i)

      if (gps_type(i).eq.3) then
       
      if (useGPS(i) .and. ndx(i,1).gt.0) then
       ksyn=0
       call ts_calc (0, i, ksyn)
       
       n1 = ndx(i,1)
       n2 = ndx(i,2)
       nn = n2-n1+1
       
       do j=1,3
       
c-- estimate offset, slope and seasonal terms for time series 
c-- return the corrected time series    
      call adjust_ts (i, j)
      if ( XVflag(i,j,1) .gt. 0 ) then
             
c-- get new chi**2 misfit         
         do n = n1, n2
           tt = t_disp(n) 
           d = x_disp(n,j) - x_calc(n,j)  
           w = fnwt(x_sig(n,j))
           tchi = tchi +  d*d*w
           nwdat=nwdat+1
         enddo
        endif

       enddo
       
       endif
       endif
      enddo

c------------------------------------------------------------
c-- INSAR line-of-sight displacements
c-- solve for offsets and correct here
c-- solve for tropospheric correction, per Chuck Wicks
c-- dLOS = a + b Elevation
c-- total correction =  A + Bx + Cy +Dz 

       do jnf = 1, MAX_insar_files

c clear all coefficients
       do j= 7, 10
        insar_info(jnf,j) = 0.0d0
       enddo

c how many parameters to get
       npi = insar_corr(jnf)
       np4 = 4

c      if (npi.gt.0 .and. insar_flag(jnf)  ) then
c changed 9-22-2020 to work when no corrections made
      if (insar_flag(jnf)  ) then

       wres = 0.0d0
       sumw = 0.0d0
       call cleareal(pp,4)
       ni = 0

      do i=1, num_insar
       if (insar_file(i).eq.jnf ) then
         ni=ni+1
        
        call ts_calc (1, i, ksyn)

        call getinsarinfo (i, nf, xpt, ypt, zpt, obs, sig, dlos, 
     .     rlos, cobs, corr, Ux, Uy, Uz, sname)

        rtmp(ni) = obs-dlos
        rsig(ni) = sig
        w = fnwt(sig)
        wres = wres + w*rtmp(ni) 
        sumw = sumw + w

        if(npi.gt.1) then
         aa(ni,1) = 1.0d0
         if (npi.eq.3 .or. npi.eq.4) aa(ni,2) = xpt-insar_tmp(jnf,1)
         if (npi.eq.3 .or. npi.eq.4) aa(ni,3) = ypt-insar_tmp(jnf,2)
         if (npi.eq.2) aa(ni,2) = zpt
         if (npi.eq.4) aa(ni,4) = zpt
        endif

       endif
      enddo
        
       if (npi .eq. 1 .or. ni.lt.10) then
         if ( sumw.gt.0.0d0 ) pp(1) = wres/sumw
       else
         call linsolve (ni, MAX_insar_pts, npi, np4, 
     .    aa, rtmp, rsig, pp, pper)
       endif

         insar_info(jnf,7) = pp(1)
         if (npi.eq.3 .or. npi.eq.4) insar_info(jnf,8) = pp(2)
         if (npi.eq.3 .or. npi.eq.4) insar_info(jnf,9) = pp(3)
         if (npi.eq.2) insar_info(jnf,10) = pp(2)
         if (npi.eq.4) insar_info(jnf,10) = pp(4)
       endif
      enddo

c-- now get residuals with corrected values
       do i=1, num_insar
c        call ts_calc (1, i, ksyn)
        call getinsarinfo (i, nf, xpt, ypt, zpt, obs, sig, dlos, 
     .     rlos, cobs, corr, Ux, Uy, Uz, sname)
         
         wt = fnwt(sig)
         d = obs-rlos
         insar_los(i) = rlos
         tchi = tchi +  d*d*wt
         nwdat=nwdat+1
       enddo

      
c------------------------------------------------------------
c-- displacements
       ksyn=0
       do i=1, num_gps
        if( gps_type(i).eq.2 ) then
         call ts_calc (0, i, ksyn)
         dc = zero
         do j=1,3
          if( useENU(i,j) ) then
           U = gps_calc(i, 2, j) - gps_calc(i, 1, j)
           o = gps_obs(i,j)
           xs = gps_obs(i,j+2)
            if (j.eq.3) then
             o = gps_obs(i,6)
             xs = gps_obs(i,7)
            endif
           d = o - U
           w = fnwt(xs)
           dc = dc +  d*d*w
           nwdat=nwdat+1
          endif
         enddo
           tchi = tchi +  dc
        endif
       enddo


c----------------------------------------------------------------
c-- tilt rate data
c        print *, 'num_tilts',num_tilts
        do i=1, num_tilts
          sig=tilt_sig(i)
          if (sig.gt.0) then 
            r = tilt_obs(i) - tilt_calc(i)*1.0e9
            w = fnwt(sig)
            nwdat=nwdat+1
            tchi = tchi +  r*r*w
          endif
        enddo
        
c----------------------------------------------------------------
c-- fault slip rates
      do i=1, num_sr

        call getsr (i, Vx, Vy, Vtot, Vaz, Vz, r, rs)
        nwdat=nwdat+1
        tchi = tchi +  rs*rs

        enddo

c----------------------------------------------------------------
c--  slip vectors
       do i=1, num_sv
        ypt=sv_pos(i,2)
        xpt=sv_pos(i,1)
        sig = sv_sig(i)

        if ( sig.gt.0) then 

c-- slip vectors are the direction of the second block relative to the first
        nfx = kblk_sv(i,1)
        nmv = kblk_sv(i,2)

       call relvel (3, nmv, nfx, xpt, ypt, Ve, Se, Vn, Sn, rho)
       call svaz (ve, vn, az)
       obs = sv_obs(i)
       a360=360.0d0
       call svres (az, obs, sig, r, rs, a360)
       sv_calc(i) = az

c-- fitting function from NUVEL-1
       if ( .not. no_wt_svs ) then
        nwdat=nwdat+1
        tchi = tchi +  rs*rs
       endif

        endif
        enddo


c----------------------------------------------------------------
c-- strain rates
      if(num_ss.gt.0) then
        call getss
        do 20 i=1, num_ss
         nt = ss_type(i)
         nph=3
         if (nt.eq.0) nph=2
          do 20 j=1,nph
           sig = ss_sig(i,j)
           if ( sig.gt.0) then
             r = ss_obs(i,j)-ss_calc(i,j) 
             rs = r/sig
             if (j.eq.3 .and. nt.eq.1) 
     .          rs =  two * sin (r*d2r/two) / (sig*d2r)
             if (j.eq.2 .and. nt.eq.0) 
     .          rs =  two * sin (r*d2r/two) / (sig*d2r)
             nwdat=nwdat+1
             tchi = tchi +  rs*rs
          endif
   20   continue
      endif


c----------------------------------------------------------------
c-- line lengths
      if(num_ll.gt.0) then
        call getll
        do i=1, num_ll
         sig = sig_ll(i)
          if ( sig.gt.0) then
            r = obs_ll(i)-calc_ll(i) 
            rs = r/sig
            nwdat=nwdat+1
            tchi = tchi +  rs*rs
          endif
        enddo
      endif


c----------------------------------------------------------------
c-- rotations
        do i=1, num_rot
          sig=rot_sig(i)
          if (sig.gt.0) then 
            xpt=rot_pos(i,1)
            ypt=rot_pos(i,2)
            sig = rot_sig(i)
            np = npole_block(kblk_rot(i))
           do k=1,9
            p(k) = poles(np,k)
           enddo
            call vertaxrot (xpt, ypt, p, vrot)
            rot_calc(i) = vrot
            r = rot_obs(i) - rot_calc(i)
            nwdat=nwdat+1
            tchi = tchi +  r*r / (sig*sig)
          endif
        enddo

c----------------------------------------------------------------
c-- moment tensors; get factor that gives best fit
      if (invmt) then
       do j=1, nblocks
          ks = nstrain_block(j)
          useg = .false.

          if (nqmt(j).gt.0 .and. ks.gt.0  .and.
     .      block_flag(j) ) then

        do k=1, num_strain_invert
         if(nstrain_invert(k).eq.ks) useg = .true.
        enddo

        if(useg) then

         sumx = zero
         sumy = zero
         sumxy= zero
         sumx2= zero
         rn = 3.0d0
         slope = zero

         do i=1,3
          sumx  = sumx  + strain(ks,i)
          sumx2 = sumx2 + strain(ks,i)**2
          sumy  = sumy  + qms(j,i) 
          sumxy = sumxy + qms(j,i) * strain(ks,i)
         enddo
         den = rn*sumx2 - sumx**2
         if (den .ne. zero ) slope = (rn*sumxy-sumx*sumy)/den
         fmtfac(j) = slope
c        print *, j, fmtfac(j)

c-- get misfits
        if( .not. no_wt_mts ) then
         do i=1,3
          xM = strain(ks,i) * fmtfac(j)
          S = qmtwt
          dM = (qms(j,i) - xM)/S
          nwdat = nwdat + 1
          tchi = tchi + dM**2  
         enddo
        endif

         endif
        endif
       enddo
      endif
c234567
c----------------------------------------------------------------
c-- degrees of freedom        
      kdof = nwdat - nparms 
      kdof = max(1,kdof)
c      print *, 'misfit 2'

      return
      end
      
c**********************************************************************      
      subroutine clearcalc  

c-- clear arrays for calculated values

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"


      if ( .not. ksim_done ) then

c       call cleareal(gps_rot,  5*MAX_gps)
c       call cleareal(gps_str,  2*MAX_gps)
c       call cleareal(gps_net,  5*MAX_gps)

        do  k=1, num_gf_gps
         gps_ela(k,1) = 0.0d0  
         gps_ela(k,2) = 0.0d0
         gps_ela(k,3) = 0.0d0
        enddo

        do  k=1, num_insar
         ins_ela(k,1) = 0.0d0  
         ins_ela(k,2) = 0.0d0
         ins_ela(k,3) = 0.0d0
        enddo

       call cleareal(tilt_calc,  num_tilts)
       call cleareal(rot_calc,   num_rot)
       call cleareal(calc_ll,    num_ll)


        do  k=1, num_ss
         ss_calc(k,1) = 0.0d0  
         ss_calc(k,2) = 0.0d0
         ss_calc(k,3) = 0.0d0
         do j=1,9
          ss_vel(k,j,1) = 0.0d0
          ss_vel(k,j,2) = 0.0d0
         enddo
        enddo

        do  k=1, num_ll
         vel_ll(k,1) = 0.0d0  
         vel_ll(k,2) = 0.0d0
        enddo

      endif


      return
      end

c**********************************************************************      

      subroutine amotsa(a, p,y, psum,mp,np,ndim,pb,yb,ihi,yhi,fac,tt,
     . dchi2,pensum)

      implicit real*8 (a-h,o-z)
      PARAMETER (NMAX=3000)

      dimension p(mp,np),pb(np),psum(np),y(mp)
      dimension ptry(NMAX)

      COMMON /rnd1/ idum

      xdim=real(ndim)
      fac1=(1.0d0-fac)/xdim
      fac2=fac1-fac

      do 11 j=1,ndim
        ptry(j) = psum(j)*fac1-p(ihi,j)*fac2
11    continue

      call solve2(ptry, ytry, dchi2, pensum,maxp)

      if (ytry.le.yb) then

        do 12 j=1,ndim
          pb(j)=ptry(j)
12      continue

        yb=ytry

      endif

      yflu=ytry-tt*dlog(ran1(idum))
      if (yflu.lt.yhi) then
        y(ihi)=ytry
        yhi=yflu
        do 13 j=1,ndim
          psum(j)=psum(j)-p(ihi,j)+ptry(j)
          p(ihi,j)=ptry(j)
13      continue
      endif

      a = yflu

      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.

c************************************************************************

      SUBROUTINE amebsa(p,y,mp,np,ndim,pb,yb,ftol,iter,temptr, 
     .   dchi2, pensum)

      implicit real*8 (a-h,o-z)
      PARAMETER (NMAX=3000)

      dimension p(mp,np),pb(np),y(mp)
      dimension psum(NMAX)
c      logical no

      COMMON /rnd1/ idum

c      no = .false.
      safac = 1.0d0
      two = 2.0d0
      tt=-temptr

1     do 12 n=1,ndim
        sum=0.0d0
        do 11 m=1,ndim+1
          sum=sum+p(m,n)
11      continue
        psum(n)=sum
12    continue

2     ilo=1
      inhi=1
      ihi=2
      ylo=y(1)+tt*dlog(ran1(idum))
      ynhi=ylo
      yhi=y(2)+tt*dlog(ran1(idum))
c          print *, iter, ylo, yhi

      if (ylo.gt.yhi) then
        ihi=1
        inhi=2
        ilo=2
        ynhi=yhi
        yhi=ylo
        ylo=ynhi
      endif

      do 13 i=3,ndim+1
        yt=y(i)+tt*dlog(ran1(idum))
        if(yt.le.ylo) then
          ilo=i
          ylo=yt
        endif
        if(yt.gt.yhi) then
          inhi=ihi
          ynhi=yhi
          ihi=i
          yhi=yt
        else if(yt.gt.ynhi) then
          inhi=i
          ynhi=yt
        endif
13    continue

      rtol=two*abs(yhi-ylo)/(abs(yhi)+abs(ylo))

      if (rtol.lt.ftol .or. iter.lt.0) then
        call swap( y(1), y(ilo))
        do n=1,ndim
          call swap (p(1,n),p(ilo,n))
        enddo
        return
      endif

      iter=iter-2
      s=-safac
        call amotsa(ytry,p,y,psum,mp,np,ndim,pb,yb,ihi,yhi,s,tt, 
     .  dchi2,pensum)
      if (ytry.le.ylo) then
        s=two*safac
        call amotsa(ytry,p,y,psum,mp,np,ndim,pb,yb,ihi,yhi,s,tt,
     .  dchi2,pensum)
      else if (ytry.ge.ynhi) then
        ysave=yhi
        s=safac/two
        call amotsa(ytry,p,y,psum,mp,np,ndim,pb,yb,ihi,yhi,s,tt,
     .  dchi2,pensum)
        if (ytry.ge.ysave) then
          do 16 i=1,ndim+1
            if(i.ne.ilo)then
              do 15 j=1,ndim
                ps = (p(i,j)+p(ilo,j))/two
                psum(j) = ps
                p(i,j)=psum(j)
15            continue
              call solve2(psum, y(i), dchi2, pensum, maxp)
            endif
16        continue
          iter=iter-ndim
          goto 1
        endif
      else
        iter=iter+1
      endif
      goto 2
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.

c************************************************************************
      function rand2 ( )
c* random number generator
      implicit real*8 (a-h,o-z)

      COMMON /rnd1/ idum

c* Numerical Recipes RAN1
       rand2 = ran1(idum)

      return
      end
c************************************************************************

      FUNCTION ran1(idum)
      implicit real*8 (a-h,o-z)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
c      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1= min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.


c************************************************************************

      FUNCTION pythag(a,b)
      implicit real*8 (a-h,o-z)
c      REAL a,b,pythag
c      REAL absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa * dsqrt(1.+(absb/absa)**2)
      else
        if(absb.eq.0.)then
          pythag=0.
        else
          pythag=absb*dsqrt(1.+(absa/absb)**2)
        endif
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.

c**********************************************************************
      subroutine update_trans

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

c* reassign source parameters to transient arrays based on inversion parameters

      do nt = 1, MAX_srce

       if(sflag(nt)) then
       
       kf   = info_source(nt,1)
       kq   = info_source(nt,2)
       kaz  = info_source(nt,3)
       kt   = info_source(nt,4)
       ntau = info_source(nt,7)

c all the transient parameters
      do k = 1, 20
       np = nsource_parm(nt, k) 
       if ( np.gt.0 ) then

c        if (k.ne.13) transient(nt,k) = parm(np)
        transient(nt,k) = parm(np)

c-- k=13 azimuth of slip if being estimated     
c        if (k.eq.13 .and. kaz.eq.1) 
c     .    transient(nt,k) = parm(np)

c-- k=6 and kq=12; A/B ratio
        if (k.eq.6 .and. kq.eq.12) then
         transient(nt,k) = max(parm(np),1.0d0)
         parm(np) = transient(nt,k)
        endif

c-- k=12 and kq=12; dip of Spheroid
        if (k.eq.12 .and. kq.eq.12) then
         transient(nt,k) = parm(np)
         transient(nt,k) = max(transient(nt,k),1.01d0)
         transient(nt,k) = min(transient(nt,k),89.99d0)
         parm(np) = transient(nt,k)
        endif
       endif
      enddo

c-- independent phi source node values     
      if ( kq.eq.1 ) then
      tpmax = 1.0d-20
       do ix = 1, nxf(kf)
        do iz = 1, nzf(kf)
         np = nppt(ix, iz, nt)
         if (np.gt.0) tphi(ix, iz, nt) = parm(np)
         tpmax=max(tpmax,tphi(ix, iz, nt))
        enddo
       enddo
c normalize tphi
       do ix = 1, nxf(kf)
        do iz = 1, nzf(kf)
         tphi(ix, iz, nt) = tphi(ix, iz, nt)/tpmax
        enddo
       enddo
       transient(nt,5) = transient(nt,5)*tpmax       
      endif

c-- 1D Boxcar or 1D Gauss source   
      if ( kq.eq.3 .or. kq.eq.4) then
       do ix = 1, nxf(kf)
        do ipar = 1,3
          np = nppt(ix, ipar, nt)
          if (np.gt.0) tf_parm(nt,ix,ipar) = parm(np)
        enddo
       enddo
      endif

c--  polygon source
      if ( kq.eq.8 ) then
       n = info_source(nt,6)
        do k =1, n
         np = nrad_parm(nt, k) 
         if (np.gt.0) rpoly(nt, k) = parm(np)
        enddo
      endif       

c amplitudes      
c-- triangle time function         
      if (kt.eq.2 .or. kt.eq.5 ) then
        do k = 1, ntau
          np = ntau_parm(nt,k)
          if (np.gt.0) atau(nt, k) = parm(np)
        enddo
      endif
         
       endif
      enddo

      return
      end
       
c**********************************************************************
c** Gaussian 1D function of phi(z)
      function Gauss1D( G, z1, zsig, zn, skew)
       implicit real*8 (a-h,o-z)
        sigfac = 1.0d0
        if (skew .ne. 0.0d0 ) sigfac = skew
        zsig = max(zsig, 0.1d0)
        zsig2 = zsig*sigfac
        Gauss1D = G * exp( -0.5d0*((zn-z1)/zsig)**2)
        if (zn.lt.z1 ) Gauss1D = G*exp(-0.5d0*((zn-z1)/zsig2)**2)
      return
      end

c**********************************************************************
c** Half-Gaussian 1D function of phi(z)
      function HalfGauss1D( G, z1, zsig, zn)
       implicit real*8 (a-h,o-z)
        sigfac = 0.25d0
        zsig = max(zsig, 0.1d0)
        zsig2 = zsig*sigfac
        HalfGauss1D = G * exp( -0.5d0*((zn-z1)/zsig)**2)
        if (zn.lt.z1 ) HalfGauss1D = G*exp(-0.5d0*((zn-z1)/zsig2)**2)
      return
      end

c**********************************************************************
c** Wang function of phi(z)
      function wang( G, z1, z2, zn)
        implicit real*8 (a-h,o-z)
          a = G
          if( G.gt.5.0d0 ) a = G - 10.0d0 
          dz = max( (z2-z1), 0.5d0)
          exp1og = exp( -1.0d0/a)
          omexp1og = 1.0d0 - exp1og
          zz = abs( zn - z1 )/dz
          wang = ( exp(-zz/a) - exp1og ) / omexp1og
          if (z1 .ge. zn ) wang = 1.0d0
          if (z2 .le. zn ) wang = 0.0d0
        return
        end
c**********************************************************************
c boxcar/trapezoid phi(z)
      function Boxcar( A, z1, z2, zn, i1, i2)
c if i1=1 Z1 is free parameter, if i2=1 Z2 is free parameter
c add 3-km taper if free, toget non-zero derivatives
        implicit real*8 (a-h,o-z)
        logical noboxtaper
        noboxtaper = .true.
        
          dz = (z2-z1)/2.0d0
          dz = min (dz, 3.0d0)
          pp = 1.0d0

         if (noboxtaper) then
          if (zn.lt.z1 ) pp = 0.0d0
          if (zn.gt.z2 ) pp = 0.0d0
         else
          if (i1.eq.1 .and. zn.gt.z1 .and. zn.lt.(z1+dz) )  
     .              pp = (zn-z1)/dz
          if (i2.eq.1 .and. zn.gt.(z2-dz) .and. zn.lt.z2 )  
     .              pp = (z2-zn)/dz
         endif

         Boxcar = A*pp

        return
        end
c**********************************************************************
      subroutine update_phi
      
c* re-assign coupling parameters to phi arrays based on inversion parameters

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"
      
      g_err = 0

c** first do transient sources
      call update_trans
      call set_tphi(1)
      
      if (get_flt_parm) then
       do kf = 1, nfault
        kfft = fault_fit_type(kf)
        if (all_fault_0 .or. all_fault_1 .or. all_fault_fix) kfft = 0

c* independent nodes or downdip constraint
       if ( kfft .le. 1) then
        do ix = 1, nxf(kf)
         do iz = 1, nzf(kf)
          if (phi_free(ix,iz,kf)) then
             np = npp(ix, iz, kf)
             phi(ix, iz, kf)=parm(np)
             phi_err(ix, iz, kf)=parm_err(np)
          endif
         enddo
        enddo
       endif

c* Wang interseismic
      if ( kfft.eq.2 ) then
        gmin = psmin(5)
        gmax = psmax(5)

        do ix = 1, nxf(kf)

         np = npp(ix, 1, kf)
         if ( np. gt. 0) then
           parm(np) = rlimit(parm(np), gmin, gmax)
           f_parm(kf,ix,1) = parm(np) 
           f_parm_err(kf,ix,1) = parm_err(np) 
         endif

         np = npp(ix, 2, kf)
         if ( np. gt. 0) then
           f_parm(kf,ix,2) = parm(np)
           f_parm_err(kf,ix,2) = parm_err(np)
         endif

         np = npp(ix, 3, kf)
         if ( np. gt. 0) then
           f_parm(kf,ix,3) = parm(np)
           f_parm_err(kf,ix,3) = parm_err(np)
         endif

         g  = f_parm(kf,ix,1)
         z1 = f_parm(kf,ix,2)
         z2 = f_parm(kf,ix,3)

c         a = g
c         if ( g.gt.5.0d0) a = g - 10.0d0 
c         exp1og = exp( -1.0d0/a)
c         omexp1og = 1.0d0 - exp1og
c         dz = max( (z2 - z1), 0.5d0)

         do iz = 1, nzf(kf)
           zn = znode(iz,kf)
           phi(ix, iz, kf) = wang( G, z1, z2, zn)

c           zz = ( zn - z1 )/dz
c           s = ( exp(-zz/a) - exp1og ) / omexp1og
c           phi(ix, iz, kf) = s
c           if (z1 .ge. zn ) phi(ix, iz, kf)=1.0d0
c           if (z2 .le. zn ) phi(ix, iz, kf)=0.0d0
         enddo

        enddo
      endif

c** others

      if (kfft.eq.3 .or. kfft.eq.4 .or. kfft.eq.6) then

        do ix = 1, nxf(kf)
         if (phi_free(ix,1,kf)) then
          np2=0
          np3=0
         np = npp(ix, 1, kf)
         if ( np. gt. 0) then
           f_parm(kf,ix,1) = parm(np) 
           f_parm_err(kf,ix,1) = parm_err(np) 
           G_err  = parm_err(np)
         endif

          np = npp(ix, 2, kf)
          if ( np. gt. 0) then
            f_parm(kf,ix,2) = parm(np)
            f_parm_err(kf,ix,2) = parm_err(np)
            np2 = 1
          endif

          np = npp(ix, 3, kf)
          if ( np. gt. 0) then
            f_parm(kf,ix,3) = parm(np)
            f_parm_err(kf,ix,3) = parm_err(np)
            np3 = 1
          endif

          G  = f_parm(kf,ix,1)
          z1 = f_parm(kf,ix,2)
          z2 = f_parm(kf,ix,3)

          do iz = 1, nzf(kf)
           zn = znode(iz,kf)

c* boxcar / trapezoid
           if (kfft.eq.3) then
             phi(ix, iz, kf) = Boxcar(G, z1,z2,zn, np2,np3)
           endif

c* 1D Gaussian distribution Amplitude, mean depth, sigma depth
           if (kfft.eq.4) then
             phi(ix, iz, kf) = Gauss1D( G, z1, z2, zn, Gskew(kf) )
              if ( get_phi_err) then
               phi_err(ix, iz, kf) = dsqrt( e*e * G_err*G_err)
              endif
           endif

c* cosine taper distribution top depth, bottom depth
           if (kfft.eq.5) then
              dz = max(z2-z1, 0.1d0)
              dz2 = zn - z1
              s = 0.5d0 * ( dcos ( pii* dz2/dz ) + 1.0d0 )
              if ( zn .le. z1 ) s = psmin(4)
              if ( zn .ge. z2 ) s = psmax(4)
              phi(ix, iz, kf) = s
           endif

c* Half-Gaussian distribution Amplitude, mean depth, sigma depth
           if (kfft.eq.6) then
             phi(ix, iz, kf) = HalfGauss1D(G, z1, z2, zn)
             if ( get_phi_err) then
              phi_err(ix, iz, kf) = dsqrt( e*e * G_err*G_err)
             endif
           endif
c iz
          enddo

cphi free
         endif

cix
        enddo

c fit type
       endif

c kf
      enddo

      if ( get_phi_err .and. getcovariance) call phisigma
      
      endif

      return
      end
c**********************************************************************
c* get sigmas for phi values by MC
      subroutine phisigma

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"
      logical flock

      print *, 'Calculating phi sigmas'

      ntrials=10000

      do kf = 1, nfault
       kfft = fault_fit_type(kf)

       if (flock(kf).and.(kfft.eq.2 .or. kfft.eq.3 .or.
     .    kfft.eq.4 .or. kfft.eq.6))then

c* Gaussian distribution Amplitude, mean depth, sigma depth
c* or Wang distribution

      do ix=1,nxf(kf)
         A     = f_parm(kf,ix,1)
         Aerr  = f_parm_err(kf,ix,1)
         Z1    = f_parm(kf,ix,2)
         Z1err = f_parm_err(kf,ix,2)
         Z2    = f_parm(kf,ix,3)
         Z2err = f_parm_err(kf,ix,3)

       do iz=1,nzf(kf)
         Zn = znode(iz,kf)
         sum=0.0d0
         sum2=0.0d0
         ng=0

c Monte Carlo uncertainties
      do i = 1,ntrials
       call normal ( Ar,  A, Aerr) 
       call normal (Z1r, Z1, Z1err) 
       call normal (Z2r, Z2, Z2err)

c* Wang
       if ( kfft.eq.2 .and. Z2r.gt.Z1r .and. 
     .      Ar.gt.0.0d0 .and. Ar.lt.10.0d0) then

            s = wang( Ar, z1r, z2r, Zn)

          if ( s .ge. psmin(4) .and. s .le. psmax(4) ) then
           sum = sum + s
           sum2 = sum2+ s*s
           ng=ng+1
          endif
       endif

c* 1D Boxcar phi(z)
       if ( kfft.eq.3 .and. Z2r.gt.0.0d0) then 
         s = Boxcar( Ar, z1r, z2r, zn, 0, 0 )
         if ( s .ge. psmin(4) .and. s .le. psmax(4) ) then
          sum = sum + s
          sum2 = sum2+ s*s
          ng=ng+1
         endif
       endif

c* 1D Gaussian phi(z)
       if ( kfft.eq.4 .and. Z2r.gt.0.0d0) then 
         s = Gauss1D( Ar, z1r, z2r, zn, Gskew(kf) )
         if ( s .ge. psmin(4) .and. s .le. psmax(4) ) then
          sum = sum + s
          sum2 = sum2+ s*s
          ng=ng+1
         endif
       endif

c* 1D Half=Gaussian phi(z)
       if ( kfft.eq.6 .and. Z2r.gt.0.0d0) then 
         s = HalfGauss1D( Ar, z1r, z2r, zn)
         if ( s .ge. psmin(4) .and. s .le. psmax(4) ) then
          sum = sum + s
          sum2 = sum2+ s*s
          ng=ng+1
         endif
       endif
       enddo

      call meansd (ng, sum, sum2, S, Ser, sdmean) 
      phi_err(ix, iz, kf) = Ser

       enddo
      enddo

       endif
      enddo

      return
      end
c**********************************************************************
      subroutine swap(x,y)
c  swap x and y
      implicit real*8 (a-h,o-z)
         tmp=x
         x=y
         y=tmp
      return
      end
c**********************************************************************
      subroutine iswap(ix,iy)
c  swap integers ix and iy
         itmp=ix
         ix=iy
         iy=itmp
      return
      end
c**********************************************************************
      subroutine bilinterp (F, dx, dy, F0, dF) 

c'-- bilinear interpolate within a 4-corner figure, F() gives values at corners, clockwise
c'   dx is distance in X direction, dy is distance in Y direction,
c'-- dx,dy comes in 0 to +1 range, must convert to -1 to +1 range as x,y
c'   dF is relative contribution of each corner

      implicit real*8 (a-h,o-z)
      dimension f(4), df(4)

      data r4, r2, r1, r0 /4.0d0, 2.0d0, 1.0d0, 0.0d0/
      
      x= r2 * dx - r1
      y= r2 * dy - r1
      F0 = r0
      
      xy=x*y
      dF(1)=(r1-x-y+xy)/r4
      dF(2)=(r1+x-y-xy)/r4
      dF(3)=(r1+x+y+xy)/r4
      dF(4)=(r1-x+y-xy)/r4

      do i=1,4
       F0 = F0 + dF(i)*F(i)
      enddo

      return
      end 
      
c**********************************************************************
      subroutine interp4 (f, i, j, nix, niy, fout ) 
      
c'-- find 4 corners of sub-grid box
      
      implicit real*8 (a-h,o-z)
      dimension f(4), fout(4), d(4)

      rnx=real(nix)
      rny=real(niy)

      xi = real(i)
      xj = real(j)

      dx=(xi-1.d0)/rnx
      dy=(xj-1.d0)/rny
      call bilinterp ( f, dx, dy, fout(1), d )
      
      dx=xi/rnx
      dy=(xj-1.d0)/rny
      call bilinterp ( f, dx, dy, fout(2), d )
      
      dx=xi/rnx
      dy=xj/rny
      call bilinterp ( f, dx, dy, fout(3), d )
      
      dx=(xi-1.d0)/rnx
      dy=xj/rny
      call bilinterp ( f, dx, dy, fout(4), d )
      
      return
      end 
      
c**********************************************************************
      
      subroutine quadarea (xx, yy, barea) 
c'-- area of arbitrary quadrilateral in 2D, pts input clockwise 
c    around perimeter

      implicit real*8 (a-h,o-z)
      dimension xx(4), yy(4), x(4), y(4)

      x0=xx(1)
      y0=yy(1)

c* convert to cartesian
      do i=1,4
       call project(xx(i), yy(i), x0, y0, x(i), y(i))
      enddo

c* break into 2 triangles
      b = abs( x(1)*(y(2)-y(3)) + x(2)*(y(3)-y(1)) + x(3)*(y(1)-y(2)) )
      b = b+ abs( x(1)*(y(4)-y(3))+x(4)*(y(3)-y(1)) + x(3)*(y(1)-y(4)))

      barea=b/2.0d0
      return
      end
      
c**********************************************************************
      subroutine getu1u2u3 (k3d, strike, dip, Ux, Uy, Uz, U1, U2, 
     .    U3, Utot, frake )

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

c      logical k3d

c-- rotate input slip vector Ux,Uy from horizontal Earth coords (X,Y) to fault coords
c   U1 is strike-slip component - left-lateral is positive
c   U2 is dip slip component    - thrust is positive
c   U3 is tension component     - tension is positive

c  frake is the rake angle following Aki&Richards p. 106
c   frake =  90         thrust
c   frake = -90 or 270  normal
c   frake =   0 or 360  sinstral, left-lateral
c   frake = 180 or -180 dextral, right-lateral
      
        a = strike * d2r 
        d = dip * d2r

c-- Y-axis is North, rotate it so it is along strike
c-- rotate clockwise around Z-axis by angle strike
        Uxp=  Ux*dcos(a) - Uy*dsin(a)
        Uyp=  Ux*dsin(a) + Uy*dcos(a)
        Uzp = Uz
      
c-- rotate clockwise around Y' axis by angle dip
c-- X'' (=U2) axis becomes downdip and Z'' (=U3) axis perpendicular to fault plane
c-- Y'' (=U1) axis still along strike
c-- if its only a shear fault (not 3D), the fault-normal component is 0.0
      
      if ( k3d.eq.0 ) then 
       U1 =  Uyp
       U2 = -Uxp
       U3 = 0.0d0
       frake = fn360(datan2 (u2,u1)*r2d + 180.0d0)
      endif

      if ( k3d.eq.1 ) then 
       U1 =    Uyp
       U2 =   -Uxp * dcos(d) + Uzp * dsin(d)
       U3 =    Uxp * dsin(d) + Uzp * dcos(d)
       frake = fn360(datan2 (u2,u1)*r2d + 180.0d0)
      endif
      
      if ( k3d.eq.2 ) then 
       U1 = 0.0d0
       U2 = 0.0d0
       U3 = Uz
       frake = 0.0d0
      endif

c total slip rate
      Utot = dsqrt(U1*U1 + U2*U2 +U3*U3 )


      return
      end 



c*********************************************************************
      subroutine normal (value, xmean, sdev)

      implicit real*8 (a-h,o-z)
      COMMON /rnd1/ idum

c  generate a random number with Normal distribution
c   rand must generate a uniform random number

      xm = 0.0d0
      six = 6.0d0

      do i = 1, 12
        xm = xm + ran1(idum)
      enddo

      value = ( xm - six) * sdev + xmean

      return
      end 
c'=======================================================================
      subroutine meansd (nin, sum, sum2, average, sig, sdmean)

      implicit real*8 (a-h,o-z)

          average = 0.0d0
          sig     = 0.0d0
          sdmean  = 0.0d0
          rn = nin

          if ( nin.eq.0 ) return

          if (nin.eq.1) then
            average=sum

          elseif (nin.gt.1) then 
c'mean
            average = sum/rn 

c' standard deviation
            rn2=rn*rn
            sig  = dsqrt( (rn*sum2 - sum*sum)/rn2 )

c' standard deviation of the mean
            sdmean = sig / dsqrt(rn)

          endif
      return
      end
c'=======================================================================
      subroutine meansd2 (nin, s, average, sig)

      implicit real*8 (a-h,o-z)

      dimension s(nin)

          average = 0.0d0
          sig     = 0.0d0
          rn = real(nin)
          sum = 0.0d0
          sum2 = 0.0d0

          if (nin.eq.1) then
            average=s(1)

          elseif (nin.gt.1) then 
c'mean
          do i=1, nin
            sum = sum + s(i)
          enddo
          
          average = sum/rn 

c' standard deviation
          do i=1, nin
            sum2 = sum2 + ( s(i) - average )**2
          enddo
          
          sig  = dsqrt( sum2/rn )

          endif
      return
      end

c**********************************************************************
      subroutine covelip1( exx, eyy, exy, emin, emax, alphmin, alphmax)

c''-- gets error/strain ellipse principal axes from 2d covariance matrix
c'  based on Charles Williams email
c' tested against covelips which uses jacobi
      
c' exx, eyy, exy are strain/covariance tensor
      
      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      z=0.0d0
      a90 = 90.0d0
      half = 5.0d-1
      two=2.0d0
      po2 = pii/two
      
      emin=z
      emax=z
      alphmin=z
      alphmax=z
      
      if ( exy.eq.z) then

       if (exx .ge. eyy) then 
         emax=exx
         emin=eyy
         alphmin=z
         alphmax=a90
       endif

       if (eyy.gt.exx) then 
         emax=eyy
         emin=exx
         alphmin=a90
         alphmax=z
       endif

       return
      endif
      
      se2 = exx
      sn2 = eyy
      a12 = exy
      twoa12 = two*a12
      ds = sn2-se2
      ang1 = half * ( po2 - datan2 (ds, twoa12) )
      
      sa=sin(ang1)
      ca=cos(ang1)
      
      if ( sa.eq.z .or. ca.eq.z) then
       e1=-1.0d10
      else
       e1=half*(se2 + sn2 - a12/(sa*ca) )
      endif
      
      e2= se2 + sn2 - e1
      ang2 = ang1 + po2
      
      if ( e1.gt.e2 ) then
       emax = e1
       emin = e2
       alphmax = ang1
       alphmin = ang2
      else
       emax = e2
       emin = e1
       alphmax = ang2
       alphmin = ang1
      endif

      alphmin= -( a90 - alphmin*r2d)
      alphmax= -( a90 - alphmax*r2d)

      return
      end 


c**********************************************************************
c** check node bounds for fault
c**********************************************************************

      subroutine check_f_bounds (kf, nx, nz)

      implicit real*8 (a-h,o-z)
      include 'tdefcom1.h'
      include 'tdefcons.h'

          if (nx.lt.1 ) nx = 1
          if (nx.gt. nxf(kf) ) nx = nxf(kf) 
          if (nz.lt.1 ) nz = 1
          if (nz.gt. nzf(kf) ) nz = nzf(kf) 

      return
      end

c**********************************************************************
c** get InSAR information
c**********************************************************************

      subroutine getinsarinfo (isite, nf, xpt, ypt, zpt, obs, sig,  
     .     dlos, clos, cobs, corr, Ux, Uy, Uz, sname)

      implicit real*8 (a-h,o-z)
      include 'tdefcom1.h'
      include 'tdefcons.h'

      character sname*4
 
       ii = isite
       nf = insar_file(ii)

        xpt = insar_pos(ii,1)
        ypt = insar_pos(ii,2)
        zpt = insar_pos(ii,3)
 
        sname = insar_sname(ii)
        obs = insar_obs(ii,1)
        sig = insar_obs(ii,2)

        Ux = insar_calc(ii, 2, 1) - insar_calc(ii, 1, 1)
        Uy = insar_calc(ii, 2, 2) - insar_calc(ii, 1, 2)
        Uz = insar_calc(ii, 2, 3) - insar_calc(ii, 1, 3)

c displacement LOS         
c 
c flip so negative dLOS is negative if ground motion toward satellite

         dlos = -insar_unit(ii,1)*Ux - insar_unit(ii,2)*Uy 
     .       - insar_unit(ii,3)*Uz 

         corr = insar_info(nf,7)
     .       + (xpt-insar_tmp(nf,1))*insar_info(nf,8)
     .       + (ypt-insar_tmp(nf,2))*insar_info(nf,9)
     .       + zpt*insar_info(nf,10)

     
c corrected LOS
         clos = dlos + corr

c corrected obs 
         cobs = obs - corr

     

      return
      end
c**********************************************************************
c** make profile line files
c**********************************************************************

      subroutine defprofiles

      implicit real*8 (a-h,o-z)
      include 'tdefcom1.h'
      include 'tdefcons.h'

      dimension volc_pos(2000,2)
      dimension ulines(18,1000), xtmp(5000,2)
      dimension iftmp(5)
      dimension rx9(MAX_gps,2), tx9(MAX_gps,2), ux9(MAX_gps,2),
     .          r9(MAX_gps), t9(MAX_gps), u9(MAX_gps),
     .          rw9(MAX_gps), tw9(MAX_gps), uw9(MAX_gps),
     .          pp(2), pper(2)

      character c2*3, c1*1, c1a*1, quote*1, bfile*80, c3*3, c4*4
      character*4 sname, block_name
      logical useGPS, useENU, fexist, fend

c* commons for profiles
      character*80 volcfile, quakefile
      common /vq1/ volcfile, quakefile(MAX_qfiles)
c      common /vq2/ num_quakefiles


      quote = char(039)

      x90=90.0d0
      i2 = 2
      xm3 = 1.0d3

c'-- data for line types

      if(nlines.eq.0 ) return

      print *, 'Writing profiles '

       call fopen (k23, 1, '_profiles.out ')

      if ( read_votw ) then
        call readvolcs(volcfile, volc_pos, nvolcs)
      endif

c*** write calculated profile lines
      
      do 112 j = 1, nlines

      print *, 'Processing profile line ', prof_num(j)

       call i2c(prof_num(j), 3, c2)

c* profile output
       call fopen (k22, 1, '_p'//c2//'.out ')

       azd = x90 - prof_az(j)
       azr=azd*d2r
       sa=dsin(azr)
       ca=dcos(azr)

       x0 = prof_pos(kfirst_point(j),1)
       y0 = prof_pos(kfirst_point(j),2)

       aze = -prof_az(j)
       xp = real(prof_n(j))
       midpt = int(xp/2.0d0)
       umin = 1.0d12

        do 111 i=1, prof_n(j)
         k = i -1 + kfirst_point(j)
         x=prof_pos(k,1)
         y=prof_pos(k,2)

         if ( i.eq.midpt) then
          olono = x
          olato = y
         endif

         call distkm(x, y, x0, y0, d)

c rotation component
         uxr = u_lines(k,1,1) 
         uyr = u_lines(k,2,1) 
         rot_rad =   uxr*ca + uyr*sa
         rot_trans= -uxr*sa + uyr*ca
 
c elastic slip component
         uxe = u_lines(k,1,2) 
         uye = u_lines(k,2,2) 
         slp_rad=    uxe*ca + uye*sa
         slp_trans= -uxe*sa + uye*ca
 
c strain component
         uxs = u_lines(k,1,3) 
         uys = u_lines(k,2,3) 
         str_rad=    uxs*ca + uys*sa
         str_trans= -uxs*sa + uys*ca
c total
         ux = uxr + uxe + uxs
         uy = uyr + uye + uys
         uz = u_lines(k,3,2)

         uh = dsqrt (ux*ux + uy*uy)
         urad=    ux*ca + uy*sa
         utrans= -ux*sa + uy*ca
         uaz=fnstriker(ux,uy)

      ulines(1,i)= x
      ulines(2,i)= y
      ulines(3,i)= d
      ulines(4,i)= ux
      ulines(5,i)= uy
      ulines(6,i)= uz
      ulines(7,i)= uh
      ulines(8,i)= urad
      ulines(9,i)= utrans
      ulines(10,i)= uaz
c     spot 11 reserved for tilt rate
      ulines(12,i) = rot_rad
      ulines(13,i) = rot_trans
      ulines(14,i) = slp_rad
      ulines(15,i) = slp_trans
      ulines(16,i) = str_rad
      ulines(17,i) = str_trans
c     spot 18 reserved for horizontal strain rate along profile

      do ii = 4,9
       umin=min(umin, ulines(ii,i))
      enddo
      
  111 continue

c* pdist is offset between profile X and that from the srproject subroutine which uses the
c   center of profile as reference point
      call distkm (olono, olato, x0, y0, pdist)

c* calculate tilt and strain rates
      do i=1, prof_n(j)
       if (i.eq.1) then
        n2=2
        n1=1
       else if (i.eq.prof_n(j)) then
        n2=i
        n1=i-1
       else
        n2=i+1
        n1=i-1
       endif

       tilt = (ulines(6,n2)-ulines(6,n1))/(ulines(3,n2)-ulines(3,n1))
       ulines(11,i) = tilt*1.0d3
       exx = (ulines(8,n2)-ulines(8,n1))/(ulines(3,n2)-ulines(3,n1))
       ulines(18,i) = exx*1.0d3

c* write to file       
       write(k22, 113) (ulines(k,i), k=1,18)

c* write to test file
c       write(ktest, 1131) (ulines(k,i), k=1,2),
c     .   (ulines(k,i), k=14,15), ulines(6,i)
c 1131 format(5f12.3)

      enddo
  113 format('C',2f12.4, 16f12.3)

      prof_length = ulines(3,prof_n(j))
      
c do GPS
      if (num_gps.gt.0) then

      xnr=0.0d0
      sumr2 = 0.0d0
      n9r = 0
      n9t = 0
      n9u = 0

      r_rnrms = 0.0d0
      t_rnrms = 0.0d0
      u_rnrms = 0.0d0


      do 50 i=1, num_gps

      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)
      call gpslongname(i)

      if (useGPS(i) .and. gps_type(i).eq.1) then
      
      call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
      dpxx = dpxx + pdist

      if (dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .   .and. dpxx .ge. 0.0d0 ) then

      ux=xobs
      uy=yobs
      uxs=sigx
      uys=sigy
      resx=xobs-xcalc
      resy=yobs-ycalc
      resz=zobs-zcalc
      azv=fnstriker(ux,uy)
      azv_sig = 0.0

c* for Nrms
      xnr = xnr + 2.0d0
      sumr2 = sumr2 + (resx/sigx)**2 + (resy/sigy)**2

c total vector and sigma
      u2 = dsqrt( ux*ux + uy*uy )
      u2s= dsqrt( uxs*uxs + uys*uys)

c  components of velocity
      u_rad=     ux*ca + uy*sa
      u_trans=  -ux*sa + uy*ca
      
c  components of residual
      r_rad   =   resx*ca + resy*sa
      r_trans =  -resx*sa + resy*ca
      r_up    =   resz

      
c' get component of error ellipse in profile

        aa=max(sigx*sigx, 1.0d-3) 
        bb=max(sigy*sigy, 1.0d-3)

        ae = azr
        xe = one / ( one/aa + (tan(ae)**2)/bb )
        ye = ( one - xe/aa) * bb
        sig_rad = dsqrt( xe + ye )

        ae = azr - 90.0d0*d2r
        xe = one / ( one/aa + (tan(ae)**2)/bb )
        ye = ( one - xe/aa) * bb
        sig_trans = dsqrt( xe + ye )

c Nrms of components
       if ( useENU(i,1) .and. useENU(i,2) ) then
        r_rnrms = r_rnrms + (r_rad/sig_rad)**2
        t_rnrms = t_rnrms + (r_trans/sig_trans)**2
       endif

       if ( useENU(i,3) ) u_rnrms = u_rnrms + (r_up/sigz)**2


c store info for slope calculations
        if ( useENU(i,1) .and. useENU(i,2) ) then
           n9r = n9r + 1
           r9(n9r) = u_rad
           rw9(n9r) = sig_rad
           rx9(n9r,1) = dpxx
           rx9(n9r,2) = 1.0d0

           n9t = n9t + 1
           t9(n9t) = u_trans
           tw9(n9t) = sig_trans
           tx9(n9t,1) = dpxx
           tx9(n9t,2) = 1.0d0
        endif

        if ( useENU(i,3) ) then
           n9u = n9u + 1
           u9(n9u) = zobs
           uw9(n9u) = sigz
           ux9(n9u,1) = dpxx
           ux9(n9u,2) = 1.0d0
        endif

        write (k22, 49) xpt, ypt, dpxx, 
     .    (int(gps_info(gps_index(i),k+10)), k=1,3),
     .    ux, uxs, uy, uys, zobs, sigz,
     .    u_rad, sig_rad, u_trans, sig_trans, 
     .    xcalc, ycalc, zcalc, 
     .    resx, resy, resz, u2, u2s, azv, azv_sig,
     .    dpyy, longname

 49   format ('G',2f12.4,f12.3,3i3,21f12.3, 1x, a19)

       umin=min(umin,ux)
       umin=min(umin,uy)
       umin=min(umin,u2)
       umin=min(umin,u_rad)
       umin=min(umin,u_trans)

      endif

      endif
      
 50   continue

        sumt2 = 0.0d0
        sumr2 = 0.0d0
        sumu2 = 0.0d0

c get slopes
c transverse
       if (n9t.gt.0) then
        call linsolve (n9t, MAX_gps, i2, i2, tx9, t9, tw9, pp, pper)
        tslope = pp(1)*xm3
        tb = pp(2)
        terr = pper(1)*xm3

        write(k23,'("Profile line ",i3, " transverse")' ) prof_num(j)
        write (k23, *) '     X    Obs  Calc   Res    Wt '
        do i=1,n9t
          c = tb + tx9(i,1)*tslope/xm3
          r = t9(i)-c
          sumt2 = sumt2 + (r/tw9(i))**2
          write(k23, '(5f8.2)') tx9(i,1), t9(i), c, r, tw9(i) 
        enddo
       endif

c radial
       if (n9r.gt.0) then
        call cleareal (pp,2)
        call cleareal (pper,2)
        call linsolve (n9r, MAX_gps, i2, i2, rx9, r9, rw9, pp, pper)
        rslope = pp(1)*xm3
        rb = pp(2)
        rerr = pper(1)*xm3
        write(k23,'("Profile line ",i3, " radial")' ) prof_num(j)
        write (k23, *) '     X    Obs  Calc   Res    Wt '
        do i=1,n9r
          c = rb + rx9(i,1)*rslope/xm3
          r = r9(i)-c
          sumr2 = sumr2 + (r/rw9(i))**2
          write(k23, '(5f8.2)') rx9(i,1), r9(i), c, r, rw9(i) 
        enddo
       endif

c vertical
       if (n9u.gt.0) then
        call cleareal (pp,2)
        call cleareal (pper,2)
        call linsolve (n9u, MAX_gps, i2, i2, ux9, u9, uw9, pp, pper)
        uslope = pp(1)*xm3
        ub = pp(2)
        uerr = pper(1)*xm3
        write(k23,'("Profile line ",i3, " vertical")' ) prof_num(j)
        write (k23, *) '     X    Obs  Calc   Res    Wt '
        do i=1,n9u
          c = ub + ux9(i,1)*uslope/xm3
          r = u9(i)-c
          sumu2 = sumu2 + (r/uw9(i))**2
          write(k23, '(5f8.2)') ux9(i,1), u9(i), c, r, uw9(i) 
        enddo
       endif

      tNrms = 0.0d0
      rNrms = 0.0d0
      uNrms = 0.0d0

      if ( n9r.gt.0) rNrms = dsqrt ( sumr2/real(n9r) )
      if ( n9t.gt.0) tNrms = dsqrt ( sumt2/real(n9t) )
      if ( n9u.gt.0) uNrms = dsqrt ( sumu2/real(n9u) )

c residual Nrms
        if (n9r.gt.0) r_rnrms = dsqrt (r_rnrms / real(n9r) )
        if (n9t.gt.0) t_rnrms = dsqrt (t_rnrms / real(n9t) )
        if (n9u.gt.0) u_rnrms = dsqrt (u_rnrms / real(n9u) )

 307  format ("#ProfileRTU ",i4, 3(i5, 4f8.3) )


       write (k22, 307) prof_num(j), 
     .   n9r, rslope, rerr, rNrms, r_rnrms,
     .   n9t, tslope, terr, tNrms, t_rnrms, 
     .   n9u, uslope, uerr, uNrms, u_rnrms 
          
       write (k23, 307) prof_num(j), 
     .   n9r, rslope, rerr, rNrms, r_rnrms,
     .   n9t, tslope, terr, tNrms, t_rnrms, 
     .   n9u, uslope, uerr, uNrms, u_rnrms 

      endif 
c--- end of GPS
      
c-- do Insar
       
      do i=1, num_insar
 
      call getinsarinfo (i, nf, xpt, ypt, zpt, obs, sig, dlos, 
     .     rlos, cobs, corr, Ux, Uy, Uz, sname)

      call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
       dpxx = dpxx + pdist

      if (dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .   .and. dpxx .ge. 0.0d0 ) then

       write (k22, 497) xpt, ypt, dpxx, obs, sig, rlos, obs-rlos, dpyy,
     .   sname, insar_fname(nf), block_name (nblock_insar(i))

       endif
      enddo

 497   format ('I', 2f12.4, 6f12.3, 3(1x, a4) )

c-- end of Insar


c  tilts
       if (num_tilts.gt.0 ) then
        do i=1, num_tilts
          xpt =(tilt_pos(i,1,1)+tilt_pos(i,2,1))/2.
          ypt =(tilt_pos(i,1,2)+tilt_pos(i,2,2))/2.

      call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
      dpxx = dpxx + pdist

      if ( dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .   .and. dpxx .ge. 0.0d0) then
          obs = tilt_obs(i)
          s = tilt_length(i)
          sig = tilt_sig(i)
          tilt = tilt_calc(i)*1.0d9

          write (k22, 494) 'T',xpt,ypt,dpxx, obs, sig, tilt, dpyy
      endif

        enddo
       endif
 494   format (a1, 2f12.4,5f12.3)


c'-- plot azimuth data
      
      if (num_sv.gt.0) then

      do 57 i=1,num_sv
       xpt = sv_pos(i,1)
       ypt = sv_pos(i,2)

       call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
       dpxx = dpxx + pdist

      if ( dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .   .and. dpxx .ge. 0.0d0) then

       obs= sv_obs(i)
       sig=sv_sig(i)

       write (k22, 492) sv_pos(i,1),sv_pos(i,2),dpxx, obs, sig, dpyy

      endif
 57   continue
      endif
 492  format ('S',2f12.4,4f12.3)

c node information along profile
c loop through profile points and find phi, vphi
c      do 113 i=1, prof_n(j)
c         k = i -1 + kfirst_point(j)
c         x=prof_pos(k,1)
c         y=prof_pos(k,2)
c 113  continue


      
c for individual faults
      nodes_on_profile = .false.
      if ( nodes_on_profile ) then
      if ( fault_atr .and. myflag ) then
        kf=1
c profile width 
        pw2 = 10.0d0
        call i2c (kf, 3, c3)
        call fopen (katr2, 1,  '_flt_'//c3//'_atr.gmt ') 
         do in = 1, 32000
         read(katr2, 107,end=222 ) c4,
     .    kf2, uslip, p, pe, U1s, U2s, U3s, frake, xpt, ypt, zpt,
     .    ix, iz, ii, jj, area, patch_Mom, patch_err
          do kt=1,4
            read(katr2, 106,end=222) xsub, ysub, zsub, xww, www
          enddo

         call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
         dpxx = dpxx + pdist

        if (dpxx .le. prof_length .and. abs(dpyy) .le. pw2 
     .     .and. dpxx .ge. 0.0d0 ) then
          pv = uslip*p
          write (k22, 499) xpt, ypt, zpt, dpxx, dpyy, kf2, ix, iz, 
     .                     p, pe, pv
         endif

       enddo
 222   ik = kfclose(katr2)

      endif
      endif
 107  format(a4,i5,9f10.3,f7.2, 4i4, 1x, 3(1x,e12.4))
 106  format(5f16.8)
 499  format ('O', 2f12.4, 3f8.2, 3i3, 3f8.3 )

c-- do fault nodes on profile
      nodes_on_profile = .false.
      if ( nodes_on_profile ) then
      do kf = 1, MAX_f
       if(fflag(kf,1)) then
       do iz = 1, nzf(kf)
        do ix = 1, nxf(kf)
         xpt = xynode(1,ix,iz,kf)
         ypt = xynode(2,ix,iz,kf)
         zpt = znode(iz,kf)
         call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
         dpxx = dpxx + pdist

        if (dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .     .and. dpxx .ge. 0.0d0 ) then
          pxE = slip_n(ix,iz,kf,1)
          pxN = slip_n(ix,iz,kf,2)
          p = phi(ix,iz,kf)
          px = pxE * p
          py = pxN * p
          pe = phi_err(ix,iz,kf)
          pv = dsqrt(px*px+py*py)
          write (k22, 498) xpt, ypt, zpt, dpxx, dpyy, kf, ix, iz, 
     .                     p, pe, px, py, pv
         endif
        enddo
       enddo
      endif
      enddo
      endif

 498   format ('N', 2f12.4, 3f8.2, 3i3, 2f8.3, 3f8.2 )

c-- end of Nodes


c    PLOT VOLCS
      if (nvolcs.gt.0 .and. read_votw ) then

      zero=0.0

      do 75 i=1,nvolcs

      xpt = volc_pos(i,1)
      ypt = volc_pos(i,2)

      call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
      dpxx = dpxx + pdist

      if (dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .   .and. dpxx .ge. 0.0d0) then

       write (k22, 2 ) volc_pos(i,1), volc_pos(i,2), dpxx, dpyy
  2   format( 'V', 2f12.4, 2f12.3)

      endif
 75   continue
      endif


c earthquakes from file
c format longitide, latitude, depth (km)
c no filtering is done

      if (quakes) then
      do numq = 1, MAX_qfiles

      call existfile(quakefile(numq), fexist,0)
      if ( fexist) then
c       print *, 'Reading ', quakefile(numq)

      k33=kfopen(33)
      open (k33, file=quakefile(numq))

c      do i=1,300000

 22   read(k33, *, end=33) qx,qy,qz

      call srproject ( qx, qy, olato, olono, aze, dpyy, dpxx)
      dpxx = dpxx + pdist

      if (dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .   .and. dpxx .ge. 0.0d0) then

       write (k22, 3 ) qx,qy, dpxx, qz, dpyy

      endif

      goto 22
c      enddo
  3   format( 'Q', 2f12.4,3f12.3)
      
 33   ik = kfclose(k33)

      endif
      enddo

      endif

c****** slip rate data
      
      if (num_sr.gt.0) then

      do i=1,num_sr

      xpt = sr_pos(i,1)
      ypt = sr_pos(i,2)

      call srproject ( xpt, ypt, olato, olono, aze, dpyy, dpxx)
      dpxx = dpxx + pdist

      if (dpxx .le. prof_length .and. abs(dpyy) .le. prof_width(j) 
     .   .and. dpxx .ge. 0.0d0) then

       obs = sr_obs(i,1)
       sig = sr_sig(i)

       write (k22, 491) 'R',sr_pos(i,1),sr_pos(i,2), dpxx, obs, sig, 
     .  sr_calc(i), dpyy

 491   format (a1, 2f12.4,5f12.3, 1x, a8, 1x, a30, f12.3)

      endif
      enddo
      endif

c-----------------------------------------------------
c intersection of profile with block boundaries

        do i = 1, prof_n(j)-1
         k1 = i - 1 + kfirst_point(j)
         k2 = i + kfirst_point(j)
         x1=prof_pos(k1,1)
         y1=prof_pos(k1,2)
         x2=prof_pos(k2,1)
         y2=prof_pos(k2,2)

c get intersection of line (X1,Y1)-(X2,Y2) with faults  
        iz=1
       do kf=1, nfault

c fault flags
          call clearint(iftmp,5)
          do k=1,5
           if (fflag(kf,k)) iftmp(k) = 1
          enddo

         do ix = 1,nxf(kf)-1 

        xf1 = xynode(1,ix,iz,kf)
        yf1 = xynode(2,ix,iz,kf)
        xf2 = xynode(1,ix+1,iz,kf)
        yf2 = xynode(2,ix+1,iz,kf)

c (X,Y) is intersection of line (X1,Y1)-(X2,Y2) and line (X3,Y3)-(X4,Y4)       
c see http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
c if ua=0 lines are parallel
c if 0 <= ua or ub <= 1 the intersection point falls within this segment

      call intsct2lines (X1, Y1, X2, Y2, xf1, yf1, 
     .         xf2, yf2, ua, ub, Xi, Yi )

      if (((Xi.ge.X1 .and. Xi.le.X2) .or. (Xi.le.X1 .and. Xi.ge.X2))
     .   .and. 
     .   ((Yi.ge.Y1 .and. Yi.le.Y2) .or. (Yi.le.Y1 .and. Yi.ge.Y2)))
     .   then
      if (((Xi.ge.Xf1 .and. Xi.le.Xf2) .or. (Xi.le.Xf1 .and. Xi.ge.Xf2))
     .   .and. 
     .   ((Yi.ge.Yf1 .and. Yi.le.Yf2) .or. (Yi.le.Yf1 .and. Yi.ge.Yf2)))
     .   then
c       if ( ua.gt.0.0d0 .or. ub .le. 1.0d0 ) then
          call srproject ( Xi, Yi, olato, olono, aze, dpyy, dpxx)
          dpxx = dpxx + pdist

         if (dpxx .le. prof_length .and. dpxx .ge. 0.0d0 ) then
          write (k22, 493) 'B', Xi, Yi, dpxx, kf, (iftmp(k),k=1,5),
     .        fault_name(kf)
         endif

       endif
       endif

      enddo
      enddo
 493   format (a1, 2f12.4,f12.3, i5, 5i3, 1x, a10)
      enddo
     

c-----------------------------------------------------
c intersect of profile with input faults from file faults.profile in GMT format
c   with > as separator, use for making slab contours

c* see if external file is to be read
      bfile = 'faults.profile'
      call existfile ( bfile, fexist, 0)

      if (fexist .and. myflag ) then

      k33=kfopen(33)
      open (k33, file = bfile )
      print *, 'Processing ', bfile
      fend = .false.
       depth0 = 0.0
       nc = 0

  77    nb = 0
  80    read (k33, '(a256)', end=74 ) aline2 
        read (aline2, '(a1)' ) c1

        if(c1.eq.'>' ) then
         read (aline2, * ) c1a, depth
         nc = nc+1
         if(nc.gt.1) goto 79
        else
         nb = nb+1
c Lon, lat
         read (aline2, * ) xt1, xtmp(nb,2)
c Lat, lon
c         read (aline2, * )  xtmp(nb,2), xt1 
          xtmp(nb,1) = fnlong(xt1)
         depth0 = abs(depth)
         if ( nb.gt.1 ) then
         if (xtmp(nb,1).eq.xtmp(nb-1,1).and.xtmp(nb,2).eq.xtmp(nb-1,2)) 
     .          nb = nb -1
         endif
        endif
       goto 80

c-----------------------------------------------------
c intersect of profile with fault
   74  fend = .true.
   79  depth0 = abs(depth0)
c       print *, 'Depth N ', depth0, nb
       do i = 1, prof_n(j)-1
         k1 = i - 1 + kfirst_point(j)
         k2 = i + kfirst_point(j)

         x1=prof_pos(k1,1)
         y1=prof_pos(k1,2)
         x2=prof_pos(k2,1)
         y2=prof_pos(k2,2)

c get intersection of line (X1,Y1)-(X2,Y2) with faults  
       do k = 1, nb-1

        xf1 = xtmp(k,1)
        yf1 = xtmp(k,2)
        xf2 = xtmp(k+1,1)
        yf2 = xtmp(k+1,2)

c (Xi,Yi) is intersection of line (X1,Y1)-(X2,Y2) and line (X3,Y3)-(X4,Y4)       
c see http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
c if ua=0 lines are parallel
c if 0 <= ua or ub <= 1 the intersection point falls within this segment

      call intsct2lines (X1, Y1, X2, Y2, xf1, yf1, 
     .         xf2, yf2, ua, ub, Xi, Yi )

      if (((Xi.ge.X1 .and. Xi.le.X2) .or. (Xi.le.X1 .and. Xi.ge.X2))
     .   .and. 
     .   ((Yi.ge.Y1 .and. Yi.le.Y2) .or. (Yi.le.Y1 .and. Yi.ge.Y2)))
     .   then
      if (((Xi.ge.Xf1 .and. Xi.le.Xf2) .or. (Xi.le.Xf1 .and. Xi.ge.Xf2))
     .   .and. 
     .   ((Yi.ge.Yf1 .and. Yi.le.Yf2) .or. (Yi.le.Yf1 .and. Yi.ge.Yf2)))
     .   then
c       if ( ua.gt.0.0d0 .or. ub .le. 1.0d0 ) then

       call srproject ( Xi, Yi, olato, olono, aze, dpyy, dpxx)
         dpxx = dpxx + pdist

        if (dpxx .le. prof_length .and. dpxx .ge. 0.0d0 )  
     .    write (k22, '(a1,2f12.4,2f12.3)' ) 'F', Xi, Yi, dpxx, depth0

        endif
       endif

      enddo
      enddo
      if ( .not. fend ) goto 77

       ik = kfclose (k33)

      endif

c*************************************

c* Label for line
c  234.5000   39.3500 6 0 0 CM 1.0
       x = prof_pos(kfirst_point(j),1)
       y = prof_pos(kfirst_point(j),2)
    
      write (k22, 1) prof_num(j), x, y, prof_az(j),
     .    prof_width(j), prof_title(j)

      ik = kfclose (k22)

 112  continue
  1   format( 'Line ', i3, 2f12.4, 2f7.1,1x,a10)

      ik = kfclose (k23)
      return
      end
      
c**********************************************************************

      subroutine readvolcs(vfile, volc_pos, nvolc)

c* read volcanoes file
c* format longitude, latitude, volcano_name
c  format(f8.3, f9.3, a80)

      implicit real*8 (a-h,o-z)
      dimension volc_pos(2000,2)
      character*80 vname, vfile
      logical yesfile

      nvolc=0
      k=0

      call existfile( vfile, yesfile,1)

      if ( yesfile ) then
      print *, 'Reading ', vfile

      kv=kfopen(22)
      open (kv, file=vfile)

      do 10 i=1,2000
      read(kv, 1, end=99,err=2) x, y, vname
  1   format(f8.3, f9.3, a80) 

  2   x=fnlong(x)

      k=k+1
      volc_pos(k,1)=x
      volc_pos(k,2)=y

  10  continue

  99  ik = kfclose(kv)
      nvolc=k

      else

      print *, 'File does not exist ', vfile

      endif

      return
      end

c***********************************************************************      
      subroutine OKADA85 (strike, dip, Xf, Yf, flength,
     .    Zmin, Zmax, ud1in, ud2in, ud3in, Xpt, Ypt, U, dU )
      
c** set up and run finite source solution from Okada, 2D surface version

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      dimension u(3), du(3,3)

      data eps,f0,f1 / 7.0d-3, 0.d0, 1.d0 /
      izero=0

      alp = xmu/(xlambda+xmu)

      call cleareal(dU, 9)
      call cleareal(U, 3)

      cd=dcos(dip*d2r)
      sd=dsin(dip*d2r)
      if (cd.lt.eps ) then
        cd = f0
        sd = f1
      endif

c-- strike
      alph = (90.0d0 - strike)*d2r
      
c--- downdip width of fault
      wide = dabs ((zmax - zmin) / sd ) 
      
c-- translate origin, all points are in lon,lat, use fault as origin
c      call project( xpt, ypt, xf, yf, xp, yp)
      call ps_project( xpt, ypt, xf, yf, xp, yp)

c'-- rotate to fault coordinate system
      x =  xp * dCOS(alph) + yp * dSIN(alph)
      y = -xp * dSIN(alph) + yp * dCOS(alph)
      
      dep=zmax
      al1=0.0d0
      al2=flength
      aw1=0.0d0
      aw2=wide


      d1=ud1in
      d2=ud2in
      d3=ud3in
      
c***********************************************************************
c** run Okada subroutines
c***********************************************************************
      Ux=f0
      Uy=f0
      Uz=f0


c surface only version
c      SUBROUTINE  SRECTF(ALP,X,Y,DEP,AL1,AL2,AW1,AW2,
c     *                   SD,CD,DISL1,DISL2,DISL3,
c     *                   U1,U2,U3,U11,U12,U21,U22,U31,U32)
c

      call SRECTF(ALP,X,Y,DEP,AL1,AL2,AW1,AW2,
     .  SD,CD,D1,D2,D3,Ux,Uy,Uz,U11,U12,U21,U22,U31,U32)
c
c
c***********************************************************************

      
c-- rotate by -ALPH back to coordinate system
       alphn=-alph
       U1 =  Ux * dcos(alphn) + Uy * dsin(alphn)
       U2 = -Ux * dsin(alphn) + Uy * dcos(alphn)
       U3 =  Uz

       u(1)=u1
       u(2)=u2
       u(3)=u3

c-- rotate derivatives also
c      do 30 j=1, 2
c        dU1j = dU(1,j) 
c        dU2j = dU(2,j)
c        dU(1,j)=  dU1j*dcos(alphn) + dU2j*dsin(alphn)
c        dU(2,j)= -dU1j*dsin(alphn) + dU2j*dcos(alphn)
c  30  continue

      return
      end 

c***********************************************************************      
      subroutine okadaquake (strike, dip, Xf, Yf, flength,
     .    Wide, Zmax, ud1in, ud2in, ud3in, Xpt, Ypt, U, dU )
      
c** set up and run finite source solution from Okada, 2D surface version

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      dimension u(3), du(3,3)

      data half,eps,f0,f1 / 0.5d0, 7.0d-3, 0.d0, 1.d0 /
      
      alp = xmu/(xlambda+xmu)

      call cleareal(dU, 9)
      call cleareal(U, 3)

      cd=dcos(dip*d2r)
      sd=dsin(dip*d2r)
      if (cd.lt.eps ) then
        cd = f0
        sd = f1
      endif

      
c-- translate origin and convert observation lat/lon (xpt,ypt) to Cartesian (xp,yp)
c  all points are in lon,lat, use fault (xf,yf) as origin
c      call project( xpt, ypt, xf, yf, xp, yp)
      call ps_project( xpt, ypt, xf, yf, xp, yp)
      
c-- shift (xp,yp,zp) so it corresponds to Okada origin rather than centroid
      al = strike*d2r
      ss = dsin(al)
      cs = dcos(al)
      dX = -half * (flength*ss - Wide*cd*cs)
      dY = -half * (flength*cs + Wide*cd*ss)
c      Xp = Xp + dX
c      Yp = Yp + dY
      
c-- moving to bottom center of fault      
      dX = -half * Wide*cd*cs
      dY =  half * Wide*cd*ss
      Xp = Xp + dX
      Yp = Yp + dY
      
c-- rotate anticlockwise to fault coordinate system, 
c --  X-axis is parallel to fault, Y is across fault
      alph = (90.0d0 - strike)*d2r
      x =  xp * dCOS(alph) + yp * dSIN(alph)
      y = -xp * dSIN(alph) + yp * dCOS(alph)
      
     
      Zp = zmax
      al1 = 0.0d0
      al2 = flength
      aw1 = 0.0d0
      aw2 = Wide
      
c** for centroid      
c      Zp=(zmax-zmin)/2.0d0
      al1 = -flength/2.0d0
      al2 =  flength/2.0d0
c      aw1 = -wide/2.0d0
c      aw2 =  wide/2.0d0


      d1=ud1in
      d2=ud2in
      d3=ud3in
      
c***********************************************************************
c** run Okada subroutines
c***********************************************************************
      Ux=f0
      Uy=f0
      Uz=f0


c surface only version
c      SUBROUTINE  SRECTF(ALP,X,Y,DEP,AL1,AL2,AW1,AW2,
c     *                   SD,CD,DISL1,DISL2,DISL3,
c     *                   U1,U2,U3,U11,U12,U21,U22,U31,U32)
c

      call SRECTF(ALP,X,Y,Zp,AL1,AL2,AW1,AW2,
     .  SD,CD,D1,D2,D3,Ux,Uy,Uz,U11,U12,U21,U22,U31,U32)

c
c
c***********************************************************************

      
c-- rotate by -ALPH back to coordinate system
       alphn=-alph
       U1 =  Ux * dcos(alphn) + Uy * dsin(alphn)
       U2 = -Ux * dsin(alphn) + Uy * dcos(alphn)
       U3 =  Uz

       u(1)=u1
       u(2)=u2
       u(3)=u3

c-- rotate derivatives also
c      do 30 j=1, 2
c        dU1j = dU(1,j) 
c        dU2j = dU(2,j)
c        dU(1,j)=  dU1j*dcos(alphn) + dU2j*dsin(alphn)
c        dU(2,j)= -dU1j*dsin(alphn) + dU2j*dcos(alphn)
c  30  continue

      return
      end 

c**********************************************************************

      subroutine writemodel(nnn)

c* write model to file in .dfn format

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character*2 blankline, c2
      character*50 bb  
      character*4 bname(MAX_block), bmin, fwb, hwb
      character*4 block_name
      character*80 fm125, fm105, fm123
      logical write_coords, dowells 

c Wells' basins
      dimension wells(100,7,2), nb(7), xcc(100), ycc(100), 
     .   nw(Max_x)

c      dimension v(MAX_nodes)
c      logical fnilim

c* common for removing sites
      integer rm_type, rm_reason
      character*180 rm_site
      common /rm1/  rm_site(MAX_rm)
      common /rm2/  nrm_site, nrm_circ, nrm_p, nrm_poly(3),
     .   rm_type(MAX_rm), rm_reason(MAX_gps)
      common /rm3/  rm_circ(20,3), rm_poly(3,20,2)

      blankline = '  '
      bb = '             '


c write out model information
      
      if ( nnn.eq.0) then
       call fopen (kf17, 1, '_model.input ')
      else
       kf17 = nnn
      endif

      write (kf17, *) blankline
      write (kf17, 1) '** Sites removed '//bb
      write (kf17, '(i5, " RM lines")') nrm_site
       do i=1,nrm_site
        if (rm_type(i).eq.0 ) write (kf17, 113) rm_site(i)//bb
        if (rm_type(i).eq.1 ) write (kf17, 114) rm_site(i)//bb
        if (rm_type(i).eq.2 ) write (kf17, 1148) rm_site(i)//bb
       enddo

      write (kf17, *) blankline
      write (kf17, 1) '** Hard Constraints'//bb
      do i=1, num_hc
       if ( hc_flag(i) ) then
         write (kf17, 115) hc(i), hc_pos(i,1), hc_pos(i,2), 
     .    block_name(hc_block(i,2)), block_name(hc_block(i,1)),
     .    hc_val(i,1), hc_val(i,2)
       endif
      enddo

      write (kf17, *) blankline
      write(kf17, 1) '** Block  Pole Strain'//bb
       do j=1, nblocks
       if (block_flag(j) .and. 
     .      block_name(j) .ne. 'xxxx' )
     .   write (kf17, 111)
     .     block_name(j), npole_block(j), nstrain_block(j)
       enddo
       
      write (kf17, *) blankline
      write(kf17, 1) '** Block      Centroid      Pole Strain'//bb
       do j=1, nblocks
       if (block_flag(j) .and. 
     .      block_name(j) .ne. 'xxxx' )
     .   write (kf17, 117)
     .     block_name(j), block_centroid(j,1), block_centroid(j,2),
     .     npole_block(j), nstrain_block(j)
       enddo

      write (kf17, *) blankline
      write (kf17, 1) 
     .  '** Block strain rates (nanostrain/yr) and origins'//bb
      do kb=1, MAX_strain
        write (kf17, 112 ) kb,  
     .   (strain(kb,i)*1.0d3, i=1,3), (strain2(kb,i),i=1,4)
      enddo

      write (kf17, *) blankline
      write (kf17, 1) '** GPS file poles '//bb
      do i = 1, num_gps_file
        k = ngps_index(i)
        write (kf17,103) gps_fname(i), (gps_pole(k,j), j=1,3)
      enddo

      write (kf17, *) blankline
      write (kf17, 1) '** Block poles '//bb
      do j=1, MAX_poles
        write (kf17,102) j, (poles(j,i), i=1,3)
      enddo

c
c *** Smoothing constraints
      write (kf17, *) blankline
       write (kf17,'(a28)' ) '#Fault smoothing constraints'
       write (kf17,'(a48)')
     .   '#    Fault  Type    A1     A2      A3    A4   A5'
       do kf = 1, MAX_f
         ksm = int(smooth(kf,1))
        if (ksm.gt.0 ) 
     .    write (kf17, '(a4, 2i5, 5(1x,1pe12.4) )')
     .    'SM: ',kf, ksm, (smooth(kf,j),j=2,6)
       enddo

      write (kf17, *) blankline
       write (kf17,'(a32)' ) '#Transient smoothing constraints'
       write (kf17,'(a48)')
     .   '#    Trans  Type    A1     A2      A3    A4   A5'
       do nt = 1, MAX_srce
         ksm = int(trsmooth(nt,1))
        if (ksm.gt.0 ) 
     .    write (kf17, '(a5, 2i5, 5(1x,1pe12.4))')
     .    'SMt: ',nt, ksm, (trsmooth(nt,j),j=2,6)
       enddo

c* output node information in input format
      write (kf17, *) blankline
      write (kf17, 1) '** Fault Node information '//bb

      do kf = 1, nfault
       if (nzf(kf).gt.1 ) then
        kfft = fault_fit_type(kf)
        write (kf17, *) blankline
        write (kf17, 116) fault_name(kf), kf

c variable formats
        write (fm125, '("(8x,", I4, "(f7.2))" )' )  nxf(kf)
        write (fm105, '("(", I4, "(I4))" )' )  nxf(kf)
        write (fm123, '("(a4, i4,",I4, "(I7))" )' )  nxf(kf)

c* write node indices, in NNg: or PV: format
      if (kfft .le. 1 ) then
       write (kf17, 104) kf, nxf(kf), nzf(kf)
       do iz = 1, nzf(kf)
        write (kf17, fm105) (nphi(ix,iz,kf), ix=1, nxf(kf) )
       enddo
      elseif (kfft.gt.1 .and. kfft.lt.7 .and. myflag ) then
       write (kf17, fm123) "##  ",kf, (ix, ix=1,nxf(kf))
       write (kf17, fm123) "PN: ",kf, (node_prof(kf,ix), ix=1,nxf(kf))
       write (kf17, 124) kf, nxf(kf) 
       write (kf17, fm125) (f_parm(kf,ix,1), ix=1, nxf(kf))
       write (kf17, fm125) (f_parm(kf,ix,2), ix=1, nxf(kf))
       write (kf17, fm125) (f_parm(kf,ix,3), ix=1, nxf(kf))
      endif


c* write node values in NVg: format
c        write (kf17, *) blankline
c        write (kf17, 109) kf, nxf(kf), nzf(kf)
c        maxn = 0
c        call cleareal(v, MAX_nodes)
c
c        do iz = 1, nzf(kf)
c         write (kf17, 110) (phi(ix,iz,kf), ix=1, nxf(kf))
c         do ix=1, nxf(kf)
c          n = nphi(ix,iz,kf)
c          if ( n.gt.0) v(n) = phi(ix,iz,kf)
c          maxn = max(n , maxn)
c         enddo
c        enddo
c
c* write node values in NV: format
c        write (kf17, *) blankline
c        write (kf17, 106) kf, (v(i), i=1, maxn)

       endif
      enddo

c Ray Wells' basins
      dowells = .false.
      if(myflag .and. dowells ) then
        write (fm105, '("(", I4, "(I3))" )' )  nxf(1)
      write (kf17, *) blankline
      write (kf17, 1) '** Wells Node information '//bb
      ddx = 0.0
       open (33, file = 'wells_basins.dat')
       do j=1,7
        read (33, *) c2, nb(j)
c        print *, j, nb(j)
         do i=1,nb(j)
           read(33,*) wells(i,j,1),  wells(i,j,2)
            wells(i,j,1) = fnlong(wells(i,j,1))
         enddo
       enddo
       close(33)

        do iz = 1, nzf(1)
         do ix = 1, nxf(1)
          nw(ix) = 0
           do j=1,7
            do i=1, nb(j) 
             xcc(i)=wells(i,j,1)
             ycc(i)=wells(i,j,2)
            enddo
            call getnodexy(xpt, ypt, 1, ix,  iz, ddx)
            call inside ( xpt, ypt, xcc, ycc, nb(j), insde)
            if ( abs(insde).eq.1 ) nw(ix) = j
           enddo
          enddo
         write (kf17, fm105) (nw(ix), ix=1, nxf(1) )
        enddo
      endif

c******  fault coordinates with dd: or zd: lines
      write (kf17, *) blankline
      write (kf17, 1) '** Fault coordinates '//bb

      if ( myflag .and. wfsegs ) call fopen (kf18, 1, '_fault.segs ')
      nfseg = 0

      do kf=1, nfault
       if (nzf(kf).gt.0) then
       midx = int(real(nxf(kf))/1.99d0)
       midx = ilimit(midx, 1, nxf(kf))

       khw = khw_blk(midx,1,kf)
       hwb = block_name(khw)
       kfw = kfw_blk(midx,1,kf)
       fwb = block_name(kfw)

      ks = 0
      if (fflag(kf,2)) ks = 1
      ks = ksliptype(kf)

      write (kf17, *) blankline
      
      write (kf17, 108) kf, fault_name(kf), kf
      write (kf17, '(2i5,2(1x,a4),i4 )') nxf(kf), nzf(kf), 
     .  hwb, fwb, ks 

c write fault segments 
      if (myflag .and. fflag(kf,4) .and. nzf(kf).gt.1 .and. wfsegs) then
       call get_str_dip (kf)      
       do ix = 1, nxf(kf)-1
        nfseg = nfseg +1

        dx=  xynode(1,ix+1,1,kf)- xynode(1,ix,1,kf)
        dy=  xynode(2,ix+1,1,kf)- xynode(2,ix,1,kf)
        zmax = znode(nzf(kf),kf)

          ddx= 0.005
          call getnodexy(xpt1, ypt1, kf, ix,   1, ddx)
          call getnodexy(xpt2, ypt2, kf, ix+1, 1, ddx)
          xpt = (xpt1+xpt2)/two
          ypt = (ypt1+ypt2)/two
          call nodeblock(xpt, ypt, khw)
          hwb = block_name(khw)

        write(kf18, '(i6, 4f10.4, 3f8.1, 2(1x,a4),i5, 1x,a10 )' ) 
     .    nfseg,
     .    xynode(1,ix,  1,kf), xynode(2,ix,  1,kf),
     .    xynode(1,ix+1,1,kf), xynode(2,ix+1,1,kf), 
     .    (fault_dip(ix+1,1,kf)+fault_dip(ix,1,kf))/two, 
c     .    (fault_strike(ix+1,1,kf)+fault_strike(ix,1,kf))/two, 
     .    fn360(fnstriker(dx,dy)), zmax, hwb, fwb,
     .    kf, fault_name(kf)
       enddo
      endif
          
      kone = 1

      do iz=1, nzf(kf)
       c2 = '  '
       if (iz.gt.1) c2 = dd_line(kf,iz-1)(1:2)

       if ( c2 .ne. 'dd' .and. c2 .ne. 'zd') then
        write (kf17, '(f8.3)') znode(iz,kf)
        do ix=1, nxf(kf)
          write (kf17, 101) xynode(1,ix,iz,kf), xynode(2,ix,iz,kf)
        enddo
       else
          write (kf17, '(a80)') dd_line(kf,iz-1) 
       endif
      enddo

      endif
      enddo
      if ( myflag .and. wfsegs ) ik = kfclose (kf18)


c******  fault coordinates w/o dd: or zd: lines
c write coordinates of fault
        write_coords = .false.
c        write_coords = .true.
      if (write_coords) then
       call fopen (kf19, 1, '_fault.coords ')
      do kf=1, nfault
       if (nxf(kf).gt.0) then
      ks = 0
      if (fflag(kf,2)) ks = 1
      ks = ksliptype(kf)

      write (kf19, *) blankline
      write (kf19, 108) kf, fault_name(kf), kf
      write (kf19, '(2i5,2(1x,a4),i4)') nxf(kf), nzf(kf), 
     .  block_name(khw_blk(1,1,kf)), 
     .  block_name(kfw_blk(1,1,kf)), ks 
      
      do iz=1, nzf(kf)
       write (kf19, '(f8.3)') znode(iz,kf)
        do ix=1, nxf(kf)
          write (kf19, 101) xynode(1,ix,iz,kf), xynode(2,ix,iz,kf)
        enddo
        if(iz.lt.nzf(kf)) then
         write (kf19, '(f8.3)') (znode(iz,kf)+znode(iz+1,kf))/2.0d0
         do ix=1, nxf(kf)
           write (kf19, 101) 
     .     (xynode(1,ix,iz,kf)+xynode(1,ix,iz+1,kf))/2.0d0, 
     .     (xynode(2,ix,iz,kf)+xynode(2,ix,iz+1,kf))/2.0d0
         enddo
        endif
      enddo

      endif
      enddo
       ik = kfclose (kf19)
      endif

c write coordinates every km of depth
      if ( myflag .and. wf1km ) then
       call fopen (kf19a, 1, '_fault_1km.coords ')

      do kf=1, nfault
       if (nxf(kf).gt.1 .and. nzf(kf).gt.1) then
        ks = 0
        if (fflag(kf,2)) ks = 1
        ks = ksliptype(kf)


      write (kf19a, *) blankline
      write (kf19a, 108) kf, fault_name(kf), kf
      write (kf19a, '(2i5,2(1x,a4),i4)') nxf(kf), nzf(kf), 
     .  block_name(khw_blk(1,1,kf)), 
     .  block_name(kfw_blk(1,1,kf)), ks 

      zkm = znode(1,kf) -1.0d0

      do izkm = 1, int(znode(nzf(kf),kf)) + 1
       zkm = zkm + 1.0d0
       if(izkm.gt.1) zkm = real(int(zkm))
c       if(zkm .ge. 30.0d0) zkm = zkm + 5.0d0
      
      do iz=1, nzf(kf)-1

        if ( znode(iz,kf).eq.zkm ) then
          write (kf19a, '(f8.3)') znode(iz,kf)
          do ix=1, nxf(kf)
           write (kf19a, 101) xynode(1,ix,iz,kf), xynode(2,ix,iz,kf)
          enddo
        endif

        if ( zkm.gt.znode(iz,kf) .and. zkm.lt.znode(iz+1,kf) ) then
         write (kf19a, '(f8.3)') zkm
          do ix=1, nxf(kf)
           dz = (zkm-znode(iz,kf))/(znode(iz+1,kf)-znode(iz,kf))
           x = xynode(1,ix,iz,kf) + 
     .             dz * (xynode(1,ix,iz+1,kf)-xynode(1,ix,iz,kf))
           y = xynode(2,ix,iz,kf) + 
     .             dz * (xynode(2,ix,iz+1,kf)-xynode(2,ix,iz,kf))
           write (kf19a, 101) x,y
          enddo
        endif

      enddo

      enddo

      endif
      enddo
       ik = kfclose (kf19a)
      endif

c** write blocks in alphabetical order
      write (kf17, *) blankline
      write (kf17, 1) '** Blocks '//bb
      nc = 9999
c      call fopen (kf18, 1, '_blocks.segs ')
      
      do j=1,nblocks
       bname(j) = block_name(j)
      enddo

      do jk=1,nblocks
       bmin = 'zzzy'
       j=jk
       do k=1,nblocks
        if ( bname(k).lt.bmin ) then
         bmin = bname(k)
         j = k
        endif
       enddo
       bname(j) = 'zzzz'
       
      if ( nc_block(j).gt.0) then

       write (kf17, *) blankline
       write (kf17,107) 'BLock: ', block_name(j), npole_block(j), 
     .   nstrain_block(j)

       write (kf17,'(i5, 2f12.4)') nc,
     .   block_centroid(j,1), block_centroid(j,2)

       write(kf17, 101) ( blockxy(jj,j,1), 
     .    blockxy(jj,j,2), jj=1,nc_block(j)-1 )
       write(kf17, '(2i10)') nc,nc
       
c       write(kf18, '(a1,1x,a4,2f9.3)' ) '>',block_name(j), 
c     .   block_centroid(j,1), block_centroid(j,2)
c       write(kf18, '(4f10.4)' ) 
c     .  ( blockxy(jj,j,1), blockxy(jj,j,2), 
c     .    blockxy(jj+1,j,1), blockxy(jj+1,j,2), 
c     .    jj=1,nc_block(j) )
     
      endif

      enddo

      if(nnn.eq.0) ik = kfclose (kf17)
c       ik = kfclose (kf18)

   1  format(a60)
c 100  format(2f10.4, i3, f7.2)
 101  format(2f10.4)
 102  format('POc: ', i3, 3f10.5)
 103  format('PGc: ', a4, 3f10.5)
 104  format('NNg: ', 3i3)
c 105  format(500i4)
c 106  format('NV: ', i3, 500f9.3)
 107  format(a7, 1x, a4, 2i4 )
 108  format('FA', 1x, i3,': ',  a10, i4)
c 109  format('NVg: ', 3i3)
c 110  format(500(1x,f9.3))
 111  format('BP: ', a4, 2i5)
 117  format('BC: ', a4, 2f9.3, 2i5)
 112  format('ST: ', i3, 3f12.5, 4f10.3)
 113  format('RM: ', a180)
 114  format('RMb: ', a180)
 1148 format('RM8: ', a180)
 115  format('HC: ', i2, 2f9.3, 2(1x,a4), 2f9.3) 
 116  format('** Fault ', a10, i5)
 124  format("PV: ", 2i4 )
c  125 format(100f6.1)
c  123 format("PN: ",100i4)


      return
      end
 
c*********************************************************************
c write out poles and strain tensors
c*********************************************************************
      subroutine pole_strain_out

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      dimension pin(9), pout(9), ptmp(9)

      character lab(6)*210, blank*2, lab3*210
      logical block_check, all_poles
      character block_name*4      

      zero=0.0d0
      x90 = 90.0d0
      x3 = 1.0d3
      block_check = .true.

      all_poles = .true.

      lab(1)='G# P# Name      Wx        Wy        Wz        Sx     '//
     . '   Sy        Sz        Sxy       Sxz       Syz'
      lab(2)='G# P# Name      Lon.      Lat.     Omega   '//
     . '  SigOm    Emax    Emin      Az'
      lab(3)='   '
      lab(4)='   S# Blok   Long.   Lat.        Exx       Eyy       '//
     . 'Exy       Sxx'//
     . '       Syy       Sxy    Cxx-yy    Cxx-xy    Cyy-xy'
      lab(5)='   S# Blok   Long.   Lat.         E1     SigE1        '//
     . 'E2     SigE2    '//
     . '    A1     SigA1'

      blank = '  '

c write all poles and strain rates
      
      if (wair) call fopen (kair, 1, '.strain.air ')

      call fopen (kf4, 1, '.strain ')
      call fopen (kf15, 1, '_blocks.out ')

c** block strain rates
      write (kf4, *) 'Block strain rates (nanostrain/yr)'
      write (kf4, '(a210)' ) lab(4)
      
      do j=1, nblocks
       if (block_flag(j)) then
        kb=nstrain_block(j)
        
CALS!!! moved the if(kb ..... up to keep strain2(kb,1) from array outofbounds
       if (kb.gt.0) then
        Xc = block_centroid(j,1)
        Yc = block_centroid(j,2)

        if ( strain2(kb,1) .ne. 0.0d0 .or. 
     .     strain2(kb,2) .ne. 0.0d0) then
         Xc = strain2(kb,1) 
         Yc = strain2(kb,2)
        endif

        write (kf4,114) 'T:',kb, block_name(j),Xc,Yc, 
     .   (strain(kb,i)*x3, i=1,6), (strain(kb,i), i=7,9)
       endif
       endif
      enddo
      
      write (kf4, *) blank
      write (kf4, *) 'Block principal strain rates (nanostrain/yr)'
      write (kf4, '(a210)') lab(5)

      do j=1, nblocks
        kb=nstrain_block(j)
        if (kb.gt.0 .and. block_flag(j)) then
        Xc = block_centroid(j,1)
        Yc = block_centroid(j,2)

        if ( strain2(kb,1) .ne. 0.0d0 .or. 
     .     strain2(kb,2) .ne. 0.0d0) then
         Xc = strain2(kb,1) 
         Yc = strain2(kb,2)
        endif

        Exx = strain(kb,1)
        Eyy = strain(kb,2)
        Exy = strain(kb,3)
        SExx = strain(kb,4)
        SEyy = strain(kb,5)
        SExy = strain(kb,6)

      call prinaxes ( Exx, SExx, Exy, SExy, Eyy, SEyy, 
     .   e1, sige1, e2, sige2, alph1, siga1, alph2, siga2)

       sxmu = 3.0d10
       sthick = 2.0d5
       write (kf4,1163) 'P:',kb, block_name(j),Xc,Yc, e1*x3, sige1*x3, 
     .     e2*x3, sige2*x3, fn0to180(alph1), siga1,
     .     two * str_max(Exx, Eyy, Exy) * block_area(j) * 
     .     sxmu * sthick
 1163 format (a2, i4, 1x, a4, 2f8.3, 6f10.2, 1x, 1e12.4)

c** AIR
       if (wair) then
        write (kair,'(a1,1x, a4, 8f10.2)' )
     .     'P', block_name(j), e1*x3, sige1*x3, 
     .     e2*x3, sige2*x3, fn0to180(alph1), siga1, 
     .     air_strain(Exx, Eyy, Exy)*x3,
     .     str_max(Exx, Eyy, Exy)*x3
       endif


      bl_strain(j,1) = e1
      bl_strain(j,2) = sige1
      bl_strain(j,3) = e2
      bl_strain(j,4) = sige2
      bl_strain(j,5) = fn0to180(alph1)
      bl_strain(j,6) = siga1

      endif

      enddo

      lab(6)='     Block     Lon    Lat        E1    SigE1       E2'//
     . '    SigE2       A1    SigA1      Rot   SigRot       Cx      '//
     . ' Cy      SCx      SCy     Nrms   N      Exx      Eyy      Exy'
                        
cR:   6 Port 237.500  46.000   -12.38     2.20    12.89     2.89    23.21     4.30    11.07     1.96     0.03    -0.25     0.10     0.11     1.55  29

      if ( do_dgt ) then
      if(wair) write (kair, *) blank

      write (kf4, *) blank
      write (kf4, *) 'Block residual principal strain rates (ns/yr)'//
     . ' and rotations (nanoradians/yr)'
      write (kf4, '(a210)') lab(6)
      do j=1, nblocks
         Xc = block_centroid(j,1) 
         Yc = block_centroid(j,2)
         n = int(res_strain(j,14))

        if (block_flag(j) .and. n .ge. 3 )  then
          write (kf4,113) 'R:', j, block_name(j), Xc, Yc,
     .     (res_strain(j,i)*x3, i=1,4), fn0to180(res_strain(j,5)),
     .     (res_strain(j,i), i=6,13), n,
     .     (res_strain(j,i)*x3, i=15,17)

      if (wair) then
       write (kair,'(a1,1x,a4, 8f10.2)' ) 'R', block_name(j), 
     .  (res_strain(j,i)*x3, i=1,4), fn0to180(res_strain(j,5)),
     .  res_strain(j,6),
     .air_strain(res_strain(j,15),res_strain(j,16),res_strain(j,17))*x3,
     .str_max(res_strain(j,15),res_strain(j,16),res_strain(j,17))*x3
      endif

       endif

      enddo
      endif

 113  format (a2, i4, 1x, a4, 2f8.3, 13f9.2, i4, 3f9.2)

      ik = kfclose (kf4)
      if (wair) ik = kfclose (kair)

c******************************************
      call fopen (kf4, 1, '.poles ')
      
c-- velocity field poles
      write (kf4, *) 'Rotation of gps files relative to reference frame'
      write (kf4, '(a210)') lab(1)

      do i = 1, num_gps_file
       kpole = ngps_index(i)
       write (kf4,11) i, kpole, gps_fname(i), (gps_pole(kpole,j),j=1,3),
     .   (dsqrt(gps_pole(kpole,j)),j=4,6),(gps_pole(kpole,j),j=7,9)
      enddo

        write (kf4, '(a210)') lab(2)

      do i=1, num_gps_file
        kpole = ngps_index(i)

        do k=1,9
         pin(k) = gps_pole(kpole,k)
         pout(k) =0.0d0
        enddo

       call xyz2llo (pin, pout)
        write (kf4,7) i, kpole, gps_fname(i), (pout(j), j=1,7)
  7   format (2i3, 1x, a4, 4f10.4, 5f8.2)

      enddo

c-- block poles
      write (kf4, *) blank
      write (kf4, *) 'Rotation of blocks relative to reference frame'
      lab(1)='B# P# Name      Wx        Wy        Wz        Sx     '//
     . '   Sy        Sz        Sxy       Sxz       Syz'
      write (kf4, '(a210)') lab(1)

      do j=1, nblocks
        kb=npole_block(j)
        if(kb.gt.0 .and. block_flag(j) ) 
     .   write (kf4,11) j, kb, block_name(j), (poles(kb,i), i=1,3),
     .    (dsqrt(poles(kb,i)), i=4,6),(poles(kb,i), i=7,9)
      enddo

      lab(2)='G# P# Name      Lon.      Lat.     Omega   '//
     . '  SigOm    Emax    Emin      Az     VAR'
      write (kf4, '(a210)') lab(2)

      do j=1, nblocks
        kb=npole_block(j)
        if ( kb.gt.0 .and. block_flag(j) ) then

        do k=1,9
         pin(k) = poles(kb,k)
         pout(k) =0.0d0
        enddo

        call xyz2llo (pin, pout)

        blx = block_centroid(j,1)
        bly = block_centroid(j,2)

        bx = block_centroid(1,1)
        by = block_centroid(1,2)

        call closepole( blx, bly, pout)

        gmtmax = pout(5)
        gmtmin = pout(6)
        gmtaz  = pout(7)

        plon = pout(1)
        plat = pout(2)
        omega = pout(3)
        sigom = pout(4)
        call dist(blx, bly, plon, plat, d)

        call relvel (1, j, nblock_ref, blx, bly, Vx, Sx, Vy, Sy, rho)

c-- vertical axis rotation, Omega dot block centroid
        call vertaxrot(blx, bly, pin, vert_rot)
        spin(j) = vert_rot
        dudy = vert_rot*d2r*1.0d3

        write (kf4,7) j, kb, block_name(j), (pout(k), k=1,7), vert_rot

c* temporary, dpn = distance of block to PAC-NAM pole
c*            dpp = distance of block pole to PAC-NAM pole
c*            dpb = distance of block pole to block
       if (wusflag) then
c PAC-NAM
        pn_lat = 50.347
        pn_lon = 283.456
       else
c PAC-AUS
        pn_lat = -60.1
        pn_lon = 181.7
       endif
  
        call dist(blx,   bly, pn_lon, pn_lat, dpn)
        call dist(plon, plat, pn_lon, pn_lat, dpp)
        call dist(blx,   bly, plon,   plat,   dpb)

c write to blocks.out file
      ng = int(block_res(j,3))

c** probability that data are fit
      rn = ng
      chisq = rn * block_res(j,1)**2
      par_block = 3
      pfit = 1.0d2 * gammq (rn, chisq)

      write (kf15, 115) block_name(j), blx, bly, plon, plat, omega, 
     . sigom, gmtaz, gmtmax, gmtmin, Vx, Vy, Sx, Sy, rho, d, 
     . vert_rot, block_area(j)/1.0d6,
     . (bl_strain(j,i)*1.0d3, i=1,4), (bl_strain(j,i), i=5,6), 
     . (res_strain(j,i)*1.0d3, i=1,4),(res_strain(j,i), i=5,6),
     . (res_strain(j,i), i=7,8),
     . ng, dpn, dpp, dpb,
     . (block_res(j,i), i=1,2), pfit, npole_block(j), nstrain_block(j)

 115  format(a4, 4f9.3, 2f9.3, 2f8.2, f7.2, 4f7.1, f8.4, f6.1,
     .   f9.3, 1x, f9.3, 14f8.1, i5, 3f6.1, 2f8.2, f8.1, 2i4)

      endif
      enddo

c**  output all relative poles
      if( all_poles) then
      write (kf4, *) blank
      write (kf4, *) 'Relative poles '
      lab3 = 'Fixd P# Movg P#     Long.      Lat.     Omega'//
     .  '  SigOmega   Emax   Emin    Azi       Wx        Wy'//
     .  '        Wz        Sx        Sy        Sz       Sxy'//
     .  '       Sxz       Syz   '

cFixd P# Movg P#     Long.      Lat.     Omega  SigOmega   Emax   Emin    Azi  
cEBNR 13 SNEV  4  251.6305   44.6689   -0.5079    0.0431   1.26   0.30 215.54

      write (kf4, '(a210)') lab3
c 12   format(2(1x,a4,i3), 4f10.4, 3f7.2)
          
      do 5 jj1=1, nblocks
      
       do 5 jj2= 1, nblocks
        if(block_flag(jj1) .and. block_flag(jj2)) then
         j1=npole_block(jj1)
         j2=npole_block(jj2)
      
c relative poles, -fixed + moving
      if ( j1.ne.j2 .and. j1.gt.0 .and. j2.gt.0) then
      
      do i=1,3
       ptmp(i)   = -poles(j1,i)   + poles(j2,i)
       ptmp(i+3) =  poles(j1,i+3) + poles(j2,i+3)
       ptmp(i+6) =  poles(j1,i+6) + poles(j2,i+6)
      enddo

      call xyz2llo(ptmp, pout)
        bx = block_centroid(jj1,1)
        by = block_centroid(jj1,2)

        bx = block_centroid(1,1)
        by = block_centroid(1,2)

      call closepole(bx, by, pout)
      
      write(kf4, 122) block_name(jj1), j1, 
     .  block_name(jj2), j2, (pout(i), i=1,7),
     .  (ptmp(i),i=1,3),(dsqrt(ptmp(i)),i=4,6),(ptmp(i),i=7,9) 

      endif
      endif
  5   continue
      endif
      
      ik = kfclose (kf4)
      ik = kfclose (kf15)

  11  format (2i3, 1x, a4, 3f10.4, 6f10.4, f8.2)
c 111  format (i4, 3f10.4, 6f10.4)
c 101  format(a4, 9f10.5)
c 105  format(100i4)
c 107  format(a7, 1x, a4, i4 )
 114  format (a2, i4, 1x, a4, 2f8.3, 6f10.2, 3f10.4)
 122  format(2(1x,a4,i3), 4f10.4, 3f7.2, 9f10.4)

      return
      end
 
c******************************************************     
      subroutine closepole (bx, by, p)

      implicit real*8 (a-h,o-z)
      dimension p(9)

c find pole closest to block
        plon=p(1)
        plat=p(2)
        p3 = p(3)
        p7 = p(7)

        call dist(plon, plat, bx, by, dist1)

c check other pole
        plon2 = fnlong(p(1)+180.d0)
        plat2 = -p(2)
        call dist(plon2, plat2, bx, by, dist2)

c swap pole if 2nd is closer
        if ( dist2.lt.dist1 ) then
          p(1) = fnlong(plon2)
          p(2) = plat2
          p(3) = -p3
          p(7) = 180.0d0 - p7
        endif

      return
      end
c***********************************************************

      subroutine xyz2llo ( pin, pout ) 

c convert Cartesian Euler pole and its covariance matrix to lat-lon-omega    
c  and its error ellipse

c based on subroutines from Chuck DeMets
   
c
c   find principal axes for 2x2 and 3x3 blocks on var-cov diagonal
c
c To do this, the following steps are taken :
c
c    1. For each 3x3 needed, do a similarity transformation on
c   the var-cov matrix to put it into n,e, and d components of sigma**2
c                             t
c     w(n,e,d) =  R w(x,y,z) R
c
c      where R is the rotation matrix found in subroutine getned

c    2. Extract the 2x2 that corresponds to the n and e components
c       from each 3x3 and diagonalize the 2x2. This leaves the major
c       and minor axes in radians.
c
c              These axes are the 68% error ellipse axes that 
c              correspond to one standard deviation error in the data.

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      dimension pin(9), pout(9)
      dimension a1(3,3), simmat(3,3), allmat(3,3)
      dimension a2(2,2), rots(3,3), el2(2), ev2(2,2)
      
      logical allzero

      data zero, one, two /0.0d0, 1.0d0, 2.0d0/

      eps = 1.0d-6

c pin - input array ( Wx, Wy, Wz, Sx, Sy, Sz, Sxy, Sxz, Syz) in degrees/Ma
c pout - output array = ( Lon, lat, omega,  Slat, Slon, Somega, Emax, Emin, Az)

       nine=9
       call cleareal(pout,nine)
       eom  = 0.0d0
       elat = 0.0d0
       elon = 0.0d0

       Wx=pin(1)
       Wy=pin(2)
       Wz=pin(3)

      if (abs(wx).lt.eps .and. abs(wy).lt.eps .and. abs(wz).lt.eps ) 
     .        return

c-- get pole position and rate
          call carsph(Wx,Wy,Wz,elat,elon,eom)
          pout(1) = fnlong(elon)
          pout(2) = elat
          pout(3) = eom
c          print *, 'eom ', eom

c-- get error ellipse
           a1(1,1) = pin(4)
           a1(2,2) = pin(5)
           a1(3,3) = pin(6)
           a1(1,2) = pin(7)
           a1(2,1) = pin(7)
           a1(1,3) = pin(8)
           a1(3,1) = pin(8)
           a1(2,3) = pin(9)
           a1(3,2) = pin(9)

c RM1104a - check for zero array      
           if ( allzero(a1,nine) ) then
c           print *, 'A1 all zero'
            return
           endif
  

           eln=elon*d2r
           elt=elat*d2r
           simmat(1,1) = -dsin(elt)*dcos(eln)
           simmat(1,2) = -dsin(elt)*dsin(eln)
           simmat(1,3) =  dcos(elt)
           simmat(2,1) = -dsin(eln)
           simmat(2,2) =  dcos(eln)
           simmat(2,3) =  zero
           simmat(3,1) = -dcos(elt)*dcos(eln)
           simmat(3,2) = -dcos(elt)*dsin(eln)
           simmat(3,3) = -dsin(elt)

c  Initialize necessary matrices
       call cleareal(rots,nine)
       call cleareal(allmat,nine)

c RM1104a  initialize el2 and ev2
       i4=4
       call cleareal(el2, i4)
       call cleareal(ev2, i4)


c  Premultiply the a1 tensor by the rotation and then postmultiply it
c  by the transpose of the rotation.
        do i6 = 1,3
         do i7 = 1,3
          do i8 = 1,3
            rots(i6,i7) = rots(i6,i7) + simmat(i6,i8)*a1(i8,i7)
          enddo
         enddo
        enddo
c
c  Now postmultiply by simmat transpose
       do i6 = 1,3
        do i7 = 1,3
         do i8 = 1,3
          allmat(i6,i7) = allmat(i6,i7) + rots(i6,i8)*simmat(i7,i8)
         enddo
        enddo
       enddo

c   Extract 2x2 of n and e components and remove velocity scale

c squared rotation rate in (radians/Myr) **2

          ww = eom*eom
          if (ww.eq.0.0d0 ) then
           return
          endif

          a2(1,1) = allmat(1,1)/ww
          a2(1,2) = allmat(1,2)/ww
          a2(2,1) = allmat(2,1)/ww
          a2(2,2) = allmat(2,2)/ww

c RM1104a - check for zero array      
           if ( allzero(a2,4) ) then
            print *, 'A2 all zero'
            return
           endif

c   Diagonalize this 2x2 "a2" matrix to get error ellipse info.
           call eigen2(a2,el2,ev2)

c   Find the magnitude of sigma (semi-major and semi-minor axes)
c in units of degrees

           ax1=datan(dsqrt(el2(1)))*r2d
           ax2=datan(dsqrt(el2(2)))*r2d

c aza is semimajor direction, ccwise from East
c         aza=artan2(one, ev2(1,2))*r2d

c aza is semimajor direction, cwise from North
          aza=artan2(ev2(1,2), one)*r2d

c convert to cwise from North
c           aza = fn360(90.0d0 - aza)

c   The lengths of the error axes for this bivariate distribution
c   must be multiplied by sqrt(2) ( see Gordon and Cox, 1984).

           ax1 = dsqrt(two) * ax1
           ax2 = dsqrt(two) * ax2

c   Find the uncertainty in the rotation rate.

       domg   = dsqrt(allmat(3,3))
       siglat = dsqrt(allmat(1,1))*r2d
       siglon = dsqrt(allmat(2,2))*r2d

       pout(4) = domg
       pout(5) = ax1
       pout(6) = ax2
       pout(7) = fn0to180(aza)
       pout(8) = siglon
       pout(9) = siglat

      return
      end
c*********************************************************************

      subroutine eigen2(aa,eval,evec)
c eigenvalues and eigenvectors of 2x2 matrix
      implicit real*8 (a-h,o-z)

       dimension aa(2,2),eval(2),evec(2,2)
       a=aa(1,1)
       b=aa(1,2)
       c=aa(2,2)
       tr=a+c
       det=a*c-b*b
       s = dsqrt(tr*tr-4.d0*det)
       eval(1)=(tr+s)/2.0d0
       eval(2)=(tr-s)/2.0d0
       evec(1,1)=1.0d0
       evec(2,1)=1.0d0
       evec(1,2)=(eval(1)-a)/b
       evec(2,2)=(eval(2)-a)/b
      return
      end

c**********************************************************************
      subroutine fill(a,val,ndim)
c
c...  subroutine to fill a real matrix with a scalar value
c
      implicit real*8 (a-h,o-z)
      dimension a(ndim)
      do i=1,ndim
        a(i)=val
      enddo
      return
      end
c
c**********************************************************************
      SUBROUTINE gaussj2(a,n,np,b,m,mp,sing)

      implicit real*8 (a-h,o-z)

      PARAMETER (NMAX=3000)

      real*8 a(np,np), b(np, mp)
      integer indxc(NMAX),indxr(NMAX),ipiv(NMAX)

      logical sing
      irow=0
      icol=0

      sing = .false.

      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (dabs(a(j,k)).ge.big)then
                  big=dabs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                print *, '1singular matrix in gaussj'
                sing = .true.
                return
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) then
         print *, '2singular matrix in gaussj'
         sing = .true.
         return
        endif
        pivinv=1.0d0/a(icol,icol)
        a(icol,icol)=1.0d0
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.d0
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.


c**************************************************************************
      subroutine delaz(slat,slon,eplat,eplon,delta,azimuth)

c get delta and azimuth from eplat,eplon to slat,slon
c slat, slon - station lat & lon
c eplat, eplon - epicenter lat & lon
c from Jim Dewey in 1970's, origin unknown

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      one=1.0d0
      half=0.5d0
      zero=0.0d0
      f = d2r
      g = r2d
      ef =0.993277d0

c --- distaz
        tslat = slat*f
        tslon = slon*f
        glat=datan(ef*dtan(tslat))
        dca = dcos(glat)*dcos(tslon)
        dcb = dcos(glat)*dsin(tslon)
        dcc = dsin(glat)
        slat = tslat/f

      geplat = datan(ef*dtan(eplat*f))
      epdca = dcos(geplat)*dcos(eplon*f)
      epdcb = dcos(geplat)*dsin(eplon*f)
      epdcc = dsin(geplat)

c---- calculate distance and azimuth from earthquake to station

      par1=(one-half*((dca -epdca)**2 + 
     .  (dcb -epdcb)**2+(dcc  - epdcc)**2))

        DELTA1  = g*datan(dsqrt((one/(par1*par1)) - one))
        if (par1.lt.zero) DELTA1  = 180.0d0 - DELTA1
        if (delta1.eq.zero) DELTA1 = 1.0d-8
        delta=delta1

        t1 = dca  - dsin(eplon*f)
        t2 = dcb  + dcos(eplon*f)
        t3 = dsin(delta1 *f)
        par2 = (half*(t1*t1 + t2*t2 + dcc *dcc ) - one )/t3
        t1 = dca  - dsin(geplat)*dcos(eplon*f)
        t2 = dcb  - dsin(geplat)*dsin(eplon*f)
        t4 = dcc  + dcos(geplat)
        apar2 = dabs(par2)
        AZIMUTH  = 90.0d0

        if ( apar2.lt.one)  then 
          if (par2.eq.zero) par2=1.0d-8 
          if (par2.eq.one)  par2=0.99999999d0
          AZIMUTH  = g*datan(one/dsqrt((one/par2**2)-one))
        endif

        par3 = (half*(t1*t1+t2*t2+t4*t4) - one)/t3

        if (par2.lt.zero) then

         if (par3.lt.zero) then 
            AZIMUTH  = 180.0d0 + AZIMUTH 
            return
         endif

         AZIMUTH  = 360.0d0 - AZIMUTH 
         return
        endif

        if(par3.lt.zero) AZIMUTH  = 180.0d0 - AZIMUTH

      return
      end 

c======================================================================
      SUBROUTINE gaussj(a,n,np,b,m,mp)

      implicit real*8 (a-h, o-z)

      PARAMETER (NMAX=3000)
      dimension a(np,np),b(np,mp)
      integer indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      irow=0
      icol=0

      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
c                pause 'singular matrix in gaussj'
                 print *, 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) then
           print *, 'singular matrix in gaussj'
c* pause
        endif
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.


c********************************************
      subroutine faultslip 

c* calculate relative velocities at specified points
c   get fault-normal and strike-slip if necessary

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      character*9 c9
      character*4 block_name      
c      logical kch

      Sx =0.0
      cc =0.0

      if ( num_fs.gt.0) then

      call fopen (k18, 1, '_fslip.out ')
c      call fopen (k19, 1, '_fslip.blocks ')

      do 99 i = 1, num_fs

       nb1 = fs_block(i,1)
       nb2 = fs_block(i,2)
       xc = fs_xy(i,1)
       yc = fs_xy(i,2)
       faz = fs_xy(i,3)


c      call closest_blocks(xc, yc, nb1,nb2,kb1, kb2, kf1, kch)
     
c      write (k19, '( 2f10.3, "  Nearest blocks:",
c     .  2(1x,a4),2i3," Nearest fault",i4)') xc, yc, 
c     .  block_name(kb1), block_name(kb2), kb1, kb2, kf1

       call relvel(3, nb1, nb2, xc, yc, Vx, Sx, Vy, Sy, rho)
       call velsig ( Vx, Vy, Sx, Sy, rho, V, Verr )
       call azsig (Vx, Vy, Sx, Sy, Az, Azer)
       c9 = block_name(nb1)//'_'//block_name(nb2)

c** get fault-parallel and fault-normal velocities and sigmas

      Vnorm = Vx
      Vpar  = Vy

      if (faz .ne. 0.0d0) then
c- get fault azimuth      
      azr = faz*d2r

c* fault-parallel, right-lateral is positive
c* unit vector parallel to fault
       Ux = dsin(azr)
       Uy = dcos(azr)
c* velocity and sigma in this direction
       Vpar = -(Vx * Ux + Vy * Uy)
       call velsig(Ux, Uy, Sx, Sy, rho, Ut, Vpsig)

c* fault-normal, extension is postitive, thrust is negative
       azr = (faz+90.0d0)*d2r

c* unit vector normal to fault
       Ux = dsin(azr)
       Uy = dcos(azr)

c* velocity and sigma in this direction
       Vnorm = (Vx * Ux + Vy * Uy)
       call velsig(Ux, Uy, Sx, Sy, rho, Un, Vnsig)

       endif

       write (k18,1) xc, yc, Vx, Vy, Sx, Sy, rho, c9, V, Verr, 
     .    az, azer, faz, Vpar, Vnorm 

    1  format (2f10.4, 4f9.2, f9.4, 1x, a9, 7f9.2 )

   99 continue

      ik = kfclose (k18)
c      ik = kfclose (k19)

      endif

      return
      end

c********************************************
      subroutine scecslip

c* calculate relative velocities at specified points
c   get fault-normal and strike-slip  

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      character*9 c9
      character*4 block_name      
c      logical kch

      Sx =0.0
      cc =0.0


      call fopen (k18, 1, '_scec.out ')
       k19 = 19
       open (k19, file = 'scec.in')

      do 99 i = 1, 1200

       read (k19,*, end=110 ) n,nf,xc,yc,strike

       srad = strike*d2r
       dx = 0.01d0

c find blocks
       xpt = xc + dx * dcos(srad)
       ypt = yc - dx * dsin(srad)
       call getblock(xpt, ypt, nb1)
       xpt = xc - dx * dcos(srad)
       ypt = yc + dx * dsin(srad)
       call getblock(xpt, ypt, nb2)

 
c-- get hangingwall block for each node      
       nb1 = kfw_blk(1,1,nf) 
       nb2 = khw_blk(1,1,nf) 


c       nb1 = fs_block(i,1)
c       nb2 = fs_block(i,2)
c       xc = fs_xy(i,1)
c       yc = fs_xy(i,2)
       faz = strike

c      call closest_blocks(xc, yc, nb1,nb2,kb1, kb2, kf1, kch)
       
c      write (k19, '( 2f10.3, "  Nearest blocks:",
c     .  2(1x,a4),2i3," Nearest fault",i4)') xc, yc, 
c     .  block_name(kb1), block_name(kb2), kb1, kb2, kf1

       call relvel(3, nb1, nb2, xc, yc, Vx, Sx, Vy, Sy, rho)
       call velsig ( Vx, Vy, Sx, Sy, rho, V, Verr )
       call azsig (Vx, Vy, Sx, Sy, Az, Azer)

       c9 = block_name(nb1)//'_'//block_name(nb2)

c** get fault-parallel and fault-normal velocities and sigmas

      Vnorm = Vx
      Vpar  = Vy

c      if (faz .ne. 0.0d0) then
c- get fault azimuth      
      azr = faz*d2r

c* fault-parallel, right-lateral is positive
c* unit vector parallel to fault
       Ux = dsin(azr)
       Uy = dcos(azr)

c* velocity and sigma in this direction
       Vpar = -(Vx * Ux + Vy * Uy)
       call velsig(Ux, Uy, Sx, Sy, rho, Ut, Vpsig)

c* fault-normal, extension is postitive, thrust is negative
       azr = (faz+90.0d0)*d2r

c* unit vector normal to fault
       Ux = dsin(azr)
       Uy = dcos(azr)

c* velocity and sigma in this direction
       Vnorm = (Vx * Ux + Vy * Uy)
       call velsig(Ux, Uy, Sx, Sy, rho, Un, Vnsig)

c       endif

       write (k18,1) n, nf,xc, yc, Vx, Vy, Sx, Sy, rho, c9, V, Verr, 
     .    fn360(az), azer, faz, Vpar, Vnorm 

    1  format (2i5, 2f10.4, 4f8.1, f9.4, 1x, a9, 7f8.1)

   99 continue

 110  ik = kfclose (k18)
      close (k19)


      return
      end


c********************************************
      subroutine azsig (Vx, Vy, Sx, Sy, Az, Azer)
c* get azimuth and sigma of velocity vector

      implicit real*8 (a-h,o-z)

      Az = fnstriker(Vx, Vy)
      Azer = 0.0d0
      z = 0.0d0

      if(Sx.eq.z .and. Sy.eq.z) return

c'-- do az uncertainties by monte carlo
      ns=6000
      n=0
      azsum=0.0d0
      azsum2=0.0d0
      daz = 90.0d0

      do kk = 1, ns
        call normal (Vxr, Vx, Sx)
        call normal (Vyr, Vy, Sy)
        azr = fnstriker(Vxr, Vyr)
        if ( abs(azr-az) .le. daz) then
         n=n+1
         azsum = azsum+azr
         azsum2 = azsum2+azr*azr
        endif
      enddo

       call meansd (n, azsum, azsum2, a, Azer, sdmean) 

      return
      end

c****************************************************************

      subroutine matrix_mult(mat1,row1,col1,mat2,row2,col2,mulmat)

c This subroutine multiplies together any two matrices
      implicit real*8 (a-h,o-z)

      integer row1,col1,row2,col2
      real*8 mat1(row1,col1),mat2(row2,col2),mulmat(row1,col2)

      call cleareal (mulmat, row1*col2)
                               
      do  i = 1,row1 
       do  j = 1,col2 
        do  k = 1,col1 
         mulmat(i,j) = mulmat(i,j) + mat1(i,k) * mat2(k,j)
        enddo
       enddo
      enddo

      return
      end

c****************************************************************
c-- convert character to real
      function c2r(aa)
      
      implicit real*8 (a-h,o-z)
      character (*) aa
      
c      print *, aa
      c2r = 0.0d0
      if ( aa.eq.'' ) return
      read(aa, *) v
      c2r = v
      
      return 
      end
c****************************************************************
c-- convert character to integer
      function c2i(aa)
      
      implicit real*8 (a-h,o-z)
      character (*) aa
      integer c2i
      c2i = 0
      if ( aa.eq.'' ) return
      
      read(aa, *) iv
      c2i = iv
      
      return 
      end
      
c**********************************************************************

      subroutine i2c(k, n, ch2)

c get n digit character from integer k, adds leading zeroes
c   eg k= 1, n=2 returns character ch2 = '01'
c   eg k=99, n=5 returns character ch2 = '00099'

      character ch2(n)*1
      character formt*8, cc*50

      if ( n.lt.10)  write (formt, '(a2,i1,a1)' ) '(i',n,')'
      if ( n .ge. 10)  write (formt, '(a2,i2,a1)' ) '(i',n,')'

      write(cc, formt) k

      do i=1,n
       ch2(i) = cc(i:i)
       if (ch2(i).eq.' ') ch2(i) = '0'
      enddo

      return
      end

c****************************************************************
      subroutine getcm (cm, aone, nitems)

c* parse the input line, see if it's to be skipped
c* aline is full input line
c* aline1 is input line with key information removed
c* aline2 is input line with key information and extra spaces removed
c* kskip is flag set by sk: and co:
c* modskip is flag for skipping unwanted models
c RM1107 - added nitems to list

      character*1 aone
      character*2 cm
      character*4, c4, c4tmp(64)
      character*256 aline1

      character*4, modelname
      logical kskip, modskip, modelin, moremods, noem
      common /cm1/ kskip, modskip, modelin, moremods, noem
      common /cm2/ modelname
      logical write_input
      common /cm3/ write_input
      character*256 aline, aline2
      common /io2/ aline, aline2

      aone = " "
      cm = "  "
      
c* make aline2
c      print *, "aline ", aline
c      call clearchar(c4tmp, 256)
      c4tmp = ""
c      do i=1,64
c       c4tmp(i)="    "
c      enddo
c      print *, "getcm2"

      if (write_input) print *, 'a : ', aline

c get part of line between colons, assign to aline1
      le = index(aline, ':')
c      if ( le. gt. 2 ) 
       aline1 = aline(le+1: len(aline)-le)
c       print *, 'le a0: ', le, aline 
c       print *, 'le a1: ', le, aline1
       le2 = index(aline1, ':')
       if ( le2 .ne. 0 ) aline1 = aline1(1:le2-1)
c       print *, 'le2 aline1: ', le2, aline1
      
c RM1107 - new parsing routine
c-- make aline2 by remove leading blanks, count items, and put 1 space between entries
      call count_items(aline1, aline2, nitems)
c      print *, 'N aline2: ',nitems,aline2

      cm = aline(1:2)
      call lcase(cm,2)

c check for old unused commands
c      if ( cm.eq.'mm' ) print *, '*** MM: replaced by SM: '
c      if ( cm.eq.'gf' ) print *, '*** GF: replaced by GD: '
c      if ( cm.eq.'up' ) print *, '*** UP: replaced by GP: '
c      if ( cm.eq.'ni' ) print *, '*** NI: replaced by IC: '

      aone = aline(3:3)
      call lcase(aone,1)
c       print *, "cm __ a3 ", cm, " ", aone
       
c* set flag for skipping models
      if ( modelin .and. cm.eq.'mo' .and. moremods) then
       read (aline2, *) (c4tmp(i),i=1,nitems)
       modskip = .true.
        do i=1,nitems
         c4 = c4tmp(i)
         if ( c4.eq.modelname ) modskip = .false.
        enddo
      endif
c       print *, "1"
c** end of models input section
      if ( cm.eq.'em' ) then
         modskip = .false.
         moremods = .false.
         noem = .false.
      endif
c       print *, "2"


c* set flag for skipping lines
      if ( cm.eq.'sk' ) kskip = .true.
      if ( cm.eq.'co' ) kskip = .false.

      if (kskip .or. modskip) then
        cm = '  '
        return
      endif
c       print *, "3"

      if (write_input) print *, 'cm : ', cm
c       print *, "4 cm ", cm, " aone ", aone, " aline2 ", aline2  

      return
      end

c****************************************************************
c RM1207 - remove blanks from a string

      subroutine rmblanks (a, b, k)
      character*256 a, a2, b
      character*1 spc, c1
      
      spc = ' '

c-- character counter for string b      
      k=0
      
c      call clearchar(b,256)
      b = ""
c      a2 = trim(adjustl(a))//' '
      a2 = a(1:len_trim(a))//' '
       do j=1,len(a2)
        c1 = a2(j:j)
         if (c1.ne.spc) then
          k=k+1
          b(k:k) = c1
         endif
         
       enddo
      return
      end
      
c****************************************************************
c RM1107 - new parsing routine
c-- remove leading blanks, count items, and put 1 space between entries
c-- a is input string, b is output string, n is number of items in list

      subroutine count_items (a, b, n)
      character*256 a, a2, b
      character*1 c1, spc, com
      logical firstblank
      
      spc = ' '
      com = ','
      firstblank = .true.

c-- items counter      
      n=0
      
c-- character counter for string b      
      k=0
      
c      call clearchar(b,256)
      b = ""
      a2 = trim(adjustl(a))//' '
       do j=1,len(a2)
        c1 = a2(j:j)
         if (c1.eq.spc .or. c1.eq.com) then
          if (firstblank) then
           n=n+1
           k=k+1
           b(k:k) = spc
          endif
           firstblank = .false.
          
         else
          firstblank = .true.
          k=k+1
          b(k:k) = c1
         endif
         
       enddo
      return
      end
      
c****************************************************************
      subroutine parse(aa, args, nargs)

c Parses the string 'aa' into arguments args(1), ..., args(nargs) based on
c the delimiter ' ' (space) 
c The integer output variable nargs contains the number of arguments found.

       character*256 aa, bb
       character*20 args(100)
       character*1 c1, spc
       character*2 spc2
      
       spc = ' '
       spc2 = '  '
       bb = aa
       call count_items (bb, aa, na)
       nmax = 100
c       call clearchar(args, 20*nmax)
       args = ""
c       do ic=1,100
c        args(ic) = ""
c        enddo
        
       lena = len(aa)
       nargs = 1
       m=0
       
       do j=1,lena
        c1 = aa(j:j)
        if(aa(j:j+1).eq.spc2) return
         if (c1.eq.spc) then
          nargs = nargs+1
          m=0
         else
          m=m+1
          args(nargs)(m:m) = c1
         endif
       enddo
       
       return
       end
       
c****************************************************************
      subroutine velerr (w, rlat, rlon, Se, Sn, rho, aza, ax1, ax2)


C  program for calculating uncertainty ellipses for velocity vectors. This
C  requires the variance-covariance matrix for the Euler vector
C  Units required for the variance-covariance matrix are (radians/Myr)**2
C  Written by C. DeMets
c  Modified 3/15/03 by R. McCaffrey to be f77 subroutine

c  w is input covariance matrix for Euler vector, in (radians/Ma)**2
c  rlat, rlon of point where v is calculated

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      logical nonzero

      dimension r(3,3), w(3,3), allmat(3,3), simmat(3,3), a1(3,3),
     .  rloc(3,3), a1t(3,3)
      dimension a2(2,2),el2(2),ev2(2,2)

      sqr2 = dsqrt(2.0d0)
      nonzero = .false.
      zero = 0.0d0
      nine=9

      Se = zero
      Sn = zero
      rho = zero
      Ax1 = zero
      Ax2 = zero
      Az = zero

      do i=1,3
       do j=1,3
        if ( w(i,j) .ne. zero ) nonzero = .true.
       enddo
      enddo

      if ( .not. nonzero ) return

      call cleareal(r,nine)
      call cleareal(allmat,nine)
      call cleareal(rloc,nine)

      if(rlon.lt.zero ) rlon = rlon + 360.d0
      om=1.0d0
      call sphcar(rlat,rlon,om,x,y,z)

C Set up the antisymmetric R position matrix
      r(1,2) = z 
      r(2,1) = -z
      r(1,3) = -y 
      r(3,1) = y
      r(3,2) = -x 
      r(2,3) = x

C Now multiply var-covar and R matrices
      call matrix_mult(r,3,3,w,3,3,allmat)

C Now post-multiply this by R transpose to get symmetric velocity matrix in
C global coordinate system
      r(1,2) = -z 
      r(2,1) = z
      r(1,3) = y 
      r(3,1) = -y
      r(3,2) = x 
      r(2,3) = -x

      call matrix_mult(allmat,3,3,r,3,3,a1t)

C We now have a 3x3 matrix specifying error ellipsoid for velocity prediction
C To finish, need to transform from global Cartesian to local coordinates and
C then pull out the 2x2 n,e components of the ellipse. This gives n,e,d
C components of sigma**2. Diagonalize to get major and minor axes in radians.
C                             t
C     w(n,e,d) =  R w(x,y,z) R
C
C              These axes are the 68% error ellipse axes that 
C              correspond to one standard deviation error in the data.
C  Create the similarity rotation matrix

      rt = rlat*d2r
      rn = rlon*d2r
      simmat(1,1) = -dsin(rt)*dcos(rn)
      simmat(1,2) = -dsin(rt)*dsin(rn)
      simmat(1,3) =  dcos(rt)
      simmat(2,1) = -dsin(rn)
      simmat(2,2) =  dcos(rn)
      simmat(2,3) =  zero
      simmat(3,1) = -dcos(rt)*dcos(rn)
      simmat(3,2) = -dcos(rt)*dsin(rn)
      simmat(3,3) = -dsin(rt)

C  Premultiply the a1 tensor by the rotation and then postmultiply it
C  by the transpose of the rotation.
C Reinitialize rloc matrix

      call cleareal (rloc, nine)

      call matrix_mult(simmat,3,3,a1t,3,3,a1)

      do i = 1,3 
       do j = 1,3 
        do k = 1,3
         rloc(i,j) = rloc(i,j) + a1(i,k)*simmat(j,k)
        enddo
       enddo
      enddo

C   This gives the local n,e,d tensor. All down components should be 0. (They
C   are; I checked). Extract 2x2 of n and e components

      a2(1,1) = rloc(1,1)
      a2(1,2) = rloc(1,2)
      a2(2,1) = rloc(2,1)
      a2(2,2) = rloc(2,2)

      Sn = dsqrt(a2(1,1)) *r2d*d2x
      Se = dsqrt(a2(2,2)) *r2d*d2x
      rho = a2(1,2)/(dsqrt(a2(1,1))*dsqrt(a2(2,2)))

C   Diagonalize the matrix

      if(abs(a2(1,2)).gt.0.0) then
        call eigen2(a2,el2,ev2)
      else
       if(a2(1,1) .ge. a2(2,2)) then
        el2(1) = a2(1,1)
        el2(2) = a2(2,2)
       endif
       if(a2(1,1).lt.a2(2,2)) then
        el2(2) = a2(1,1)
        el2(1) = a2(2,2)
       endif
      endif

C   Find the magnitude of sigma (major and minor axes) and put into km/m.y.
      ax1 = dsqrt(el2(1)) *r2d*d2x
      ax2 = dsqrt(el2(2)) *r2d*d2x

C If there are no off-diagonal components, then the ellipse points in the
C direction of the north or east axis, depending which is the major-axis
      if(ev2(1,2) .ne. 0.0d0)then
        aza=artan2(1.0d0,ev2(1,2))*r2d
      else
        if(a2(1,1) .ge. a2(2,2))then
         aza = 0.0d0
        endif
       if(a2(1,1).lt.a2(2,2))then
        aza = 90.0d0
       endif
      endif

C   In order to correctly find the lengths of the error axes since this
C   is a bivariate distribution and not a univariate distribution, in this
C   case, one multiplies the axis lengths by sqrt(2) ( see Gordon and Cox,
C   1984)

      ax1 = sqr2 * ax1
      ax2 = sqr2 * ax2

      return
      end
c****************************************************

      subroutine domoves (jbin, kfin)
      
c** apply AV/DV/MV - add/delete/move block corners and fault node points
c jbin is the block number
c kfin is the fault number

      implicit real*8 (a-h,o-z)

      include "tdefcom1.h"
      include "tdefcons.h"

      dimension xtmp(MAX_corner, 2)

      xnear = 1.0d0
      jblock = jbin
      kfault = kfin

c*** BLOCKS ************************************
      if ( jblock.gt.0) then
         j=jblock
         nc   = nc_block(j)
         ncp1 = nc_block(j)+1
         blockxy(ncp1,j,1) = blockxy(1,j,1)
         blockxy(ncp1,j,2) = blockxy(1,j,2)
         
         do i=1,ncp1
          xtmp(i,1) = blockxy(i,j,1)
          xtmp(i,2) = blockxy(i,j,2)
         enddo
         
       do 2 k=1, nedits
        kt = edit_type(k)

         x1=fnlong(ptedit(k,1))
         y1=ptedit(k,2)
         x2=fnlong(ptedit(k,3))
         y2=ptedit(k,4)
         x3=fnlong(ptedit(k,5))
         y3=ptedit(k,6)
         
      
        do 1 i=1, nc
          x   = fnlong(xtmp(i,1))
          y   = xtmp(i,2)
          xp1 = fnlong(xtmp(i+1,1))
          yp1 = xtmp(i+1,2)

c-- replace a point
        if ( kt.eq.1 .and. iclose( x1, y1, x, y, xnear).eq.1 ) then
           xtmp(i,1)= x2
           xtmp(i,2)= y2
           edit_flag(k) = edit_flag(k)+1
c           goto 2
        endif
        
c-- add a point
        if ( kt.eq.2 .and.  
     .   (( iclose( x1, y1, x, y, xnear).eq.1 .and. 
     .         iclose( x2, y2, xp1, yp1, xnear).eq.1 ) .or.
     .   ( iclose( x2, y2, x, y, xnear).eq.1 .and. 
     .         iclose( x1, y1, xp1, yp1, xnear).eq.1)) ) then
     
           do kk = ncp1, i+1, -1
            xtmp(kk,1) = xtmp(kk-1,1)
            xtmp(kk,2) = xtmp(kk-1,2)
           enddo
           
           if ( x3.eq.zero .and. y3.eq.zero) then
            x3 = (x1+x2)/two
            y3 = (y1+y2)/two
           endif
     
            xtmp(i+1,1)= x3
            xtmp(i+1,2)= y3
            nc=nc+1
            ncp1 = nc+1
            
           edit_flag(k) = edit_flag(k)+1
        endif
        
c-- delete a point
        if ( kt.eq.3 .and. iclose( x1, y1, x, y, xnear).eq.1 ) then
           edit_flag(k) = edit_flag(k)+1
           do kk = i,nc
            xtmp(kk,1) = xtmp(kk+1,1)
            xtmp(kk,2) = xtmp(kk+1,2)
            nc = nc-1
            goto 2
           enddo
        endif

  1    continue
  2    continue
  
         nc_block(j) = nc
         do i=1,nc
          blockxy(i,j,1) = xtmp(i,1)  
          blockxy(i,j,2) = xtmp(i,2) 
         enddo
         
      endif

c*** FAULTS ***********************************
      if ( kfault.gt.0) then
       kf=kfault
 
       do  20 k=1, nedits
        kt = edit_type(k)

         x1=fnlong(ptedit(k,1))
         y1=ptedit(k,2)
         x2=fnlong(ptedit(k,3))
         y2=ptedit(k,4)
         x3=fnlong(ptedit(k,5))
         y3=ptedit(k,6)
         


c* move fault surface nodes, shift deeper nodes by same amount
        do 10 i=1,nxf(kf)
         x   = xynode(1,i,1,kf)
         y   = xynode(2,i,1,kf)
         xp1 = xynode(1,i+1,1,kf)
         yp1 = xynode(2,i+1,1,kf)

c-- move point
        if (kt.eq.1 .and. iclose( x1, y1, x, y,xnear).eq.1 ) then
          dxx = x2-x1
          dyy = y2-y1
          xynode(1, i, 1, kf) = x2
          xynode(2, i, 1, kf) = y2
          edit_flag(k) = edit_flag(k)+1
           do iz = 2, nzf(kf)
            xynode(1,i,iz,kf)=xynode(1,i,iz,kf) + dxx
            xynode(2,i,iz,kf)=xynode(2,i,iz,kf) + dyy
           enddo
        endif
        
c-- add a point
        if ( kt.eq.2 .and.  
     .   (( iclose( x1, y1, x, y, xnear).eq.1 .and. 
     .         iclose( x2, y2, xp1, yp1, xnear).eq.1 ) .or.
     .   ( iclose( x2, y2, x, y, xnear).eq.1 .and. 
     .         iclose( x1, y1, xp1, yp1, xnear).eq.1)) ) then
     
           do kk = nxf(kf), i+1, -1
            do iz = 1, nzf(kf)
             xynode(1,kk+1,iz,kf)=xynode(1,kk,iz,kf) 
             xynode(2,kk+1,iz,kf)=xynode(2,kk,iz,kf) 
            enddo
           enddo
           
           if ( x3.eq.zero .and. y3.eq.zero) then
            x3 = (x1+x2)/two
            y3 = (y1+y2)/two
           endif
     
            xynode(1,i+1,1,kf)= x3
            xynode(2,i+1,1,kf)= y3
            
            nxf(kf) = nxf(kf)+1
            
            dxx = x3-x1
            dyy = y3-y1
            
           do iz = 2, nzf(kf)
            xynode(1,i+1,iz,kf)=xynode(1,i,iz,kf) + dxx
            xynode(2,i+1,iz,kf)=xynode(2,i,iz,kf) + dyy
           enddo
            
           edit_flag(k) = edit_flag(k)+1
           goto 20
           
        endif
        
c-- delete point        
        if (kt.eq.3 .and. iclose( x1, y1, x, y,xnear).eq.1 ) then
          edit_flag(k) = edit_flag(k)+1
           do ix = i, nxf(kf)-1 
            do iz = 1, nzf(kf)
              xynode(1,ix,iz,kf)=xynode(1,ix+1,iz,kf) 
              xynode(2,ix,iz,kf)=xynode(2,ix+1,iz,kf)
            enddo
           enddo
          nxf(kf) = nxf(kf)-1 
        endif
        
  10  continue

  20  continue
      endif
      
      return
      end

c**********************************************************************
c* read line length file
c**********************************************************************

      subroutine readll 

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      character llfile*80 
      character*4 sname, sname1, sname2
      logical fncomment, fexist

      xcrit =1.0d0


c** read from file
       read (aline2,*,err=11,end=11) llfile, wtfac

  11  print *, 'Reading line lengths ', llfile(1:40)
      call existfile(llfile, fexist,1)
      if ( .not. fexist) return

c start with station positions

      k=num_ll_sites

      if (wtfac.eq.0) wtfac=1

      k1=kfopen(12)
      open (k1, file=llfile)

  5   read (k1, '(a250)', end=99) aline
      aline2=aline

      if ( aline(1:3).eq.'end' ) goto 19

      if ( len(aline).lt.10 .or. fncomment(aline(1:1)) ) GOTO 5
      read(aline, *) sname, xlon, xlat

       n=0
      if ( num_ll_sites.gt.0 ) then
       do i=1,num_ll_sites
       if( iclose(xlon,xlat,pos_ll(i,1),pos_ll(i,2),xnear).eq.1 
     .    .and. sname.eq.name_ll(i) ) n = i
       enddo
      endif

      if ( n.eq.0) then
       k=k+1
       n=k
      endif

      xlon=fnlong(xlon)
      pos_ll(n,2)=xlat
      pos_ll(n,1)=xlon
      name_ll(n) = sname
      goto 5

c* read in line lengths

  19  num_ll_sites = k
      k=num_ll

  15  read (k1, '(a250)', end=99) aline
      aline2=aline

      if ( aline(1:3).eq.'end' ) goto 99

      if ( len(aline).lt.10 .or. fncomment(aline(1:1)) ) GOTO 15
      read(aline, *) sname1, sname2, o,s

      n1=0
      n2=0

       do i=1, num_ll_sites
         if (sname1.eq.name_ll(i)) n1=i
         if (sname2.eq.name_ll(i)) n2=i
       enddo

       if ( n1.gt.0 .and. n2.gt.0) then
        k=k+1
        obs_ll(k) = o
        sig_ll(k) = s*wtfac
        x1 = pos_ll(n1,1)
        y1 = pos_ll(n1,2)
        x2 = pos_ll(n2,1)
        y2 = pos_ll(n2,2)
        num_site_ll(k,1)=n1
        num_site_ll(k,2)=n2
 
        call delaz(y2, x2, y1, x1, d, az)
        daz_ll(k,1) = d*d2x
        daz_ll(k,2) = az
       else
        print *, 'LL Site not found ',sname1, ' or ', sname2
       endif

       goto 15

   99 num_ll = k
      ik = kfclose (k1)

      return
      end

c**********************************************************************
c* read slip vector file
c**********************************************************************

      subroutine readsvs (a3)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character svfile*80, bline*256, a3*1, sname*30
      character*4 mb, fb, block_rename
      logical fncomment, fexist, block_check, samepole, bothfixed
      logical kch
      dimension v(3)

      block_check = .true.

c* fb is the name of the fixed block, mb is the name of the moving block


c* read from control file
      if (a3.eq.'d' ) then

      sv_file(1) = infile
c      call clearchar(sname, 30)
      sname = ""


       read(aline2,*, err=1, end=1) fb, mb, xlon, xlat, o, s, sname
  1    fb = block_rename(fb)
       mb = block_rename(mb)
       call getplate(fb, kp1, block_check)
       call getplate(mb, kp2, block_check)
       call checkpoles(kp1, kp2, samepole, bothfixed)
       if (samepole .or. bothfixed) return

      if (check_boundary) then
       call closest_blocks(xlon, xlat, kp1, kp2, kb1,kb2, kk, kch)
       if (kch) print *, 'DATUM: sv ', aline2
      endif

       num_sv=num_sv+1

       if(num_sv.gt.MAX_sv ) then
         print *, 'MAX_sv exceeded'
        call stop1
       endif

       k=num_sv
       xlon=fnlong(xlon)
       sv_obs(k)=o
       sv_sig(k)=s
       sv_pos(k,2)=xlat
       sv_pos(k,1)=xlon 
       kblk_sv(k,1) = kp1
       kblk_sv(k,2) = kp2
       sv_label(k) = sname
       ksv_file(k) = 1

       return
 
      endif
c--------------------------------------------------

c** read from file
      if (a3.eq.'f') then
        read (aline2,*,err=250,end=250) svfile, sv_wtfac 
      else
       read (aline2,*,err=11,end=11) svfile, fb, mb, sv_wtfac,
     .    (v(i),i=1,3) 
  11   fb = block_rename(fb)
       mb = block_rename(mb)
       call getplate(fb, kp1, block_check)
       call getplate(mb, kp2, block_check)
       call checkpoles(kp1, kp2, samepole, bothfixed)
       if (samepole .or. bothfixed) return
      endif

  250 print *, 'Reading slip vectors ', svfile
      call existfile(svfile, fexist,1)
      if ( .not. fexist) return

      num_svfile = num_svfile + 1
      sv_file(num_svfile) = svfile
c      call clearchar(sname, 30)
      sname = ""

      k=num_sv

      if (sv_wtfac.eq.0) sv_wtfac=1
      
      k1=kfopen(12)
      open (k1, file=svfile)
      
      do 5 i=1, 50000

      read (k1, '(a250)', end=99) aline
      aline2=aline

      call count_items (aline, bline, nitems)
c      print *, nitems, aline

      if ( aline(1:3).eq.'end' ) goto 99
      if ( fncomment(aline(1:1)) .or. nitems.lt.4 ) GOTO 5

      sname = trim(svfile(1:30))
      
      if (a3.eq.'f' .and. nitems .ge. 6) then
       read(aline,*,err=2,end=2) fb, mb, xlon, xlat, o, s, sname
  2    fb = block_rename(fb)
       mb = block_rename(mb)
       call getplate(fb, kp1, block_check)
       call getplate(mb, kp2, block_check)
       call checkpoles(kp1, kp2, samepole, bothfixed)
       if (samepole .or. bothfixed) goto 5
      endif


      if (a3 .ne. 'f' .and. nitems .ge. 4) then
        read(aline,*,err=3,end=3) xlon, xlat, o, s, sname
  3     continue
      endif

      if ( a3.eq.'1') call swap (xlon, xlat)

      if (check_boundary) then
       call closest_blocks(xlon, xlat, kp1, kp2, kb1,kb2,kk, kch)
       if (kch) print *, 'DATUM: sv ', aline2
      endif

       k=k+1
       if( k.gt.MAX_sv ) then
         print *, 'MAX_sv exceeded'
        call stop1
       endif

        xlon=fnlong(xlon)
        sv_obs(k)=o
        sv_sig(k)=s * sv_wtfac
        sv_pos(k,2)= xlat
        sv_pos(k,1)= xlon 
        kblk_sv(k,1) = kp1
        kblk_sv(k,2) = kp2
        sv_label(k) = sname
        ksv_file(k) = num_svfile

    5 continue

   99 num_sv = k
      ik = kfclose (k1)

      return
      end


c**********************************************************************
c* check poles for blocks
c**********************************************************************
      subroutine checkpoles( kp1, kp2, badpole, bbfixed)

c* see if the two blocks have the same pole, or if one has no pole
c* also check if both poles/strains are fixed in the inversion, and give warning

c* kp1, kp2 are the block numbers
c* np1, np2 are the block pole numbers
c* ns1, ns2 are the block strain numbers

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      character*4 block_name      

      logical badpole, bbfixed
      character*70 c70

      badpole = .false.
      bbfixed = .false.

      c70 = aline2(1:70)

c* undefined block(s)
      if (kp1.eq.0 .or. kp2.eq.0) then
       if (kp1.eq.0) print *, '*Undefined 1st block'
       if (kp2.eq.0) print *, '*Undefined 2nd block'
       print *, 'DATUM: ', c70
       badpole = .true.
       return
      endif

c* pole/strain numbers for the two blocks
      np1 = npole_block(kp1)
      np2 = npole_block(kp2)
      ns1 = nstrain_block(kp1)
      ns2 = nstrain_block(kp2)

c* no poles
      if ( np1.eq.0 .or. np2.eq.0) then

      if (np1.eq.0 ) then
        print *, '*No pole for block ',kp1,' ', block_name(kp1)
        print *, 'DATUM: ', c70
      endif

      if (np2.eq.0 ) then
        print *, '*No pole for block ',kp2,' ', block_name(kp2)
        print *, 'DATUM: ', c70
      endif

      badpole = .true.
      return

      endif

c* same poles
      if ( np1.eq.np2 .and. ns1.eq.ns2) then
       print *, '*Blocks have same poles and strain'
       print *, 'DATUM: ', c70
       badpole = .true.
       return
      endif

c* check if both poles fixed (if either pole is free)
      bbfixed = .true.
      
      do i = 1, num_pole_invert
       if ( npole_invert(i).eq.np1 ) bbfixed = .false.
       if ( npole_invert(i).eq.np2 ) bbfixed = .false.
      enddo
      
      do i = 1, num_strain_invert
       if ( nstrain_invert(i).eq.ns1 ) bbfixed = .false.
       if ( nstrain_invert(i).eq.ns2 ) bbfixed = .false.
      enddo
      
      if (bbfixed) then
       print *, '*Both poles and strains fixed'
       print *, 'DATUM: ', c70
      endif
      
      return
      end



c**********************************************************************
c* read fault slip rate file
c**********************************************************************

      subroutine readsrs (a3)

c*** read slip rates
c*** ktype controls how data are treated; see set_srs

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
      character srfile*80, bline*256, a3*1, sname*30
      character*4 mvng, fixd, block_rename

      logical fncomment, fexist, block_check, samepole, bothfixed
      logical kch

      z = 0.0d0
      srsig = z
      sig=z
      ker = 0
      sr_wtfac = 1.0d0
      block_check = .true.
      ktype = 0
      sr_min_sig = z
      srsig = 0.1

c* read data from input line in command file
      if (a3.eq.'d' ) then
       read(aline2,*, end=6,err=6) fixd, mvng, xlon, xlat, V1, V2, sa, 
     .   sr_min_sig, ktype, sname

c positive for now
c       V1=abs(V1)
   6   fixd = block_rename(fixd)
       mvng = block_rename(mvng)

      call getplate(fixd, kp1, block_check)
      call getplate(mvng, kp2, block_check)
      call checkpoles(kp1, kp2, samepole, bothfixed)
      if (samepole .or. bothfixed) return
       
      if (check_boundary) then
       call closest_blocks(xlon, xlat, kp1, kp2, kb1,kb2,kk, kch)
       if (kch) print *, 'DATUM: sr ', aline2
      endif
       
       num_sr = num_sr + 1
c       num_sr_file = 1
       
       sr_file(1) = infile

       k=num_sr
       sr_az(k) = sa
       sr_pos(k,2)=xlat
       sr_pos(k,1)=fnlong(xlon)
       kblk_sr(k,1) = kp1
       kblk_sr(k,2) = kp2
       sr_label (k) = sname
       if(ktype.eq.2) sig = sr_min_sig 

       call set_srs ( V1, V2, ktype, ker, sig)

       sr_obs(k,1) = V1
       sr_obs(k,2) = V2
       sr_sig(k)= sig
       if(ktype.eq.0) sr_sig(k)= max(sig, sr_min_sig )
       ksr_file(k) = 1
       ksr_type(k) = ktype

       return
      endif
c--------------------------------------------- 
c**** read from file
c** read from file that has block assignments within it
      ker=0
      if (a3.eq.'f' ) then
       read (aline2,*,err=250,end=250) srfile, sr_wtfac, srsig, ktype 

      else

c** read from file that has block assignments on input line in command file
        read (aline2, *, err=11, end=11) 
     .    srfile, fixd, mvng, sr_wtfac, srsig, ktype 

  11   fixd = block_rename(fixd)
       mvng = block_rename(mvng)
       call getplate(fixd, kp1, block_check)
       call getplate(mvng, kp2, block_check)
       call checkpoles(kp1, kp2, samepole, bothfixed)
       if (samepole .or. bothfixed) return
      endif

  250 print *, 'Reading slip rates ', srfile(1:40)
      sr_min_sig = srsig

      k=num_sr

      if (sr_wtfac.eq.0) sr_wtfac=1.0

c open file  
      call existfile(srfile, fexist,1)
      if ( .not. fexist) return
      
      k1=kfopen(12)
      open (k1, file=srfile)
      num_sr_file = num_sr_file + 1
      sr_file(num_sr_file) = srfile
      
      do 5 i=1, 50000
        sa=z
        sig=z
        sname= '    '

      read (k1, '(a250)', end=99, err=88) aline
  88  aline2 = aline

      call count_items (aline, bline, nitems)
c      print *, nitems, aline

      if ( aline(1:3).eq.'end' ) goto 99
      if ( fncomment(aline(1:1)) .or. nitems.lt.4) GOTO 5
      
c read from file that has blocks in file
      if (a3.eq.'f' .and. nitems .ge. 7) then
        read(aline, *, err=33, end=33) 
     .        fixd, mvng, xlon, xlat, v1, v2, sa, sname
  33    fixd = block_rename(fixd)
        mvng = block_rename(mvng)
        call getplate(fixd, kp1, block_check)
        call getplate(mvng, kp2, block_check)
        call checkpoles(kp1, kp2, samepole, bothfixed)
        if (samepole .or. bothfixed) goto 5
        if (nitems.lt.8) sname = trim(srfile(1:30))
      endif

c read from file without blocks in file
      if (a3 .ne. 'f' .and. nitems .ge. 5) then
        read(aline,*,err=7,end=7) xlon, xlat, v1, v2, sa, sname
 7      if(nitems.lt.6) sname = trim(srfile(1:30))
      endif


c positive for now
c       V1=abs(V1)

      call set_srs (V1, V2, ktype, kerin, sig)

      k=k+1
      sr_obs(k,1)=v1
      sr_obs(k,2)=v2
      sr_sig(k)= max( sr_min_sig, sig * sr_wtfac)
      sr_az(k) = sa
      sr_pos(k,2)=xlat
      sr_pos(k,1)=fnlong(xlon)
      kblk_sr(k,1) = kp1
      kblk_sr(k,2) = kp2
      sr_label(k) = sname
      ksr_type(k) = ktype
      ksr_file(k) = num_sr_file

c      if (check_boundary) then
c       call closest_blocks(xlon, xlat, kp1, kp2, kk, kch)
c       if (kch) print *, 'DATUM: sr ', aline2
c      endif

    5 continue

   99 num_sr = k
      ik = kfclose (k1)
 
      return
      end

c****************************************************
c set up different types of fault slip rates
c
c ker = 0 means Gaussian
c     = 1 means min/max
c
c ktype = 0 Gaussian horizontal read in, used as Gaussian
c         1 Gaussian horizontal read in, used as Uniform (min/max)
c         2 Uniform horizontal read in, used as Uniform
c         3 Uniform horizontal read in, used as Gaussian
c         4 Gaussian vertical read in, used as Gaussian
c         5 Uniform vertical read in, used as Uniform
      
      subroutine set_srs ( V1, V2, ktype, ker, sig)
      implicit real*8 (a-h,o-z)

      s1=v1
      s2=v2
      z=0.0d0

      if ( ktype.eq.0) then
       v1 = s1
       v2 = z
       sig = s2
       ker = 0
      elseif ( ktype.eq.1) then
       v1 = max(0.0d0, s1-s2)
       v2 = s1+s2
       ker = 1
       sig = s2
      elseif ( ktype.eq.2) then
       v1 = s1
       v2 = s2
       ker = 1
       if ( sig.eq.z) sig = (s2-s1)/2.0d0
      elseif ( ktype.eq.3) then
       v1 = (s1+s2)/2.0d0
       v2 = z
       ker = 0
       if ( sig.eq.z) sig = (s2-s1)/2.0d0
      elseif ( ktype.eq.4) then
       v1 = s1
       v2 = z
       sig = s2
       ker = 0
      elseif ( ktype.eq.5) then
       v1 = s1
       v2 = s2
       ker = 1
       if ( sig.eq.z) sig = (s2-s1)/2.0d0
      endif

      return
      end

c***************************************************************
      subroutine defstop(modname, yestop, inkey)
c if file stop, XXXXstop or stopXXXX exists, end iterating
c XXXX = model name
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      logical yes0, yes1, yes2, yestop
      character*4 modname

c-- using tdefavail.c
      integer key, charavail
      character*1 inkey
      inkey = ' '

c-- look for 'stop' file
      yestop = .false.
      call existfile ( 'stop', yes0,0)
      call existfile (modname//'stop', yes1,0)
      call existfile ('stop'//modname, yes2,0)
      yestop = ( yes0 .or. yes1 .or. yes2)
      if (yestop) then
        print *, '**** stop file detected ****'
        last_iter = .true.
        return
      endif
      
c-- using tdefavail.c
c-- check for a 'q', from Dave Hollinger
      key = charavail()
      inkey = char(key)
      call lcase (inkey,1)
      if( inkey.eq.'q' .or. inkey.eq.'Q' ) then
        yestop = .true.
        last_iter = .true.
        print *, '**** q key detected ****'
      endif
      
      return
      end
c***************************************************************
      subroutine existfile( infile, fexist, kwincode)
c* check if file exists, kcode =1 to print
      character infile*80, infile2*80
      logical fexist
c LMB INQUIRE only if file name is non-blank
c     inquire( file=infile, exist=fexist)
      infile2 = adjustl(infile)
      fexist = .false.
      if ( infile2 .ne. ' ' ) then
       inquire( file=infile2, exist=fexist)
      endif
       if ( .not. fexist .and. kwincode.eq.1) 
     .       print *, '****** File does not exist: '//infile2
      return
      end


c***********************************************************
      subroutine sphcar(xlat,xlon,omega,x1,x2,x3)
c     change spherical coordinates to cartesian
c     xlat and xlon are assumed to be in degrees
c   from Chuck DeMets

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"
      
      elat = xlat*d2r 
      elon = xlon*d2r 
      eomega = omega 
      x1 = eomega * dcos(elat) * dcos(elon)
      x2 = eomega * dcos(elat) * dsin(elon)
      x3 = eomega * dsin(elat)
      return
      end

c***********************************************************
      subroutine dist ( slon, slat, plon, plat, del)

c get distance between 2 points, in degrees

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      one=1.0d0
      half=0.5d0
      zero=0.0d0
      ef =0.993277d0

c --- dist
      tslat = slat*d2r
      tslon = slon*d2r
      teplon = plon*d2r
      teplat = plat*d2r

      glat=datan(ef*dtan(tslat))
      dca = dcos(glat)*dcos(tslon)
      dcb = dcos(glat)*dsin(tslon)
      dcc = dsin(glat)

      geplat = datan(ef*dtan(teplat))
      epdca = dcos(geplat)*dcos(teplon)
      epdcb = dcos(geplat)*dsin(teplon)
      epdcc = dsin(geplat)

c---- calculate distance 
      par1=(one-half*((dca -epdca)**2 +
     .  (dcb -epdcb)**2+(dcc  - epdcc)**2))

        del  = r2d*datan(dsqrt((one/(par1*par1)) - one))
        if (par1.lt.zero) del = 180.0d0 - del
        if (del.eq.zero) del = 1.0d-8

      return
      end

c***********************************************************
      subroutine distkm ( xlon1, xlat1, xlon2, xlat2, deltakm)

c get distance between 2 points, in kilometers

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      call dist(xlon1, xlat1, xlon2, xlat2, del)

      deltakm = del*d2x

      return
      end

c***********************************************************

      subroutine carsph(w1,w2,w3,elat,elon,emega)
c     change cartesian coordinates to spherical
c     output elat and elon are in degrees
c   from Chuck DeMets

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      z=0.0d0

      x1=w1
      x2=w2
      x3=w3

      elat=z
      elon=z
      emega=z

      if (x1.eq.z .and. x2.eq.z .and. x3.eq.z ) return

      x = dsqrt(x1*x1 + x2*x2 + x3*x3)
      elat = dasin(x3/x)*r2d
      elon = artan2(x2, x1)*r2d
      emega = x
      return
      end

c***********************************************************
      function artan2(y,x)
       implicit real*8 (a-h,o-z)
       include "tdefcons.h"

c   version of artan2 which will not choke on (0,0) entry
c   from Chuck DeMets, ~2000

       hpii = pii/2.0d0
       zero = 0.0d0

       xx=x
       yy=y
       artan2 = zero
       
        if (xx.eq.zero) then
            artan2 = sign(hpii,yy)
        endif

        if (xx.gt.zero .or. xx.lt.zero) then
               artan2 = atan(yy/xx)
        endif

        if (xx.lt.zero) then
            artan2 = artan2+sign(pii,yy)
        endif

        return
       end

c***********************************************************
      subroutine readfs(a3)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character fs_file*80, a3*1
      character*4 hw, fw

      logical fexist, block_check, nopole, bothfixed

      block_check = .true.
      faz = 0.0d0


      if (a3.eq.'p' ) then
       read(aline2,*,err=1,end=1) hw, fw, xlon, xlat, faz
  1    call getplate(fw, kp1, block_check)
       call getplate(hw, kp2, block_check)
       call checkpoles(kp1, kp2, nopole, bothfixed)
        if (nopole) then
          print *, 'FSp: ',fw,' or ', hw, ' not found'
        else
         num_fs = num_fs+1
          if (num_fs.gt.MAX_num_fs) then
           print *, 'MAX_num_fs exceeded'
          call stop1
          endif
         fs_block(num_fs,1)=kp1
         fs_block(num_fs,2)=kp2
         fs_xy(num_fs,1)=fnlong(xlon)
         fs_xy(num_fs,2)=xlat
         fs_xy(num_fs,3)=faz

        endif

       else

        read (aline2, *, err=65,end=65) fs_file, hw, fw 

  65   call getplate(fw, kp1, block_check)
       call getplate(hw, kp2, block_check)
       call checkpoles(kp1, kp2, nopole, bothfixed)
       if ( nopole ) return

       call existfile( fs_file, fexist, 1)

       if ( fexist ) then
       print *, 'Reading FS file ', fs_file
       
       kfile=kfopen(17)
       open (kfile, file=fs_file )

       do j=1,1000
        faz = 0.0d0
         read (kfile, *, end=88 ) xlon, xlat, faz
         num_fs=num_fs+1
          if (num_fs.gt.MAX_num_fs) then
           print *, 'MAX_num_fs exceeded'
          call stop1
          endif
         fs_block(num_fs,1)=kp1
         fs_block(num_fs,2)=kp2
         fs_xy(num_fs,1)=fnlong(xlon)
         fs_xy(num_fs,2)=xlat
         fs_xy(num_fs,3)=faz
       enddo

  88   ik = kfclose(kfile)

       endif

       endif

       return
       end

c****************************************************************

      subroutine srproject ( xlon, xlat, olato, olono, aze, xwide, xlen)

c* subroutine to project point xlon,xlat onto a spherical profile line
c* based on Steve Roecker's program crsevps2.f

c----------------------------------------------------------- 
c 
c        aze         azmiuth of plane of projection, counterclockwise 
c                    from north (str is clockwise from north) 
c 
c        xwide is distance from profile line in km
c        xlen is distance along profile line in km
c---------------------------------------------------------- 

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      common /parm3/ b11,b12,b21,b22,b23,a21,a22,b31,b32,b33,a11,a12 

      twopi = 2.0d0*pii 
      hpi = pii/2.0d0 
      degrad = d2r
      re = Erad
       
      oz = 0.d0
      dep = 0.d0

      olat = olato 
      olon = olono 
      rb = re - oz 
      dep = dep - oz 
      xrad = olat*degrad 
      ts = hpi - glat(xrad) 
      if(olon.lt.0.0d0) olon = 360.0d0 + olon 
      ps = olon*degrad 

c------calculate direction cosines from x1 to x2 
         cost = dcos(ts) 
         sint = dsin(ts) 
         cosp = dcos(ps) 
         sinp = dsin(ps) 
         b11 = -sinp 
         b12 = cosp 
         b21 = cost*cosp 
         b22 = cost*sinp 
         b23 = -sint 
         b31 = -sint*cosp 
         b32 = -sint*sinp 
         b33 = -cost 

c------calculate direction cosines from x2 to x3 
         bt = (180.0d0 - aze)*degrad 
         a21 = -dsin(bt) 
         a22 = dcos(bt) 
         a11 = a22 
         a12 = -a21 

c* project point
        depth=0.0d0
        xrad = xlat*degrad 
        x1 = hpi - glat(xrad) 
        y1 = xlon*degrad 
        if(y1.gt.pii) y1 = y1 - twopi 
        z1 = re - depth 

        call convp(x1,y1,z1,xwide,xlen,ydown) 

        return
        end


c****************************************************************

      subroutine convp(xlat,xlon,zp,xw,x,y) 
c -- convp.f -- to convert from global to cross-section coordinates
c  from SW Roecker 2003

      implicit real*8 (a-h,o-z)
      common /parm3/ b11,b12,b21,b22,b23,a21,a22,b31,b32,b33,a11,a12 
c------convert coords to x1 space 
         ts = xlat 
         ps = xlon 
         z = zp 
         x1 = z*dsin(ts)*dcos(ps) 
         y1 = z*dsin(ts)*dsin(ps) 
         z1 = z*dcos(ts) 
c------convert coords to x2 space 
         x2 = x1*b11 + y1*b12 
         y2 = x1*b21 + y1*b22 + z1*b23 
         z2 = x1*b31 + y1*b32 + z1*b33 
c------convert coords to x3 space 
         x3 = x2*a11 + y2*a12 
         y3 = x2*a21 + y2*a22 
         z3 = dsqrt(x3*x3 + z2*z2) 
c         z = dsqrt(z*z - y3*y3) 
         rask=1.0d0
         x = y3*rask 
         y = z3*rask 
        xw = x3 
        return 
        end 
c*********************************************************************
      subroutine trislope(xi1, yi1, zi1, xi2, yi2, zi2, 
     .   xi3, yi3, zi3, dip, strike)

c* get dip and strike of a triangle
c* 3 point slope of plane Ax + By + Cz + D = 0
c* z = -A/C x - B/C y - D/C

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      zero = 0.0d0
      i0 = 0

      dip = zero
      strike = zero

c* two corners should be at same depth, get strike from these
      if ( zi1.eq.zi2) call delaz ( yi2, xi2, yi1, xi1, del, az)
      if ( zi1.eq.zi3) call delaz ( yi3, xi3, yi1, xi1, del, az)
      if ( zi2.eq.zi3) call delaz ( yi3, xi3, yi2, xi2, del, az)

      
      call project ( xi1, yi1, xi1, yi1, x1, y1)
      call project ( xi2, yi2, xi1, yi1, x2, y2)
      call project ( xi3, yi3, xi1, yi1, x3, y3)

      z1=zi1
      z2=zi2
      z3=zi3

      A = y1 *(z2 - z3) + y2* (z3 - z1) + y3* (z1 - z2)
      B = z1 *(x2 - x3) + z2* (x3 - x1) + z3* (x1 - x2)
      C = x1 *(y2 - y3) + x2* (y3 - y1) + x3* (y1 - y2)

      if ( C.eq.0.d0) return

c      A = y(1)*(z(2) - z(3)) + y(2)*(z(3) - z(1)) + y(3)*(z(1) - z(2))
c      B = z(1)*(x(2) - x(3)) + z(2)*(x(3) - x(1)) + z(3)*(x(1) - x(2))
c      C = x(1)*(y(2) - y(3)) + x(2)*(y(3) - y(1)) + x(3)*(y(1) - y(2))
c      D = -( x(1)* (y(2)* z(3) - y(3)* z(2)) 
c     .    + x(2)* (y(3)* z(1) - y(1)* z(3)) 
c     .    + x(3)* (y(1)* z(2) - y(2)* z(1)) )


        dzdx = -A/C
        dzdy = -B/C

c* get strike

      strike= az
      if ( dzdy.eq.zero ) then
c        strike = 0.0d0
c        if ( dzdx.lt.zero ) strike = 180.0d0
      elseif ( dzdx.eq.zero) then
c        strike = 90.0d0
c        if ( dzdy.gt.zero) strike = 270.0d0
      else
c        strike = datan(dzdx/dzdy)*r2d - 90.0d0
      endif

c* get dip
        slope = dsqrt ( dzdx**2 + dzdy**2 )
        dip = datan(slope)*r2d

        strike = fnlong(strike)

      return
      end

c****************************************************************

      subroutine relvel (kflag, kmove, kfixd, xlon, xlat, Ve, Se, Vn, 
     .   Sn, rho)

c   computes relative velocity between 2 blocks at a point

c   takes into account block rotations and block strains

c   kflag = 1 --> rotation only
c         = 2 --> strain only
c         = 3 --> rotation and strain

c   kfixd and kmove are block numbers, not pole numbers

c   the array npole_block(nblock) has the pole number for block number nblock

c   velocity vector is for block kmove relative to block kfixd

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension p(9)
      logical kverr

      zero=0.0d0

c kverr is flag for getting the velocity uncertainties
c supressed during inversions
      kverr = vel_errors

      Ves = zero
      Vns = zero
      Ver = zero
      Vnr = zero
      Ve  = zero
      Vn  = zero
      Se  = zero
      Sn  = zero
      rho = zero
      Ses = zero
      Sns = zero

c      if (kmove.le.0 .or. kfixd.le.0) return
c      if (kmove.gt.MAX_block .or. kfixd.gt.MAX_block) return

c* rotation part

      if ( kflag.eq.1 .or. kflag.eq.3) then

      kfx = npole_block(kfixd)
      kmv = npole_block(kmove)

       if (kmv .ne. kfx) then

c* difference the poles
       do i=1,3
        p(i) = poles(kmv,i) - poles(kfx,i)
       enddo

c* add covariances
       do i=4,9
        p(i) = poles(kmv,i) + poles(kfx,i)
       enddo

       call vomega( kverr, p, xlon, xlat, Ver, Ser, Vnr, Snr, rho)

       Ve = Ver
       Vn = Vnr
       Se = Ser
       Sn = Snr

       endif
      endif

c* strain part

      if ( kflag.eq.2 .or. kflag.eq.3 ) then

       call calcstrain( kfixd, xlon, xlat, Vfx, Sfx, Vfy, Sfy)
       call calcstrain( kmove, xlon, xlat, Vmx, Smx, Vmy, Smy)

c** fixed this 10/15/04, if straining block = ref block
       if (nblock_ref.eq.kfixd) then
        Ves = Vmx
        Vns = Vmy
        Ses = Smx
        Sns = Smy
       else
        Ves = Vmx - Vfx
        Vns = Vmy - Vfy
        Ses = dsqrt(Smx*Smx + Sfx*Sfx)
        Sns = dsqrt(Smy*Smy + Sfy*Sfy)
c        Ves = Vmx  
c        Vns = Vmy  
c        Ses = Smx
c        Sns = Smy
       endif

       Ve = Ves
       Vn = Vns
       Se = Ses
       Sn = Sns

      endif


c** if doing both strain and rotation
      if ( kflag.eq.3) then
        Ve = Ver + Ves
        Vn = Vnr + Vns
        Se = dsqrt(Ser*Ser + Ses*Ses)
        Sn = dsqrt(Snr*Snr + Sns*Sns)
      endif

      return
      end
c*******************************************************************************
      subroutine setflags(nflags, flagline)      

c** set the flags

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
      logical write_input
      common /cm3/ write_input

      logical quit_on_error, input_error
      common /er/ quit_on_error, input_error

      character*80 flagline(50), c4(20)*4, c*4, c3*3
      character*200 valid_flags

      valid_flags =' igm int ddc cov pos wcv wdr phf ph0 ph1 sim'//
     .             ' ahw afw mif rnd gcv inf pen eko vrb inv ins'//
     .             ' pio l1n dst prm cr2 mkb equ syn atr vtw eqk'//
     .             ' dgt erq sra kml erq ddc ela inp isl iof itr'//
     .             ' eqv eqn atr sts        '
  


      do j=1, nflags
c       call clearchar (c4, 80)
        c4=""
c       do i=1,20
c        c4(i) = "    "
c        enddo
       read (flagline(j), *, err=1,end=1) (c4(k), k=1,20)

  1    do k=1,20
        c = c4(k)
        c3 = c(2:4)  
        kk  = index(valid_flags, c3)
        if(kk.eq.0) print *, 'Flag ',c,' not recognized '

c plot files for insar
      if (c.eq.'+igm' ) insar_gmt = .true.
      if (c.eq.'-igm' ) insar_gmt = .false.
     
c kml
      if (c.eq.'+kml' ) makekml = .true.
      if (c.eq.'-kml' ) makekml = .false.

c interactive
      if (c.eq.'+int' ) interactive = .true.
      if (c.eq.'-int' ) interactive = .false.

c change HW and FW
      if (c.eq.'+ahw' ) adjust_hw = .true.
      if (c.eq.'-ahw' ) adjust_hw = .false.

      if (c.eq.'+afw' ) adjust_fw = .true.
      if (c.eq.'-afw' ) adjust_fw = .false.

c L1 norm
c      if (c.eq.'+l1n' ) l1norm = .true.
c      if (c.eq.'-l1n' ) l1norm = .false.

c use magnitude of GPS sigma as cutoff criterion
      if (c.eq.'+sra' ) use_sr_az = .true.
      if (c.eq.'-sra' ) use_sr_az = .false.

c use magnitude of GPS sigma as cutoff criterion
c      if (c.eq.'+sgm' ) use_sigma_mag = .true.
c      if (c.eq.'-sgm' ) use_sigma_mag = .false.

c quit if error in encountered
      if (c.eq.'+erq' ) quit_on_error = .true.
      if (c.eq.'-erq' ) quit_on_error = .false.

c force node values to decrease downdip on all faults
      if (c.eq.'ndec' ) node_decrease = .true.
      if (c.eq.'+ddc' ) node_decrease = .true.
      if (c.eq.'-ddc' ) node_decrease = .false.

c calculate parameter uncertainties
      if (c.eq.'noco' ) getcovariance = .false.
      if (c.eq.'+cov' ) getcovariance = .true.
      if (c.eq.'-cov' ) getcovariance = .false.

c longitude range
      if (c.eq.'-pos' ) long_pos = .false.
      if (c.eq.'+pos' ) long_pos = .true.
      
c write covariance matrix to file
      if (c.eq.'wcov' ) write_cov = .true.
      if (c.eq.'+wcv' ) write_cov = .true.
      if (c.eq.'-wcv' ) write_cov = .false.

c write derivative matrix to file
      if (c.eq.'wder' ) write_der = .true.
      if (c.eq.'+wdr' ) write_der = .true.
      if (c.eq.'-wdr' ) write_der = .false.

c fix phi for all faults to current value
      if (c.eq.'fltf' ) all_fault_fix = .true.
      if (c.eq.'+fxf' ) all_fault_fix = .true.
      if (c.eq.'-fxf' ) all_fault_fix = .false.
      if (c.eq.'+phf' ) all_fault_fix = .true.
      if (c.eq.'-phf' ) all_fault_fix = .false.

c set phi for all faults to zero
      if (c.eq.'flt0' ) all_fault_0 = .true.
      if (c.eq.'+ph0' ) all_fault_0 = .true.
      if (c.eq.'-ph0' ) all_fault_0 = .false.

c set phi for all faults to one
      if (c.eq.'flt1' ) all_fault_1 = .true.
      if (c.eq.'+ph1' ) all_fault_1 = .true.
      if (c.eq.'-ph1' ) all_fault_1 = .false.

c write simplex in _sa.out file
      if (c.eq.'wsim' ) wsimplex = .true.
      if (c.eq.'+sim' ) wsimplex = .true.
      if (c.eq.'-sim' ) wsimplex = .false.

c look for volcanoes file
      if (c.eq.'votw' ) read_votw = .true.
      if (c.eq.'+vtw' ) read_votw = .true.
      if (c.eq.'-vtw' ) read_votw = .false.

c look for earthquakes file
      if (c.eq.'+eqk' ) quakes = .true.
      if (c.eq.'-eqk' ) quakes = .false.

c check which boundary a datum is closest to
      if (c.eq.'+ckb' ) check_boundary = .true.
      if (c.eq.'-ckb' ) check_boundary = .false.

c mapinfo .mid/.mif file output
      if (c.eq.'mapi' ) mapinfo = .true.
      if (c.eq.'+mif' ) mapinfo = .true.
      if (c.eq.'-mif' ) mapinfo = .false.

c add random noise to .vec file 
      if (c.eq.'ran0' ) add_rand0 = .true.
      if (c.eq.'rand' ) add_rand = .true.
      if (c.eq.'+rnd' ) add_rand = .true.
      if (c.eq.'-rnd' ) add_rand = .false.
      
c use GPS covariance
      if (c.eq.'nogc' ) gps_cov = .false.
      if (c.eq.'-gcv' ) gps_cov = .false.
      if (c.eq.'+gcv' ) gps_cov = .true.

c write EXPT_info.out file (details of individual fault patches)
      if (c.eq.'info' ) write_info = .true.
      if (c.eq.'+inf' ) write_info = .true.
      if (c.eq.'-inf' ) write_info = .false.

c write penalties
      if (c.eq.'wpen' ) write_pens = .true.
      if (c.eq.'+pen' ) write_pens = .true.
      if (c.eq.'-pen' ) write_pens = .false.

c* write input lines to screen
      if (c.eq.'+inp' ) write_input = .true.
      if (c.eq.'-inp' ) write_input = .false.
      if (c.eq.'+eko' ) write_input = .true.
      if (c.eq.'-eko' ) write_input = .false.

c* write extra information to screen
      if (c.eq.'+vrb' ) verbose = .true.
      if (c.eq.'-vrb' ) verbose = .false.

c invert
      if (c.eq.'+inv' ) invert = .true.
      if (c.eq.'-inv' ) invert = .false.
      
c solve for insar offset parameters
      if (c.eq.'+iof' ) insar_offset = .true.
      if (c.eq.'-iof' ) insar_offset = .false.
      if (c.eq.'+ins' ) insar_offset = .true.
      if (c.eq.'-ins' ) insar_offset = .false.

c solve for insar slope parameters
      if (c.eq.'+isl' ) insar_slope = .true.
      if (c.eq.'-isl' ) insar_slope = .false.
      if (c.eq.'+ins' ) insar_slope = .true.
      if (c.eq.'-ins' ) insar_slope = .false.

c solve for insar tropospheric corrections
      if (c.eq.'+itr' ) insar_tropo = .true.
      if (c.eq.'-itr' ) insar_tropo = .false.

c calculate residual deformation gradients and strains
      if (c.eq.'+dgt' ) do_dgt = .true.
      if (c.eq.'-dgt' ) do_dgt = .false.

c write synthetic time series
      if (c.eq.'+syn' ) w_syn = .true.
      if (c.eq.'-syn' ) w_syn = .false.
      
c use PREM rigidities
      if (c.eq.'+prm' ) then
        use_prem = .true.
        use_crust2 = .false.
      endif
      if (c.eq.'-prm' ) use_prem = .false.

c use CRUST2 rigidities
      if (c.eq.'+cr2' ) then
         use_crust2 = .true.
         use_prem = .false.
      endif
      if (c.eq.'-cr2' ) use_crust2 = .false.
      
c check for velocity equates
      if (c.eq.'+equ' .or. c.eq.'+eqv' ) check_v_equates = .true.
      if (c.eq.'-equ' .or. c.eq.'-eqv' ) check_v_equates = .false.

c check for name equates
      if (c.eq.'+eqn' ) check_name_equates = .true.
      if (c.eq.'-eqn' ) check_name_equates = .false.

c* make blocks from faults
      if (c.eq.'+mkb' ) make_blocks = .true.
      if (c.eq.'-mkb' ) make_blocks = .false.

c* write individual atr files for each fault
      if (c.eq.'+atr' ) fault_atr = .true.
      if (c.eq.'-atr' ) fault_atr = .false.

c* write elastic strain velocities by fault
      if (c.eq.'+ela' ) ela_by_fault = .true.
      if (c.eq.'-ela' ) ela_by_fault = .false.

c* backup pio file
      if (c.eq.'+pio' ) pioflag = .true.
      if (c.eq.'-pio' ) pioflag = .false.

c* sort time series
      if (c.eq.'+sts' ) sort_ts = .true.
      if (c.eq.'-sts' ) sort_ts = .false.


      
c************************************************************
c* author's undocumented options for testing
c valid flags
      if (c.eq.'robm' ) myflag = .true.

       if (myflag) then

c gpf
      if (c.eq.'+gfs' ) readgflag = .true.
      if (c.eq.'-gfs' ) readgflag = .false.


      if (c.eq.'+air' ) wair = .true.
      
c  +fgf flag set
      if (c.eq.'+fgf' ) funcgfs = .true.
      if (c.eq.'-fgf' ) funcgfs = .false.

c calculate forward model
c      if (c.eq.'noca' ) kcalculate = .false.
c      if (c.eq.'-for' ) kcalculate = .false.
c      if (c.eq.'+for' ) kcalculate = .true.

c time dependence
      if (c.eq.'+tim' ) time_dependence = .true.
      if (c.eq.'-tim' ) time_dependence = .false.

c use_node_delay
      if (c.eq.'+ndl' ) use_node_delay = .true.
      if (c.eq.'-ndl' ) use_node_delay = .false.

c check data
      if (c.eq.'+sph' ) sphstrain = .true.
      if (c.eq.'-sph' ) sphstrain = .false.

c check data
      if (c.eq.'+ckd' ) chk_data = .true.
      if (c.eq.'-ckd' ) chk_data = .false.
      
c set random seed IDUM = 1
      if (c.eq.'+rs1' ) rand_seed = .true.
      if (c.eq.'-rs1' ) rand_seed = .false.

c hold transient parameters within amount of change 
      if (c.eq.'-cks') check_psig = .false.
      if (c.eq.'+cks') check_psig = .true.
      
c read errors from file
      if (c.eq.'+rer') read_errors = .true.
      if (c.eq.'-rer') read_errors = .false.

c calculate GPS pole parameters
      if (c.eq.'+gpp' ) get_gps_parm = .true.
      if (c.eq.'-gpp' ) get_gps_parm = .false.

c calculate block pole parameters
      if (c.eq.'+blp' ) get_blk_parm = .true.
      if (c.eq.'-blp' ) get_blk_parm = .false.

c calculate fault parameters
      if (c.eq.'+ftp' ) get_flt_parm = .true.
      if (c.eq.'-ftp' ) get_flt_parm = .false.

c calculate strain rate tensor parameters
      if (c.eq.'+stp' ) get_str_parm = .true.
      if (c.eq.'-stp' ) get_str_parm = .false.

c use parameter covariance
      if (c.eq.'+pcv' ) parm_cov = .true.
      if (c.eq.'-pcv' ) parm_cov = .false.

c grid of residuals output
      if (c.eq.'+grs' ) gres = .true.
      if (c.eq.'-grs' ) gres = .false.

c* backup pio file
      if (c.eq.'+pio' ) pioflag = .true.
      if (c.eq.'-pio' ) pioflag = .false.
      
c* output only time series that has data
      if (c.eq.'+dsp' ) data_span = .true.
      if (c.eq.'-dsp' ) data_span = .false.
      
c damp block strain
      if (c.eq.'+dst' ) damp_strain = .true.
      if (c.eq.'-dst' ) damp_strain = .false.

c misc, undocumented flags
c      if (c.eq.'robm' ) myflag = .true.
      if (c.eq.'suma' ) sumflag = .true.
      if (c.eq.'+pnw' ) pnwflag = .true.
      if (c.eq.'-pnw' ) pnwflag = .false.
      if (c.eq.'+wus' ) wusflag = .true.
      if (c.eq.'-wus' ) wusflag = .false.
      if (c.eq.'+bad' ) keep_bad = .true.
      if (c.eq.'whdr' ) write_hdr = .true.
      if (c.eq.'scec' ) scec = .true.
      if (c.eq.'-srs' ) no_wt_srs = .true.
      if (c.eq.'-svs' ) no_wt_svs = .true.
      if (c.eq.'-gps' ) no_wt_gps = .true.
      if (c.eq.'-mts' ) no_wt_mts = .true.
      if (c.eq.'+xe3' ) xe3 = .true.
      
c write long form of vectors and residuals
      if (c.eq.'+rwk' .or. c.eq.'+bob') wkbob = .true.
      if (c.eq.'-bob' .or. c.eq.'-rwk') wkbob = .false.

c* write out HTDP grid file
      if (c.eq.'htdp' ) htdp = .true.

c write faults every km
      if (c.eq.'+w1k' ) wf1km = .true.

c write fault segments
      if (c.eq.'+wfs' ) wfsegs = .true.

c* force Wang gamma to equal 5.0
      if (c.eq.'+gm5' ) gamma5 = .true.
      if (c.eq.'-gm5' ) gamma5 = .false.

c --
      if (c.eq.'+agf' ) do_all_gf = .true.
      if (c.eq.'-agf' ) do_all_gf = .false.

c* write all faults to .nod file, even if not used
      if (c.eq.'+all' ) write_all_fault = .true.
      if (c.eq.'-all' ) write_all_fault = .false.
      
      endif
c************************************************************

      enddo

      enddo
c       write_input = .true.
c      print *, 'FLAGS set'
c      call printflag(node_decrease, 'Node decrease                 ')
c      call printflag(getcovariance, 'Parameter uncertainties       ')
c      call printflag(gps_cov,       'GPS covariance                ')
c      call printflag(use_crust2,    'Using CRUST2 rigidities       ')
c      call printflag(use_prem,      'Using PREM rigidities         ')
c      call printflag(invert,        'Inverting                     ')
c      call printflag(.not. okada,   'Layering                      ')
c      call printflag(get_flt_parm,  'Fault inversion               ')
c      call printflag(get_blk_parm,  'Block inversion               ')
c      call printflag(get_str_parm,  'Strain inversion              ')
c      call printflag(get_gpp_parm,  'GPS file inversion            ')
c      call printflag(pnwflag,       'PNW flag                      ')
c       call printflag(write_input,   'write input                   ')

      return
      end
      
c********************************************************************

      subroutine printflag ( kflag, title )

      logical kflag
      character*30 title

      if ( kflag ) then
       write(*, '(a4, a30)') 'ON  ', title
      else
       write(*, '(a4, a30)') 'OFF ', title
      endif

      return
      end

c********************************************************************

      subroutine vertaxrot (xpt, ypt, p, v)
c* vertical axis rotation at xpt, ypt
c*  Omega dot position
      implicit real*8 (a-h,o-z)

      dimension p(9)

        Wx = p(1)
        Wy = p(2)
        Wz = p(3)

        om=1.0d0
        call sphcar(ypt,xpt,om,x,y,z)
        v = Wx*x + Wy*y + Wz*z
        return
        end

c********************************************************************
      subroutine getblock(xpt, ypt, kblock)
c** find the block for a point

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      dimension xcc(MAX_corner), ycc(MAX_corner)

      kblock = 0

      if ( .not. use_blocks ) return

      do j=1, nblocks
         nb = nc_block(j)
       if(block_flag(j) .and. nb.gt.2 ) then
         if(nb.gt.MAX_corner) print *, nb, 'too big'
          
        do i=1, nb 
         xcc(i)=blockxy(i,j,1)
         ycc(i)=blockxy(i,j,2)
        enddo

        call inside ( xpt, ypt, xcc, ycc, nb, insde)
        if ( abs(insde).eq.1 ) then
         kblock=j
         return
        endif
       endif
      enddo
      return
      end

c********************************************************************
      subroutine readrot (a3)

      implicit real*8 (a-h,o-z)
      character rot_file*80, a3*1, sname*40
      logical fncomment, fexist

      include "tdefcom1.h"

      if (a3.eq.'d' ) then

      read(aline2,*,err=1,end=1) xlon, xlat, o, s, sname
  1   xlon = fnlong(xlon)
      call getblock(xlon, xlat, kp)
      if (kp.eq.0 ) return

      if (num_rot.eq.MAX_rot) then
        print *, 'Exceeding maximum number of rotation rates', MAX_rot
        return
      endif

       num_rot=num_rot+1

       k=num_rot
       rot_obs(k)=o
       rot_sig(k)=s
       rot_pos(k,1)=fnlong(xlon)
       rot_pos(k,2)=xlat
       kblk_rot(k) = kp
       rot_name (k) = sname
c       print *, sname, ' ', block_name(kp)
 
      else

       read (aline2, *, err=250,end=250) rot_file, rot_wtfac

  250 print *, 'Reading ', rot_file
      call existfile(rot_file, fexist,1)

      if ( .not. fexist) then
       print *, rot_file, ' does not exist'
       return
      endif

      num_rotfile = num_rotfile + 1

      k=num_rot

      if (rot_wtfac.eq.0) rot_wtfac=1
      
      k1=kfopen(12)
      open (k1, file=rot_file)
      
      do 5 i=1, 10000

      read (k1, '(a256)', end=99) aline

      if ( aline(1:3).eq.'end' ) goto 99
      
      if ( len(aline).lt.10 .or. fncomment(aline(1:1)) ) GOTO 5
      
      read(aline,*,err=2,end=2) xlon, xlat, o, s, sname

      if (k.eq.MAX_rot) then
        print *, 'Exceeding maximum number of rotation rates', MAX_rot
        ik = kfclose(k1)
        return
      endif

  2   if ( a3.eq.'1') call swap (xlon, xlat)

      xlon = fnlong(xlon)
      call getblock(xlon, xlat, kp)

       if (kp.gt.0 ) then
        k=k+1
        rot_obs(k)=o
        rot_sig(k)=s * rot_wtfac
        rot_pos(k,1)= fnlong(xlon) 
        rot_pos(k,2)= xlat
        kblk_rot(k) = kp
        rot_name(k) = sname
       endif

    5 continue

   99 num_rot = k
      ik = kfclose (k1)

      endif

      print *, 'Num Rotations =', num_rot
 
      return
      end


c**********************************************************************

      subroutine writerot(tchi)
c -- output rotation rates

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      character*4 block_name      
      character l*4, fname*80
      dimension p(9)

c      call clearchar(fname, 80)
      fname = ""
      l='RR  '
      o2=0.0d0

      call stats (l,0,n, obs,o2,sig, calc, r, rs, sumwt, tchi, 
     . datavar, ssfit, fname, dn, dw, npar, izero)

      if (num_rot.gt.0) then 

       call fopen (k2, 1, '.rrs ')

       write (k2,*)'   Long.     Lat.    Obs    Sig   Calc    '//
     .    'Res    R/S  Blok Label'

      do 10 i=1, num_rot
        xpt=rot_pos(i,1)
        ypt=rot_pos(i,2)
        sig = rot_sig(i)
        wt = fnwt(rot_sig(i))

        np = npole_block(kblk_rot(i))

        do k=1,9
         p(k) = poles(np,k)
        enddo

        call vertaxrot (xpt, ypt, p, vrot)
        rot_calc(i) = vrot
        rr = rot_obs(i) - rot_calc(i)
        rrs = rr*sqrt(wt)

        call stats (l, 0,n, rot_obs(i), o2, sig, 
     .    rot_calc(i),r, rs, sumwt, 
     .    tchi, datavar, ssfit, fname, dn, dw, npar, ione)

        write (k2,2) rot_pos(i,1), rot_pos(i,2), rot_obs(i), sig, 
     .   rot_calc(i), rr, rrs, block_name(kblk_rot(i)), rot_name(i)

   2  format(2f9.3, 5f7.2, 2x, a4, 1x, a40)

  10  continue

      call stats (l,0, n, obs, o2,sig,calc, r, rs, sumwt, tchi, 
     .  datavar, ssfit, fname, dn, dw, npar, ksum)

      ik = kfclose(k2)
      endif
      return
      end

c**********************************************************************
      subroutine getpole (M, ndata, pos, v, sig, cor, bpole, bperr, 
     .   chi1)

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      dimension pos(M,2), v(M,2), sig(M,2), cor(M)
      dimension bpole(3), bperr(3), der(3,2)
      dimension A(2*M,3), o(2*M), W(2*M,2*M)

c-- temporary arrays for building matrices
      dimension AtW(3,2*M), AtWA(3,3), AtWO(3,1), 
     .  P(3,1), b1(2*M,1), b2(3,1)

      M2 = 2*M
      kone = 1
      k3 = 3
      nine=9
      eps = 0.001d0

      postchi2 = 0.0d0
      chi1 = 0.0d0

      do i=1,3
       bpole(i) = 0.0d0
       bperr(i) = 0.0d0
      enddo

      call cleareal(W,4*M*M)
      call cleareal(a, 6*M)
      call cleareal(o, 2*M)
      call cleareal(atW, 6*M)
      call cleareal(AtWA, nine)
      call cleareal(AtWO, k3)
      call cleareal(b1,2*M)
      call cleareal(b2,k3)
      call cleareal(p, k3)

      do i=1,ndata
       plon = pos(i,1)
       plat = pos(i,2)
       call velder( bpole, plon, plat, Ve, Se, Vn, Sn, rho, der)

       sx=max(sig(i,1), eps)
       sy=max(sig(i,2), eps)

       j = 2*i -1
       o(j) = v(i,1)
       a(j,1) = der(1,1)
       a(j,2) = der(2,1)
       a(j,3) = der(3,1)
       W(j,j) = sx*sx

       j=2*i
       o(j) = v(i,2)
       a(j,1) = der(1,2)
       a(j,2) = der(2,2)
       a(j,3) = der(3,2)
       W(j,j) = sy*sy

       W(j,j-1) = cor(i)*sx*sy
       W(j-1,j) = cor(i)*sx*sy

      enddo

      ndat=2*ndata
      npar=3

c invert covariance to get weight matrix
      call gaussj3 (W, ndat, M2, b1, kone, kone)

c make AtW
      call atransb3(a, W, AtW, ndat,M2, npar,npar, ndat,M2)

c-- make AtWA
      call amultb3(AtW,A,AtWA, npar,npar, ndat,M2, npar,npar)

c-- make AtWO
      call amultb3(AtW,O,AtWO, npar,npar, ndat,M2, kone,kone)

      call gaussj3(atWA, npar, npar, b2, kone, kone)

c -- get solution
      call amultb3(AtWA, AtWO, p, npar,npar, npar,npar, kone,kone)

      do i=1,3
       bpole(i) = p(i,1)*r2d
       bperr(i) = dsqrt ( AtWA(i,i) ) *r2d
      enddo

      do i=1,ndata
       plon = pos(i,1)
       plat = pos(i,2)
       call velder( bpole, plon, plat, Ve, Se, Vn, Sn, rho, der)
       sx=max(sig(i,1), eps)
       sy=max(sig(i,2), eps)
       postchi2 = postchi2 + ((v(i,1)-Ve)/sx)**2 + 
     .   ((v(i,2)-Vn)/sy)**2
      enddo

      twon = ndat
      chi1 = dsqrt(postchi2/twon)

      return
      end

c***********************************************************************
      subroutine velder(p, plon, plat, Ve, Se, Vn, Sn, rho, der)

c computes plate velocity in mm/a at (plon, plat) (in degrees) from Euler vector p()
c in degrees per million years

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      dimension p(3), der(3,2)
c      dimension w(3,3)
      logical kerr, kder

      rpd2= d2r*d2r
      Re =  Erad
      kerr = .false.
      kder = .false.

      pt = plat*d2r
      pn = plon*d2r
      Sn = 0.0d0
      Se = 0.0d0
      rho = 0.0d0

c point vector
      Px = Re * dcos(pt) * dcos(pn)
      Py = Re * dcos(pt) * dsin(pn)
      Pz = Re * dsin(pt)

c Euler vector
      Wx = p(1)*d2r
      Wy = p(2)*d2r
      Wz = p(3)*d2r

c velocity by cross-product
      Vx = Wy*Pz - Py*Wz
      Vy = Wz*Px - Pz*Wx
      Vz = Wx*Py - Px*Wy

c dot product with (E,N) vector
      Tex = -dsin(pn)
      Tey =  dcos(pn)
      Tez =  0.0d0
      Tnx = -dsin(pt)*dcos(pn)
      Tny = -dsin(pt)*dsin(pn)
      Tnz =  dcos(pt)

      Ve = Tex * Vx + Tey * Vy + Tez * Vz
      Vn = Tnx * Vx + Tny * Vy + Tnz * Vz

c derivatives
      if (kder) then
      dVe_dWx = - Tey * Pz + Tez * Py
      dVe_dWy =   Tex * Pz - Tez * Px
      dVe_dWz = - Tex * Py + Tey * Px

      dVn_dWx = - Tny * Pz + Tnz * Py
      dVn_dWy =   Tnx * Pz - Tnz * Px
      dVn_dWz = - Tnx * Py + Tny * Px

      der(1,1) = dVe_dWx
      der(2,1) = dVe_dWy
      der(3,1) = dVe_dWz
      der(1,2) = dVn_dWx
      der(2,2) = dVn_dWy
      der(3,2) = dVn_dWz
      endif

c* fill the covariance matrix W and convert to (radians/Ma)**2
c      if (kerr) then
c      w(1,1) = p(4)*rpd2
c      w(2,2) = p(5)*rpd2
c      w(3,3) = p(6)*rpd2
c      w(1,2) = p(7)*rpd2
c      w(2,1) = w(1,2)
c      w(1,3) = p(8)*rpd2
c      w(3,1) = w(1,3)
c      w(2,3) = p(9)*rpd2
c      w(3,2) = w(2,3)
c      call velerr (w, plat, plon, Se, Sn, rho, Az, Ax1, Ax2 )
c      endif

      return
      end
c***************************************************************
      SUBROUTINE gaussj3(a,n,np,b,m,mp)

      implicit real*8 (a-h,o-z)
      PARAMETER (NMAX=3000)

      integer indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      dimension a(np,np), b(np,mp)
      irow=0
      icol=0

      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.0d0
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                print *, '1singular matrix in gaussj'
c                pause '1singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.d0) then
c         pause '2singular matrix in gaussj'
         print *, '2singular matrix in gaussj'
        endif
        pivinv=1.0d0/a(icol,icol)
        a(icol,icol)=1.0d0
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.
c*************************************************************************
   
      subroutine prinaxes ( Exx, SExx, Exy, SExy, Eyy, SEyy, 
     .   e1, se1, e2, se2, alph1, sa1, alph2, sa2)

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      parameter (ntr = 5000)
      dimension ree1(ntr), ree2(ntr), ra1(ntr), ra2(ntr)
      ntrials = ntr
c      ntrials = 100

      call cleareal(ree1,ntr)
      call cleareal(ree2,ntr)
      call cleareal(ra1,ntr)
      call cleareal(ra2,ntr)

c get new axes E1 and E2
      call covelip1( Exx, Eyy, Exy, E1, E2, alph1, alph2)
      
c Monte Carlo uncertainties for alph and E

      z=0.0d0       
      ntry=0
      sm11=Exx
      se11=SExx
      sm22=Eyy
      se22=SEyy
      sm21=Exy
      se21=SExy

      acut = 60.0d0

      do i = 1,ntrials

c generate random value
      call normal (exxr, sm11, se11) 
      call normal (eyyr, sm22, se22) 
      call normal (exyr, sm21, se21) 

      call covelip1( exxr, eyyr, exyr, ee1, ee2, a1, a2)

c do sums, kick out big outliers      
       if (abs(a1-alph1).lt.acut .and. abs(a2-alph2).lt.acut) then
        ntry=ntry+1
        ree1(ntry) = ee1
        ree2(ntry) = ee2
        ra1(ntry) = a1
        ra2(ntry) = a2
       endif

      enddo

      call meansd2(ntry, ree1, ave1, se1)
      call meansd2(ntry, ree2, ave2, se2)
      call meansd2(ntry,  ra1, ava1, sa1)
      call meansd2(ntry,  ra2, ava2, sa2)
c      print *, 'ntry se1 se2', ntry,se1,se2

      if (e2.lt.e1) then 
        call swap (e1,e2)
        call swap (se1,se2)
        call swap (alph1,alph2)
        call swap (sa1,sa2)
      endif

      return
      end
c**********************************************************************
      subroutine fitpole(kpole, blpole, nsites)

c**  fit poles using residuals

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      parameter ( M  = 1000 )

      character sd(M)*8
      dimension sig(M,2), cor(M), vels(M,2), posll(M,2)
      dimension blpole(3), blperr(3), der(3,2)
      logical useGPS

      xt=1.0d3
      xm=1.0d6
      xb=1.0d9
      xconv=xm
      z = 0.0d0

      kp = kpole
                  
c'-- get GPS sites for this pole
      nsites=0

      do i=1, num_gps

      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)
       if (npole_block(nblock_gps(i)).eq.kp .and. useGPS(i) ) then

         nsites=nsites+1
         posll(nsites,1)=xpt
         posll(nsites,2)=ypt
         vels(nsites,1)= xobs - xcalc
         vels(nsites,2)= yobs - ycalc
         sig(nsites,1)=sigx
         sig(nsites,2)=sigy
         sd(nsites)= gps_name(i)
         cor(nsites)= gps_obs(i,5)

       endif
      enddo

      if (nsites.lt.3) goto 1001

      call fopen (kf8, 1, '.polefit ')
      
c      write (kf8, *) ' '
      write (kf8, *) '-------------------------------------------------'
      write (kf8, *) 'Pole ',kp,'  NumPts=',nsites
      
c** get rigid-body rotation from residuals
      call getpole (M, nsites, posll, vels, sig, cor, blpole, blperr, 
     .   chi1)
       write(kf8, *) ' Pole   (sigma) '
       write(kf8, '(2f8.4)') (blpole(i), blperr(i), i=1,3)

c* adjust pole
c       do i=1,3
c        poles(kp,i) = poles(kp,i) + blpole(i)
c       enddo

c       write(kf8, *) ' Fits to pole '

      do i=1, nsites
       plon = posll(i,1)
       plat = posll(i,2)
       call velder(blpole, plon, plat, Ve, Se, Vn, Sn, rho, der)
       write(kf8, '(a8, 2f9.3, 6f6.1)') sd(i), plon, plat,
     .   vels(i,1), Ve, vels(i,1)-Ve, vels(i,2), Vn, vels(i,2)-Vn
      enddo
      
      ik = kfclose (kf8)

 1001 return
      end


c****************************************************
c*** write hard constraints and transient constraints

      subroutine writehc 

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

      character*4 block_name      
      character ptype(50)*15, p2codes(50)*2
      common /ptype1/ ptype, p2codes


      dimension p(9)
      character*4 ll(3)

      data ll / 'Rate', 'Azim', 'Spin' /
      nine=9
      call cleareal(p,nine)

      call fopen (k33, 1, '.constraints ')

      if ( num_hc.gt.0) then

      write (k33,'(a17)' ) '#Hard constraints'
      write (k33,'(a74)')
     . '#   Type      Long.      Lat.  MOVG FIXD    Min.    Max.   '//    
     . 'Calc.   Penalty'

      do i=1, num_hc
       if ( hc_flag(i) ) then

      if (hc(i).eq.1) then

       call relvel (3, hc_block(i,1), hc_block(i,2), hc_pos(i,1),
     .   hc_pos(i,2), Vx, Sx, Vy, Sy, rho)
         calc = dsqrt( Vx*Vx + Vy*Vy)

      else if (hc(i).eq.2) then

       call relvel (3, hc_block(i,1), hc_block(i,2), hc_pos(i,1),
     .   hc_pos(i,2), Vx, Sx, Vy, Sy, rho)
       call svaz(Vx, Vy, calc)
       obs = (hc_val(i,1) + hc_val(i,2))/2.0d0
       a360=360.0d0
       sig=1.0d0
       call svres (calc, obs, sig, r, rs, a360)

c* rotations
      else if (hc(i).eq.3) then
       kmv = hc_block(i,1)
       kfx = hc_block(i,2)
       xpt = hc_pos(i,1)
       ypt = hc_pos(i,2)
       nmv = npole_block(kmv)
       nfx = npole_block(kfx)

       do k=1,3
        p(k) = poles(nmv,k) - poles(nfx,k)
       enddo
       call vertaxrot (xpt, ypt, p, calc)

      endif

        write (k33, 1) 'HC: ', ll(hc(i)), hc(i), 
     .   hc_pos(i,1), hc_pos(i,2), 
     .   block_name(hc_block(i,1)), block_name(hc_block(i,2)),
     .   hc_val(i,1), hc_val(i,2), calc, hc_pen(i)

      endif
      enddo
  1   format(2a4, i2, 2f10.4, 2(1x,a4), 3f8.2, 1x, e12.4)
  
     
      endif


c
c *** Smoothing constraints
c      
       write (k33,'(a1)' ) ' '
       write (k33,'(a28)' ) '#Fault smoothing constraints'
       write (k33,'(a48)')
     .   '#    Fault  Type    A1     A2      A3    A4   A5'
       do kf = 1, MAX_f
         ksm = int(smooth(kf,1))
        if (ksm.gt.0 ) 
     .    write (k33, '(a4, 2i5, 5(1x,1pe12.4) )')
     .    'SM: ',kf, ksm, (smooth(kf,j),j=2,6)
       enddo

       write (k33,'(a1)' ) ' '
       write (k33,'(a32)' ) '#Transient smoothing constraints'
       write (k33,'(a48)')
     .   '#    Trans  Type    A1     A2      A3    A4   A5'
       do nt = 1, MAX_srce
         ksm = int(trsmooth(nt,1))
        if (ksm.gt.0 ) 
     .    write (k33, '(a5, 2i5, 5(1x,1pe12.4))')
     .    'SMt: ',nt, ksm, (trsmooth(nt,j),j=2,6)
       enddo
c
c *** Transient constraints
c      
       write (k33,'(a1)' ) ' '
       write (k33,'(a22)' ) '#Transient constraints'
       write (k33,'(a70)')
     .   '#     Srce  Type    Min       Max     Value       ' //
     .   'Diff    NP   Penalty'

      do nt = 1, MAX_srce
       kq = info_source(nt,2)
       
       if ( sflag(nt) ) then
        do i=1,14
         if ( nsource_parm (nt,i).gt.0 .and. mmparm(nt,i) ) then
         
          np = 100*nt+i
          pp = 0.0d0
          ip=i
          
          dt1 = tminmax(nt,ip,1) - transient(nt,i) 
          if ( dt1.gt.zero) pp = pp+dt1*1.0d2
          dt2 = transient(nt,i) - tminmax(nt,ip,2)  
          if ( dt2.gt.zero) pp = pp + dt2*1.0d2
           dmin = zero
           if (dt1 .le. zero ) dmin = -dt1
           if (dt2 .le. zero .and. dmin.gt.-dt2) dmin = -dt2

          write (k33, '(a4,i5,2x,a2,1x, 4f10.3, i6, 1pe10.3)') 
     .     'TC: ', nt, p2codes(i+20), tminmax(nt,ip,1), 
     .     tminmax(nt,ip,2), transient(nt,i), dmin, np, pp

         endif
        enddo 
        
c** transient moment, penalty np*100+15  
         ip = 30      
         if ( mmparm(nt,ip) ) then
          np = 100*nt +ip
c          write (k33, '(a4,i5,2x,a2,1x, 4(1pe10.3), i6, 1pe10.3)') 
c     .     'TC: ', nt, p2codes(41), tminmax(nt,ip,1), 
c     .     tminmax(nt,ip,2), trans_sums(nt,3), dmin, np, pp
          call tr_moment(nt)
c          print *, 'constr', tminmax(nt,ip,1), tminmax(nt,ip,2)
          call M0_penalty(nt, pp, dmin)
c          tmn = trsmooth(nt,5)
c          tmx = trsmooth(nt,6)
          tmn = tminmax(nt,30,1)
          tmx = tminmax(nt,30,2)

          write (k33, '(a4,i5,2x,a2,1x, 4(1pe10.3), i6, 1pe10.3)') 
     .     'TC: ', nt, p2codes(50), tmn, 
     .       tmx, trans_sums(nt,3), dmin, np, pp
         endif
       endif
      enddo
      
      ik = kfclose(k33)

      return
      end

c****************************************************
c -- penalty for misfit Moment of event nt
      subroutine M0_penalty (nevent, pp, dmin)
        implicit real*8 (a-h,o-z)
        include "tdefcom1.h"
c        include "tdefcom2.h"
         pp   = 0.0d0
         dmin = 0.0d0
         nt = nevent
         pen_Mo = 1.0d0

         if (nt.gt.0 .and. nt.le.MAX_srce .and. mmparm(nt,30) ) then

          tmn = tminmax(nt,30,1)
          tmx = tminmax(nt,30,2)
          
c ***** tminmax not holding the right numbers
c          tmn = trsmooth(nt,5)
c          tmx = trsmooth(nt,6)

         kq = info_source(nt,2)
         if ( kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. kq.eq.6 .or. 
     .          kq.eq.7 .or. kq.eq.8 .or. kq.eq.9) then

          if ( tmn .gt. tmx ) then
             print *, 'M1 > M2 for event ', nevent,
     .        ' M1 ',tmn, ' M2 ', tmx
             return
          endif

           xm = trans_sums(nt,3)

           if ( xm .lt. tmn ) then
            pp = dlog10(tmn-xm)*pen_Mo
            dmin = xm - tmn
           endif

           if ( xm .gt. tmx ) then
            pp = dlog10(xm-tmx)*pen_Mo
            dmin = xm - tmx
           endif

         endif
        endif
      return
      end

c****************************************************
      subroutine tfaults ( x1, y1, z1, x2, y2, z2, xpt, ypt, zpt)

c** find intersection (xpt, ypt, zpt) of plane given by 3 points (x1, y1, z1) with plane (x2, y2, z2)
c*  at depth = zpt

      implicit real*8 (a-h, o-z)
      include "tdefcons.h"

      logical sing

      dimension x1(3), y1(3), z1(3), x2(3), y2(3), z2(3)
      dimension a(3,3), r(3,1)

      xpt = 0.0d0
      ypt = 0.0d0

c* get coefficients for planes

c* one fault plane
      call plane3(x1, y1, z1, A1, B1, C1, D1)
  
c*  second fault plane
      call plane3(x2, y2, z2, A2, B2, C2, D2)

c* horizontal plane through deeper point
      A3 = 0.0d0
      B3 = 0.0d0
      C3 = 1.0d0
      D3 = -zpt

c** set up matrices to solve for intersection

      a(1,1) = A1
      a(1,2) = B1
      a(1,3) = C1
      r(1,1)   = -D1

      a(2,1) = A2
      a(2,2) = B2
      a(2,3) = C2
      r(2,1)   = -D2

      a(3,1) = A3
      a(3,2) = B3
      a(3,3) = C3
      r(3,1)   = -D3

c      do i=1,3
c       write (*, '(4f12.4)') (a(i,j), j=1,3), r(i,1)
c      enddo

      np=3
      n = 1

      call gaussj2( a, np, np, r, n, n, sing)

      if ( sing ) then
       print *, 'Singular matrix'
       return
      endif

c      write(*,'(3f10.4)') ( r(i,1), i=1,3)

      xpt = r(1,1)
      ypt = r(2,1)

      return
      end

c**********************************************************************

      subroutine plane3 (x,y,z, A, B, C, D)

c* 3 point slope of plane Ax + By + Cz + D = 0

      implicit real*8 (a-h,o-z)

      dimension x(3), y(3), z(3)

      A = y(1)*(z(2) - z(3)) + y(2)*(z(3) - z(1)) + y(3)*(z(1) - z(2))
      B = z(1)*(x(2) - x(3)) + z(2)*(x(3) - x(1)) + z(3)*(x(1) - x(2))
      C = x(1)*(y(2) - y(3)) + x(2)*(y(3) - y(1)) + x(3)*(y(1) - y(2))
      D = -( x(1) * ( y(2) * z(3) - y(3) * z(2) )
     .   +   x(2) * ( y(3) * z(1) - y(1) * z(3) )
     .   +   x(3) * ( y(1) * z(2) - y(2) * z(1) ) )

      return
      end


c**********************************************************************
c* calculate velocity from strain based on Savage's spherical strain formulas
c* Savage et al. JGR 2001 p. 21,995
c**********************************************************************
      subroutine calcstrain (nbl, xi, yi, Vx, Vxsig, Vy, Vysig)
 
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      z = 0.0d0

      Vx = z
      Vy = z
      Vxsig = z
      Vysig = z

      if (nbl.eq.0) return

      kbl = nstrain_block(nbl)
      if (kbl.eq.0) return

      x = fnlong(xi)
      y = yi

c* use block centroid if strain tensor centroid not specified
      Cx = fnlong(block_centroid(nbl,1))
      Cy = block_centroid(nbl,2)

c* use strain tensor centroid if specified
      if ( strain2(kbl,1) .ne. z .or. strain2(kbl,2) .ne. z) then
       Cx = fnlong(strain2(kbl,1))
       Cy = strain2(kbl,2)
      endif


      r = Erad*1.0d6

      clat = ( 90.0d0 - Cy ) *d2r
      clon = Cx * d2r
      sint = dsin(clat)

      plon = x*d2r
      plat = ( 90.0d0 - y )*d2r
      dthet = -(plat - clat)
      dphi = plon - clon
      sd = sint * dphi

      xmil = 1.0d6
      Exx = strain(kbl,1)/xmil
      Eyy = strain(kbl,2)/xmil
      Exy = strain(kbl,3)/xmil
      varExx = (strain(kbl,4)/xmil)**2
      varEyy = (strain(kbl,5)/xmil)**2
      varExy = (strain(kbl,6)/xmil)**2
      
c* get sigmas by propagation of errors

c-- spherical strain
      if ( sphstrain ) then
        Vx = r * ( sd * Exx + dthet * Exy )
        Vy = r * ( sd * Exy + dthet * Eyy )
        Vxsig = dsqrt ( sd*sd*varExx + dthet*dthet*varExy)
        Vysig = dsqrt ( sd*sd*varExy + dthet*dthet*varEyy)
      else
c-- cartesian strain
c-- lon, lat to x,y; return position in km
        call project( x, y, Cx, Cy, xx, yy)
c        Exx2 = strain(kbl,10)/xmil
c        Eyy2 = strain(kbl,11)/xmil
c        Exy2 = strain(kbl,12)/xmil
        Vx = (Exx * xx + Exy * yy ) * xmil
        Vy = (Exy * xx + Eyy * yy ) * xmil
        Vxsig = dsqrt ( xx*xx*varExx + yy*yy*varExy)
        Vysig = dsqrt ( xx*xx*varExy + yy*yy*varEyy)
      end if
      
      
      return
      end


c**********************************************************************
c* get current date and time
      subroutine dater (datetime )
      character d*8, t*10, datetime*12, zone*5
      dimension iv(8) 

      call date_and_time (d,t,zone,iv)
      datetime = d//t(1:4)

      return
      end

c**********************************************************************
      subroutine getrms(m, n, v, s, xnrms, xwrms)

c* get Nrms and Wrms from data

      implicit real*8 (a-h,o-z)
      dimension v(m,2), s(m,2)

      chisq = 0.0d0
      sumwt = 0.0d0

      if ( n.lt.1) return

      do i=1,n
         wx = (1.0d0/s(i,1))**2
         wy = (1.0d0/s(i,2))**2
         chisq = chisq + wx*v(i,1)**2 + wy*v(i,2)**2
         sumwt = sumwt + wx + wy
      enddo

      rn = n
      rn = 2.0d0 * rn
      xnrms = dsqrt ( chisq / rn )
      xwrms = dsqrt ( chisq / sumwt )

      return
      end

c**********************************************************************

      subroutine blockstrain

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      parameter ( M  = 2000 )
      character*4 block_name      

      character sd(1000)*8
      logical useENU, useGPS

c'-- get strain tensor for each block from the residuals

      dimension  sig(M,2), cor(M), vels(M,2), pos(M,2)
      dimension  indg(MAX_gps)
      dimension  dres(MAX_gps,2) 

      ndim = M
      rescut=1000.
      xt=1.0d3
      xm=1.0d6
      x6=xm
      xb=1.0d9
      xconv=xb
      z = 0.0d0

      call fopen (kf8, 1, '.dgt ')
      
      do 1001 kbl=1, nblocks
       if(block_flag(kbl) ) then

c  get strain index for this block
      indx = nstrain_block(kbl)

      bx=block_centroid(kbl,1)
      by=block_centroid(kbl,2)

c'-- get GPS sites in this block
      nsites=0

      do i=1, num_gps
      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, s)
       if (nblock_gps(i).eq.kbl .and. useGPS(i) .and. 
     .       useENU(i,1) .and. useENU(i,2)) then
         nsites=nsites+1
         indg(nsites) = i
         pos(nsites,1)=xpt
         pos(nsites,2)=ypt
         vels(nsites,1)= xobs - xcalc
         vels(nsites,2)= yobs - ycalc
         sig(nsites,1)=sigx
         sig(nsites,2)=sigy
         sd(nsites)= gps_name(i)
         cor(nsites)= gps_obs(i,5)
       endif
      enddo

      call getrms(ndim, nsites, vels, sig, dnrms1, dwrms1)

      write (kf8, *) ' '
      write (kf8, *) '-------------------------------------------------'
      write (*, 1) kbl, block_name(kbl), indx, nsites
      write (kf8, 1) kbl, block_name(kbl), indx, nsites
   1  format ("Block#",i4," Name ",a4," Str_indx",i4," NumPts",i5)
      
      if (nsites.lt.3) then
        write (kf8, *) 'Too few sites - skipping'
        goto 1000
      endif

c* get strain tensor and rotation

      call getstrain (Bx, By, nsites, ndim, pos, vels, sig, cor, 
     .  Exx, SExx, Exy, SExy, Eyy, SEyy, Conx, SCx, Cony, SCy, Wr, SWr,
     .  chi2out)

      call prinaxes ( Exx, SExx, Exy, SExy, Eyy, SEyy, 
     .    e1, sige1, e2, sige2, alph1, siga1, alph2, siga2)

      write (kf8, *) ' '
      write (kf8, '("Origin: Lon =",f9.3," Lat =",f9.3)' ) bx,by
      write (kf8, *) ' '
      write (kf8, *) 'Strain rate tensor - in nanostrain/yr'
      write (kf8, 40) Exx*xconv, SExx*xconv, Exy*xconv, SExy*xconv
      write (kf8, 40) Exy*xconv, SExy*xconv, Eyy*xconv, SEyy*xconv
 40   format(2(2x, f9.3, '(',f9.3,')'))

      call prinaxes ( Exx, SExx, Exy, SExy, Eyy, SEyy, 
     .    e1, se1, e2, se2, a1, sa1, a2, sa2)

      write (kf8, *) ' '
      write (kf8, *) 'Principal axes - in nanostrain/yr and degrees'
      write (kf8,40) e1*xconv, se1*xconv, fn0to180(a1), sa1
      write (kf8,40) e2*xconv, se2*xconv, fn0to180(a2), sa2
      
      write (kf8, *) ' '
      write (kf8, *) 'Rotation rate - in nanoradians/yr '
      write (kf8,40) Wr*xconv, SWr*xconv
      write (kf8, *) 'Rotation rate - in deg/Myr '
      write (kf8,40) Wr*r2d*x6, SWr*r2d*x6

      write (kf8, *) ' '
      write (kf8, *) 'Constants Cx, Cy - in mm/yr '
      write (kf8,40) Conx, SCx, Cony, SCy

      res_strain(kbl,1) = e1*x6
      res_strain(kbl,2) = se1*x6
      res_strain(kbl,3) = e2*x6
      res_strain(kbl,4) = se2*x6
      res_strain(kbl,5) = fn0to180(a1)
      res_strain(kbl,6) = sa1
      res_strain(kbl,7) = Wr*xconv
      res_strain(kbl,8) = SWr*xconv
      res_strain(kbl,9) = Conx
      res_strain(kbl,10) = Cony
      res_strain(kbl,11) = SCx
      res_strain(kbl,12) = SCy
      res_strain(kbl,13) = dnrms1
      res_strain(kbl,14) = real(nsites)
      res_strain(kbl,15) = Exx*x6
      res_strain(kbl,16) = Eyy*x6
      res_strain(kbl,17) = Exy*x6
      res_strain(kbl,18) = SExx*x6
      res_strain(kbl,19) = SEyy*x6
      res_strain(kbl,20) = SExy*x6

c total strain in microstrain/yr
      if (indx.gt.0) then
       tot_strain(kbl,1) = res_strain(kbl,15) + strain(indx,1)
       tot_strain(kbl,2) = res_strain(kbl,16) + strain(indx,2)
       tot_strain(kbl,3) = res_strain(kbl,17) + strain(indx,3)
      else
       tot_strain(kbl,1) = res_strain(kbl,15)  
       tot_strain(kbl,2) = res_strain(kbl,16)  
       tot_strain(kbl,3) = res_strain(kbl,17)  
      endif

      write (kf8, *) ' '
      write (kf8, *) 'Fits to data'
      write (kf8, *) 'Site     Longitude  Latitude    Obs Vx'//
     .'    Sig Vx   Calc Vx    Vx Res    Obs Vy    Sig Vy'//
     .'    Calc Vy   Vy Res  '

      bx=block_centroid(kbl,1)
      by=block_centroid(kbl,2)
      r = 6.371d9
      clat = ( 90.0d0 - by )*d2r
      clon = bx*d2r
      sint = dsin(clat)

      do i=1, nsites

      xpt = pos(i,1)
      ypt = pos(i,2)

      plon = xpt*d2r
      plat = (90.0d0 - ypt )*d2r
      dthet = -(plat - clat)
      dphi = plon - clon

      Vxc = r*(sint * dphi * Exx + dthet * Exy + dthet * Wr )+Conx
      Vyc = r*(sint * dphi * Exy + dthet * Eyy - sint * dphi * Wr )+Cony

      rdx = vels(i,1)-vxc
      rdy = vels(i,2)-vyc

      write (kf8, 110) sd(i), xpt, ypt, vels(i,1), 
     . sig(i,1), vxc, rdx, vels(i,2), sig(i,2), vyc, rdy

      vels(i,1) = rdx
      vels(i,2) = rdy
      ind = indg(i)
      dres(ind,1) = rdx
      dres(ind,2) = rdy

      enddo

 110  format(a8, 1x, 2f10.3, 8f10.2)

      call getrms(ndim,nsites, vels, sig, dnrms3, dwrms3)
      res_strain(kbl,13) = dnrms3

c      write (kf8, *) ' '
c      write (kf8, 104) 'Nrms:',block_name(kbl), dnrms1, dnrms3
c      write (kf8, 104) 'Wrms:',block_name(kbl), dwrms1, dwrms3
       write (*, 104)'Nrms:',block_name(kbl), dnrms1, dnrms3
       write (*, 104) 'Wrms:',block_name(kbl), dwrms1, dwrms3
  104  format (a5, 1x, a4, ' Prefit ', f10.4, ' Postfit ', f10.4)

1000  continue      
      endif
1001  continue

c   1  format(5f10.3)

      ik = kfclose (kf8)
      
      return
      end

c**********************************************************************

      subroutine getstrain(Xcen, Ycen, nsite, ndim, pos, vels, sig, cor, 
     . Exx, SExx, Exy, SExy, Eyy, SEyy, Cx, SCx, Cy, SCy, Wr, SWr, chi2)

c** fits spherical strain rate tensor and vertical axis rotation 
c     to N, E velocity components simultaneously, using covariance

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

c-- temporary arrays for building matrices

      dimension A(2,6), O(2,1), W(2,2), B6(6,1), B2(2,1)
      dimension AtW(6,2), AtWA(6,6), AtWO(6,1)  
      dimension AtWAg(6,6), AtWOg(6,1), P6(6,1)

      dimension pos(ndim,2), sig(ndim,2), cor(ndim), vels(ndim,2)

      logical sing

      ndat=2
      npar=6
      ione = 1
      itwo = 2
      zero =0.0d0
      chi2=zero
      Cx=zero
      Cy=zero

      if ( nsite.lt.2) return

      r = Erad * 1.0d6
      clat = ( 90.0d0 - Ycen)*d2r
      clon = Xcen*d2r
      sint = dsin(clat)

      call fill(AtWAg, zero, 36)
      call fill(AtWOg, zero,  6)

c-- build matrices
      do 1 kk = 1, nsite

      call fill(    A, zero, 12)
      call fill(    W, zero,  4)
      call fill(  AtW, zero, 12)
      call fill( AtWA, zero, 36)
      call fill( AtWO, zero,  6)

      xpt = pos(kk,1)
      ypt = pos(kk,2)

      plon = xpt*d2r
      plat = (90.0d0 - ypt )*d2r
      dthet = -(plat - clat)
c      dthet = (plat - clat)
      dphi = plon - clon

      sx = sig(kk,1)
      sy = sig(kk,2)
      c12 = cor(kk)

      O(1,1) = vels(kk,1)
      O(2,1) = vels(kk,2)
c      print *, xpt,ypt,o(1,1), o(2,1)

c spherical
c      Vx = r * ( sint * dphi * Exx + dthet * Exy + dthet * Wr) + Cx
c      Vy = r * ( sint * dphi * Exy + dthet * Eyy - sint * dphi * Wr ) + Cy
c parameters
c      1-Exx 2-Exy 3-Eyy 4-Cx 5-Cy 6-Wr

      A(1,1) = r * sint * dphi
      A(1,2) = r * dthet
      A(1,4) = 1.0d0
      A(1,6) = r * dthet

      A(2,2) = r * sint * dphi
      A(2,3) = r * dthet
      A(2,5) = 1.0d0
      A(2,6) = -r * sint * dphi

      W(1,1) = sx*sx
      W(2,2) = sy*sy
      W(1,2) = c12*sx*sy
      W(2,1) = W(1,2)

c-- invert covariance to get weight matrix
      call fill(B2, zero, itwo)
      call gaussj2(W, itwo, itwo, B2, ione, ione, sing)

      if (sing) then
        print *, 'Singular matrix'
        return
      endif

c-- make AtW
      call atransb(A, W, AtW, ndat, npar, ndat)

c-- make AtWA
      call amultb(AtW, A, AtWA, npar,ndat,npar)

c-- make AtWO
      call amultb(AtW, O, AtWO, npar, ndat, ione)

c-- stick into global arrays
      do i=1, npar
         AtWOg(i,1) = AtWOg(i,1) + AtWO(i,1)
      enddo

      do i=1, npar
       do j=1, npar
         AtWAg(i,j) = AtWAg(i,j) + AtWA(i,j)
       enddo
      enddo

 1    continue

c-- invert matrix
      call fill(B6, zero, npar)

      call gaussj2(AtWAg, npar, npar, B6, ione, ione, sing)
      if (sing) then
        print *, 'Singular matrix'
        return
      endif

c -- get solution
      call amultb(AtWAg, AtWOg, P6, npar, npar, ione)

c fill dgt and dgterr arrays

      Exx = p6(1,1)
      Exy = p6(2,1)
      Eyy = p6(3,1)
      Cx  = p6(4,1)
      Cy  = p6(5,1)
      Wr  = p6(6,1)

      SExx = dsqrt(AtWag(1,1))
      SExy = dsqrt(AtWag(2,2))
      SEyy = dsqrt(AtWag(3,3))
      SCx  = dsqrt(AtWag(4,4))
      SCy  = dsqrt(AtWag(5,5))
      SWr  = dsqrt(AtWag(6,6))


c** misfits
      chi2 = 0.0d0

c     print *, 'Cx, Cy ', Cx, Cy

      do kk = 1, nsite

      xpt = pos(kk,1)
      ypt = pos(kk,2)

      plon = xpt*d2r
      plat = (90.0d0 - ypt )*d2r
      dthet = -(plat - clat)
      dphi = plon - clon

      Vx = r * (sint * dphi * Exx + dthet * Exy + dthet * Wr ) + Cx
      Vy = r * (sint * dphi * Exy + dthet * Eyy - sint * dphi * Wr )+Cy

      Sx=sig(kk,1)
      Sy=sig(kk,2)
      Rx = vels(kk,1) - Vx
      Ry = vels(kk,2) - Vy

      chi2 = chi2 + (Rx/Sx)**2 + (Ry/Sy)**2

      enddo

      return
      end


c**********************************************************************
      SUBROUTINE  SRECTF(ALP,X,Y,DEP,AL1,AL2,AW1,AW2,
     *                   SD,CD,DISL1,DISL2,DISL3,
     *                   U1,U2,U3,U11,U12,U21,U22,U31,U32)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C*********************************************************
C*****           *****
C*****    SURFACE DISPLACEMENT,STRAIN,TILT           *****
C*****    DUE TO RECTANGULAR FAULT IN A HALF-SPACE   *****
C*****              CODED BY  Y.OKADA ... JAN 1985   *****
C*****           *****
C*********************************************************
C
C***** INPUT
C*****   ALP     : MEDIUM CONSTANT  MYU/(LAMDA+MYU)=1./((VP/VS)**2-1)
C*****   X,Y     : COORDINATE OF STATION
C*****   DEP     : SOURCE DEPTH
C*****   AL1,AL2 : FAULT LENGTH RANGE
C*****   AW1,AW2 : FAULT WIDTH RANGE
C*****   SD,CD   : SIN,COS OF DIP-ANGLE
C*****          (CD=0.D0, SD=+/-1.D0 SHOULD BE GIVEN FOR VERTICAL FAULT)
C*****   DISL1,DISL2,DISL3 : STRIKE-, DIP- AND TENSILE-DISLOCATION
C
C***** OUTPUT
C*****   U1, U2, U3      : DISPLACEMENT ( UNIT= UNIT OF DISL     )
C*****   U11,U12,U21,U22 : STRAIN       ( UNIT= UNIT OF DISL /
C*****   U31,U32         : TILT                 UNIT OF X,Y,,,AW )
C
C***** SUBROUTINE USED...SRECTG
C

      DIMENSION  U(9),DU(9)
      DATA  F0, F1 / 0.D0, 1.D0 /
C-----
      P = Y*CD + DEP*SD
      Q = Y*SD - DEP*CD
      DO 1111  I=1,9
 1111 U(I)=F0
C-----
      DO 5555  K=1,2
       IF(K.EQ.1)  ET=P-AW1
       IF(K.EQ.2)  ET=P-AW2
       DO 4444  J=1,2
        IF(J.EQ.1)  XI=X-AL1
        IF(J.EQ.2)  XI=X-AL2
        JK=J+K
        IF(JK.NE.3)  SIGN= F1
        IF(JK.EQ.3)  SIGN=-F1
        CALL SRECTG(ALP,XI,ET,Q,SD,CD,DISL1,DISL2,DISL3,
     *           DU(1),DU(2),DU(3),DU(4),DU(5),DU(6),DU(7),DU(8),DU(9))
        DO 3333  I=1,9
         U(I)=U(I)+SIGN*DU(I)
 3333   CONTINUE
 4444  CONTINUE
 5555 CONTINUE
      U1 =U(1)
      U2 =U(2)
      U3 =U(3)
      U11=U(4)
      U12=U(5)
      U21=U(6)
      U22=U(7)
      U31=U(8)
      U32=U(9)
      RETURN
      END

c*********************************************************************

      SUBROUTINE  SRECTG(ALP,XI,ET,Q,SD,CD,DISL1,DISL2,DISL3,
     *                   U1,U2,U3,U11,U12,U21,U22,U31,U32)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C*********************************************************************
C*****                       *****
C*****  INDEFINITE INTEGRAL OF SURFACE DISPLACEMENT,STRAIN,TILT  *****
C*****  DUE TO RECTANGULAR FAULT IN A HALF-SPACE                 *****
C*****                          CODED BY  Y.OKADA ... JAN 1985   *****
C*****                       *****
C*********************************************************************
C
C***** INPUT
C*****   ALP     : MEDIUM CONSTANT  MYU/(LAMDA+MYU)=1./((VP/VS)**2-1)
C*****   XI,ET,Q : FAULT COORDINATE
C*****   SD,CD   : SIN,COS OF DIP-ANGLE
C*****          (CD=0.D0, SD=+/-1.D0 SHOULD BE GIVEN FOR VERTICAL FAULT)
C*****   DISL1,DISL2,DISL3 : STRIKE-, DIP- AND TENSILE-DISLOCATION
C
C***** OUTPUT
C*****   U1, U2, U3      : DISPLACEMENT ( UNIT= UNIT OF DISL    )
C*****   U11,U12,U21,U22 : STRAIN       ( UNIT= UNIT OF DISL /
C*****   U31,U32         : TILT                 UNIT OF XI,ET,Q )
C
      DATA  F0,F1,F2/ 0.D0, 1.D0, 2.D0 /
      PI2=6.283185307179586D0
C-----
      XI2=XI*XI
      ET2=ET*ET
      Q2=Q*Q
      R2=XI2+ET2+Q2
      R =DSQRT(R2)
      R3=R*R2
      D =ET*SD-Q*CD
      Y =ET*CD+Q*SD
      RET=R+ET
      IF(RET.LT.F0)  RET=F0
      RD =R+D
      RRD=F1/(R*RD)
C-----
      IF( Q .NE.F0)  TT = DATAN( XI*ET/(Q*R) )
      IF( Q.eq.F0)  TT = F0
      IF(RET.NE.F0)  RE = F1/RET
      IF(RET.EQ.F0)  RE = F0
      IF(RET.NE.F0)  DLE= DLOG(RET)
      IF(RET.EQ.F0)  DLE=-DLOG(R-ET)
      RRX=F1/(R*(R+XI))
      RRE=RE/R
      AXI=(F2*R+XI)*RRX*RRX/R
      AET=(F2*R+ET)*RRE*RRE/R
      IF(CD.EQ.F0)  GO TO 20
C==============================
C=====   INCLINED FAULT   =====
C==============================
      TD=SD/CD
      X =DSQRT(XI2+Q2)
      IF(XI.EQ.F0)  A5=F0
      IF(XI.NE.F0)
     *A5= ALP*F2/CD*DATAN( (ET*(X+Q*CD)+X*(R+X)*SD) / (XI*(R+X)*CD) )
      A4= ALP/CD*( DLOG(RD) - SD*DLE )
      A3= ALP*(Y/RD/CD - DLE) + TD*A4
      A1=-ALP/CD*XI/RD        - TD*A5
      C1= ALP/CD*XI*(RRD - SD*RRE)
      C3= ALP/CD*(Q*RRE - Y*RRD)
      B1= ALP/CD*(XI2*RRD - F1)/RD - TD*C3
      B2= ALP/CD*XI*Y*RRD/RD       - TD*C1
      GO TO 30
C==============================
C=====   VERTICAL FAULT   =====
C==============================
   20 RD2=RD*RD
      A1=-ALP/F2*XI*Q/RD2
      A3= ALP/F2*( ET/RD + Y*Q/RD2 - DLE )
      A4=-ALP*Q/RD
      A5=-ALP*XI*SD/RD
      B1= ALP/F2*  Q  /RD2*(F2*XI2*RRD - F1)
      B2= ALP/F2*XI*SD/RD2*(F2*Q2 *RRD - F1)
      C1= ALP*XI*Q*RRD/RD
      C3= ALP*SD/RD*(XI2*RRD - F1)
C-----
   30 A2=-ALP*DLE - A3
      B3=-ALP*XI*RRE - B2
      B4=-ALP*( CD/R + Q*SD*RRE ) - B1
      C2= ALP*(-SD/R + Q*CD*RRE ) - C3
C-----
      U1 =F0
      U2 =F0
      U3 =F0
      U11=F0
      U12=F0
      U21=F0
      U22=F0
      U31=F0
      U32=F0
C======================================
C=====  STRIKE-SLIP CONTRIBUTION  =====
C======================================
      IF(DISL1.EQ.F0)  GO TO 200
      UN=DISL1/PI2
      REQ=RRE*Q
      U1 =U1 - UN*( REQ*XI +   TT    + A1*SD )
      U2 =U2 - UN*( REQ*Y  + Q*CD*RE + A2*SD )
      U3 =U3 - UN*( REQ*D  + Q*SD*RE + A4*SD )
      U11=U11+ UN*( XI2*Q*AET - B1*SD )
      U12=U12+ UN*( XI2*XI*( D/(ET2+Q2)/R3 - AET*SD ) - B2*SD )
      U21=U21+ UN*( XI*Q/R3*CD + (XI*Q2*AET - B2)*SD )
      U22=U22+ UN*( Y *Q/R3*CD + (Q*SD*(Q2*AET-F2*RRE)
     *                            -(XI2+ET2)/R3*CD - B4)*SD )
      U31=U31+ UN*(-XI*Q2*AET*CD + (XI*Q/R3 - C1)*SD )
      U32=U32+ UN*( D*Q/R3*CD + (XI2*Q*AET*CD - SD/R + Y*Q/R3 - C2)*SD )
C===================================
C=====  DIP-SLIP CONTRIBUTION  =====
C===================================
  200 IF(DISL2.EQ.F0)  GO TO 300
      UN=DISL2/PI2
      SDCD=SD*CD
      U1 =U1 - UN*( Q/R             - A3*SDCD )
      U2 =U2 - UN*( Y*Q*RRX + CD*TT - A1*SDCD )
      U3 =U3 - UN*( D*Q*RRX + SD*TT - A5*SDCD )
      U11=U11+ UN*( XI*Q/R3            + B3*SDCD )
      U12=U12+ UN*( Y *Q/R3 - SD/R     + B1*SDCD )
      U21=U21+ UN*( Y *Q/R3 + Q*CD*RRE + B1*SDCD )
      U22=U22+ UN*( Y*Y*Q*AXI - (F2*Y*RRX + XI*CD*RRE)*SD + B2*SDCD )
      U31=U31+ UN*( D *Q/R3 + Q*SD*RRE + C3*SDCD )
      U32=U32+ UN*( Y*D*Q*AXI - (F2*D*RRX + XI*SD*RRE)*SD + C1*SDCD )
C========================================
C=====  TENSILE-FAULT CONTRIBUTION  =====
C========================================
  300 IF(DISL3.EQ.F0)  GO TO 900
      UN=DISL3/PI2
      SDSD=SD*SD
      U1 =U1 + UN*( Q2*RRE                       - A3*SDSD )
      U2 =U2 + UN*(-D*Q*RRX - SD*(XI*Q*RRE - TT) - A1*SDSD )
      U3 =U3 + UN*( Y*Q*RRX + CD*(XI*Q*RRE - TT) - A5*SDSD )
      U11=U11- UN*( XI*Q2*AET             + B3*SDSD )
      U12=U12- UN*(-D*Q/R3 - XI2*Q*AET*SD + B1*SDSD )
      U21=U21- UN*( Q2*(CD/R3 + Q*AET*SD) + B1*SDSD )
      U22=U22- UN*((Y*CD-D*SD)*Q2*AXI - F2*Q*SD*CD*RRX
     *                      - (XI*Q2*AET - B2)*SDSD )
      U31=U31- UN*( Q2*(SD/R3 - Q*AET*CD) + C3*SDSD )
      U32=U32- UN*((Y*SD+D*CD)*Q2*AXI + XI*Q2*AET*SD*CD
     *                       - (F2*Q*RRX - C1)*SDSD )
C-----
  900 RETURN
      END

c*******************************************************
c* get rigidity from crust2
c http://mahi.ucsd.edu/Gabi/rem.dir/crust/crust2.html

      subroutine rdcrust2

c layer one and two flipped, after the read statement!
c layer 1: water
c layer 2: ice

      implicit real*8 (a-h,o-z)

      parameter(ityp=360)
      parameter(nla=90,nlo=180)

      common /crust1/
     +          amapvp(8,nlo,nla),amaprho(8,nlo,nla),
     +          amapvs(8,nlo,nla),amapthi(7,nlo,nla),
     +          amapele(nlo,nla)

      character*80 CNkey, CNtype, CNelev
      common /crust2/ CNkey, CNtype, CNelev

      dimension fvel(ityp,8),fvels(ityp,8),frho(ityp,8)
      dimension fthi(ityp,7)

      character*2 ctype(ityp),line*506,dum*1
      character*2 types(nlo),atype(nlo,nla)

c     character*12 names(7)
c     data names/'water','ice','soft sed.','hard sed.',
c    +         'upper crust','middle crust','lower crust'/

      k2=kfopen(0)
      k7=kfopen(0)
      k8=kfopen(0)

      open(k2, file=CNkey  )
      open(k7, file=CNtype )
      open(k8, file=CNelev )

c... read in key for crust types
c...............................
      read(k2,890)dum
      print *,' Reading CRUST2 files ...'
      do 101 i=1,ityp
         read(k2,899)ctype(i)
c        print 899,ctype(i)
         read(k2,899)line
         read(line,*)(fvel(i,l),l=1,8)
         read(k2,899)line
         read(line,*)(fvels(i,l),l=1,8)
         read(k2,899)line
         read(line,*)(frho(i,l),l=1,8)
         read(k2,899)line
         read(line,*)(fthi(i,l),l=1,7)
c flip layers
         aux=fvel(i,1)
         fvel(i,1)=fvel(i,2)
         fvel(i,2)=aux
         aux=fvels(i,1)
         fvels(i,1)=fvels(i,2)
         fvels(i,2)=aux
         aux=frho(i,1)
         frho(i,1)=frho(i,2)
         frho(i,2)=aux
         aux=fthi(i,1)
         fthi(i,1)=fthi(i,2)
         fthi(i,2)=aux
 101  continue
      ik = kfclose(k2)

c... read CNtype file
c...............................
      read(k7,*)flons
      read(k8,899)line
      do 40 j=1,nla
         read(k8,*)ilat,(amapele(i,j),i=1,nlo)
         read(k7,901)ilat,types
         do 10 i=1,nlo
            do 20 l=1,ityp
            if(types(i).eq.ctype(l))then
              atype(i,j)=ctype(l)
              do 30 k=1,8
              amapvp(k,i,j)=fvel(l,k)
              amapvs(k,i,j)=fvels(l,k)
              amaprho(k,i,j)=frho(l,k)
 30           continue
              do 31 k=1,7
 31           amapthi(k,i,j)=fthi(l,k)
              goto 10
            endif
 20         continue
c           print*,' crust type code not found: ',types(i)
c           print*,' latitude: ',ilat,' long index: ',i
 10      continue
 40   continue
      ik = kfclose(k7)
      ik = kfclose(k8)

 890  format(////a)
 899  format(a)
 901  format(i4,1x,180(2x,a2,1x))

      return
      end

c***************************************************************
      subroutine getcrust2 ( xpt, ypt, depth, xmu, vpvs)

c layer one and two flipped, after the read statement!
c layer 1: water
c layer 2: ice

      implicit real*8 (a-h,o-z)

      parameter(nla=90,nlo=180)

      common /crust1/
     +          amapvp(8,nlo,nla),amaprho(8,nlo,nla),
     +          amapvs(8,nlo,nla),amapthi(7,nlo,nla),
     +          amapele(nlo,nla)

c     dx=360/nlo
      dx=2.0d0
      z=0.0d0

c-------------------
c     now look up coordinates

      flat = ypt
      flon = xpt
      cola=90.0d0-flat
      if(flon.gt.180.d0)flon=flon-360.d0
      ilat=int(cola/dx)+1
      ilon=int((flon+180.d0)/dx)+1

c* thickness of crust
      vthi=z
      do i=1,7
       vthi=vthi+amapthi(i,ilon,ilat)
      enddo

c** if in mantle
      if ( depth.gt.vthi) then 
       xmu = amaprho(8,ilon,ilat)*amapvs(8,ilon,ilat)**2
       vpvs = amapvp(8,ilon,ilat)/amapvs(8,ilon,ilat)
       goto 999
      endif

c** if in crust, find layer

      if(amapele(ilon,ilat).lt.0..and.amapthi(1,ilon,ilat).ne.0)
     +  amapthi(1,ilon,ilat)=-amapele(ilon,ilat)/1000.

c* dep will be bottom of layer
      dep=z

      do i=1,7
        vpvs = z
        dep = dep + amapthi(i,ilon,ilat)
        xmu = amaprho(i,ilon,ilat)*amapvs(i,ilon,ilat)**2
        if (amapvs(i,ilon,ilat).gt.z)
     .          vpvs = amapvp(i,ilon,ilat)/amapvs(i,ilon,ilat)
        if ( depth .le. dep ) goto 999
      enddo

 999  xmu=xmu*1.0d9

      return
      end
c**********************************************************************

      subroutine prem ( z, xmu, vpvs )
c** get PREM mu and Vp/Vs
      implicit real*8 (a-h,o-z)

      dimension prem_z(6), prem_vp(6), prem_vs(6), prem_rho(6)

      data prem_z  /    0.0,    0.1,   15.0,   24.4,   80.0,  150.0 /
      data prem_vp / 1450.0, 5800.0, 6800.0, 8110.6, 8076.9, 8033.7 /
      data prem_vs /    0.0, 3200.0, 3900.0, 4490.9, 4469.5, 4443.6 /
      data prem_rho/ 1020.0, 2600.0, 2900.0, 3380.8, 3374.7, 3367.1 /

      xmu=0.0d0
      vpvs=0.0d0

c* skip water layer
      do i=2,6
       if ( z.gt.prem_z(i) ) then
        vs =prem_vs(i)
        xmu = prem_rho(i)*vs**2
        if (vs.gt.0.0d0) vpvs = prem_vp(i)/vs
       endif
      enddo
      return
      end
c**********************************************************************

c**********************************************************************
c** statistical subroutines from Numerical recipes, see eqn 6.2.17
c* for computing probabilities that data are satisfied
c*
      FUNCTION gammq(a1,x1)
      REAL*8 a, gammq, x, a1, x1, two
CU    USES gcf,gser
      REAL*8 gammcf,gamser,gln,z

      z = 0.0d0
      two = 2.0d0
      gammq = z
      a = a1/two
      x = x1/two

      if(x .le. z) then
c        print *, 'x <= 0 in gammq'
        return
      endif

      if(a .le. z) then
c       print *,  'a <= 0 in gammq'
       return
      endif

c      if(x.lt.z .or. a.le.z) pause 'bad arguments in gammq'

      if(x.lt.a+1.d0)then
       call gser(gamser,a,x,gln)
       gammq=1.0d0-gamser
      else
       call gcf(gammcf,a,x,gln)
       gammq=gammcf
      endif

      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.

c***********************
      FUNCTION gammp(a,x)
      REAL*8 a,gammp,x
CU    USES gcf,gser
      REAL*8 gammcf,gamser,gln
      if(x.lt.0.) print *,  'x < 0 in gammp'
      if(a.le.0.) print *,  'a <= 0 in gammp'
      if(x.lt.a+1.0d0)then
        call gser(gamser,a,x,gln)
        gammp=gamser
      else
        call gcf(gammcf,a,x,gln)
        gammp=1.-gammcf
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.

c*****************
      SUBROUTINE gcf(gammcf,a,x,gln)
      INTEGER ITMAX
      REAL*8 a,gammcf,gln,x,EPS,FPMIN
      PARAMETER (ITMAX=500,EPS=3.e-7,FPMIN=1.e-30)
CU    USES gammln
      INTEGER i
      REAL*8 an,b,c,d,del,h,gammln
      gln=gammln(a)
      b=x+1.-a
      c=1./FPMIN
      d=1./b
      h=d
      do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.
        d=an*d+b
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
      print *, 'a too large, ITMAX too small in gcf'
1     gammcf=exp(-x+a*dlog(x)-gln)*h
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.
c*****************
      SUBROUTINE gser(gamser,a,x,gln)
      INTEGER ITMAX
      REAL*8 a,gamser,gln,x,EPS
      PARAMETER (ITMAX=500,EPS=3.e-7)
CU    USES gammln
      INTEGER n
      REAL*8 ap,del,sum,gammln
      gln=gammln(a)

      if(x.le.0.)then
        if(x.lt.0.) print *, 'x < 0 in gser'
        gamser=0.
        return
      endif

      ap=a
      sum=1./a
      del=sum

      do n=1,ITMAX
        ap=ap+1.
        del=del*x/ap
        sum=sum+del
        if(abs(del).lt.abs(sum)*EPS)goto 1
      enddo

      print *, 'a too large, ITMAX too small in gser'
      gamser = 0.0
      return

1     gamser=sum*exp(-x+a*dlog(x)-gln)

      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.
c*****************
      FUNCTION gammln(xx)
      REAL*8 gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*dlog(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+dlog(stp*ser/x)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .zW.

c******************************************************
c**  DIMCHECK  ****************************************
c******************************************************
      subroutine dimcheck ( n, maxn, label, yestop )
      character*12 label
      logical yestop
      if ( n.gt.maxn ) then
       print *, label//' exceeded'
       print *, 'Dimension=', maxn, ' Needed=', n
       yestop = .true.
      endif
      return
      end

c******************************************************
c**  WRITE GRID SCREEN  *******************************
c******************************************************

      subroutine grid_screen(kin, kk, cmin0, dchi, pensum,
     .  pminin, ptmpin,
     .  star, islope, ntrials, kgstype, ngr, maxp, dp)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"

      character ptype(50)*15, p2codes(50)*2
      common /ptype1/ ptype, p2codes

      character star*1, pform*60
      logical kgrid, kgrad, krand
      dimension islope(100)

      character*1 pm(3)
      data pm   / '\', '.', '/' /

      pmin9 = pminin
      ptmp9 = ptmpin
      dp2=pmin9-ptmp9
      cmin = cmin0

      kgrid = ( kgstype.eq.0 .or. kgstype.eq.1 )
      kgrad = ( kgstype.eq.2 .or. kgstype.eq.3 )
      krand = ( kgstype.eq.4 )

c* if kin = 0 just write out header
      if (kin.eq.0 ) then

      if ( kgrid ) then
        write (*,*) ' No Type   Ptot    D_Chi       P_sum  '//
     .   'Parameter    Change '//
     .   'Derivative sign'
      endif

      if ( kgrad ) then
        write (*,*) ' No Type   Ptot    D_Chi       P_sum  '//
     .   'Parameter    Change '//
     .   'Max_P Steps  Step'
      endif

      if ( krand ) then
        write (*,*) ' No        Ptot     D_Chi     P_sum  '//
     .   'Max_P Steps  Step'
      endif
 
 
      else

      if ( kgrid ) then
       pform='(i4,1x,a2,1pe11.4,4(1x,1pe9.2),i5,1x,100a1)'
        write( *, pform) kk, p2codes(kparm_type(kk)), 
     .   cmin, dchi, pensum, pmin9, dp2, maxp,
     .   (pm(islope(k)+2), k=1,ntrials-1) 
      endif

      if ( kgrad ) then
       pform='(i4,1x,a2,1pe11.4,4(1x,1pe9.2),i5,i4,1x,1pe9.2,a1)'
        write( *, pform) kk, p2codes(kparm_type(kk)), 
     .   cmin, dchi, pensum,
     .   pmin9, dp2, maxp, ngr-1, dp, star
      endif

      if ( krand ) then
       pform='(i4,1x,1pe11.4,2(1x,1pe9.2),2i6,1x,1pe9.2)'
        write( *, pform) kk, cmin, dchi, pensum,
     .   maxp, ngr-1, dp
      endif

      endif

      return
      end

c******************************************************
c**  WRITE SA SCREEN  *********************************
c******************************************************
      subroutine sa_screen(kin, nc, tchi, dd, ps, p)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      dimension p(MAX_parms) 
      
      nd = nparms
      k = kin
      

c* if kin = 0 just write out header
      if (k.eq.0 ) then

      write (*, *) 'It      Ptot   D_Chi      P_sum    ' //
     .  'Parameters -->'

      else

c      psum = 0.0d0
c      do i=1,MAX_pen
c       psum=psum+pentype(i)
c      enddo

       write (*,'(i3,1x,1pe10.3,2000(1x,1pe9.2))') 
     .    nc, tchi, dd, ps, (p(i),i=1,nd)
      endif

      return
      end

c**********************************************************************
c* convert lon,lat of point on fault plane to X,W coordinate on fault
c**********************************************************************
      subroutine ll2xw (kf, xpt, ypt, x, w)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension xtmp(4), ytmp(4), xt(4), wt(4)

      n4 = 4
      two = 2.0d0

c      x=0.0d0
c      w=0.0d0

      do ix=1,nxf(kf)-1
       do iz = 1,nzf(kf)-1
        xtmp(1) = xynode(1,ix,iz,kf)
        ytmp(1) = xynode(2,ix,iz,kf)
        xtmp(2) = xynode(1,ix,iz+1,kf)
        ytmp(2) = xynode(2,ix,iz+1,kf)
        xtmp(3) = xynode(1,ix+1,iz+1,kf)
        ytmp(3) = xynode(2,ix+1,iz+1,kf)
        xtmp(4) = xynode(1,ix+1,iz,kf)
        ytmp(4) = xynode(2,ix+1,iz,kf)
        call inside ( xpt, ypt, xtmp, ytmp, n4, insde)

       if ( insde .eq. 2 .or. insde .eq. 1 ) then
        xt(1) = xwnode(1,ix,iz,kf)
        wt(1) = xwnode(2,ix,iz,kf)
        xt(2) = xwnode(1,ix,iz+1,kf)
        wt(2) = xwnode(2,ix,iz+1,kf)
        xt(3) = xwnode(1,ix+1,iz+1,kf)
        wt(3) = xwnode(2,ix+1,iz+1,kf)
        xt(4) = xwnode(1,ix+1,iz,kf)
        wt(4) = xwnode(2,ix+1,iz,kf)

        call bilin (xtmp, ytmp, xt, Xpt, Ypt, x) 
        call bilin (xtmp, ytmp, wt, Xpt, Ypt, w) 

c      print *, 'LL2xw', insde,kf,ix,iz,xpt,ypt,x,w

        return

       end if

       enddo
      enddo

      return
      end

c**********************************************************************
c* convert X,W coordinate on fault to lon,lat 
c**********************************************************************
      subroutine xw2ll (kf, x, w, xpt, ypt)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension xtmp(4), ytmp(4), xt(4), wt(4)

      n4 = 4
      two = 2.0d0

      xpt=0.0d0
      ypt=0.0d0

      do ix=1,nxf(kf)-1
       do iz = 1,nzf(kf)-1
        xtmp(1) = xwnode(1,ix,iz,kf)
        ytmp(1) = xwnode(2,ix,iz,kf)
        xtmp(2) = xwnode(1,ix,iz+1,kf)
        ytmp(2) = xwnode(2,ix,iz+1,kf)
        xtmp(3) = xwnode(1,ix+1,iz+1,kf)
        ytmp(3) = xwnode(2,ix+1,iz+1,kf)
        xtmp(4) = xwnode(1,ix+1,iz,kf)
        ytmp(4) = xwnode(2,ix+1,iz,kf)
        call inside ( x, w, xtmp, ytmp, n4, insde)

       if ( insde .ne. 0 ) then
        xt(1) = xynode(1,ix,iz,kf)
        wt(1) = xynode(2,ix,iz,kf)
        xt(2) = xynode(1,ix,iz+1,kf)
        wt(2) = xynode(2,ix,iz+1,kf)
        xt(3) = xynode(1,ix+1,iz+1,kf)
        wt(3) = xynode(2,ix+1,iz+1,kf)
        xt(4) = xynode(1,ix+1,iz,kf)
        wt(4) = xynode(2,ix+1,iz,kf)

        call bilin (xtmp, ytmp, xt, X, W, xpt) 
        call bilin (xtmp, ytmp, wt, X, W, ypt) 

c        print *, 'LL2xw', ix,iz,xpt,ypt,x,w

        return

       end if

       enddo
      enddo

      return
      end

c*******************************************************************************
      subroutine bilin(x, y, z, xi, yi, zi)
      implicit real*8 (a-h, o-z)

c** bilinear interpolation between irregular points

      dimension x(4), y(4), z(4)
      dimension o(4,1), a(4,4), p(4)

      call cleareal(o,4)
      call cleareal(a,16)
      call cleareal(p,4)

      np=4
      n = 1

      do i=1,4
       o(i,1) = z(i)
       a(i,1) = 1.0d0
       a(i,2) = x(i)
       a(i,3) = y(i)
       a(i,4) = x(i)*y(i)
      enddo

      call gaussj( a, np, np, o, n, n)

      zi = o(1,1) + o(2,1)*xi + o(3,1)*yi + o(4,1)*xi*yi

      return
      end


c******************************************************
c* zero nodes for a fault
      subroutine clearnodes (kf)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      do iz=1, nzf(kf)
       do ix=1, nxf(kf)
         phi(ix, iz, kf) = 0.0d0
       enddo
      enddo
      return
      end




c*******************************************************************************

c******************************************************
c**  GETDOF  ******************************************
c******************************************************
c calculate the number of data (ndat) and degrees-of-freedom (kdof)
c******************************************************
      subroutine getdof (kdof, ndat)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      
      ndat = 0
      kdof = 1

c get number of observations
      call get_ndata (ndat)

c-- degrees of freedom        
      kdof = ndat - nparms 

c include hard constraints
      kdof = kdof + num_hc

      kdof = max(1,kdof)

      return
      end

c******************************************************
      subroutine get_str_dip (kfin)      

c get strike and dip at all nodes

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"


      dimension x(4), y(4), z(4), u(4), v(4), s(4), d(4), e(4)
      dimension p(4) 


      data xd4, x99, thou /1.0d4, 99.0d0, 1.0d3/

      kf=kfin
      nx=nxf(kfin)
      nz=nzf(kfin)
      
      do iz=1, nz-1
       do ix=1, nx-1

c-- corner nodes of this element, starting in +X direction
      call setxyz(ix,  iz,  kf,x(1),y(1),z(1),u(1),v(1),e(1),p(1))
      call setxyz(ix+1,iz,  kf,x(2),y(2),z(2),u(2),v(2),e(2),p(2))
      call setxyz(ix+1,iz+1,kf,x(3),y(3),z(3),u(3),v(3),e(3),p(3))
      call setxyz(ix,  iz+1,kf,x(4),y(4),z(4),u(4),v(4),e(4),p(4))

c-- get dips d() at 4 corners of element
      call trislope(x(4),y(4),z(4),x(1),y(1),z(1),x(3),y(3),
     .   z(3), dp1, st)

      call trislope(x(4),y(4),z(4),x(2),y(2),z(2),x(3),y(3),
     .   z(3), dp2, st)

        d(1)=dp1 
        d(2)=dp2
        d(3)=dp2
        d(4)=dp1

        fault_dip(ix,iz,kf) = dp1
        fault_dip(ix+1,iz,kf) = dp2
        fault_dip(ix,iz+1,kf) = dp1
        fault_dip(ix+1,iz+1,kf) = dp2
        sindip = dsin(d2r*(dp1+dp2)/2.0d0)
c        sindip = 1.0d0

c-- get strikes s() at 4 corners of element
       call delaz( y(2), x(2), y(1), x(1), del1, str1)
       call delaz( y(3), x(3), y(4), x(4), del2, str2)
       call fixaz(str1, str2)

        s(1)=str1
        s(2)=str1
        s(3)=str2
        s(4)=str2

        fault_strike(ix,iz,kf) = str1
        fault_strike(ix,iz+1,kf) = str2
        fault_strike(ix+1,iz,kf) = str1
        fault_strike(ix+1,iz+1,kf) = str2

       enddo
      enddo

      return
      end

c******************************************************
      subroutine okgfcalc (kfin, ixin, izin, potncy)      

c performs Okada calculation of GFs using rectangular dislocations

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

      logical nonzero_slip, wrt

      dimension Uc(3), u0(3,3)
      dimension xsub(4), ysub(4), zsub(4)
      dimension x(4), y(4), z(4), u(4), v(4), s(4), d(4), dd(4), e(4)
      dimension p(4)
      dimension tilt_tmp(MAX_tilt, 2, 2)

      real*8 ins_gf
      common /gf1/ gps_gf(MAX_gps,2,3), tilt_gf(MAX_tilt,2), 
     . ss_gf(MAX_ss,9,2,2), xll_gf(MAX_ll,2,2),
     . ins_gf(MAX_insar_pts,2,3)

      data xd4, x99, thou /1.0d4, 99.0d0, 1.0d3/

      call cleareal(tilt_tmp, 4*MAX_tilt)
      call cleareal(gps_gf,   6*MAX_gps)
      call cleareal(tilt_gf,  2*MAX_tilt)
      call cleareal(ss_gf,   36*MAX_ss)
      call cleareal(xll_gf,   4*MAX_ll)
      call cleareal(ins_gf,   6*MAX_insar_pts)
      
      call uminmax (umin, umax) 
      tmu = xmu

      potncy = 0.0d0

      kf=kfin
      nx=nxf(kfin)
      nz=nzf(kfin)
      
c* flag for shear or 3D slip vector
      k3d = 0
      if (fflag(kf,2)) k3d = 1
      k3d = ksliptype(kf)

c write flag
      wrt = .false.

      call cleareal(slip_n, MAX_f*MAX_x*MAX_z*2)
      slip_n(ixin,izin,kfin,1)=1.0d0
      slip_n(ixin,izin,kfin,2)=0.0d0

      do 40 iz=1, nz-1
       do 30 ix=1, nx-1

c-- corner nodes of this element, starting in +X direction
      call setxyz(ix,  iz,  kf,x(1),y(1),z(1),u(1),v(1),e(1),p(1))
      call setxyz(ix+1,iz,  kf,x(2),y(2),z(2),u(2),v(2),e(2),p(2))
      call setxyz(ix+1,iz+1,kf,x(3),y(3),z(3),u(3),v(3),e(3),p(3))
      call setxyz(ix,  iz+1,kf,x(4),y(4),z(4),u(4),v(4),e(4),p(4))

c-- get dips d() at 4 corners of element
      call trislope(x(4),y(4),z(4),x(1),y(1),z(1),x(3),y(3),
     .   z(3), dp1, st)

      call trislope(x(4),y(4),z(4),x(2),y(2),z(2),x(3),y(3),
     .   z(3), dp2, st)

        d(1)=dp1 
        d(2)=dp2
        d(3)=dp2
        d(4)=dp1

        fault_dip(ix,iz,kf) = dp1
        fault_dip(ix+1,iz,kf) = dp2
        fault_dip(ix,iz+1,kf) = dp1
        fault_dip(ix+1,iz+1,kf) = dp2
        sindip = dsin(d2r*(dp1+dp2)/2.0d0)
c        sindip = 1.0d0

c-- get strikes s() at 4 corners of element
       call delaz( y(2), x(2), y(1), x(1), del1, str1)
       call delaz( y(3), x(3), y(4), x(4), del2, str2)
       call fixaz(str1, str2)

        s(1)=str1
        s(2)=str1
        s(3)=str2
        s(4)=str2

        fault_strike(ix,iz,kf) = str1
        fault_strike(ix,iz+1,kf) = str2
        fault_strike(ix+1,iz,kf) = str1
        fault_strike(ix+1,iz+1,kf) = str2

c-- interpolation distances on grid in x and z
       X_int = GFx_interp
       Z_int = GFw_interp 

       if ( X_int.eq.0.0d0) X_int = 5.0d0
       if ( Z_int.eq.0.0d0) Z_int = 2.0d0

       dx = max(X_int, 0.1d0)
       call distkm( x(2), y(2), x(1), y(1), del1)
       call distkm( x(3), y(3), x(4), y(4), del2)
       dx12 = dsqrt( del1**2 + (z(2)-z(1))**2 ) /dx
       dx34 = dsqrt( del2**2 + (z(4)-z(3))**2 ) /dx

       dz = max(Z_int, 0.1d0)
       call distkm( x(2), y(2), x(3), y(3), del23)
       call distkm( x(1), y(1), x(4), y(4), del14)
       dw23 = dsqrt( del23**2 + (z(2)-z(3))**2 ) /dz
       dw14 = dsqrt( del14**2 + (z(4)-z(1))**2 ) /dz

       i2 = int( max ( dx12, dx34) )
       nix = max(5, i2 )
       i2 = int( max ( dw23, dw14) )
       niy = max(5, i2 )

       MAX_nix = max(MAX_nix, nix)
       MAX_niy = max(MAX_niy, niy)

c** integrate over element      
c  get X,Y,Z of interior points
        rnix = nix
        rniy = niy

      do 60 i= 1, nix
       do 70 j = 1, niy

        ri = i
        rj = j

        dxc=(two*ri-one)/(two*rnix)
        dyc=(two*rj-one)/(two*rniy)
      
c-- get values at center of sub-box
        call bilinterp (x, dxc, dyc, xf, dd)
        call bilinterp (y, dxc, dyc, yf, dd)
        call bilinterp (z, dxc, dyc, zf, dd)
        call bilinterp (s, dxc, dyc, strike, dd)
        call bilinterp (d, dxc, dyc, dip, dd)

        dipr = dip*d2r

c-- get xyz values at 4 corners of sub-box
        call interp4 (x, i, j, nix, niy, xsub )
        call interp4 (y, i, j, nix, niy, ysub )
        call interp4 (z, i, j, nix, niy, zsub )

c** slip deficit rates in x,y,z directions
        call bilinterp (u, dxc, dyc, Ucent, dd)
        call bilinterp (v, dxc, dyc, Vcent, dd)
        call bilinterp (p, dxc, dyc, Pcent, dd)
        call bilinterp (e, dxc, dyc, Ecent, dd)

c* get plane that approximates the quadrilateral
        xleft  = ( xsub(1) + xsub(4)) / two
        yleft  = ( ysub(1) + ysub(4)) / two
        xright = ( xsub(2) + xsub(3)) / two
        yright = ( ysub(2) + ysub(3)) / two

        call distkm(xleft, yleft, xright, yright, xlen2)
        zmax=(zsub(3)+zsub(4))/two 
        zmin=(zsub(1)+zsub(2))/two
        w=(zmax-zmin)/dsin(dipr)

        xlen=xlen2
        xff= xsub(4)
        yff= ysub(4)
        zff=zmax
        area_rec=w*xlen2
c        Uslip = dsqrt(Ucent**2 + Vcent**2)
      
c* get actual area of quadrilateral patch
        call quadarea (xsub, ysub, area)
        area=area/dcos(dipr)

c*** correct for difference in true quadrilateral area vs. rectangular area
        area_fac = area/area_rec

c** do the two slip components
        do icomp = 1,2

         Uc1 = Ucent * Pcent
         Vc1 = Vcent * Pcent
         zc1 = zero

        if (icomp.eq.2) then
         Uct = -Vc1
         Vct =  Uc1
         Uc1 = Uct
         Vc1 = Vct
        endif


c-- get s-s(U1), dip-slip(U2), and tensional (U3)  components in rotated frame
        call getu1u2u3 (k3d, strike, dip, uc1, vc1, zc1, 
     .     U1, U2, U3, Uslip, frake )

c* potency for this node
       if (icomp.eq.1) potncy = potncy + area * uslip * thou
      
c**************************************************************************
c*** loop through all points and calculate surface deformation

      nonzero_slip =( U1 .ne. zero .or. U2 .ne. zero .or. U3 .ne. zero)
      umult =  -area_fac
      zpt = -1.0d-3
      
      if (nonzero_slip) then

c-- loop through gps data locations
      if ( data_flag(1) ) then
          nr = 0

       do ii = 1, num_gf_gps2
          xpt = ggf_pos2(ii,1)
          ypt = ggf_pos2(ii,2)
          zpt = ggf_pos2(ii,3)
c          zpt = 0.0d0
          z1 = zmin + zpt
          z2 = zmax + zpt

c        if ( okada ) then
          call OKADA85 (strike, dip, Xff, Yff, xlen, z1, z2, u1,
     .       u2, u3, xpt, ypt, Uc, U0)
c        else
c           call wangfin (strike, dip, frake, Xff, Yff, xlen, 
c     .       Zmin, Zmax, Uslip, Xpt, Ypt, Uc)
c        endif

        do kk=1,3
         gps_gf(ii, icomp, kk) = gps_gf(ii, icomp, kk) + umult*Uc(kk)
        enddo

        if (wrt) write(*, '("G",i3,9f10.3,6e12.4)' ) icomp,
     .    Xpt,Ypt,Xff,Yff,xlen,zmin,strike,dip,frake,(uc(k),k=1,3),
     .    gps_gf(ii,icomp,3)

       enddo
      endif
      
c-- loop through INSAR
      if ( data_flag(6) ) then
          nr = 0

       do ii = 1, num_gf_ins2
          xpt = igf_pos2(ii,1)
          ypt = igf_pos2(ii,2)
          zpt = igf_pos2(ii,3)
c          zpt = 0.0d0
          z1 = zmin + zpt
          z2 = zmax + zpt

c        if ( okada ) then
          call OKADA85 (strike, dip, Xff, Yff, xlen, z1, z2, u1,
     .       u2, u3, xpt, ypt, Uc, U0)
c        else
c           call wangfin (strike, dip, frake, Xff, Yff, xlen, 
c     .       Zmin, Zmax, Uslip, Xpt, Ypt, Uc)
c        endif

       do kk=1,3
        ins_gf(ii, icomp, kk) = ins_gf(ii, icomp, kk) + umult*Uc(kk)
       enddo

        if (wrt) write(*, '("I",i3,9f10.3,6e12.4)' ) icomp,
     .    Xpt,Ypt,Xff,Yff,xlen,zmin,strike,dip,frake,(uc(k),k=1,3),
     .    ins_gf(ii,icomp,3)

       enddo
      endif

c-- loop through tilts, get uplift at both ends of line
      if(data_flag(2)) then
       do iii = 1, num_tilts
        do kt = 1, 2
          xpt= tilt_pos(iii,kt,1)
          ypt= tilt_pos(iii,kt,2)
           call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .       u2, u3, xpt, ypt, Uc, U0)
           tilt_tmp(iii,kt,icomp) = tilt_tmp(iii,kt,icomp) + umult*Uc(3)
        enddo
       enddo
      endif

c-- loop through surface strain rates
c   calculate displacements at 4 points around centroid of network
      if(data_flag(4) ) then
       do ii = 1, num_ss
        do kx = 1, 9
         call get_ss_pos( ii, kx, xpt, ypt )

         call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .       u2, u3, xpt, ypt, Uc, U0)

         do kk=1, 2
           ss_gf(ii,kx,kk,icomp) = ss_gf(ii,kx,kk,icomp) + umult*Uc(kk)
         enddo

        enddo

       enddo 
      endif
c* end of strain points
      
c-- loop through line length rates
c   calculate displacements at 2 points
      if(data_flag(5)) then
       do ii = 1, num_ll_sites
         xpt = pos_ll(ii,1)
         ypt = pos_ll(ii,2)

         call OKADA85 (strike, dip, Xff, Yff, xlen, zmin, zmax, u1,
     .       u2, u3, xpt, ypt, Uc, U0)

         do kk=1, 2
           xll_gf(ii,kk,icomp) = xll_gf(ii,kk,icomp) + umult*Uc(kk)
         enddo

       enddo 
      endif
c* end of line_length points

      endif

      enddo
c* end of icomp loop

c** END OF CALC
      
   70 continue
   60 continue

c       endif

c* 30 end of IX loop
   30 continue 

c* 40 end of IZ loop
   40 continue 

c      enddo
c* end of icomp loop


c* fault flag if
c      endif

c* 50 end of fault loop
c  50 continue 

c* calculate tilt rates
      if(data_flag(2)) then
       do i = 1, num_tilts
         rmm = tilt_length(i)*1.0d6
        do ic=1,2
         tilt_gf(i,ic) = (tilt_tmp(i,2,ic) - tilt_tmp(i,1,ic) )/rmm 
        enddo
       enddo
      endif


c 101  format(13f12.5)
c 102  format(4f12.4)
c 103  format(f12.5)
c 104  format(a2,1x,5i2,16f9.3)
c 105  format(a15)
c 106  format (3f16.8)

      return
      end

c************************************************************************
      subroutine setxyz (ix, iz, kf, x, y, z, u, v, e, p) 
c'--- set arrays for temporary corners of present element

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

       x = xynode(1,ix,iz,kf) 
       y = xynode(2,ix,iz,kf)
       z = znode(iz,kf)
       u = slip_n(ix,iz,kf,1)
       v = slip_n(ix,iz,kf,2)
       p = phi(ix,iz,kf)
       e = phi_err(ix,iz,kf)

      return
      end 


c************************************************************************

      subroutine makehtdp (kg, nxgrid, nygrid)

c'--- write grid for HTDP

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension uu(nxgrid, nygrid, 2)

      character*10 hdr
      character*1 c1, ns1, ew1, ns2, ew2
      logical krect

      krect = .true.

      ew1 = 'E'
      ns1 = 'N'
      ew2 = 'E'
      ns2 = 'N'
      hdr = 'IT20050101'
      call i2c(kg, 1, c1)
      
      call gridinfo(kg, x0grid, nxgrid, dxgrid, 
     .    y0grid, nygrid, dygrid, nxyg )

      call fopen (k33, 1,  '_grid_'//c1//'.htdp ')
      

c** make header line
      x0=x0grid
      if (x0.gt.180.0d0 ) x0 = x0-360.0d0
      if (x0.lt.0.0d0 ) ew1 = 'W'
      x0 = abs(x0)
      call dms (x0, ix1, minx1, xsec1)
      ixs1 = int(xsec1*1.0d2)

      x0 = x0grid+(nxgrid-1)*dxgrid
      if (x0.gt.180.0d0 ) x0 = x0-360.0d0
      if (x0.lt.0.0d0 ) ew2 = 'W'
      x0 = abs(x0)
      call dms (x0, ix2, minx2, xsec2)
      ixs2 = int(xsec2*1.0d2)

      y0 = y0grid
      if (y0.lt.0.0d0 ) ns1 = 'S'
      y0 = abs(y0)
      call dms (y0, iy1, miny1, ysec1)
      iys1 = int(ysec1*1.0d2)

      y0 = y0grid + (nygrid-1)*dygrid
      if (y0.lt.0.0d0 ) ns1 = 'S'
      y0 = abs(y0)
      call dms (y0, iy2, miny2, ysec2)
      iys2 = int(ysec2*1.0d2)

      write(k33, 1) hdr, ix1, minx1, ixs1, ew1,
     .                   ix2, minx2, ixs2, ew2, nxgrid-1,
     .                   iy1, miny1, iys1, ns1,
     .                   iy2, miny2, iys2, ns2, nygrid-1

   1  format(a10, 2(i3,i2,i4,a1),i3, 2(i3,i2,i4,a1),i3 )

      do kx=1, nxgrid
       do ky=1, nygrid

        call gridxyk (kg, kx, ky, xpt, ypt, k, kb )

        ew1='E'
        x0 = xpt
        if (x0.gt.180.0 ) x0 = x0-360.0
        if (x0.lt.0.0d0 ) ew1 = 'W'
        x0 = abs(x0)
        call dms (x0, ix, minx, secx)

        y0 = ypt
        ns1='N'
        if (y0.lt.0.0d0 ) ns1 = 'S'
        y0 = abs(y0)
        call dms (y0, iy, miny, secy)

        Ve = u_grid(1,k) + u_grid(6,k) + u_grid(10,k) 
        Se = dsqrt(u_grid(3,k)**2 + u_grid(8,k)**2 + 
     .      u_grid(12,k))
     
        Vn = u_grid(2,k) + u_grid(7,k) + u_grid(11,k) 
        Sn = dsqrt(u_grid(4,k)**2 + u_grid(9,k)**2 + 
     .      u_grid(13,k))
     
        Vu = u_grid(14,k)
        Su = u_grid(15,k)

        uu(kx,ky,1) = Ve
        uu(kx,ky,2) = Vn

       write (k33,40) kx,ky, iy,miny,secy,ns1, abs(ix),minx,secx,ew1,
     .      VN,SN,VE,SE,VU,SU

   40  FORMAT(4I3,f6.2,a1,2i3,f6.2,a1,6F8.2)

       enddo
      enddo
       ik = kfclose(k33)

c** write out gradients

      call fopen (k33, 1, '_grid_'//c1//'.grad ')

      do kx=1, nxgrid-1
       do ky=1, nygrid-1
        x1=x0grid + (kx)*dxgrid
        y1=y0grid + (ky)*dygrid
        x2=x0grid + (kx+1)*dxgrid
        y2=y0grid + (ky)*dygrid
        x3=x0grid + (kx+1)*dxgrid
        y3=y0grid + (ky+1)*dygrid
        x4=x0grid + (kx)*dxgrid
        y4=y0grid + (ky+1)*dygrid

        call distkm(x1,y1,x2,y2, dx)
        call distkm(x1,y1,x4,y4, dy)

      if ( krect) then

c**********  use rectangle

        x = (x1+x2)/2.0
        y = (y1+y4)/2.0

        twodx = 2.0*dx
        twody = 2.0*dy

        dedx =(uu(kx+1,ky,1)-uu(kx,ky,1) + 
     .         uu(kx+1,ky+1,1)-uu(kx,ky+1,1))/twodx
        dedy =(uu(kx,ky+1,1)-uu(kx,ky,1) + 
     .         uu(kx+1,ky+1,1)-uu(kx+1,ky,1))/twody

        gradx = sqrt(dedx*dedx + dedy*dedy)

        dndx =(uu(kx+1,ky,2)-uu(kx,ky,2) + 
     .         uu(kx+1,ky+1,2)-uu(kx,ky+1,2))/twodx
        dndy =(uu(kx,ky+1,2)-uu(kx,ky,2) + 
     .         uu(kx+1,ky+1,2)-uu(kx+1,ky,2))/twody

        grady = sqrt(dndx*dndx + dndy*dndy)

        write (k33, '(2f10.4, 2(1x, f12.6) )' ) x,y,gradx, grady

      else

c**********  use 2 triangles

        x = (x1+x2+x3)/3.0
        y = (y1+y2+y3)/3.0
        ddx =(uu(kx+1,ky,  1) - uu(kx,  ky,1))/dx
        ddy =(uu(kx+1,ky+1,1) - uu(kx+1,ky,1))/dy
        gradx = sqrt(ddx*ddx + ddy*ddy)

        ddx =(uu(kx+1,ky,  2) - uu(kx,  ky,2))/dx
        ddy =(uu(kx+1,ky+1,2) - uu(kx+1,ky,2))/dy
        grady = sqrt(ddx*ddx + ddy*ddy)

        write (k33, '(2f10.4, 2(1x, f12.6) )' ) x,y,gradx, grady

c**********
        x = (x1+x3+x4)/3.0
        y = (y1+y3+y4)/3.0
        ddx =(uu(kx+1, ky+1, 1) - uu(kx, ky+1,1))/dx
        ddy =(uu(kx,   ky+1, 1) - uu(kx, ky,  1))/dy
        gradx = sqrt(ddx*ddx + ddy*ddy)

        ddx =(uu(kx+1, ky+1, 2) - uu(kx, ky+1,2))/dx
        ddy =(uu(kx,   ky+1, 2) - uu(kx, ky,  2))/dy
        grady = sqrt(ddx*ddx + ddy*ddy)

        write (k33, '(2f10.4, 2(1x, f12.6) )' ) x,y,gradx, grady
      endif

        enddo
        enddo
        ik = kfclose(k33)

      return
      end


c************************************************************************
      subroutine dms(x, deg, min, sec)
c** convert decimal degrees to deg, min, sec

      implicit real*8 (a-h,o-z)
      integer deg

      sixty = 60.0d0
      zero = 0.0d0

      deg = int(x)
      dx = x - real(deg)
      min = int(dx*sixty)
      dx = x - real(deg) - real(min)/sixty
      sec = dx*sixty*sixty

      if ( sec.lt.zero ) then
        sec = sec + sixty
        min = min-1
      endif

      if ( sec.gt.59.9949d0) then
        sec = zero
        min = min+1
      endif

      if ( min .ge. 60) then
        min = min - 60
        deg = deg + 1
      endif

      return
      end

c************************************************************************
      subroutine wmapinfo
c* write to mapinfo format

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
      character*4 block_name      
      character*1 tab, quote
      
      tab = char(9)
      quote = char(34)

c** write blocks
        call fopen (k14,  1, '_blocks.mid ')
        call fopen (k15,  1, '_blocks.mif ')
        
      write(k15,'(a11)') 'VERSION 650'
      write(k15,'(a23)') 'Charset '//quote//'WindowsLatin1'//quote
      write(k15,'(a9)' ) 'Columns 5'
      write(k15,'(a15)') 'Bl_Name char(4)'
      write(k15,'(a15)') 'Pole    integer'
      write(k15,'(a15)') 'Strain  integer'
      write(k15,'(a15)') 'Xcenter   float'
      write(k15,'(a15)') 'Ycenter   float'
      write(k15,'(a4)' ) 'Data'

      do  j=1,nblocks
         if(block_flag(j) ) then

       write (k14, '(a4,2(a1,i4),2(a1,f10.4))') 
     .    block_name(j), tab, npole_block(j), tab, nstrain_block(j), 
     .    tab, fnlong(block_centroid(j,1)), tab, block_centroid(j,2)
       write(k15, '(a8)' ) 'Region 1'
       write(k15, '(i3)' ) nc_block(j)
       write(k15,'(2f14.6)') ( fn180(blockxy(jj,j,1)), 
     .    blockxy(jj,j,2), jj=1,nc_block(j) )
       endif
      enddo
      ik = kfclose (k14)
      ik = kfclose (k15)

c** write faults
      call fopen (k14,  1, '_faults.mid ')
      call fopen (k15,  1, '_faults.mif ')
      
      write(k15,'(a11)') 'VERSION 650'
      write(k15,'(a23)') 'Charset '//quote//'WindowsLatin1'//quote
      write(k15,'(a9)' ) 'Columns 8'
      write(k15,'(a17)') 'Ft_Name  char(10)'
      write(k15,'(a17)') 'Ft_Number integer'
      write(k15,'(a17)') 'NumX      integer'
      write(k15,'(a17)') 'NumZ      integer'
      write(k15,'(a17)') 'HangWall  char(4)'
      write(k15,'(a17)') 'FootWall  char(4)'
      write(k15,'(a17)') 'Depth       float'
      write(k15,'(a17)') 'Dip         float'
      write(k15,'(a4)' ) 'Data'

      do  j=1,nfault
       if (nxf(j).gt.0) then
c        dd = dd_line(j,1)(5:80)
c        print *, dd
c        read(dd, *) depp, dipp
        depp = 0.0d0
        dipp = 0.0d0
        write (k14, '(a10, 3(a1,i3), 2(a1,a4), 2(a1,f8.2) )') 
     .   fault_name(j), tab, j, tab, nxf(j), tab, nzf(j), tab,
     .   block_name(khw_blk(1,1,j)), tab, block_name(kfw_blk(1,1,j)),
     .   tab,depp, tab,dipp
        write(k15, '(a5)' ) 'Pline'
        write(k15, '(i3)' ) nxf(j)
        write(k15,'(2f14.6)') ( fn180(xynode(1,jj,1,j)), 
     .    xynode(2,jj,1,j), jj=1,nxf(j) )
       endif
      enddo

       ik = kfclose (k14)
       ik = kfclose (k15)

    
      return
      end
      
c***********************************************************************
c* write file of non-fault block boundaries      
c*********************************************************************
      subroutine wblock0
      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
      character*4 block_name      
      logical segbrk, found    

c-- boundaries with same pole      
       call fopen (kf2,  1, '_blk7.gmt ')

       do  j=1, nblocks

         nj = nc_block(j)
         blockxy(nj+1,j,1) = blockxy(1,j,1)
         blockxy(nj+1,j,2) = blockxy(1,j,2)
         segbrk = .true.

         if(block_flag(j) ) then

c get block segment
         do kk=1, nj
          bx1 = blockxy(kk,j,1)
          by1 = blockxy(kk,j,2)
          bx2 = blockxy(kk+1,j,1)
          by2 = blockxy(kk+1,j,2)
          found = .false.

c look for this segment in a fault
      do kf=1,MAX_f
         if (fflag(kf,4)) then
          do ix = 1, nxf(kf)-1
           fx1 = xynode(1,ix,1,kf)
           fy1 = xynode(2,ix,1,kf)
           fx2 = xynode(1,ix+1,1,kf)
           fy2 = xynode(2,ix+1,1,kf)
           
         if ( 
     .  (bx1.eq.fx1 .and. by1.eq.fy1 .and. bx2.eq.fx2 .and. by2.eq.fy2)
     .    .or.
     .  (bx1.eq.fx2 .and. by1.eq.fy2 .and. bx2.eq.fx1 .and. by2.eq.fy1)
     .   ) then
        found = .true.
        segbrk = .true.
         endif
        enddo
       endif
      enddo

       if ( .not. found) then
         if (kk.eq.1 .or. segbrk ) write(kf2, '("> ", i5,1x,a4)') 
     .        j, block_name(j)
         if (kk.eq.1 .or. segbrk ) write(kf2, '(2f12.4)') bx1, by1
         write(kf2, '(2f12.4)') bx2, by2
         segbrk = .false.
       endif

         enddo
        endif
       enddo
       ik = kfclose (kf2)
       
      return
      end
c***********************************************************************
c* write file of common block boundaries      
c*********************************************************************
      subroutine wblock2
      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      character*4 block_name      

c-- boundaries with same pole      
       call fopen (kf2,  1, '_blk2.gmt ')

       do  j=1, nblocks-1

         nj = nc_block(j)
         blockxy(nj+1,j,1) = blockxy(1,j,1)
         blockxy(nj+1,j,2) = blockxy(1,j,2)
         blockxy(nj+2,j,1) = blockxy(2,j,1)
         blockxy(nj+2,j,2) = blockxy(2,j,2)
         nj = nc_block(j)+2

        do k=j+1, nblocks

         if(block_flag(j) .and. block_flag(k)) then

          nk = nc_block(k)
          blockxy(nk+1,k,1) = blockxy(1,k,1)
          blockxy(nk+1,k,2) = blockxy(1,k,2)
          blockxy(nk+2,k,1) = blockxy(2,k,1)
          blockxy(nk+2,k,2) = blockxy(2,k,2)
          nk = nc_block(k)+2

       if ( (k .ne. j) .and. (npole_block(j).eq.npole_block(k)) ) then

         do kk=1, nk-1
          xk = blockxy(kk,k,1)
          yk = blockxy(kk,k,2)

          do 687 jj=2, nj-1
           xj = blockxy(jj,j,1)
           yj = blockxy(jj,j,2)

           if (  xj.eq.xk  .and. yj.eq.yk )  then

            xkp1 = blockxy(kk+1,k,1)
            ykp1 = blockxy(kk+1,k,2)

            if ( jj.gt.1) then
              xjm1 = blockxy(jj-1,j,1)
              yjm1 = blockxy(jj-1,j,2)
              if ( xjm1.eq.xkp1 .and. yjm1.eq.ykp1 ) then
               write(kf2, '(a5,1x,a4,i5,1x,a4,i5)' ) '> -Z ', 
     .             block_name(j), npole_block(j),
     .             block_name(k), npole_block(k)
               write(kf2,'(2f12.4)') xk, yk
               write(kf2,'(2f12.4)') xkp1, ykp1
               go to 687
              endif
            endif

            xjp1 = blockxy(jj+1,j,1)
            yjp1 = blockxy(jj+1,j,2)
            if ( xjp1.eq.xkp1 .and. yjp1.eq.ykp1 ) then
               write(kf2, '(a5,1x,a4,i5,1x,a4,i5)' ) '> -Z ',
     .             block_name(j), npole_block(j),
     .             block_name(k), npole_block(k)
              write(kf2,'(2f12.4)') xk, yk
              write(kf2,'(2f12.4)') xkp1, ykp1
            endif

           endif

  687    continue
  
         enddo
        endif
       endif
       enddo
       enddo
       ik = kfclose (kf2)
       
      return
      end

c-- write block boundaries - with common pole or not
      subroutine wblock3
      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      character*4 block_name      

c-- boundaries with same pole      
       call fopen (kf2,  1, '_blk2.gmt ')

c-- boundaries with different pole      
       call fopen (kf3,  1, '_blk3a.tmp ')

       do  j=1, nblocks-1

         nj = nc_block(j)
         blockxy(nj+1,j,1) = blockxy(1,j,1)
         blockxy(nj+1,j,2) = blockxy(1,j,2)
         blockxy(nj+2,j,1) = blockxy(2,j,1)
         blockxy(nj+2,j,2) = blockxy(2,j,2)
         nj = nc_block(j)+2

        do k=j+1, nblocks

         if(block_flag(j) .and. block_flag(k)) then

          nk = nc_block(k)
          blockxy(nk+1,k,1) = blockxy(1,k,1)
          blockxy(nk+1,k,2) = blockxy(1,k,2)
          blockxy(nk+2,k,1) = blockxy(2,k,1)
          blockxy(nk+2,k,2) = blockxy(2,k,2)
          nk = nc_block(k)+2

c-- different poles
       if ( (k .ne. j) .and. (npole_block(j).eq.npole_block(k)) ) then

         do kk=1, nk-1
          xk = blockxy(kk,k,1)
          yk = blockxy(kk,k,2)

          do 5 jj=2, nj-1
           xj = blockxy(jj,j,1)
           yj = blockxy(jj,j,2)

           if (  xj.eq.xk  .and. yj.eq.yk )  then

            xkp1 = blockxy(kk+1,k,1)
            ykp1 = blockxy(kk+1,k,2)

            if ( jj.gt.1) then
              xjm1 = blockxy(jj-1,j,1)
              yjm1 = blockxy(jj-1,j,2)
              if ( xjm1.eq.xkp1 .and. yjm1.eq.ykp1 ) then
               write(kf2, '(a5,1x,a4,i5,1x,a4,i5)' ) '> -Z ', 
     .             block_name(j), npole_block(j),
     .             block_name(k), npole_block(k)
               write(kf2,'(2f12.4)') xk, yk
               write(kf2,'(2f12.4)') xkp1, ykp1
               go to 5
              endif
            endif

            xjp1 = blockxy(jj+1,j,1)
            yjp1 = blockxy(jj+1,j,2)
            if ( xjp1.eq.xkp1 .and. yjp1.eq.ykp1 ) then
               write(kf2, '(a5,1x,a4,i5,1x,a4,i5)' ) '> -Z ',
     .             block_name(j), npole_block(j),
     .             block_name(k), npole_block(k)
              write(kf2,'(2f12.4)') xk, yk
              write(kf2,'(2f12.4)') xkp1, ykp1
            endif

           endif

  5     continue
  
         enddo
        endif

c-- same pole
       if ( (k .ne. j) .and. (npole_block(j).ne.npole_block(k)) ) then

         do kk=1, nk-1
          xk = blockxy(kk,k,1)
          yk = blockxy(kk,k,2)

          do 6 jj=2, nj-1
           xj = blockxy(jj,j,1)
           yj = blockxy(jj,j,2)

           if (  xj.eq.xk  .and. yj.eq.yk )  then

            xkp1 = blockxy(kk+1,k,1)
            ykp1 = blockxy(kk+1,k,2)

            if ( jj.gt.1) then
              xjm1 = blockxy(jj-1,j,1)
              yjm1 = blockxy(jj-1,j,2)
              if ( xjm1.eq.xkp1 .and. yjm1.eq.ykp1 ) then
               write(kf3, '(a5,1x,a4,i5,1x,a4,i5)' ) '> -Z ', 
     .             block_name(j), npole_block(j),
     .             block_name(k), npole_block(k)
               write(kf3,'(2f12.4)') xk, yk
               write(kf3,'(2f12.4)') xkp1, ykp1
               go to 6
              endif
            endif

            xjp1 = blockxy(jj+1,j,1)
            yjp1 = blockxy(jj+1,j,2)
            if ( xjp1.eq.xkp1 .and. yjp1.eq.ykp1 ) then
               write(kf3, '(a5,1x,a4,i5,1x,a4,i5)' ) '> -Z ',
     .             block_name(j), npole_block(j),
     .             block_name(k), npole_block(k)
              write(kf3,'(2f12.4)') xk, yk
              write(kf3,'(2f12.4)') xkp1, ykp1
            endif

           endif

  6      continue
  
         enddo
        endif


       endif
       enddo
       enddo
       ik = kfclose (kf2)
       ik = kfclose (kf3)
       
      return
      end
      
c*********************************************************************
c*** write all blocks to GMT file
c*********************************************************************
      subroutine wblock
      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      character*4 block_name      
      
       call fopen (kf2, 1,  '_blk.gmt ')
      do  j=1,nblocks
         if(block_flag(j) ) then
       write(kf2, '(a5, 2i5, f10.3,1x,a4)' ) '> -Z ', j,npole_block(j),
     .    spin(j), block_name(j)
       write(kf2,'(2f12.4)') ( blockxy(jj,j,1), 
     .    blockxy(jj,j,2), jj=1,nc_block(j) )
        endif
      enddo
      ik = kfclose (kf2)
      
      return
      end
      
c****************************************************** 
c* get moment for transient sources
c******************************************************
      subroutine tr_moment (nt)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      thou = 1.0d3
      kq = info_source(nt,2)
      
c planar sources
       if ( kq.eq.9 .or. kq.eq.11 ) then
           
       dep  = transient(nt,3)
       w    = transient(nt,4)
       flen = transient(nt,6)
       
       area = flen * w *thou*thou
       uslip = transient(nt,5)/thou
       patch_Mom = area * uslip * xmu 
      
       trans_sums(nt,1)= area
       trans_sums(nt,2)= area * uslip 
       trans_sums(nt,3)= patch_Mom
*      trans_sums(nt,4) = patch_err*patch_err
       trans_sums(nt,5)= dep * uslip*thou
       trans_sums(nt,6)= uslip*thou
       trans_sums(nt,7)= area
       endif

c other sources
       if ( kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. kq.eq.6 .or. 
     .      kq.eq.7 .or. kq.eq.8 ) then
        kf = info_source(nt,1)
        xp = 0.0d0
         do ix=1,nxf(kf)
          do iz = 1,nzf(kf)
           xp = xp + tmom_node(ix,iz,kf)*tphi(ix,iz,nt)*fnTamp(nt)
          enddo
         enddo
        trans_sums(nt,3)= xp*xmu
       endif
       
       return
       end
       
c****************************************************** 
c* make plot files and get moment for transient sources
c******************************************************
      subroutine ts_mom  

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      character*1 YN
      character*3 c3
      character*4 c4a, c4b, block_name      
      character*8 ymd
      
      logical wr, useme, useENU, wsum

      dimension xsub(4), ysub(4), zsub(4)
      dimension x(4), y(4), z(4), u(4), v(4), s(4), d(4), dd(4), e(4)
      dimension xww(4), www(4)
      dimension p(4), eventamp(3), totalvec(MAX_gps,3)
      dimension w_tmp(MAX_x,MAX_z), x_tmp(MAX_x)
      
      common /hs1/ gcurve_dt(MAX_srce), gcurve(MAX_srce,MAX_ngc)
      common /hs2/ num_gc(MAX_srce)
      common /hs3/ xsyn(11000,3)

      data xd4, x99, thou /1.0d4, 99.0d0, 1.0d3/

      if (use_crust2) call rdcrust2
      call cleareal (trans_sums, 7*MAX_srce)
      call cleareal (totalvec,   3*MAX_gps)
      tmu= xmu

      call stfarea
      kk=1
      call buildhistory (kk)

c*****************************************************
c** start integrating
c*****************************************************
      do 50 nt = 1, MAX_srce
      
       if( sflag(nt) ) then
       
       kf = info_source(nt,1)
       kq = info_source(nt,2)
       kt = info_source(nt,4)
       amp = fnTamp(nt)

       call i2c (nt, 3, c3)

       useme = (kq.eq.1 .or. kq.eq.3 .or. kq.eq.4 .or. kq.eq.6 .or. 
     .          kq.eq.7 .or. kq.eq.8)
       
             
       if(kf.gt.0 .and. kf .le. MAX_f .and. useme) then
       
      call fopen (katr, 1, '_src_'//c3//'.atr ') 
      call fopen (kf2,  1, '_src_'//c3//'.nod ')

      nx=nxf(kf)
      nz=nzf(kf)
      
     
c** max slip      
      umax = 0.0d0
      do ix=1,nx
       do iz=1,nz
        umax = max(amp*tphi(ix,iz,nt),umax)
       enddo
      enddo

c** minimum slip to be output to plotting file
c      slip_min = 0.01d0 * transient(nt,5) 
       slip_min = 0.01d0 * umax
c      slip_min = 1.0d0
      
      pmom = zero
      tmom = zero
      smom = zero
      tot_sliparea = zero
      tot_uA = zero


** integrate over fault  
**  loop through elements, each bounded by 4 nodes
**    * = nodes around quadrilateral element
**
**  updip    1 *---------------------* 2 --> strike direction
**             |                     |
**             |                     |
**             |                     |
**             |                     |
**  downdip  4 *---------------------* 3
**
      call cleareal(w_tmp, MAX_x*MAX_z)
      call cleareal(x_tmp, MAX_x)

  
      do 40 iz=1, nz 
       xstrike=0.0d0
       
        do 30 ix=1, nx

      if ( iz.lt.nz ) then
       if ( ix.lt.nx ) then

*-- corner nodes of this element, starting in +X direction
      call tset(ix,  iz,   nt,x(1),y(1),z(1),u(1),v(1),e(1),p(1))
      call tset(ix+1,iz,   nt,x(2),y(2),z(2),u(2),v(2),e(2),p(2))
      call tset(ix+1,iz+1, nt,x(3),y(3),z(3),u(3),v(3),e(3),p(3))
      call tset(ix,  iz+1, nt,x(4),y(4),z(4),u(4),v(4),e(4),p(4))

*-- get dips d() at 4 corners of element
      call trislope(x(4),y(4),z(4),x(1),y(1),z(1),x(3),y(3),
     .   z(3), dp1, st)

      call trislope(x(4),y(4),z(4),x(2),y(2),z(2),x(3),y(3),
     .   z(3), dp2, st)

        d(1)=dp1 
        d(2)=dp2
        d(3)=dp2
        d(4)=dp1

        sindip = dsin(d2r*(dp1+dp2)/2.0d0)
      
*-- get strikes s() at 4 corners of element
       call delaz( y(2), x(2), y(1), x(1), del1, str1)
       call delaz( y(3), x(3), y(4), x(4), del2, str2)
       call fixaz(str1, str2)

        s(1)=str1
        s(2)=str1
        s(3)=str2
        s(4)=str2

*-- interpolation distances on grid in x and w
       X_int = X_interp
       Z_int = W_interp 

       if ( X_int.eq.0.0d0) X_int = 5.0d0
       if ( Z_int.eq.0.0d0) Z_int = 2.0d0

       dx = max(X_int, 0.1d0)
       call distkm( x(2), y(2), x(1), y(1), del1)
       call distkm( x(3), y(3), x(4), y(4), del2)
       dx12 = dsqrt( del1**2 + (z(2)-z(1))**2 ) /dx
       dx34 = dsqrt( del2**2 + (z(4)-z(3))**2 ) /dx

       dz = max(Z_int, 0.1d0)
       call distkm( x(2), y(2), x(3), y(3), del23)
       call distkm( x(1), y(1), x(4), y(4), del14)
       dw23 = dsqrt( del23**2 + (z(2)-z(3))**2 ) /dz
       dw14 = dsqrt( del14**2 + (z(4)-z(1))**2 ) /dz

       nix = max(5, int( max ( dx12, dx34) ) )
       niy = max(5, int( max ( dw23, dw14) ) )

c       endif
c      endif

        MAX_nix = max(MAX_nix, nix)
        MAX_niy = max(MAX_niy, niy)
     
*** integrate over element      
*  get X,Y,Z of interior points
        rnix = real(nix)
        rniy = real(niy)

      do 60 i= 1, nix
       do 70 j = 1, niy

        ri = i
        rj = j

        dxc=(two*ri-one)/(two*rnix)
        dyc=(two*rj-one)/(two*rniy)
      
c*-- get values at center of sub-box
        call bilinterp (x, dxc, dyc, xf, dd)
        call bilinterp (y, dxc, dyc, yf, dd)
        call bilinterp (z, dxc, dyc, zf, dd)
        call bilinterp (u, dxc, dyc, Ucent, dd)
        call bilinterp (v, dxc, dyc, Vcent, dd)
        call bilinterp (e, dxc, dyc, Ecent, dd)
        call bilinterp (p, dxc, dyc, Pcent, dd)
        call bilinterp (s, dxc, dyc, strike, dd)
        call bilinterp (d, dxc, dyc, dip, dd)

        dipr = dip*d2r
        
c** set max error in phi to 1.0        
        Ecent = min(one, Ecent)
      
c*-- get xyz values at 4 corners of sub-box
        call interp4 (x, i, j, nix, niy, xsub )
        call interp4 (y, i, j, nix, niy, ysub )
        call interp4 (z, i, j, nix, niy, zsub )

c*** slip in x,y,z directions
        uc1 = Ucent * Pcent
        vc1 = Vcent * Pcent
        zc1 = zero

c** flag for 3D slip vector (false is for 2D; shear only)
        k3d = 0
        if (fflag(kf,2)) k3d = 1
        k3d = ksliptype(kf)

c*-- get s-s(U1), dip-slip(U2), and tensional (U3)  components in rotated frame
        call getu1u2u3 (k3d, strike, dip, uc1, vc1, zc1, 
     .     U1, U2, U3, Uslip, frake )

c        if (nt.eq.MAX_srce) print *, Ucent, Vcent, Pcent, Uslip

** get plane that approximates the quadrilateral
        xleft  = ( xsub(1) + xsub(4)) / two
        yleft  = ( ysub(1) + ysub(4)) / two
        xright = ( xsub(2) + xsub(3)) / two
        yright = ( ysub(2) + ysub(3)) / two

        call distkm(xleft, yleft, xright, yright, xlen2)
        zmax=(zsub(3)+zsub(4))/two 
        zmin=(zsub(1)+zsub(2))/two
        w=(zmax-zmin)/dsin(dipr)

        xlen=xlen2
        xff= xsub(4)
        yff= ysub(4)
        zff=zmax
        area_rec=w*xlen2
      
** get actual area of quadrilateral patch
      call quadarea (xsub, ysub, area)
      area=area/dcos(dipr)

c******************************************************************************      
c*-- write to GMT fault attribute file

       tmu = xmu

c* PREM rigidity
       if (use_prem ) call prem ( zf, tmu, vpvs )

c*** get CRUST2 rigidity
       if (use_crust2) call getcrust2 ( xf, yf, zf, tmu, vpvs)

       rso=strike*d2r
c       fpo = Ucent*dsin(rso) + Vcent*dcos(rso)
c       fno = Ucent*dcos(rso) - Vcent*dsin(rso)
       fpo = Uc1*dsin(rso) + Vc1*dcos(rso)
       fno = Uc1*dcos(rso) - Vc1*dsin(rso)
       fpo=rlimit(fpo, -x99, x99)
       fno=rlimit(fno, -x99, x99)
     

c - to source .atr file
        if (Uslip .ge. tr_umin ) then

         do kt=1,4
          call ll2xw(kf, xsub(kt),ysub(kt), xww(kt), www(kt))
         enddo

c source delay at this point
        call tdelaysite (nt, kt, Xf, Yf, delayfac )
        Tarr = transient(nt,7) + delayfac

         write(katr, 107 ) '> -Z',
     .    kf, uslip, -u1, -u2, u3, Uc1, Vc1, frake, 
     .    xf, yf, -zf, nt, ix, iz, i, j, Tarr, Pcent, area

         write(katr, 106) (xsub(kt), ysub(kt), -zsub(kt), 
     .                     xww(kt), www(kt), kt=1,4)
        endif

 106  format(5f16.8)
 107  format(a4,1x,i3,9f12.3,f7.2, 5i4, f10.4, f10.1, 1x, 1pe12.4)

******************************************************************************      

      
      if (uslip .ne. 0.0d0) tot_sliparea = tot_sliparea + area

c  total slip x Area in m^3/year
      tot_uA = tot_uA + area * uslip * thou

c total moment deficit in N-m/yr
      patch_Mom = area * uslip * tmu * thou
      patch_err = Ecent * area * tmu * thou

c trans_sums - sum of transient properties
c 1 SUM total area where slip > 0
c 2 SUM area times slip
c 3 SUM moment
c 4 SUM moment error
c 5 depth weighted slip ??
c 6 SUM slip
c 7 SUM area where slip exceeds 5% of max slip

      trans_sums(nt,1)= trans_sums(nt,1) + area *thou*thou
      trans_sums(nt,2)= trans_sums(nt,2) + area * uslip * thou
      trans_sums(nt,3)= trans_sums(nt,3) + patch_Mom
      trans_sums(nt,4)= trans_sums(nt,4) + patch_err*patch_err
      trans_sums(nt,5)= trans_sums(nt,5) + (zmin+zmax)/two * uslip*thou
      trans_sums(nt,6)= trans_sums(nt,6) + uslip*thou

c area where slip exceeds 5% of max slip
      if (uslip.gt.0.05*umax) trans_sums(nt,7)=trans_sums(nt,7)+
     .         area*thou*thou

c -- correct for difference in true quadrilateral area vs. rectangular area
        area_fac = area/area_rec

     
   70 continue
   60 continue
      endif
      endif

c-- write to source .nod file for this event
        xpt = xynode(1,ix,iz,kf)
        ypt = xynode(2,ix,iz,kf)
        zpt = znode(iz,kf)
        
        if ( ix.gt.1) then
         call distkm( xpt, ypt,xynode(1,ix-1,iz,kf), 
     .     xynode(2,ix-1,iz,kf), xx)
         xstrike = xstrike+xx
        endif

        if (iz.gt.1) then
         zz  = zpt - znode(iz-1, kf)
         call distkm( xpt, ypt,xynode(1,ix,iz-1,kf), 
     .     xynode(2,ix,iz-1,kf), xw)
         w = dsqrt( xw*xw + zz*zz)
         w_tmp(ix,iz) = w_tmp(ix,iz-1)+w
         x_tmp(ix) = x_tmp(ix)+xw
        endif
        
c fixed 9-14-20
         xstrike = xwnode(1,ix,iz,kf)
         w_tmp(ix,iz) = xwnode(2,ix,iz,kf)

        kfw = kfw_blk(ix, iz, kf)
        khw = khw_blk(ix, iz, kf)

c slip amplitude
        pxE = slip_t(ix,iz,nt,1)
        pxN = slip_t(ix,iz,nt,2)
        pp = tphi(ix,iz,nt)* amp


        px = pxE * pp
        py = pxN * pp
        pe = phi_err(ix,iz,kf)
        pe = 0.0d0

        xmom = tmom_node(ix,iz,kf)*pp*tmu
        if (xmom.lt.1.0d0 ) xmom = 0.0d0

        slipaz = fnstriker(pxE,pxN)
        if (info_source(nt,3).eq.1) slipaz = transient(nt,13)

        c4a='xxxx'
        c4b='xxxx'
        if ( khw .ne. 0) c4a=block_name(khw)
        if ( kfw .ne. 0) c4b=block_name(kfw)

c source delay at this node 
      call tdelaysite (nt, kt, Xpt, Ypt, delayfac )
      Tarr = transient(nt,7) + delayfac

c source .nod file
        write(kf2, 503)  fault_name(kf), kf, ix, iz,
     .    c4a, c4b, xpt, ypt, zpt, pp, min(pe, 1.0d10),
     .    (slip_n(ix,iz,kf,k), k=1,5), -px, -py, slipaz,
     .    xstrike, x_tmp(ix), w_tmp(ix,iz),
     .    fault_strike(ix,iz,kf), fault_dip(ix,iz,kf),
c temp output dtnode(ix,iz,nt), delayfac,
     .    Tarr, dtnode(ix,iz,nt)*dpy, xmom

c----

c  503 format (a10, 3i4, 2(1x,a4), 1x, 3f9.3, 2f10.1, 4f8.1, f8.4, 
c     .   2f10.1, 6f8.1, f10.4, 1pe12.4)
  503 format (a10, 3i4, 2(1x,a4), 1x, 3f9.3, 2f10.1, 4f8.1, f8.4, 
     .   2f10.1, 6f8.1, 2f10.4, 1pe12.4)

   30 continue 
   40 continue 
   
      ik = kfclose(katr)
      ik = kfclose(kf2)
      
      endif

c** moments for quakes      
      if(kq.eq.9 .or. kq.eq.11) then
      
       call tr_moment(nt)
     
c** find 4 corners of fault plane, xf,yf is centroid
       xf = transient(nt,1)
       yf = transient(nt,2)
       w =  transient(nt,4)
       flen = transient(nt,6)
       strike = transient(nt,11)
       dip = transient(nt,12)
       
       call faultbox(nt, xf, yf, flen, w, strike, dip )
       
      endif
      
c** write predicted displacements to file, but note with Y/N if site was operating
      if(kq.gt.0) then

       call fopen (katr, 1,  '_src_'//c3//'.disp ') 
       
c      kk=1
c      call buildhistory (kk)
      
c** start and end time      
      To = transient(nt,7)
      Te = To + tdur2(nt)/dpy
      amp = fnTamp(nt)
      Xm = transient(nt,1)
      Ym = transient(nt,2)
      call YYYYMMDD ( To, ymd)

c-- disp file for GAMIT
      if(wkbob) then
         call fopen (kbob, 1,'_src_'//c3//'_'//ymd//'_rwk.disp ') 
         write(kbob, '("EV ", f10.4)') To
      endif

c---- site loop       
      do i=1, num_gps
      
      ktype = gps_type(i)

      if ( ktype.gt.1) then

      call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sigx, sigy, sigz, ss)
       call gpslongname(i)

c       Tarrive(nt,i) = To
       call ps_project( xpt, ypt, Xm, Ym, Xc1, Yc1)
       call distkm ( xpt, ypt, Xm, Ym, dkm)

       nfile  = gps_index(i)

       ksyn=1
       call ts_calc (0, i, ksyn)

       n1 = ndx(i,1)
       n2 = ndx(i,2)
       k = loc_gps(i)
       Ux = tr_def(nt,k,1)
       Uy = tr_def(nt,k,2)
       Uz = tr_def(nt,k,3)

c-- write displacement data to .disp file, for this event
c displacement is between time t1 and t2, as supplied by user in input file
c but this includes interseismic

      if ( ktype.eq.2)  then
         t2 = timespan(i,2)
         t1 = timespan(i,1)
         YN = 'Y'
         wr = .true.
         if ( Te.lt.t1) wr =.false.
         if ( To.gt.t2) wr =.false.
         xc = 0.0d0
         yc = 0.0d0
         zc = 0.0d0
         if ( .not. wr ) YN = 'N'
        
         if(useENU(i,1)) xc = gps_calc(i, 2, 1) - gps_calc(i, 1, 1)
         if(useENU(i,2)) yc = gps_calc(i, 2, 2) - gps_calc(i, 1, 2)
         if(useENU(i,3)) zc = gps_calc(i, 2, 3) - gps_calc(i, 1, 3)
         sigx = gps_obs(i,3)
         sigy = gps_obs(i,4)
         sigz = gps_obs(i,7)
         tcd = dsqrt(xc*xc+yc*yc+zc*zc)
                
         if (YN.eq.'Y') write(katr, 126) longname, xpt, ypt, 
     .     xc, yc, zc, sigx, sigy, sigz, YN, To, t1, t2, tcd, dkm

         if (wkbob .and. tcd.ge.0.0d0) 
     .     write(kbob, '(f7.1,2f8.1,1x,a4,f8.4,f10.4,1x,a1,4f9.3)' ) 
     .     xc, yc, zc, gps_name(i)(1:4), ypt, xpt
     .     , YN,dkm,t1,t2,tcd

c for the time series data, the .disp file has the entire offset for the event
c it is not truncated by time

      elseif (ktype.eq.3) then
       
c**************************************************       
c find syn amplitude at this event time   
c**************************************************       
        ts1 = timespan(i,1)
        ts2 = timespan(i,2)
        call cleareal(eventamp, 3)
        dtsyn  = fndtsyn(i)
        
        nk = int((ts2-ts1)/dtsyn) + 1
        do kk=1,nk
          t = ts1+real(kk-1)*dtsyn

          if ( t .ge. To .and. t .le. To+dtsyn ) then
           do j=1,3
            eventamp(j) = xsyn(kk,j)-GXo(i,j,1)-GVo(i,j,2)*(t-ts1) 
           enddo
          endif
        enddo
        
c**************************************************       
       t1 = t_disp(n1)
       t2 = t_disp(n2)
       dt = t2 - t1
       
       erh = 1.0d0
       erz = 2.0d0
c       if (dt.gt.zero) erh = 2.0d0/dt
c       if (dt.gt.zero) erz = 10.0d0/dt
       
       wr = .true.
       if ( Te.lt.t1 ) wr =.false.
       if ( To.gt.t2 ) wr =.false.
       
       YN = 'N'
       if ( wr ) YN = 'Y'
       
       kt = info_source(nt,4)
       a=1.0d0
c       if (kt.eq.5) a = -a
       
c       if (wr) 

       xc = a*tr_def(nt,k,1)
       yc = a*tr_def(nt,k,2)
       zc = a*tr_def(nt,k,3)

        tcd = dsqrt(xc*xc+yc*yc+zc*zc)
       if (wkbob .and. tcd.gt.0.0d0) 
     .     write(kbob, '(f7.1,2f8.1,1x,a4,f8.4,f10.4,1x,a1,4f9.3)' ) 
     .     xc, yc, zc, gps_name(i)(1:4), ypt, xpt
     .     , YN,dkm,t1,t2,tcd

       write(katr, 126) longname,xpt,ypt, xc, yc, zc, 
     .   erh, erh, erz, YN, To, t1, t2, (eventamp(j),j=1,3), 
     .   Tarrive(nt,i), i, Xc1, Yc1, tcd, dkm

c        write(katr, 126) longname,xpt,ypt,(eventamp(j), j=1,3 ), 
c     .   erh, erh, erz, YN, To, t1, t2, (a*tr_def(nt,k,j),j=1,3), 
c     .   Tarrive(nt,i), i

 126  format(a23, 2f10.4, 3f10.2, 3f8.2,  
     .     1x,a1,1x,3f9.3,3f10.2,f9.3,i6,4f10.2)

c**************************************************       
c** total displacement at each site     
       if (kt .ne. 5) then
        do j=1,3
         totalvec(i,j) = totalvec(i,j) + a*tr_def(nt,k,j) 
        enddo
       endif
       
c**************************************************       
       endif
       
c-- ktype if       
       endif

c-- end of sites loop       
      enddo
      ik = kfclose(katr)
      if (wkbob) ik = kfclose(kbob)
      
      endif

      endif
   50 continue 

c***  output total displacements from all events in file MMMM_src_sum.disp
c not working, includes only sources on faults
      wsum = .false.
      if (wsum) then
       call fopen (katr,  1, '_src_sum.disp ') 
       erh = 1.0
       erz = 3.0
      do i=1, num_gps
       if ( ndx(i,1).gt.0) then
        call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, xc, yc, zcalc,
     .    sx, sy, sigz, ss)
        call gpslongname(i)
        write(katr, 126) 
     .    longname, xpt, ypt, (totalvec(i,j), j=1,3), erh, erh, erz
       endif
      enddo
      ik=kfclose(katr)
      endif
      


      return
      end
      
c************************************************************************
      subroutine faultbox(nt, xf, yf, flen, w, strike, dip)
c-- get outline of fault      
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      character c3*3
      
      call i2c (nt, 3, c3)
      k=0
       
c-- horizontal part of w       
      cd = dcos(dip*d2r)
      sd = dsin(dip*d2r)
      dw  = w * cd
      dl  = flen 
      dw2 = (w * cd)/two
      dl2 = flen/two 
      al = strike*d2r
      ss = dsin(al)
      cs = dcos(al)
       
c-- get offset to bottom center of fault
      dX =  0.5 * W*cd*cs
      dY = -0.5 * W*cd*ss
      call unproject( dx, dy, xf, yf, x2i, y2i)
      
       call fopen(k55, 1, '_src_'//c3//'.pxy ')
        write(k55,'(2f10.4)')  transient(nt,1), transient(nt,2)
c        write(k55,'(2f10.4)') x2i, y2i
       ik=kfclose(k55)

c  write out fault plane rectangle
      call fopen (k23,  1, '_src_'//c3//'.poly ') 
      
c get four corners of fault in cartesian, rotate about strike, and convert back to lon/lat
        write(k23,'(2f10.4)') x2i, y2i
        dx2 = dx + dl2*ss
        dy2 = dy + dl2*cs
        call unproject( dx2, dy2, xf, yf, x2, y2)
        write(k23,'(2f10.4)') x2, y2
        dx3 = dx2 - dw*cs
        dy3 = dy2 + dw*ss
        call unproject( dx3, dy3, xf, yf, x2, y2)
        write(k23,'(2f10.4)') x2, y2
        dx4 = dx3 - dl*ss
        dy4 = dy3 - dl*cs
        call unproject( dx4, dy4, xf, yf, x2, y2)
        write(k23,'(2f10.4)') x2, y2
        dx5 = dx4 + dw*cs
        dy5 = dy4 - dw*ss
        call unproject( dx5, dy5, xf, yf, x2, y2)
        write(k23,'(2f10.4)') x2, y2
        dx6 = dx5 + dl2*ss
        dy6 = dy5 + dl2*cs
        call unproject( dx6, dy6, xf, yf, x2, y2)
        write(k23,'(2f10.4)') x2, y2
      
       ik = kfclose(k23)
       
       return
       end
c*********************************************************************
c-- convert decimal year to character YYYYMMDD        
      subroutine YYYYMMDD ( yr, ymd)
      implicit real*8 (a-h,o-z)
      
      character ymd*8, cimo*2, cida*2, cyr*4
      
      call julday(3, yr, imo, ida, jday) 
      call i2c(imo,2,cimo)
      call i2c(ida,2,cida)
      iyr = int(yr)
      call i2c(iyr,4,cyr)
      ymd = cyr//cimo//cida
      
      return
      end
c*********************************************************************
c*** write transients
c*********************************************************************

      subroutine writetrans

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"
      character cymd*8, endymd*8, ic3*3
      character etype(12)*18, etype2(12)*3, 
     .          ttype(9)*11, ttype2(9)*3
      
      dimension pX(200), pW(200)
      logical hastrans
      
      data etype / 'Distributed slip  ', 
     .             'Wang exp          ', 
     .             '1D Boxcar         ', 
     .             '1D Gauss          ', 
     .             'Not used          ', 
     .             '2D Gauss          ', 
     .             '2D Boxcar         ', 
     .             'Polygon           ', 
     .             'Uniform slip plane', 
     .             'Mogi source       ', 
     .             'Planar crack      ',
     .             'Prolate spheroid  '/ 
     
      data etype2 / 'DSS','WEX','1DB','1DG','   ','2DG','2DB','PLY',
     .              'USP','MGI','PLC', 'PLS' /
     
      data ttype / 'Impulse    ',
     .             'Gaussian   ', 
     .             'Triangles  ', 
     .             'Exponential', 
     .             'One Boxcar ',
     .             'Boxcars    ',
     .             'Omori      ', 
     .             'Shen       ',
     .             'VE Mogi    ' / 
      data ttype2 / 'IMP','GSS','TRI','EXP','BOX','BSP','BXS','SHN', 
     .              'VEM' /
      
c see if there are transients to write
      hastrans = .false.
      do nt = 1, MAX_srce
        if( sflag(nt) ) hastrans = .true.
      enddo
      if ( .not. hastrans ) return

      print *, 'Writing transients '


      call ts_mom
      call stfarea
      
      tmu = xmu
      z=0.0d0
      zero=z

c-- source summary      
      call fopen (k36, 1, '.sources ')

c-- .plots file for plotting with GMT        
      call fopen (k37b, 1, '.plots ')
  
      do nt = 1, MAX_srce
       
       if( sflag(nt) ) then
       
        call i2c(nt, 3, ic3)
       
c-- write source polygon      
       call makepoly (nt, pX, pW, np, 1)
      
        kf = info_source(nt,1)
        kq = info_source(nt,2)
        kaz = info_source(nt,3)
        kt = info_source(nt,4)
        ktp1 = kt+1
        ntau = info_source(nt,7)
        
        xmo = zero
        xmw = zero
        area95 = zero
        areakm2 = zero
        avslip = zero
        stressdrop = zero
        
        if ( kq.eq.1 ) then
         xmo = trans_sums(nt,3) 
         xmw  = fnMw(xmo)
         area95 = trans_sums(nt,7)+1.0d0
         sqarea = dsqrt(area95)
         areakm2 = trans_sums(nt,7)/1.0d6
         avslip = xmo/(tmu*area95)*1.0d3
         stressdrop = tmu*(avslip/1.0d3)/sqarea
        endif

        if ( kq.eq.3 .or. kq.eq.4 ) then
         xmo = trans_sums(nt,3) 
         xmw  = fnMw(xmo)
         area95 = trans_sums(nt,7)+1.0d0
         sqarea = dsqrt(area95)
         areakm2 = trans_sums(nt,7)/1.0d6
         avslip = xmo/(tmu*area95)*1.0d3
         stressdrop = tmu*(avslip/1.0d3)/sqarea
        endif

        if ( kq.eq.6 .or. kq.eq.7 ) then
         xmo = trans_sums(nt,3) 
         xmw  = fnMw(xmo)
         area95 = trans_sums(nt,7)+1.0d0
         sqarea = dsqrt(area95)
         areakm2 = trans_sums(nt,7)/1.0d6
         avslip = xmo/(tmu*area95)*1.0d3
         stressdrop = tmu*(avslip/1.0d3)/sqarea
        endif

        if ( kq.eq.9 ) then
         xmo = trans_sums(nt,3) 
         xmw  = fnMw(xmo)
         area95 = trans_sums(nt,7)+1.0d0
         sqarea = dsqrt(area95)
         areakm2 = trans_sums(nt,7)/1.0d6
         avslip = xmo/(tmu*area95)*1.0d3
         stressdrop = tmu*(avslip/1.0d3)/sqarea
        endif
        
        if ( kq.eq.11) then
         areakm2 = transient(nt,4)*transient(nt,6)
         avslip = transient(nt,5)
         xmo = areakm2*abs(avslip)*tmu*1.0d3
         xmw  = fnMw(xmo)
         area95 = areakm2
        endif
        
c-- assign uncertainties        
        do k=1,20
         errtransient(nt,k) =0.0d0
         np = nsource_parm(nt,k)
         if (np.gt.0) errtransient(nt,k)=parm_err(np)
        enddo
        
        do k=1,ntau
         errtau(nt,k) =0.0d0
         np = ntau_parm(nt,k)
         if (np.gt.0) errtau(nt,k)=parm_err(np)
        enddo
        
        n=info_source(nt,6)
        do k=1,n
         rpoly_err(nt,k) = 0.0d0
         np = nrad_parm(nt,k)
         if (np.gt.0) rpoly_err(nt,k)=parm_err(np)
        enddo

        yr = transient(nt,7) 
        erryr = errtransient(nt,7)
        
        call YYYYMMDD (yr, cymd )
        yr2 = yr + tdur2(nt)/dpy
        call YYYYMMDD (yr2, endymd )
        ndays = int(tdur2(nt))
        ndays=0
      
        
c-- .plt file for plotting with GMT        
      call fopen (k37a, 1, '_src_'//ic3//'.plt ')

c-- distributed slip event, order yr, duration, lon, lat, dep, x-len, w-len, amp, azimuth, Ts, migr rate, migr az, 
c--                               afterslip rate, after slip duration, Mw, Mo, area, YYYYMMDD, JD
c---------------------------------------------------------------------------------------------
       if ( kq.le.8 .and. (kq.ne.3 .and. kq.ne.4) ) then
        write (k37a, 370 ) etype2(kq), ttype2(ktp1),
     .    ic3, (info_source(nt,j),j=1,5), cymd, 
     .    yr, tdur(nt),(transient(nt,j), j=1,6), 
     .    (transient(nt,j), j=8,13),
     .    xmw, avslip, xmo, area95, cymd, endymd, ndays

        write (k37b, 370 ) etype2(kq), ttype2(ktp1),
     .    ic3, (info_source(nt,j),j=1,5), cymd, 
     .    yr, tdur(nt),(transient(nt,j), j=1,6), 
     .    (transient(nt,j), j=8,13),
     .    xmw, avslip, xmo, area95, cymd, endymd, ndays
       endif
       
 370  format (3(a3,1x),5i3, 1x,a8, f9.3, f9.1, 2f10.4, 
     .    10f9.1, f8.2, f9.1, 1pe12.4, 1pe12.4, 2(1x,a8), i6)

c---------------------------------------------------------------------------------------------
       if ( kq.eq.3 .or. kq.eq.4 ) then
        write (k37a, 377 ) etype2(kq), ttype2(ktp1),
     .   ic3, (info_source(nt,j),j=1,5), cymd, 
     .   yr, tdur(nt),(transient(nt,j), j=1,5), z, z, z, z, z,
     .   (transient(nt,j), j=9,10),
     .   xmw, avslip, xmo, area95, cymd, endymd, ndays
        write (k37b, 377 ) etype2(kq), ttype2(ktp1),
     .   ic3, (info_source(nt,j),j=1,5), cymd, 
     .   yr, tdur(nt),(transient(nt,j), j=1,5), z, z, z, z, z,
     .   (transient(nt,j), j=9,10),
     .   xmw, avslip, xmo, area95, cymd, endymd, ndays
       endif
 377  format (3(a3,1x),5i3, 1x,a8, f9.3, f9.1, 2f10.4, 10f9.1,   
     .    f8.2, f9.1, 1pe12.4, 1pe12.4, 2(1x,a8), i6)

c---------------------------------------------------------------------------------------------
c-- planar event, order yr, duration, lon, lat, dep, x-len, w-len, amp, strike, dip, rake 
       if ( kq.eq.9 .or. kq.eq.11 .or. kq.eq.12 ) then
        write (k37a, 373 ) etype2(kq), ttype2(ktp1),
     .   ic3, (info_source(nt,j),j=1,5), 
     .   cymd, yr, tdur(nt),
     .   (transient(nt,j), j=1,6), (transient(nt,j), j=11,12),
     .   fn180(transient(nt,13)), (transient(nt,j), j=9,10),  z,
     .   xmw, avslip, xmo, area95, cymd, endymd, ndays
        write (k37b, 373 ) etype2(kq), ttype2(ktp1),
     .   ic3, (info_source(nt,j),j=1,5), 
     .   cymd, yr, tdur(nt),
     .   (transient(nt,j), j=1,6), (transient(nt,j), j=11,12),
     .   fn180(transient(nt,13)), (transient(nt,j), j=9,10),  z,
     .   xmw, avslip, xmo, area95, cymd, endymd, ndays
       endif
       
c-- SDR:   9  1  3  6  0  1 20071219 2007.968   0.0  178.250  -38.800    30.4    38.7   334.2    20.0    76.3    54.3   -86.3     0.0     0.0    6.61   0.334  1.0370E+19  7.7579E+08

 373  format (3(a3,1x),5i3, 1x, a8,  f9.3, f9.1, 2f10.4, 10f9.1, 
     .    f8.2,  f9.1, 1pe12.4, 1pe12.4, 2(1x,a8), i6 )
     
c---------------------------------------------------------------------------------------------
c-- MOGI order yr, duration, lon, lat, dep, amp 
       if ( kq.eq.10 ) then
        write (k37a, 372 )  etype2(kq), ttype2(ktp1),
     .   ic3, (info_source(nt,j),j=1,5), 
     .   cymd, yr, tdur(nt),
     .   (transient(nt,j), j=1,3), 
     .   (transient(nt,j), j=9,10),  
     .   stf_area(nt), cymd, endymd, ndays,
     .   z,z,z,z,z,z,z,z 
        write (k37b, 372 )  etype2(kq), ttype2(ktp1),
     .   ic3, (info_source(nt,j),j=1,5), 
     .   cymd, yr, tdur(nt),
     .   (transient(nt,j), j=1,3), 
     .   (transient(nt,j), j=9,10),  
     .   stf_area(nt), cymd, endymd, ndays,
     .   z,z,z,z,z,z,z,z 
       endif
 372  format (3(a3,1x),5i3, 1x,a8, f9.3, f9.1, 2f10.4, 4f9.1,  
     .       2(1x,a8), i6, 8f4.1)
c-- MGI:  16  1  2  6  2  7 20080303 2008.170 216.3  175.945  -38.802     3.0    23.0

c---------------------------------------------------------------------------------------------

      ik = kfclose(k37a)

c-- write .sources file     
     
      write(k36,*) ' '
      write(k36,'("Source    ",i3)') nt
      write(k36,'("Spat type ",i4,1x,a18)') kq,etype(kq)
      write(k36,'("Temp type ",i4,1x,a11)') kt,ttype(kt+1)
      if (kt.eq.2) write(k36,'("Tau      ", f6.1, " days")') dtau(nt)
      write(k36,'("YYYYMMDD   ", a8," - ",a8, 2x, i6, " days")')  
     .     cymd, endymd, int(tdur2(nt))

c      if (kq.ne.10 .and. kq.ne.12)
       write(k36,'("STF area, mm",f12.2)')  stf_area(nt)

      if ( kt.gt.0 ) write(k36,'("Duration, days ", f8.1)') tdur(nt)
     
      if (kq.ne.10 .and. kq.ne.12) 
     .   write(k36,'("Mw ",f6.2, "   Mo ", 1pe9.2," Nm")') 
     .    xMw, xMo

      if (kq.ne.10 .and. kq.ne.11 .and. kq.ne.12) then
        write(k36,'("Av Slip, mm  ",1pe9.2)') 
     .    avslip 
        write(k36,'("Area, km^2   ",1pe9.2)') 
     .    areakm2 
      endif

      if (stressdrop.gt.0.0d0 )  
     .    write(k36,'("Stress drop ",f12.5," MPa")')  stressdrop/1.0d6

c-- Volume changes for volcanic sources
      if (kq.eq.10) write(k36,'("DeltaV ",f12.5," Mm^3",f12.5," km^3")') 
     .    stf_area(nt), stf_area(nt)/1.0d3
      if (kq.eq.11) write(k36,'("DeltaV ",f12.5," Mm^3",f12.5," km^3")') 
     .    stf_area(nt)*transient(nt,4)*transient(nt,6)/1.0d3,
     .    stf_area(nt)*transient(nt,4)*transient(nt,6)/1.0d6
      if (kq.eq.12) write(k36,'("DeltaV ",f12.5," Mm^3",f12.5," km^3")') 
     .    stf_area(nt), stf_area(nt)/1.0d3

c list parameters
c                  Year         2005.600       0.000
      write(k36,'("Parameter       Value       Sigma")') 

      write(k36,'("Year     ",2f12.3)') yr, erryr
     
       if (kq.gt.5) write(k36,'("Longitude",2f12.4)') 
     .    transient(nt,1),errtransient(nt,1)
       if (kq.gt.5) write(k36,'("Latitude ",2f12.4)') 
     .    transient(nt,2),errtransient(nt,2)

c-- Mogi      
      if (kq.eq.10) then 
        write(k36,'("Depth    ",2f12.3)') 
     .    transient(nt,3),errtransient(nt,3)
       if (kt.eq.8) then
        write(k36,'("R1       ",2f12.3)') 
     .    transient(nt,4),errtransient(nt,4)
        write(k36,'("R2       ",2f12.3)') 
     .    transient(nt,6),errtransient(nt,6)
       endif
      endif
     
      if (kaz.eq.1) write(k36,'("Slip az  ",2f12.3)') 
     .    transient(nt,13),errtransient(nt,13)
      
       if (kt.eq.2 .or. kt.eq.5) then
        do j=1,ntau
         ttau1 = transient(nt,7) + real(j-1) * dtau(nt)/dpy
         ttau2 = transient(nt,7) + real(j)   * dtau(nt)/dpy
c         tau3 = atau(nt,j) / 1.0d3 / secperyr
c         write(k36,'("Tau rate ", 4f12.3, 1pe12.3)') 
         write(k36,'("Tau rate ", 4f12.3)') 
     .    atau(nt,j), errtau(nt,j), ttau1, ttau2 
        enddo
       else
        if(kt.ne.8) then
         write(k36,'("Tc, days ",2f12.3)') 
     .    transient(nt,8),errtransient(nt,8)
         write(k36,'("Tc, years",2f12.3)') 
     .    transient(nt,8)/dpy,errtransient(nt,8)/dpy
         endif
        if (kq.ne.10 .and. kq.ne.12) write(k36,'("Slip amp ",2f12.3)') 
     .    transient(nt,5),errtransient(nt,5)
       endif 

c VE model, get Ts in years, Segall eqn 7.98 
      if(kt.eq.8) then
        R1 = transient(nt,4)
        R2 = transient(nt,6)
        r3 = max(one, (R2/R1)**3 )
        visc = 10.0d0**transient(nt,8)
        Ts = 3.0d0*r3*visc*(one-poisrat) / ( xmu*(one+poisrat) )
        Ts = Ts/secperyr
        write(k36,'("Viscosity",1pe12.3)') visc
        write(k36,'("Tc, days ",f12.3)') Ts*dpy
        write(k36,'("Tc, years",f12.3)') Ts 
      endif

c-- quake or planar crack   
      if (kq.eq.9 .or. kq.eq.11 ) then
       write(k36,'("Depth    ",2f12.3)') 
     .    transient(nt,3),errtransient(nt,3)
       write(k36,'("X-width  ",2f12.3)') 
     .    transient(nt,6),errtransient(nt,6)
       write(k36,'("W-width  ",2f12.3)') 
     .    transient(nt,4),errtransient(nt,4)
       write(k36,'("Strike   ",2f12.3)') 
     .    transient(nt,11),errtransient(nt,11)
       write(k36,'("Dip      ",2f12.3)') 
     .    transient(nt,12),errtransient(nt,12)
      endif
      
c-- Spheroid   
      if (kq.eq.12 ) then
       write(k36,'("Depth(km)",2f12.3)') 
     .    transient(nt,3),errtransient(nt,3)
       write(k36,'("A (km)   ",2f12.3)') 
     .    transient(nt,4),errtransient(nt,4)
       write(k36,'("A/B      ",2f12.3)') 
     .    transient(nt,6),errtransient(nt,6)
       write(k36,'("Strike   ",2f12.3)') 
     .    transient(nt,11),errtransient(nt,11)
       write(k36,'("Plunge   ",2f12.3)') 
     .    transient(nt,12),errtransient(nt,12)
      endif

       if (kq.eq.9) write(k36,'("Rake     ",2f12.3)') 
     .    fn180(transient(nt,13)),errtransient(nt,13)
      
      
c-- 2D gaussian or 2D boxcar
      if(kq.eq.6 .or. kq.eq.7) then
       write(k36,'("X-width  ",2f12.3)') 
     .    transient(nt,6),errtransient(nt,6)
       write(k36,'("W-width  ",2f12.3)') 
     .    transient(nt,4),errtransient(nt,4)
       write(k36,'("X-azimuth",2f12.3)') 
     .    transient(nt,14),errtransient(nt,14)
      endif
      
c-- polygon  
      if(kq.eq.8) then
       np=info_source(nt,6)
       daz = 360.0d0/real(np)
       az = 0.0d0
        do k=1,np
         write(k36,'("Poly rad ",3f12.3)') 
     .    rpoly(nt,k),rpoly_err(nt,k), az
         az = az+daz
        enddo
      endif
      
       if ( transient(nt,9).ne.zero) 
     .    write(k36,'("X_Mig-rat",2f12.3)') 
     .    transient(nt,9),errtransient(nt,9)
       if ( transient(nt,10).ne.zero) 
     .    write(k36,'("W_Mig-rat",2f12.3)') 
     .    transient(nt,10),errtransient(nt,10)
     
      

c NoFltTypSrcFtpNta     Long      Lat   Xwide   Wwide    Amp   Azim      To     Ts      Mw    AvSlip   Moment     Area 
  
     
       endif
      enddo
      ik = kfclose(k36)
      ik = kfclose(k37b)
      
     
c  33  format ("Errors ",18x, 2f9.3,4f8.1, f9.3, 6f8.2)
c  34  format (i4, 1x, a8, 
c     .            2(f8.2," (",f5.2,")"), 
c     .            4(f8.1," (",f6.1,")"),
c     .            1(f9.3," (",f6.3,")"), 
c     .            3(f8.1," (",f5.1,")"),
c     .            1(f8.2," (",f5.2,")"),
c     .            1(f8.1," (",f5.1,")"),
c     .            f6.2, 2i6, 1pe12.4)  
     
     
c  35  format (i4, 1x, a8, 
c     .            2(f8.2,f5.2), 
c     .            4(f8.1,f6.1),
c     .            1(f9.3,f6.3), 
c     .            4(f8.1,f6.1),
c     .            1(f8.2,f6.2),
c     .            1(f8.1,f6.1),
c     .            f6.2, 2i6, 1pe12.4)  
  
      sflag(MAX_srce) = .false.
      return
      end
      
c************************************************************************
      subroutine tset (ix, iz, nt, x, y, z, u, v, e, p) 
c*'--- set arrays for temporary corners of present element

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
       kf = info_source(nt,1)
       amp = fnTamp(nt)

       x = xynode(1,ix,iz,kf) 
       y = xynode(2,ix,iz,kf)
       z = znode(iz,kf)
c       u = slip_n(ix,iz,kf,1)
c       v = slip_n(ix,iz,kf,2)
       u = slip_t(ix,iz,nt,1)
       v = slip_t(ix,iz,nt,2)
       p = amp*tphi(ix,iz,nt)
c       e = tphi_err(ix,iz,nt)
       e = 0.0d0

      return
      end 


c************************************************************************
c initialize tphi() for transients
c   tphi is the amount of slip at each node
c************************************************************************
      subroutine set_tphi(icall)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      logical pnr, pnr9, pnr12
      
      dimension polyX(200), polyW(200)
      
      pnr   = ( .not. parmsread )
      pnr12 = ( .not. parmpio(12) )
      pnr9  = ( .not. parmpio(9) )
      
      z = 0.0d0
      one = 1.0d0
      two = 2.0d0
      half = one/two
      
      call stfarea
      
      do nt = 1, MAX_srce
      
      kf   = info_source(nt,1)
      kq   = info_source(nt,2)
c      kfft = info_source(nt,3)
      kt   = info_source(nt,4)
      
      if ( sflag(nt) ) then

c** independent nodes
      if ( kq .le. 1 .and. icall.eq.0 ) then
       
c-- build source polygon      
       call makepoly (nt, polyX, polyW, np, 0)

         knode = 0
         do iz=1, nzf(kf)
          do ix=1, nxf(kf)
c            knode = ix + (iz-1) * nxf(kf)
            knode = knode+1
            n = NN_tr(nt,knode)
            ntphi(ix,iz,nt)= NN_tr(nt,knode)
c            print *, nt,iz,ix,knode,n,VN_tr(nt,n)
           if ( n.eq.0 ) then
              nodefree(ix,iz,nt) = 0
              tphi(ix,iz,nt) = zero
              tphi_err(ix,iz,nt) = zero
           else
              if (pnr9) tphi(ix,iz,nt) = VN_tr(nt,n)
              nodefree(ix,iz,nt) = 1
           endif

          enddo
         enddo
         endif

c* 1D Boxcar distribution Amplitude, min depth, max depth
        if ( kq.eq.3) then

         do ix = 1, nxf(kf)
          n1 = tnode_prof(nt,ix)

          if (n1.eq.0 ) then
           do j=1,3
            tf_parm(nt, ix, j) = z
           enddo
          endif
          
           G  = tf_parm(nt, ix, 1) 
           z1 = tf_parm(nt, ix, 2)
           z2 = tf_parm(nt, ix, 3)

           do iz = 1, nzf(kf)
            zn = znode(iz,kf)
            tphi(ix, iz, nt) = Boxcar(G, z1, z2, zn, 1, 1)
            nodefree(ix,iz,nt) = 1
           enddo
 
         enddo

        endif

c* 1D Gaussian distribution Amplitude, mean depth, sigma depth
        if ( kq.eq.4) then

         do ix = 1, nxf(kf)
          n1 = tnode_prof(nt,ix)

          if (n1.eq.0 ) then
           do j=1,3
            tf_parm(nt, ix, j) = z
           enddo
          endif
          
           G  = tf_parm(nt, ix, 1) 
           z1 = tf_parm(nt, ix, 2)
           z2 = tf_parm(nt, ix, 3)
           zsig = max(z2, 0.01d0)

           do iz = 1, nzf(kf)
            zn = znode(iz,kf)
            tphi(ix, iz, nt) = Gauss1D( G, z1, zsig, zn, one)
            nodefree(ix,iz,nt) = 1
           enddo
 
         enddo

        endif
        
        
c******************************************************
c* 2-D Gaussian distribution source
c* Xt0, Yt0 - mean point of Gaussian slip
c* Sx, Sw - standard deviations in kms
c* amp = peak amplitude
c
c* kf - fault number
c
c******************************************************
      if ( kq.eq.6 .or. kq.eq.7 .or. kq.eq.8) then
      
      Xt0 = transient(nt,1)
      Yt0 = transient(nt,2)
       
      call ll2xw (kf, Xt0, Yt0, Xt, Wt)
       
      Sx  = max(0.1,transient(nt,6))
      Sw  = max(0.1,transient(nt,4))
      amp = one
      saz = transient(nt,14) * d2r
      
c-- build source polygon      
      if(kq.eq.8) call makepoly (nt, polyX, polyW, np, 0)
       
      if ( amp.gt.zero ) then
       do ix=1,nxf(kf) 
        do iz = 1, nzf(kf)
        
c using X-W system
c        Xn = xwnode(1,ix,iz,kf)
c        Wn = xwnode(2,ix,iz,kf)
c        dx = (Xn - Xt)
c        dw = (Wn - Wt)
        
c  findme re-do calc of Xn Wn using distaz, xwnode is too distorted by curved slabs
        xpt = xynode(1,ix,iz,kf)
        ypt = xynode(2,ix,iz,kf)
c        zpt = znode(iz,kf)
c        call distkm (Xt0, Yt0, Xt0, ypt, dxN)
c        call distkm (Xt0, Yt0, xpt, Yt0, dxE)
        
      call delaz(ypt,xpt,Yt0,Xt0,delta,faz)
      dxE = delta*dsin(faz*d2r)*d2x
      dxN = delta*dcos(faz*d2r)*d2x

c get delta and azimuth from eplat,eplon to slat,slon


c rotate so x is along strike        
c        az = fault_strike(ix,iz,kf)*rpd
c        print *, kf, ix,iz,av_fault_strike(kf)
        faz = av_fault_strike(kf)*d2r
        dw =  dxE*dcos(faz) - dxN*dsin(faz)
        dx =  dxE*dsin(faz) + dxN*dcos(faz)

        tphi(ix,iz,nt) = 0.0d0

c-- 2D Gaussian        
      if (kq.eq.6) then
c-- X-axis is horizontal along strike, Y along W
c-- rotate clockwise around Z-axis 
       rdx =  dx*dcos(saz) - dw*dsin(saz)
       rdw =  dx*dsin(saz) + dw*dcos(saz)
       dx = rdx
       dw = rdw
       aa =  amp * exp(-half*(dx/Sx)**2) * exp(-half*(dw/Sw)**2)
       tphi(ix,iz,nt) = aa
      endif
      
c-- 2D boxcar source
      if (kq.eq.7) then
        dx = abs(dx/Sx)
        dw = abs(dw/Sw)
        rdx =  dx*dcos(saz) - dw*dsin(saz)
        rdw =  dx*dsin(saz) + dw*dcos(saz)
c        if ( (rdx .le. three) .and. (rdw .le. three) ) 
c     .    tphi(ix,iz,nt) = amp/four
c        if ( (rdx .le. two) .and. (rdw .le. two) ) 
c     .    tphi(ix,iz,nt) = amp/two
        if ( (rdx .le. one) .and. (rdw .le. one) ) 
     .    tphi(ix,iz,nt) = amp
      endif

c-- radial source
c-- taper phi downdwards towards the edge of the slip area so there is 
c      some sensitivity to moving the edge
      if (kq.eq.8) then
        call inside ( Xn, Wn, polyX, polyW, np, insde)
        if (insde .ne. 0) then 
        
        aznode = fn360(datan2( Wn-Wt, Xn-Xt ) * r2d)
        np = info_source(nt,6)
        daz = 360.0d0/real(np)
        k = int(aznode/daz)+1
         
c-- find distance of node to edge and node to center         
c       do k=1,np-1
c get intersection of line (Xt,Wt)-(Xn,Wn) with boundary of polygon  
       
      call intsct2lines (Xt, Wt, Xn, Wn, polyX(k), polyW(k), 
     .         polyX(k+1), polyW(k+1), u1, u2, X, W)
     
c-- dnode is distance of node to edge of polygon     
c-- dcen is distance of polygon center to edge of polygon   

         dcen  = dsqrt( (Xt-X)**2 + (Wt-W)**2 )
         dnode = dsqrt( (Xn-Xt)**2 + (Wn-Wt)**2 )
         
       f=0.1d0
       a = amp*( 1.0d0 - dnode * f/dcen)
c       a = amp
         
c       d0 = dsqrt( (Xn-Xt)**2 + (Wn-Wt)**2 )
c       a = amp*( one - d0/500.0d0 )
       
       tphi(ix,iz,nt) = a
          
        endif
      endif
      
      
       enddo
      enddo
      endif
      
      endif
      
      
      endif
      enddo
      
     
      return
      end
      
c************************************************************************
c intersection of 2 lines defined by endpoints
c************************************************************************
      subroutine intsct2lines (X1, Y1, X2, Y2, X3, Y3, X4, Y4, 
     .         ua, ub, X, Y)
      implicit real*8 (a-h,o-z)
      zero = 0.0d0
      
c (X,Y) is intersection of line (X1,Y1)-(X2,Y2) and line (X3,Y3)-(X4,Y4)       
c see http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
c if ua=0 lines are parallel
c if 0 <= ua or ub <= 1 the intersection point falls within this segment

       u =1.0d10
       ua=1.0d10
       ub=1.0d10
   
       dd = (Y4-Y3)*(X2-X1) - (X4-X3)*(Y2-Y1)
       ua = (X4-X3)*(Y1-Y3) - (Y4-Y3)*(X1-X3)
       ub = (X2-X1)*(Y1-Y3) - (Y2-Y1)*(X1-X3)
       if (dd.ne.zero) u = ua/dd
       X = X1 + u * (X2-X1)
       Y = Y1 + u * (Y2-Y1)
       if (dd.ne.zero) ua = ua/dd
       if (dd.ne.zero) ub = ub/dd
      return
      end
      
c************************************************************************
c build polygon source
c************************************************************************
      subroutine makefaultpoly (kf,polyf, np)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
      dimension polyf(1000,2)
      
c-- make polygon from perimeter of fault kf      
        np = 0
        nw1 = nzf(kf)
        nx1 = nxf(kf)
        
        do ix = 1, nxf(kf)
         np=np+1
         polyf(np,1) = xwnode(1, ix, 1, kf)
         polyf(np,2) = xwnode(2, ix, 1, kf)
        enddo
        
        do iz = 2, nzf(kf)
         np=np+1
         polyf(np,1) = xwnode(1, nx1, iz, kf)
         polyf(np,2) = xwnode(2, nx1, iz, kf)
        enddo
        
        do ix = nxf(kf)-1, 1, -1
         np=np+1
         polyf(np,1) = xwnode(1, ix, nw1, kf)
         polyf(np,2) = xwnode(2, ix, nw1, kf)
        enddo
        
        do iz = nw1-1, 1, -1
         np=np+1
         polyf(np,1) = xwnode(1, 1, iz, kf)
         polyf(np,2) = xwnode(2, 1, iz, kf)
        enddo
       
      return
      end       
       
c************************************************************************
c build polygon source
c************************************************************************
      subroutine makepoly (nt, pX, pW, np, kout)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      dimension pX(200), pW(200)
      character ic3*3
      
       kf  = info_source(nt,1)
       kq  = info_source(nt,2)
       kft = info_source(nt,3)
       np = 0
       
       if (kq.eq.9 .or. kq.eq.11) return

c-- outine grid on fault       
       if (kq.eq.1) then
        nx1 = kev_grid(nt,1)
        nx2 = kev_grid(nt,2)
        nw1 = kev_grid(nt,3)
        nw2 = kev_grid(nt,4)
        np=0
        do ix = nx1,nx2
         np=np+1
         pX(np) = xwnode(1, ix, nw1, kf)
         pW(np) = xwnode(2, ix, nw1, kf)
        enddo
        do iz = nw1,nw2
         np=np+1
         pX(np) = xwnode(1, nx2, iz, kf)
         pW(np) = xwnode(2, nx2, iz, kf)
        enddo
        do ix = nx2,nx1,-1
         np=np+1
         pX(np) = xwnode(1, ix, nw2, kf)
         pW(np) = xwnode(2, ix, nw2, kf)
        enddo
        do iz = nw2,nw1,-1
         np=np+1
         pX(np) = xwnode(1, nx1, iz, kf)
         pW(np) = xwnode(2, nx1, iz, kf)
        enddo
       
       endif
       
c-- 2D Gaussian source as ellipse      
       if ( kq.eq.6 ) then
       Xt0 = transient(nt,1)
       Yt0 = transient(nt,2)
       call ll2xw (kf, Xt0, Yt0, Xt, Wt)
        np = 50
        daz = 360.d0/real(np) * d2r
        raz = zero
        dX = two*transient(nt,6)
        dW = two*transient(nt,4)
        saz = -transient(nt,14) * d2r
         do k = 1, np
          pX0 = dX * dcos(raz)
          pW0 = dW * dsin(raz)
c-- rotate clockwise around Z-axis 
          pX1 =  pX0*dcos(saz) - pW0*dsin(saz)
          pW1 =  pX0*dsin(saz) + pW0*dcos(saz)
          pX(k) =  Xt + pX1 
          pW(k) =  Wt + pW1
          raz = raz + daz
         enddo
       endif
       
c-- polygon source       
       if ( kq.eq.8 ) then
       Xt0 = transient(nt,1)
       Yt0 = transient(nt,2)
       call ll2xw (kf, Xt0, Yt0, Xt, Wt)
        np  = info_source(nt,6)
        daz = 360.d0/real(np) * d2r
        raz = zero
         do k = 1, np
          pX(k) = Xt + rpoly(nt,k) * dcos(raz)
          pW(k) = Wt + rpoly(nt,k) * dsin(raz)
          raz = raz + daz
         enddo
       endif

c-- convert to lon,lat and output        
       if ( kout.gt.0) then
        call i2c(nt, 3, ic3)
        call fopen (k56, 1, '_src_'//ic3//'.pxy ')
        write(k56, '(2f12.4)') transient(nt,1), transient(nt,2)      
        ik = kfclose(k56)
        
        if ( np.gt.0) then
        call fopen (k56, 1, '_src_'//ic3//'.poly ')
        npc = np+1
         pX(npc) = pX(1)
         pW(npc) = pW(1)
        
         do k=1,npc
          call xw2ll ( kf, pX(k), pW(k), pln, plt )
          if (plt.gt.zero .and. pln.gt.zero) 
     .      write(k56, '(4f12.4)') pln, plt, pX(k), pW(k)
         enddo
         ik = kfclose(k56)
        endif
       endif
       
       return
       end

c************************************************************************
c rename block
c************************************************************************
      function block_rename (iname)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
       character block_rename*4, iname*4 
        block_rename = iname
        do k =1, n_b_renames  
         do j=2,25
          if ( b_rename(k,j).eq.iname ) then 
             block_rename = b_rename(k,1)
              print *, 'Renaming block ',iname, ' --> ', block_rename
          endif
         enddo
        enddo
      return
      end
     

c************************************************************************
c get block name, checking for errors
c************************************************************************
      function block_name (ks)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
       character c1*1, block_name*4, b*4, ltrs*64      
       logical nameok
       b = 'xxxx'

      ltrs = 
     .'ABCDEFGHIJKLMNOPQRSTUVWXYZabcedfghijklmnopqrstuvwxyz0123456789_ '
       nlet = 64

       if (ks.ge.1 .and. ks.le.MAX_block ) b = b_name(ks)
       if (b.eq.'    ') b = 'xxxx'
       if (b.eq.'' )    b = 'xxxx'

c-- see if valid name
       nameok = .true.
       do i=1,4
         c1 = b(i:i)
         k = index(ltrs,c1)
         if ( k.lt.1 .or. k.gt.nlet ) nameok = .false.
       enddo
       if ( .not. nameok ) then
         b = 'xxxx'
        endif

       block_name = b

      return
      end
c************************************************************************
c set time series amplitude for transients
c************************************************************************
      function fnTamp (ks)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      
       fnTamp = stf_area(ks)

c       if (ks.eq.MAX_srce) fnTamp =1.0d0
       
      return
      end

c************************************************************************
c initialize phi() for faults (interseismic)
c************************************************************************
      subroutine initialize_phi
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      logical dofault, pnr, pnr4, pnr5, flock
      
      z = zero
      i0 = 0
      
      do kf=1, nfault
        
        kfft = fault_fit_type(kf)
        if (all_fault_0 .or. all_fault_1 .or. all_fault_fix) kfft = 0
        dofault = flock(kf)

        pnr  = ( .not. parmsread )
        pnr5 = ( .not. from_pio(kf) )
        pnr4 = ( .not. from_pio(kf) )

      if (dofault) then

c** independent nodes
        if ( kfft .le. 1) then
         do iz=1, nzf(kf)
          do ix=1, nxf(kf)
            knode = ix + (iz-1)* nxf(kf)
            n = NN_in(kf,knode)
            nphi(ix,iz,kf)= n
c            print *, kf,ix,iz,n, VN_in(kf,n)
           if ( n.eq.0 ) then
            phi_free(ix,iz,kf) = .false.
            phi(ix,iz,kf) = zero
            phi_err(ix,iz,kf) = zero
           else
            if (pnr4) phi(ix,iz,kf) = VN_in(kf,n)
            phi_free(ix,iz,kf)= .false.
c if invert flag is set, let it go free
            if ( fflag(kf,3) ) phi_free(ix,iz,kf) = .true.
           endif

c* fix node value at zero if HW pole = FW pole
           khwp = npole_block(khw_blk(ix,iz,kf))
           kfwp = npole_block(kfw_blk(ix,iz,kf))
           if ( khwp.eq.kfwp ) then
             phi(ix,iz,kf) = zero
             phi_err(ix,iz,kf)= zero
             phi_free(ix,iz,kf)= .false.
           endif

c* nodes fixed by user, reset phi to value from control file
           do  j=1, MAX_nodes
            if(n.ne.0)then
             if ( node_fix(kf, j).eq.n) then
               phi_free(ix,iz,kf)= .false.
               phi(ix,iz,kf) = VN_in(kf,n)
               phi_err(ix,iz,kf) = zero
             endif
            endif
           enddo

c* re-set node values according to any flags, and fix them
c* do only for interseismic locking
           if ( all_fault_0 ) then
             phi(ix,iz,kf) = zero
             phi_err(ix,iz,kf) = zero
             phi_free(ix,iz,kf) = .false.
           endif

           if ( all_fault_1 ) then
             phi(ix,iz,kf) = one
             phi_err(ix,iz,kf) = zero
             phi_free(ix,iz,kf) = .false.
           endif

           if ( all_fault_fix ) then
             phi_free(ix,iz,kf) = .false.
           endif


          enddo
         enddo
        endif

c** downdip functions
        if ( kfft .ge. 2 .and. kfft .le. 6) then
         call clearnodes(kf)

         do ix = 1, nxf(kf)
          n1 = node_prof(kf,ix)

          if (n1.eq.0 ) then
           f_parm(kf, ix, 1) = zero
           f_parm(kf, ix, 2) = zero
           f_parm(kf, ix, 3) = zero
          endif

         if ( n1.gt.0) then
          if (pnr5) then
           f_parm(kf, ix, 1) = win(kf,n1,1)
           f_parm(kf, ix, 2) = win(kf,n1,2)
           f_parm(kf, ix, 3) = win(kf,n1,3)
          else
           win(kf,n1,1) = f_parm(kf, ix, 1)
           win(kf,n1,2) = f_parm(kf, ix, 2)  
           win(kf,n1,3) = f_parm(kf, ix, 3)  
          endif

          G  = win(kf,n1,1) 
          z1 = win(kf,n1,2) 
          z2 = win(kf,n1,3)
          dz = max( (z2-z1), 0.5d0)
          phi_free(ix,1,kf) = .true.

          do iz = 1, nzf(kf)
            zn = znode(iz,kf)

c* Wang interseismic
           if (kfft.eq.2) phi(ix, iz, kf) = Wang( G, z1, z2, zn)

c* boxcar 
           if (kfft.eq.3) phi(ix, iz, kf) = Boxcar(G, z1,z2,zn, i0, i0) 

c* Gaussian distribution Amplitude, mean depth, sigma depth
           if (kfft.eq.4) 
     .         phi(ix, iz, kf) = Gauss1D( G, z1, z2, zn, Gskew(kf) )

c* cosine taper distribution top depth, bottom depth
           if (kfft.eq.5) then
              dz = max(z2-z1, 0.1d0)
              dz2 = zn - z1
              s = 0.5d0 * ( dcos ( pii* dz2/dz ) +1.0d0 )
              if ( zn .le. z1 ) s = 0.0d0
              if ( zn .ge. z2 ) s = 1.0d0
              phi(ix, iz, kf) = s
           endif

c* HalfGaussian distribution Amplitude, mean depth, sigma depth
           if (kfft.eq.6)
     .        phi(ix, iz, kf) = HalfGauss1D( G, z1, z2, zn)

          enddo
 
          endif
         enddo

        endif


       endif
      enddo
      
      return
      end
      
      
c***********************************************************************
      subroutine julday(imode, yr, imo, ida, jday) 

c imode = 0 convert YYYY MM DD to Julian Day      
c imode = 1 convert Julian Day to YYYY MM DD    
c imode = 2 convert YYYY.YYYY to Julian Day      
c imode = 3 convert YYYY.YYYY to YYYY MM DD   
c imode = 4 convert YYYY MM DD to YYYY.YYYY     
  
      implicit real*8 (a-h,o-z)
      
      dimension kday(12), m1day(12)
      logical leapyear

      data kday /00,31,28,31,30,31,30,31,31,30,31,30/
      k1952 = 1952
      dpy = 365.0d0
      iyr = int(yr)
      leapyear = ( mod (iyr-k1952, 4).eq.0 )
      if (leapyear) kday(3) = 29
      if (leapyear) dpy = 366.0d0
      
c     julian day of first day of each month
      m1day(1) = 1
      do i=2,12
       m1day(i) = m1day(i-1) + kday(i)
      enddo
      
      if (imode.eq.0 .or. imode.eq.4 ) then
       kd=0
       do id=1, imo 
        kd = kd + kday(id)
       enddo
       kd = kd + ida 
       jday = kd
       if(imode.eq.4) yr = int(yr)+real(jday)/dpy
      
      elseif ( imode.eq.1) then
       do i=1,12
        if(jday .ge. m1day(i)) then
         imo = i
         ida = jday - m1day(i) + 1
        endif
       enddo
      
      elseif ( imode.eq.2) then
       dday = yr - real(iyr)
       jday = int(dday*dpy)+1
      
      elseif ( imode.eq.3) then
       dday = yr - real(iyr)
       jday = int(dday*dpy)+1
       do j=1,12
        if(jday .ge. m1day(j)) then
         imo = j
         ida = jday - m1day(j) + 1
        endif
       enddo
       
      endif
      
      return
      end
       
c***********************************************************************
C SUBROUTINE PNPOLY 
C 
C PURPOSE 
C TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON 
C 
C USAGE 
C CALL PNPOLY (PX, PY, XX, YY, N, INOUT ) 
C 
C DESCRIPTION OF THE PARAMETERS 
C PX - X-COORDINATE OF POINT IN QUESTION. 
C PY - Y-COORDINATE OF POINT IN QUESTION. 
C XX - N LONG VECTOR CONTAINING X-COORDINATES OF VERTICES OF POLYGON. 
C YY - N LONG VECTOR CONTAING Y-COORDINATES OF VERTICES OF POLYGON. 
C N - NUMBER OF VERTICES IN THE POLYGON. 
C INOUT - THE SIGNAL RETURNED: 
C -1 IF THE POINT IS OUTSIDE OF THE POLYGON, 
C  0 IF THE POINT IS ON AN EDGE OR AT A VERTEX, 
C  1 IF THE POINT IS INSIDE OF THE POLYGON. 
C 
C REMARKS 
C THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE. 
C THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY 
C OPTIONALLY BE INCREASED BY 1. 
C THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING 
C OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX 
C OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING 
C N, THESE FIRST VERTICES MUST BE COUNTED TWICE. 
C INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED. 
C THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM 
C WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70. 
C 
C SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED 
C NONE 
C 
C METHOD 
C A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT 
C CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE POINT IS INSIDE OF THE POLYGON. 
C 
C .................................................................. 
C 
      SUBROUTINE PNPOLY(PX,PY,XX,YY,N,INOUT) 
      implicit real*8 (a-h,o-z)
      dimension X(500),Y(500),XX(N),YY(N) 
      LOGICAL MX,MY,NX,NY 
      
c      INTEGER O 
c OUTPUT UNIT FOR PRINTED MESSAGES 
c      DATA O/6/ 
c      MAXDIM=200 
c      IF(N.LE.MAXDIM)GO TO 6 
c       WRITE(O,7) 
c 7    FORMAT('0WARNING:',I5,' TOO GREAT FOR THIS VERSION OF PNPOLY. 
c     1  RESULTS INVALID') 
c      RETURN 

      DO 1 I=1,N 
      X(I)=XX(I)-PX
 1    Y(I)=YY(I)-PY 
 
      INOUT=-1 
      DO 2 I=1,N 
      J=1+MOD(I,N) 
      MX=X(I).GE.0.0 
      NX=X(J).GE.0.0 
      MY=Y(I).GE.0.0 
      NY=Y(J).GE.0.0 
      IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) GO TO 2 
      IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) GO TO 3 
      INOUT=-INOUT 
      GO TO 2

 3    x0 = (Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))
      if (x0.lt.0.0d0) goto 2
      if (x0.eq.0.0d0) goto 4
      if (x0.gt.0.0d0) goto 5

c 3      IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5

 4    INOUT=0 
      RETURN
 5    INOUT=-INOUT
 2    CONTINUE
      RETURN
      END 
      
c***********************************************************************
      subroutine inside ( x0, y0, px, py, n, insde) 
 
c check if point X0,Y0 inside polygon PX,PY
c INSDE = 0 if point outside
c       = +/-1 if inside
c       = 2 if on perimeter

      implicit real*8 (a-h,o-z)
      dimension px(n), py(n)
      
      
      insde = 0
      
      call PNPOLY(X0,Y0,PX,PY,N,INOUT1) 
      
C INOUT1 - THE SIGNAL RETURNED: 
C -1 IF THE POINT IS OUTSIDE OF THE POLYGON, 
C  0 IF THE POINT IS ON AN EDGE OR AT A VERTEX, 
C  1 IF THE POINT IS INSIDE OF THE POLYGON. 

c change to
c INSDE = 0 if point outside
c       = 1 if inside
c       = 2 if on perimeter

      if(inout1.eq.-1) insde = 0
      if(inout1.eq.0)  insde = 2
      if(inout1.eq.1)  insde = 1
      
      return
      end
      
c***********************************************************************
c- generate polygons from line segments

      subroutine wrf_polygon (kfail)
      implicit real*8 (a-h,o-z)
      
c LMB system() returns INTEGER status
      integer   system, status
      
      include "tdefcom1.h"
      
      parameter (MAX_seg = 6000, MAX_vert = 10 )
      
      character*2 c2    
      character*4 bname(MAX_block), block_name, c4  
      character*80 fnout
      
      dimension seg(MAX_seg,2,2), Cx(MAX_block,2) 
      dimension nbfound(MAX_block) 
      dimension v(MAX_seg,2), vangle(MAX_seg,MAX_vert)
      dimension nav(MAX_seg,MAX_vert), Nnav(MAX_seg), Ncount(MAX_seg)
      dimension px(MAX_corner), py(MAX_corner), nc(MAX_block),
     .          bxy(MAX_corner,MAX_block,2), area0(MAX_block)
     
      logical segflag(MAX_seg), near, used(MAX_seg,MAX_vert), 
     .        wr, bfound, kfail, btf(MAX_block), ptf(MAX_block),
     .        duplicates
      
      call clearint(nav, MAX_seg*MAX_vert)
      call clearlog(btf, MAX_block)
      
      kfail = .false.
      duplicates = .false.

      zero = 0.0d0
      np = 0
      ifound = 0

      print *, 'Making blocks '

c-- build segments from faults
      ns = 0
      do kf=1,MAX_f
         if (fflag(kf,4)) then
c          print *, fault_name(kf)
          do 7 ix=1,nxf(kf)-1
           ns=ns+1
           if (ns.gt.MAX_seg ) then
             print *, 'MAX_seg exceeded in sub wrf_polygon'
            call stop1
           endif
           seg(ns,1,1) = xynode(1,ix,1,kf)
           seg(ns,1,2) = xynode(2,ix,1,kf)
           seg(ns,2,1) = xynode(1,ix+1,1,kf)
           seg(ns,2,2) = xynode(2,ix+1,1,kf)
           segflag(ns) = .true.
           
           
c-- check for same pt at both ends of this segment      
       if  
     . (near(seg(ns,1,1), seg(ns,1,2), seg(ns,2,1), seg(ns,2,2)) )
     . then
        write (*,100)  kf,ix,ix+1,seg(ns,1,1), seg(ns,1,2)
        ns=ns-1
        duplicates = .true.
       goto 7
       endif
  100  format ("Removing duplicate point on Fault",i4," X",2i4, 2f9.4) 
       
c-- check for duplicate segments
      if ( ns.gt.1) then
       do ii =1,ns-1
        if ( 
     .    (near(seg(ii,1,1), seg(ii,1,2), seg(ns,1,1), seg(ns,1,2))
     .    .and.
     .    near(seg(ii,2,1), seg(ii,2,2), seg(ns,2,1), seg(ns,2,2)))
     .    .or.
     .    (near(seg(ii,2,1), seg(ii,2,2), seg(ns,1,1), seg(ns,1,2))
     .    .and.
     .    near(seg(ii,1,1), seg(ii,1,2), seg(ns,2,1), seg(ns,2,2)))
     .    ) then
          ns = ns -1
        write (*,101)  kf,ix,ix+1, 
     .      seg(ii,1,1), seg(ii,1,2), seg(ns,1,1), seg(ns,1,2)
        duplicates = .true.
          goto 7
        endif
       enddo
      endif        
           
  101 format ("Removing duplicate segment on Fault",i4," X",2i4,4f9.4)
    7      continue
         endif
       enddo
       nsegs = ns

       if (duplicates) then
        print *, '*** Duplicate fault points or segments found, it is '
        print *, '    advised to fix these in the control file '
       endif
       
c-- reflect each segment        
      do i=1,nsegs
        ns = ns+1
         do 9 k=1,2
          do 9 j=1,2
  9        seg(ns,j,k)=seg(i,j,k)
  
        call swap( seg(ns,1,1), seg(ns,2,1) )
        call swap( seg(ns,1,2), seg(ns,2,2) )
        segflag(ns) = .true.
      enddo
      
      nsegs = ns
      
c-- sort the file segs1.tmp and put in segs2.tmp, then read back in
      call fopen (k15, 1, '_segs1.tmp ')
      do i=1,nsegs
       write(k15,'(4f12.4)') ((seg(i,l,k), k=1,2), l=1,2)
      enddo
      ik = kfclose(k15)

       status = system('sort -n '//fnout('_segs1.tmp ')//' > '//
     .    fnout('_segs2.tmp ')//char(0) )
     

      call fopen (k15, 1, '_segs2.tmp ')
      do i=1,nsegs
       read(k15,'(4f12.4)') ((seg(i,l,k), k=1,2), l=1,2)
      enddo
      ik = kfclose(k15)
      print *, 'Number of fault segments ', nsegs
      
      
c-- get vertices, put in v(), 
c       nav() has the vertices connected to this one
c       nnav() has the # of vertices connected to this one     
      Vx0 = 1.0d6
      Vy0 = 1.0d6
      nv = 0
       do j=1,nsegs
         if (seg(j,1,1) .ne. Vx0 .or. seg(j,1,2) .ne. Vy0) then
           nv = nv+1
           v(nv,1) = seg(j,1,1)
           v(nv,2) = seg(j,1,2)
           Vx0 = seg(j,1,1)
           Vy0 = seg(j,1,2)
           np = 1
           nav(nv,np) = j
         else
           np = np+1
           if (np.gt.MAX_vert ) then
             print *, 'MAX_vert exceeded in sub wrf_polygon'
            call stop1
           endif
           nav(nv,np) = j
           nnav(nv) = np
         endif
       enddo

c-- write out vertices and number of connections
      call fopen (k15, 1, '_segs3.tmp ')
       do i=1,nv
        write(k15, '(i5, 2f10.4, 11i5)') i,v(i,1),v(i,2), 
     .     nnav(i), (nav(i,j), j=1,nnav(i))
       enddo
      ik = kfclose(k15)

c-- convert segment numbers to vertice number for navigation table
      do i=1,nv
       do k=1,nnav(i)
        n = nav(i,k)
        x2 = seg(n,2,1)
        y2 = seg(n,2,2)
        do j=1,nv
         if ( near (x2, y2, v(j,1), v(j,2))) nav(i,k) = j
        enddo
       enddo
      enddo
       
c-- sort vertices by angle
      do i=1,nv
      
       x1 = v(i,1)
       y1 = v(i,2)
       nv0 = 0
       
       do k=1,nnav(i)
        n = nav(i,k)
        if ( n.gt.0) then
          nv0 = nv0+1
          x2 = v(n,1)
          y2 = v(n,2)
          call angle2 ( x1, y1, x2, y2, aa)
          vangle (i,k) = aa
        endif
       enddo
       
       do k=1,nv0
        do m=1,nv0
         if ( vangle(i,k).lt.vangle(i,m) ) then
          call swap (vangle(i,k), vangle(i,m) ) 
          call iswap (nav(i,k), nav(i,m) )
         endif
        enddo
       enddo
       
      enddo

c     Navigation table
      do i=1, nv
       do k=1, Nnav(i)
        used(i,k) = .false.
       enddo
       if ( Nnav(i) .le. 1 ) then
        kfail = .true.
        write(*,'("Fix unmatched segment", i3, " at", 2f10.3)') 
     .   i, (v(i,k), k=1,2)
        Nnav(i) = 0
       endif
       Ncount(i) = Nnav(i)
      enddo
      
      if (kfail) return
         
      wr = .true.
      if (wr) call fopen (k11, 1, '_blocks.tmp ')

c-- start Navigation algorithm     

      M = nv 
      Ipivot = 1
      nb = 0
  
  23  if (Ncount(Ipivot).eq.0) then
       Ipivot = Ipivot+1
       if( Ipivot.gt.M ) goto 999
       goto 23
      endif
      
      Irow = Ipivot
      do i=1, Nnav(Irow)
c       print *, Irow, Nav(Irow, i), used(Irow, i)
       if ( .not. used(Irow, i) ) then
         Ivertex = Nav(Irow, i)
         Iused = i
         goto 24
       endif
      enddo
      
      goto 23
      
  24  nb=nb+1
  
      nb2 = nb
      
      if (wr) then 
        write (k11, '(a2,i3)') '> ', nb2
        write (k11, 11) v(Ipivot,1), v(Ipivot,2)
        nc(nb2) = 1
        bxy(nc(nb2),nb2,1) = v(Ipivot,1)
        bxy(nc(nb2),nb2,2) = v(Ipivot,2)
      endif

      do 77 kk = 1,MAX_seg
       if (wr) then
         write (k11, 11) v(Ivertex,1), v(Ivertex,2)
         nc(nb2) = nc(nb2) + 1
         bxy(nc(nb2),nb2,1) = v(Ivertex,1)
         bxy(nc(nb2),nb2,2) = v(Ivertex,2)
       endif

        used(Irow, Iused) = .true.
        Ncount(Irow) = Ncount(Irow) -1
        
        if ( Ivertex.eq.Ipivot) goto 23
        
        IPrevRow = Irow
        Irow = Ivertex
        
        do i=1, Nnav(Irow)
         if ( nav(Irow,i).eq.IPrevRow ) then
           ip1 = i+1
           if ( i.eq.Nnav(Irow) ) ip1 = 1
           Ivertex = nav(Irow,ip1)
           Iused = ip1
           goto 77
         endif
        enddo
        
       
  77    continue
      
          
 999    if (wr) ik = kfclose(k11)
 
 
  11  format(2f10.4)
  
      print *, 'Number of polygons found = ', nb2

c-- one block is the entire perimeter, don't save it 
c-- find perimeter block (one with largest area) and remove
      armax=0.0d0
      nbmax=0 
      do i=1,nb2
       ptf(i) = .true.
       n = nc(i)
       do j=1, n
        px(j) = bxy(j,i,1)
        py(j) = bxy(j,i,2)
       enddo
       call centroid ( n, px, py, cxx, cyy, area)
       area0(i) = area
c       write (*,'("Block ",i4," area ",f10.5," x10^6 sq km")') 
c     .      i, area/1.0d6
      if (area.gt.armax) then
         armax=area
         nbmax = i
       endif
      enddo
c      print *, 'Biggest block ', nbmax, armax
      if(nbmax.gt.0) ptf(nbmax) = .false.
         
    
c-- get block names and assign numbers
      num_unnamed = 0
      

      do 333 i=1,nb2
       if(ptf(i)) then

       bname(i) = '    '
       avx=zero
       avy=zero
       bfound = .false.
       ncorn = nc(i)
      
       do j=1, ncorn
        px(j) = bxy(j,i,1)
        py(j) = bxy(j,i,2)
        avx = avx + px(j)
        avy = avy + py(j)
       enddo
       
c       print *, 'Block ', i, ' ncorn=', ncorn
       
       Cx(i,1)=avx/real(ncorn)
       Cx(i,2)=avy/real(ncorn)
       nbfound(i)=0

c-- what block points are within this block 
c-- block name and number taken from BC: option     
      do k=1,MAX_block
       x0 = block_centroid(k,1)
       y0 = block_centroid(k,2)
c       print *, k, x0, y0

       if ( x0 .ne. zero .or. y0 .ne. zero ) then

        call inside ( x0, y0, px, py, ncorn, insde) 
         if (insde.eq.1) then
          block_area(k) = area0(i)
          write(*, '("Block ",i4," is ",a4," #Corners = ",i4, 
     .               " Area = ", f12.6," x10^6 sq km")') 
     .         k, block_name(k), ncorn, block_area(k)/1.0d6

          if(ncorn.gt.MAX_corner) then
            print *, 'MAX corners in block exceeded'
           call stop1
          endif

          nc_block(k) = nc(i)
          nbfound(i) = nbfound(i) + 1
          ifound = k
          block_flag(k) = .true.

           do j=1,ncorn
            blockxy(j,k,1) = bxy(j,i,1)
            blockxy(j,k,2) = bxy(j,i,2)
           enddo

           goto 333
         endif
       endif  
      enddo
      
       if (nbfound(i).eq.0 ) then
          write( *, 
     .    '("Block no",i4, " NC=",i4," centroid x,y ",2f8.3,
     .      " name not found ")')
     .    i, ncorn, fnlong(cx(i,1)), cx(i,2)

c assign a name
          num_unnamed = num_unnamed + 1
          call i2c( num_unnamed,2, c2)
          c4 = 'UB'//c2
          print *, 'Assigning name ', c4
          nblocks = nblocks + 1
          jb=nblocks
          block_centroid(jb,1) = fnlong(cx(i,1))
          block_centroid(jb,2) = cx(i,2)
          b_name(jb) = c4
          npole_block(jb)=0
          nstrain_block(jb)=0
          block_flag(jb) = .true.
          write(*, '("Block ",i4," is ",a4," #Corners = ",i4, 
     .               " Area = ", f12.6," x10^6 sq km")') 
     .         i, block_name(ifound), ncorn, block_area(ifound)/1.0d6
       endif

       if (nbfound(i).eq.1 ) then 
          block_area(ifound) = area0(i)

          write(*, '("Block ",i4," is ",a4," #Corners = ",i4, 
     .               " Area = ", f12.6," x10^6 sq km")') 
     .         i, block_name(ifound), ncorn, block_area(ifound)/1.0d6

c         write( *, 
c     .    '("Block no",i4, " NC ",i4, " name ", a4)')
c     .    i, ncorn,block_name(ifound)

          btf(ifound) = .true. 
       endif
     
       if (nbfound(i).gt.1 ) then 
        write( *, 
     .    '("Block no",i4, " NC ",i4, " multiple names found ")')
     .    i, ncorn
       endif
        
      endif
 333  continue
      
      call fopen (k11, 1, '_blocks_lost.tmp ')
       do i=1,nb2
        if(ptf(i) .and. nbfound(i).eq.0 ) then
          write(k11, '(a1,i3)') '>', i
           do j=1,nc(i)
            write(k11, '(2f10.4)') (bxy(j,i,k),k=1,2)
           enddo
        endif
       enddo
       ik = kfclose(k11)
       
      
      return
      end
c***********************************************************************
      subroutine angle2 ( x1, y1, x2, y2, aa)
      implicit real*8 (a-h,o-z)
      
c-- get angle of the line (x1,y1)-(x2,y2)
      dpr = 180.0d0/3.1415926d0
      one = 1.0d0
      
      xx1 = x2-x1
      yy1 = y2-y1
      
c-- angle of line X1-X2      
      aa = -datan2( xx1, yy1 )*dpr + 0.1 +90.0
      if (aa.lt.0.0d0) aa=aa+360.0d0
      if (aa.gt.360.0d0) aa=aa-360.0d0
      
      return
      end
c***********************************************************************

      function near (x1, y1, x2, y2)
      implicit real*8 (a-h,o-z)
      logical near
      
      tol = 0.001d0
      
      near = .false.
      
      dx = dsqrt ( (x2-x1)**2 + (y2-y1)**2)
      
      near = (dx .le. tol ) 
      
      return
      end
      
c***********************************************************************
      subroutine gettparm(imode, c2, np, ip)
      
c  get parameter type np from the 2-letter parameter code c2, for the EX: option
c  ip is its index in the transient and tminmax arrays
c
c  imode = 1 get np and ip from c2
c        = 2 get np from ip
c        = 3 get ip from np
c
c -- in transient(ip) array
c            ip np  c2  parameter
c             1 21 'ln' longitude
c             2 22 'lt' latitude
c             3 23 'zh' depth
c             4 24 'd1' down-dip or 2D Gaussian W-width (formerly ww)
c             5 25 'am' amplitude
c             6 26 'd2' along-strike or 2D Gaussian X-width (formerly xw)
c             7 27 'to' origin year
c             8 28 'tc' time constant
c             9 29 'xr' migration rate in X (formerly mr)
c            10 30 'wr' migration rate in W (formerly ma)
c            11 31 'st' strike
c            12 32 'dp' dip
c            13 33 'rk' rake or slip azimuth
c            14 34 'az' azimuth of 2D Gaussian X-width

c -- for flags/constraints only
c            15 35 'rd' polygon radii
c            16 36 'ta' tau amplitude
c            17 37 'ga' 1D Gaussian amplitude
c            18 38 'gm' 1D Gaussian mean
c            19 39 'gs' 1D Gaussian spread
c            20 40 'ba' 1D Boxcar amplitude
c            21 41 'b1' 1D Boxcar Z1
c            22 42 'b2' 1D Boxcar Z2
c            30 50 'mo' moment

      implicit real*8 (a-h,o-z)

      character ptype(50)*15, p2codes(50)*2
      common /ptype1/ ptype, p2codes
      
      character*2 c2
      ninp = 50

c -get np and ip for the input code      
       if (imode.eq.1) then
        call lcase(c2,2)
        if ( c2.eq.'ww') c2 = 'd1'
        if ( c2.eq.'xw') c2 = 'd2'
        if ( c2.eq.'mr') c2 = 'xr'
        if ( c2.eq.'ma') c2 = 'wr'

        np = 0
        ip = 0
     
        do i=1,ninp
         if ( p2codes(i).eq.c2 ) then
          np = i
          ip = i-20
         endif
        enddo
          if(ip.le.0) print *, '***** Code ',c2, ' not found '
        

c-- get np and code for input ip
       elseif (imode.eq.2) then
          np = ip+20
          c2 = p2codes(np)
          
c-- get ip and code for input np
       elseif (imode.eq.3) then
          ip = np-20
          c2 = p2codes(np)
        
       endif
       
      return
      end
c**********************************************************************
      subroutine atransb(a,b,c,nr1,nc1,nc2)
c
c...  subroutine to form the matrix product A(transpose) * B = C
c
      implicit real*8 (a-h,o-z)
      real*8 a(nr1,nc1),b(nr1,nc2),c(nc1,nc2)
      zero=0.0d0
      call fill(c,zero,nc1*nc2)
      do i=1,nc1
        do j=1,nc2
          do k=1,nr1
            c(i,j)=c(i,j)+a(k,i)*b(k,j)
          enddo
        enddo
      enddo
      return
      end


c***********************************************************************
      subroutine atransb3(a,b,c, nr1,nr1p, nc1,nc1p, nc2,nc2p)
c
c...  subroutine to form the matrix product A(transpose) * B = C
c
c dimensions with p at end are physical dimensions of arays
      real*8 a(nr1p,nc1p),b(nr1p,nc2p),c(nc1p,nc2p),zero

      zero=0.0d0
      call fill(c,zero,nc1p*nc2p)
      do i=1,nc1
        do j=1,nc2
          do k=1,nr1
            c(i,j)=c(i,j)+a(k,i)*b(k,j)
          enddo
        enddo
      enddo
      return
      end

c**********************************************************************
      subroutine amultb(a,b,c,nr1,nc1,nc2)
c
c...  subroutine to form the matrix product A * B = C
c
      implicit real*8 (a-h,o-z)

      real*8 a(nr1,nc1),b(nc1,nc2),c(nr1,nc2)

      zero=0.0d0
      call fill(c,zero,nr1*nc2)
      do i=1,nr1
        do j=1,nc2
          do k=1,nc1
            c(i,j)=c(i,j)+a(i,k)*b(k,j)
          enddo
        enddo
      enddo
      return
      end
c**********************************************************************
      subroutine amultb3(a,b,c, nr1,nr1p, nc1,nc1p, nc2,nc2p)
c
c...  subroutine to form the matrix product A * B = C
c
c dimensions with p at end are physical dimensions of arays
      real*8 a(nr1p,nc1p),b(nc1p,nc2p),c(nr1p,nc2p), zero

      zero=0.0d0
      call fill(c,zero,nr1p*nc2p)
      do i=1,nr1
        do j=1,nc2
          do k=1,nc1
            c(i,j)=c(i,j)+a(i,k)*b(k,j)
          enddo
        enddo
      enddo
      return
      end
      
c***********************************************************************
      subroutine linsolve (Ndat, Md, Npar, Mp, A, O, Sig, 
     .    pm, pmerr)

c solve set of weighted linear equations P = (AtWA)inv AtWO
c Md is physical dimensions number of data
c Ndat number of rows
c Mp physical dimension for parameters
c Npar number of columns (parameters)
c A is derivative matrix
c O is observation matrix
c Sig is sigma matrix
c Pm is parameter matrix
c Pmerr is parameter uncertainties

      implicit real*8 (a-h,o-z)

      dimension A(Md,Mp), O(Md), sig(Md) 
      dimension pm(Mp), pmerr(Mp)

c-- temporary arrays for building matrices
      dimension AW(Md,Mp), AtW(Mp,Md), AtWA(Mp,Mp), AtWO(Mp,1), 
     .  P(Mp,1), b1(Md,1), b2(Mp,1) 
     
      M2 = Md
      Nd = Ndat
      kone = 1
      one = 1.0d0
      
      call cleareal(AtW,  Md*Mp)
      call cleareal(AtWA, Mp*Mp)
      call cleareal(AtWO, Mp)
      call cleareal(b1, Md)
      call cleareal(b2, Mp)
      call cleareal(p,  Mp)
      call cleareal(pm,  Mp)
      call cleareal(pmerr,  Mp)

      if (Ndat.lt.2 ) return
      
c make weight from sigmas
      do i=1, Ndat
        s=sig(i)
        w = 1.0d-5
        if(s.ne.0.0) W = 1.0d0/(s*s)
c        print *, W
        do j=1,Npar
         AW(i,j) = A(i,j)*W
        enddo
      enddo

* make AtW
      call atrans3(AW, AtW, ndat,Md, npar,Mp )

* make AtWA
      call amultb3(AtW, A, AtWA, npar,Mp, ndat, Md, npar,Mp)
      
* make AtWO
       call amultb3(AtW,O,AtWO, npar,Mp, ndat,Md, kone,kone)
       
* invert AtWA
      call gaussj3(AtWA, npar, Mp, b2, kone, kone)
      
* get solution, P = inv(AtWA)* AtWO
      call amultb3(AtWA, AtWO, P, npar,Mp, npar,Mp, kone,kone)
       
      do i=1,npar
       pm(i) = p(i,1)
       pmerr(i) = dsqrt( AtWA(i,i) )
      enddo

      return
      end
      
c***********************************************************************

      subroutine atrans3(a, c, nr1,nr1p, nc1,nc1p)
c
c...  subroutine to form the matrix C = A(transpose)
c
c dimensions with p at end are physical dimensions of arrays
      real*8 a(nr1p,nc1p), c(nc1p,nr1p) 

       do i=1,nc1
        do j=1,nr1
          c(i,j)=a(j,i)
        enddo
       enddo
      return
      end


c***********************************************************************
      function fsine(tt, A, B, C, D)
c return the seasonal signals at time tt given the amplitudes

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"
      twopii = 2.0d0*pii
      fourpii = 4.0d0*pii

       fsine =  A * dsin (twopii*tt )  + B * dcos (twopii*tt )
     .        + C * dsin (fourpii*tt ) + D * dcos (fourpii*tt )

       return
       end


c**********************************************************************      

      subroutine fitsine (iX, iV, ic, jc, fail )

c** fits offset, linear, annual and semiannual sine function to time series
c-- f(t) = Xo + Vt + A * sine (2*pi*t)+ B * cosine (2*pi*t) + C * sine (4*pi*t)+ D * cosine (4*pi*t) 
c-- solve for Xo, V, A, B, C and D
c-- ic is site, jc is component, iX, iV are flags

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

c-- temporary arrays for building matrices
      parameter (Npmax = 16, MAX_ii = 10000)
      
      dimension r(MAX_ii), s(MAX_ii), aa(MAX_ii,npmax), 
     .          pp(npmax), pper(npmax)
      dimension tbreak1(MAX_tsegs,2)
      dimension ipar(Npmax)

      logical fail
      
      twopii = 2.0d0*pii
      fourpii = 4.0d0*pii
      
c      NN = MAX_ii*npmax               ! 11-13-15
c      call cleareal(aa,NN)c
c      call cleareal(r,MAX_i)
c      call cleareal(s,MAX_ii)
c      call cleareal(pp,Npmax)
c      call clearint(ipar,Npmax)
      
      Xo = 0.0d0
      V  = 0.0d0
      Verr  = 0.0d0
      A  = 0.0d0
      B  = 0.0d0
      C  = 0.0d0
      D  = 0.0d0

     
cThe offset flags (Eo No Uo) control what to do with the offsets and seasonal signals for the 3 components 
c  iX = 0 don't use this component 
c       1 fix at current value
c       2 solve for offset by regression
c       3 solve for offset and one periodic signal (yearly period) 
c       4 solve for offset and two periodic signals (yearly and 6-month periods) 
        
cThe slope flags (Ev Nv Uv):
c  iV = 0 don't use slope (set slope to 0)
c       1 fix at current value
c       2 solve for slope by regression
c       3 use slope (velocity) from block model

c-- data range      
      n1 = ndx(ic,1)
      n2 = ndx(ic,2)
      nn = n2-n1+1
      dt = t_disp(n2)-t_disp(n1)

c place break at start of time series
      nbreak = 1
      tbreak1(nbreak,1) = t_disp(n1)
      tbreak1(nbreak,2) = 0.0d0

c any more offsets for this site?
      if ( num_segs.gt.0 )  then
        do i=1,num_segs
          if ( jc.eq.i_segs(i,1) ) then
           if(site_segs(i,1).eq.gps_name(ic) .and. 
     .        site_segs(i,2).eq.gps_fname(gps_index(ic)) ) then
             do n=1,i_segs(i,2)
              tbreak  = t_segs(i,n,1)
              dtbreak = t_segs(i,n,2)
c              print *, tbreak, t_disp(n1), t_disp(n2)
              if ( tbreak.gt.t_disp(n1) .and. tbreak.lt.t_disp(n2))
     .        then 
c             print *, ic,tbreak
              nbreak = nbreak+1
              tbreak1(nbreak,1) = tbreak
              tbreak1(nbreak,2) = dtbreak
              endif
             enddo
           endif
          endif
        enddo
      endif

c count parameters to estimate
      npar = 0

c offsets
      if (iX .ge. 2 ) then 
         npar = npar+1
         ipar(npar) = 1
      endif

c slope
      if (iV.eq.2 ) then 
         npar = npar+1
         ipar(npar) = 2
      endif

c yearly seasonal
      if (iX.eq.3 ) then 
         npar = npar+2
         ipar(npar-1) = 3
         ipar(npar)   = 4
      endif

c both seasonal
      if (iX.eq.4 ) then 
         npar = npar+4
         ipar(npar-3) = 3
         ipar(npar-2) = 4
         ipar(npar-1) = 5
         ipar(npar)   = 6
      endif

c     npar_no_segs = npar


c breaks
c      if(iX .ge. 2 ) then
c        do i=1,nbreak
c          npar=npar + 1
c          ipar(npar) = 1
c        enddo
c     endif


c-- no or insufficient data      
      if ( npar.eq.0 .or. n1.le.0 .or. nn.lt.npar ) then
        fail = .true.
        return
      endif
      
        
c-- estimate new constants
    
      ni = 0
      do i = n1, n2
        ni = ni+1
        tt = t_disp(i)
        r(ni) = x_disp(i,jc) - x_calc(i,jc) 
        s(ni) = x_sig(i,jc)
c        nip=1
c       do ib=1,nbreak
c         t0  = tbreak1(ib,1)
c         dt0 = tbreak1(ib,2)
c         if ( tt.gt.t0-dt0 .and. tt.lt.t0+dt0 ) s(ni) = 1.0d3
c         if ( tt .ge. t0 ) nip = ib
c       enddo

c       aa(ni,npar_no_segs+nip) = 1.0d0

c       do ip = 1,npar_no_segs

        do ip = 1,npar 
           if ( ipar(ip).eq.1 ) aa(ni,ip) = 1.0d0
           if ( ipar(ip).eq.2 ) aa(ni,ip) = tt
           if ( ipar(ip).eq.3 ) aa(ni,ip) = dsin(twopii*tt)
           if ( ipar(ip).eq.4 ) aa(ni,ip) = dcos(twopii*tt)
           if ( ipar(ip).eq.5 ) aa(ni,ip) = dsin(fourpii*tt)
           if ( ipar(ip).eq.6 ) aa(ni,ip) = dcos(fourpii*tt)
         enddo
      enddo

        call linsolve (ni, MAX_ii, npar, Npmax, aa, r, s, pp, pper)
              
          do ip = 1,npar
           if ( ipar(ip).eq.1 ) Xo = pp(ip)
c           if ( ipar(ip).eq.1 ) print *, 'Xo', Xo
           if ( ipar(ip).eq.2 ) V = pp(ip)
           if ( ipar(ip).eq.2 ) Verr = pper(ip)
           if ( ipar(ip).eq.3 ) A = pp(ip) 
           if ( ipar(ip).eq.4 ) B = pp(ip) 
           if ( ipar(ip).eq.5 ) C = pp(ip) 
           if ( ipar(ip).eq.6 ) D = pp(ip) 
          enddo

c           Xo = pp(npar_no_segs+1)

c         do ib = 1, nbreak
c           np = npar_no_segs + ib
c           o_segs(ic,jc,ib,1) = tbreak1(ib,1)
c           o_segs(ic,jc,ib,2) = pp(np)
c          if (ib.ge.2) print *, gps_name(ic), 
c     .        o_segs(ic,jc,ib,1), o_segs(ic,jc,ib,2)  
c       enddo
   
       fail = .false.

c-- corrections
         GXo(ic,jc,1) = GXo(ic,jc,1) + Xo
         GVo(ic,jc,3) = GVo(ic,jc,3) + A
         GVo(ic,jc,4) = GVo(ic,jc,4) + B
         GVo(ic,jc,5) = GVo(ic,jc,5) + C
         GVo(ic,jc,6) = GVo(ic,jc,6) + D
         if (iV .ne. 2) V = 0.0d0
         if (iV.eq.2) GVo(ic,jc,2) = GVo(ic,jc,2) + V
         if (iV.eq.2) GVo(ic,jc,7) = Verr
       
c-- correct calculated values for new offsets, slope and sine terms
         do n = n1, n2
           tt = t_disp(n) 
           x_calc(n,jc) = x_calc(n,jc) + Xo + V*tt
     .      + fsine(tt, A, B, C, D ) 
         enddo
      
      return
      end

c**********************************************************************      
      subroutine adjust_ts (ic, jc )

c  compute average weighted residual for a time series, to correct for offset
c  site ic, component jc

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
      
      parameter (MAX_i = 10000)
      
      dimension aa(MAX_i, 2), r(MAX_i), s(MAX_i), pp(2), pper(2)
      logical fail
      
      avr      = 0.0d0
      avslope  = 0.0d0
     
      fail = .true.

cThe offset flags (Eo No Uo) control what to do with the offsets and seasonal signals for the 3 components 
cFlag = 0 don't use this component 
c       1 fix at current value
c       2 solve for offset by regression
c       3 solve for offset and one periodic signal (yearly period) 
c       4 solve for offset and two periodic signals (yearly and 6-month periods) 
        
cThe slope flags (Ev Nv Uv):
cFlag = 0 don't use slope (set slope to 0)
c       1 fix at current value
c       2 solve for slope by regression
c       3 use slope (velocity) from block model

c-- flag what to do      
      iX  = XVflag(ic,jc,1)
      iV  = XVflag(ic,jc,2)

c      print *, 'ic,jc,iX,iV', ic,jc,iX,iV

c-- don't adjust anything
c      if (iX .le. 1 ) return
c      if (iV .le. 1 ) return
      if (iX .le. 1 .and. iV .le. 1 ) return

c-- data range      
      n1 = ndx(ic,1)
      n2 = ndx(ic,2)

c-- no data      
      if (n1.le.0 ) return

      nn = n2-n1+1
      if ( nn.gt.MAX_i ) then
        print *, '*** MAX_i (',MAX_i,') exceeded in adjust_ts '
        stop
      endif

      dt = t_disp(n2)-t_disp(n1)

c-- calculate offset and sine term +/- slope by iterations
      call fitsine (iX, iV, ic, jc, fail)
      if ( .not. fail ) return

c      print *, 'fitsine Fail iX,iV,ic,jc ', iX,iV,ic,jc

c-- just calculate offset by weighted averages
      if( iX.eq.2  .and. iV.eq.3 .and. nn.gt.0 ) then
       av = 0.0d0
       ww = 0.0d0
       avr = 0.0d0
       do k = n1, n2
        d = x_disp(k,jc) - x_calc(k,jc)
        w = fnwt(x_sig(k,jc))
        av = av + d*w
        ww = ww + w
       enddo
       if (ww.gt.0.0d0) avr = av/ww

       do k = n1, n2
        x_calc(k,jc) = x_calc(k,jc) + avr
       enddo

        GXo(ic,jc,1) = GXo(ic,jc,1) + avr

        return
      endif
      
c-- calculate single offset and slope by weighted regression
      if( iX.eq.2 .and. iV.eq.2 .and. nn.gt.1) then
        ni = 0
        do i = n1, n2
         ni = ni+1
         r(ni) = x_disp(i,jc) - x_calc(i,jc)
         s(ni) = x_sig(i,jc)
         aa(ni,1) = 1.0d0
         aa(ni,2) = t_disp(i)
        enddo

        call linsolve (ni, MAX_i, itwo, itwo, aa, r, s, pp, pper)
        avr     = pp(1)
        avslope = pp(2)

        do k = n1, n2
         tt = t_disp(k)
         x_calc(k,jc) = x_calc(k,jc) + avr + avslope*tt
        enddo

         GXo(ic,jc,1) = GXo(ic,jc,1) + avr
         GVo(ic,jc,2) = GVo(ic,jc,2) + avslope

      endif
      
      

      return
      end
c**********************************************************************  
    
      subroutine get_ndata(nwdat)

c--  get number of weighted data 

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      include "tdefcons.h"

c-- max pts per insar file      
c      parameter (MAX_i = 2000)

      logical use_vel, useENU, useGPS

      tchi=0.0d0
      z = 0.0d0
      one = 1.0d0
      two = 2.0d0
      nwdat=0
      use_vel = .false.
 
     
c GPS velocities

      do i=1, num_gps
       if (gps_type(i).eq.1) then
        kfile = gps_index(i)
        call gpsinfo2(i, xx, yy, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sx, sy, sz, sxy)
       
c-- use velocity vector data
       if (useGPS(i) .and. .not. no_wt_gps ) then
        do j=1,3
         if (useENU(i,j)) nwdat = nwdat+1
        enddo
       endif
       endif
      enddo
      
     
c-- time series displacement data 
      do i=1, num_gps

       if (gps_type(i).eq.3) then
       
        call gpsinfo2(i, xx, yy, xobs, yobs, zobs, xcalc, ycalc, zcalc,
     .    sx, sy, sz, sxy)


c      if (useGPS(i) .and. ndx(i,1).gt.0) then
       n1 = ndx(i,1)
       n2 = ndx(i,2)

        if ( useGPS(i) .and. (n2-n1).gt.0) then
         do j=1,3
          if ( useENU(i,j) ) nwdat = nwdat + (n2-n1)+1
         enddo
        endif
       endif
      enddo

c-- INSAR line-of-sight displacements
       nwdat = nwdat + num_insar
      
c-- displacements
       do i=1, num_gps
        if( gps_type(i).eq.2 ) then
         do j=1,3
          if( useENU(i,j) )  nwdat=nwdat+1
         enddo
        endif
       enddo


c-- tilt rate data
        do i=1, num_tilts
          sig=tilt_sig(i)
          if (sig.gt.z) nwdat=nwdat+1
        enddo
        
c-- fault slip rates
       if ( .not. no_wt_srs ) then
       do i=1, num_sr
        sig = sr_sig(i)
        if (sig.gt.z) nwdat=nwdat+1
       enddo
       endif

c--  slip vectors
       do i=1, num_sv
        sig = sv_sig(i)
        if (sig.gt.z) nwdat=nwdat+1
       enddo


c-- strain rates
      if(num_ss.gt.0) then
        call getss
        do 20 i=1, num_ss
         nt = ss_type(i)
         nph=3
         if (nt.eq.0) nph=2
          do 20 j=1,nph
           sig = ss_sig(i,j)
           if ( sig.gt.z) nwdat=nwdat+1
   20   continue
      endif


c-- line lengths
      if(num_ll.gt.0) then
        call getll
        do i=1, num_ll
         sig = sig_ll(i)
          if ( sig.gt.z) nwdat=nwdat+1
        enddo
      endif


c-- rotations
        do i=1, num_rot
          sig=rot_sig(i)
          if (sig.gt.z) nwdat=nwdat+1
        enddo
        
      return
      end
      


c**********************************************************************      
c
c Zhuqi's strain stuff
c
c**********************************************************************      

      SUBROUTINE readmts(bfile)

c This subroutine is written for importing moment tensors 
c and grouping them into related blocks

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension xcc(MAX_corner), ycc(MAX_corner)

      character*80 bfile

      logical fncomment

      call cleareal(qms,3*MAX_block)
      call clearint(nqmt,MAX_block)

c 1. Opening moment tensor data file 

      k12=kfopen(12)
      open (k12, file = bfile)

 100   FORMAT(F9.2,F8.2,F4.0,6F6.2,i3,1x,A14)
      
c 1.2 checking comments
      j = 0
      do kk=1,10000

c    98.65   24.39  15 -0.01 -1.15  1.15  0.28 -0.01  0.31 26       052976A
c    98.58   24.29  15  0.22 -9.45  9.23  0.16 -0.32  6.18 25       052976B

c lon lat depth mrr mtt mpp mrt mrp mtp iexp name

        READ(k12, '(a256)', end=99) aline
c        print *, aline
        if ( .not. fncomment(aline(1:1))) then
          j=j+1
          READ(aline,100) (qmo(j,i),i=2,10),iexp,eq_name(j)
c dyn.cm => N.m
          iexp = iexp - 7 
          qmo(j, 11)  = 10.0d0**iexp
          dep = qmo(j,4)
          fac = qmo(j,11)/(1.0d17)
c          print *, eq_name(j), iexp, dep, qmo(j,5), fac

c-- scale to moment
          do i=5,10
           qmo(j, i) = qmo(j, i) * fac
          enddo

c remove isotropic part of moment
          t = (1.0d0/3.0d0)*( qmo(j,5) + qmo(j,6) + qmo(j,7))
           do i=5,7
            qmo(j, i) = qmo(j , i) - t
           enddo

c inital index_blocks are null values
          qmo( j, 1 ) = -1

          if(j.eq.MAX_mts) goto 99
        endif

      ENDDO

  99  ik = kfclose(k12)

      WRITE(*,*) 'Number of earthquakes is : ', j 
      num_eq = j 


c 2 Attributing earthquakes to proper blocks

      do j=1, nblocks
         nb = nc_block(j)

       do i=1, nb
          xcc(i)=blockxy(i,j,1)
          ycc(i)=blockxy(i,j,2)
       enddo

c check EQ
       do i = 1, num_eq
         xpt = qmo( i, 2 )
         ypt = qmo( i, 3 )
        call inside ( xpt, ypt, xcc, ycc, nb, insde)

         if ( abs(insde).eq.1 ) then
          if ( qmo(i,1) .ne. -1 ) then
           print *, eq_name(i), ' in multiple blocks, skipping'
           qmo(i,1)=-99
          else
           qmo(i,1)=j

c-- add mt to this block
           qms(j,1) = qms(j,1) + qmo(i,7)
           qms(j,2) = qms(j,2) + qmo(i,6)
           qms(j,3) = qms(j,3) + qmo(i,10)
           nqmt(j)=nqmt(j)+1
c        write(*, '(2i5, 3f12.5)') j, nq(j), (qms(j,k),k=1,3)

         endif
        endif

         if ( abs(insde).eq.2 ) qmo(i,1)=-99
       enddo

      enddo

      print *, 'Block moments;'
      print *, '  B#    N      Mee         Mnn         Men'
      do j=1, nblocks 
       write(*, '(2i5, 3f12.2)') j, nqmt(j), (qms(j,k),k=1,3)
       if ( nqmt(j).gt.0) invmt = .true.
      enddo

      RETURN

      END


c**********************************************************************      
      SUBROUTINE writemts 

c write mts

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      logical useg

      if (invmt) then
      call fopen (kk, 1, '.mts ' )
      write(kk, * ) 'Blk Strn Neqs  ---  CAlc  Sigma  Obs   res '

c-- moment tensors; 
        do j=1, nblocks 
          ks = nstrain_block(j)
          useg = .false.

          do k=1, num_strain_invert
           if(nstrain_invert(k).eq.ks) useg = .true.
          enddo

          if (nqmt(j).gt.0 .and. ks.gt.0 .and. useg) then

c-- get misfits
         write(kk, '(3i5,14f14.3)' )
     .    j, ks, nqmt(j), qmtwt, fmtfac(j),         
     .   ( strain(ks,i) * 1.0d3,
     .    strain(ks,i) * fmtfac(j), 
     .   qms(j,i), qms(j,i) - strain(ks,i) * fmtfac(j), 
     .   i=1,3)

         write(*, '(3i5,14f14.3)' )
     .    j, ks, nqmt(j), qmtwt, fmtfac(j),         
     .   ( strain(ks,i) * 1.0d3,
     .    strain(ks,i) * fmtfac(j), 
     .   qms(j,i), qms(j,i) - strain(ks,i) * fmtfac(j), 
     .   i=1,3)

       endif
      enddo
      ik = kfclose(kk)
      endif

      return
      end
c********************************************************************
c fflag(): 1 fault locking used            (FF: option)
c          2 3D slip on fault              (FA: option)
c          3 fault locking adjusted        (FI: option)
c          4 fault used to make blocks     (FB: option)
c          5 fault read in                 (FA: option)
      subroutine wf
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      character fnyesno*3
      logical z1


      print *, 'Faults read in'
      print *, ' No. Name        X by  Z   Lock  Est. Pseudo Used   GF'
      do kf=1, nfault
      
c pseudo fault 7-28-17
      if ( nzf(kf).eq.1) then
       fflag(kf,1) = .false.
       fflag(kf,3) = .false.
       useGF(kf) = .false.
      endif
      
       if (fflag(kf,5)) then
        z1 = (nzf(kf).eq.1) 
        write (*, 1)   
     .     kf, fault_name(kf), nxf(kf), nzf(kf),
     .     fnyesno(fflag(kf,1)), fnyesno(fflag(kf,3)),
     .     fnyesno(z1), fnyesno(fflag(kf,4)), fnyesno(useGF(kf))
       endif
      enddo
  1   format ( i4, 1x, a10, 2x, i3, ' x', i3, 1x, 5(3x,a3) )
      return
      end

 
c**********************************************************************      
      subroutine writegrids

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical fnilim, ingrid, wstrain

      character a1*1, a2*2, c7*7, block_name*4
      dimension xp(9), yp(9), vx(9), vy(9), kb(9)
      dimension sig(9,2), cor(9), vels(9,2), pos(9,2)

      x9 = 1.0d9


c write grid files
      do kg = 1, MAX_grids

        call gridinfo(kg, x0grid, nxgrid, dxgrid, 
     .    y0grid, nygrid, dygrid, nxyg )

      if (nxyg.gt.0) then
        print *, 'Writing grid file ',kg

      wstrain = .false.
      if ( pgrid(kg,7).gt.0.0) wstrain = .true.

   
       
      p1 = 0.1d0
      call i2c(kg,2,a2)

c** get GPS residual statistics within grid box 
      if ( gres ) then
        gxmax = x0grid + nxgrid*dxgrid
        gymax = y0grid + nygrid*dygrid
        call  gridres (a1, x0grid, gxmax, y0grid, gymax, 
     .    dxgrid, dygrid)      
      endif

c***************************************************************************      
c** grid files      
c***************************************************************************      
      call fopen (k12, 1, '_grid_'//a2//'.info ')
      call fopen (k19, 1, '_grid_'//a2//'.vec ')
      if(wstrain) call fopen (k20, 1, '_grid_'//a2//'_strain_atr.gmt ')

      do kx=1,nxgrid
       do ky=1,nygrid
      
       call gridxyk (kg, kx, ky, xpt, ypt, k, kbl )

      if ( fnilim (kbl,1,MAX_block) ) then
     
      a1 = ' '
      if (on_fault(k)) a1 = '*'
      
      Ux = u_grid(1,k) + u_grid(6,k) + u_grid(10,k) 
      Sx = dsqrt(u_grid(3,k)**2 + u_grid(8,k)**2 + 
     .    u_grid(12,k))
      
      Uy = u_grid(2,k) + u_grid(7,k) + u_grid(11,k) 
      Sy = dsqrt(u_grid(4,k)**2 + u_grid(9,k)**2 + 
     .    u_grid(13,k))

      Uz = u_grid(14,k)
      Sz = u_grid(15,k)
     
      u_grid(12,k) = dsqrt(u_grid(12,k))
      u_grid(13,k) = dsqrt(u_grid(13,k))
      u_grid(15,k) = dsqrt(u_grid(15,k))

      write (k12, '(2f9.3, 19(1x,f10.2), 1x, a4,1x, a1)') 
     .   xpt, ypt, Ux, Uy, Sx, Sy, (u_grid(i,k),i=1,15),
     .   block_name(kbl),a1
         
      call i2c( k, 7, c7)
      write (k19, 109) xpt, ypt,Ux,Uy,Sx,Sy,zero,
     .   'G'//c7
 109  format(2f9.3, 5f9.2, 1x,a8)


c*** write out strain rates by differencing
c grid points:
c
c          9   4   6
c          1   5   2
c          8   3   7
c
c
c

      if (kx.gt.1 .and. kx.lt.nxgrid .and. 
     .     ky.gt.1 .and. ky.lt.nygrid .and. wstrain ) then
     
       call gridxyk (kg, kx-1, ky,   xp(1), yp(1), k1, kb(1) )
       call gridxyk (kg, kx,   ky-1, xp(3), yp(3), k3, kb(2) )
       call gridxyk (kg, kx+1, ky,   xp(2), yp(2), k2, kb(3) )
       call gridxyk (kg, kx,   ky+1, xp(4), yp(4), k4, kb(4) )
       call gridxyk (kg, kx,   ky,   xp(5), yp(5), k5, kb(5) )
       call gridxyk (kg, kx+1, ky+1, xp(6), yp(6), k6, kb(6) )
       call gridxyk (kg, kx+1, ky-1, xp(7), yp(7), k7, kb(7) )
       call gridxyk (kg, kx-1, ky-1, xp(8), yp(8), k8, kb(8) )
       call gridxyk (kg, kx-1, ky+1, xp(9), yp(9), k9, kb(9) )

c       x1=x0grid + (kx-2)*dxgrid
c       x2=x0grid + (kx)*dxgrid

c grid size in km
c       call distkm(x1,y1,x2,y2, xdx)
c       y1=y0grid + (ky)*dygrid
c       y2=y0grid + (ky-2)*dygrid
c       call distkm(x3,y3,x4,y4, ydy)
c       ny=nygrid

cc all points used must be on a block
      ingrid = .true.
      do i=1,9
       if ( .not. fnilim(kb(i),1,MAX_block) ) ingrid = .false.
      enddo
     
      if (ingrid) then

c-- calculate spherical strains
       bx=xp(5)
       by=yp(5)
       kbl = kb(5)
       nsites = 9
       ndim = 9

       do j=1,9
        pos(j,1)=xp(j)
        pos(j,2)=yp(j)
       enddo

c** total strain rates       
       vx(1) = u_grid(1,k1) + u_grid(6,k1) + u_grid(10,k1) 
       vx(2) = u_grid(1,k2) + u_grid(6,k2) + u_grid(10,k2) 
       vx(3) = u_grid(1,k3) + u_grid(6,k3) + u_grid(10,k3) 
       vx(4) = u_grid(1,k4) + u_grid(6,k4) + u_grid(10,k4) 
       vx(5) = u_grid(1,k5) + u_grid(6,k5) + u_grid(10,k5) 
       vx(6) = u_grid(1,k6) + u_grid(6,k6) + u_grid(10,k6) 
       vx(7) = u_grid(1,k7) + u_grid(6,k7) + u_grid(10,k7) 
       vx(8) = u_grid(1,k8) + u_grid(6,k8) + u_grid(10,k8) 
       vx(9) = u_grid(1,k9) + u_grid(6,k9) + u_grid(10,k9) 
       
       vy(1) = u_grid(2,k1) + u_grid(7,k1) + u_grid(11,k1)
       vy(2) = u_grid(2,k2) + u_grid(7,k2) + u_grid(11,k2)
       vy(3) = u_grid(2,k3) + u_grid(7,k3) + u_grid(11,k3)
       vy(4) = u_grid(2,k4) + u_grid(7,k4) + u_grid(11,k4)
       vy(5) = u_grid(2,k5) + u_grid(7,k5) + u_grid(11,k5)
       vy(6) = u_grid(2,k6) + u_grid(7,k6) + u_grid(11,k6)
       vy(7) = u_grid(2,k7) + u_grid(7,k7) + u_grid(11,k7)
       vy(8) = u_grid(2,k8) + u_grid(7,k8) + u_grid(11,k8)
       vy(9) = u_grid(2,k9) + u_grid(7,k9) + u_grid(11,k9)

       do j=1,9
        vels(j,1)=vx(j)
        vels(j,2)=vy(j)
        sig(j,1) =1.0d0
        sig(j,2) = 1.0d0
        cor(j) = 0.0d0
       enddo
       
      call getstrain (Bx, By, nsites, ndim, pos, vels, sig, cor, 
     .  Txx, SExx, Txy, SExy, Tyy, SEyy, Conx, SCx, Cony, SCy, Wr, SWr,
     .  chi2out)

c       dvxdx = (vx2-vx1)/xdx *1.0d3
c       dvydy = (vy4-vy3)/ydy *1.0d3
c       dvxdy = ( (vx4-vx3)/ydy + (vy2-vy1)/xdx )*1.0d3 / two

c* permanent strains rates (rotation + internal strains)
       vx(1) = u_grid(1,k1) + u_grid(6,k1) 
       vx(2) = u_grid(1,k2) + u_grid(6,k2)
       vx(3) = u_grid(1,k3) + u_grid(6,k3) 
       vx(4) = u_grid(1,k4) + u_grid(6,k4)
       vx(5) = u_grid(1,k5) + u_grid(6,k5)
       vx(6) = u_grid(1,k6) + u_grid(6,k6)
       vx(7) = u_grid(1,k7) + u_grid(6,k7)
       vx(8) = u_grid(1,k8) + u_grid(6,k8)
       vx(9) = u_grid(1,k9) + u_grid(6,k9)

       vy(1) = u_grid(2,k1) + u_grid(7,k1) 
       vy(2) = u_grid(2,k2) + u_grid(7,k2)
       vy(3) = u_grid(2,k3) + u_grid(7,k3)
       vy(4) = u_grid(2,k4) + u_grid(7,k4)
       vy(5) = u_grid(2,k5) + u_grid(7,k5)
       vy(6) = u_grid(2,k6) + u_grid(7,k6)
       vy(7) = u_grid(2,k7) + u_grid(7,k7)
       vy(8) = u_grid(2,k8) + u_grid(7,k8)
       vy(9) = u_grid(2,k9) + u_grid(7,k9)

       do j=1,9
        vels(j,1)=vx(j)
        vels(j,2)=vy(j)
        sig(j,1) =1.0d0
        sig(j,2) = 1.0d0
        cor(j) =0.0d0
       enddo
       
      call getstrain (Bx, By, nsites, ndim, pos, vels, sig, cor, 
     .  Pxx, SExx, Pxy, SExy, Pyy, SEyy, Conx, SCx, Cony, SCy, Wr, SWr,
     .  chi2out)

c       dbxdx = (vx2-vx1)/xdx *1.0d3
c       dbydy = (vy4-vy3)/ydy *1.0d3
c       dbxdy = ( (vx4-vx3)/ydy + (vy2-vy1)/xdx )*1.0d3 / two

c** residual strains and sigmas from .dgt file, added 9/27/12
       Rxx = res_strain(kbl, 15) * 1.0d3
       Ryy = res_strain(kbl, 16) * 1.0d3
       Rxy = res_strain(kbl, 17) * 1.0d3 
       SRxx = res_strain(kbl, 18) * 1.0d3
       SRyy = res_strain(kbl, 19) * 1.0d3
       SRxy = res_strain(kbl, 20) * 1.0d3 

c** block strains and sigmas from inversion, added 10/4/12
       kstr = nstrain_block(kbl)
       if (fnilim(kstr,1,MAX_strain)) then
        Bxx = strain(kstr, 1) * 1.0d3
        Byy = strain(kstr, 2) * 1.0d3
        Bxy = strain(kstr, 3) * 1.0d3 
        SBxx = strain(kstr, 4) * 1.0d3
        SByy = strain(kstr, 5) * 1.0d3
        SBxy = strain(kstr, 6) * 1.0d3 
       else
        Bxx = 0.0d0
        Byy = 0.0d0
        Bxy = 0.0d0 
        SBxx = 0.0d0
        SByy = 0.0d0
        SBxy = 0.0d0 
       endif

        SRBxx = dsqrt (SRxx**2 + SBxx**2 ) 
        SRByy = dsqrt (SRyy**2 + SByy**2 ) 
        SRBxy = dsqrt (SRxy**2 + SBxy**2 ) 

c now get spherical area of grid element, in sq kms
c  should be ~ 10,000 km per sq deg
c       dx2 = dxgrid/2.0d0
c       dy2 = dygrid/2.0d0
c       xc(1) = xpt+dx2
c       yc(1) = ypt+dy2
c       xc(2) = xpt+dx2
c       yc(2) = ypt-dy2
c       xc(3) = xpt-dx2
c       yc(3) = ypt-dy2
c       xc(4) = xpt-dx2
c       yc(4) = ypt+dy2

c       call gridxyk (kg, kx+1, ky+1, xc(1), yc(1), k1, kb(1) )
c       call gridxyk (kg, kx+1, ky-1, xc(2), yc(2), k2, kb(2) )
c       call gridxyk (kg, kx-1, ky-1, xc(3), yc(3), k3, kb(3) )
c       call gridxyk (kg, kx-1, ky+1, xc(4), yc(4), k4, kb(4) )

c       vx(1) = u_grid(1,k1) + u_grid(6,k1) 
c       vx(2) = u_grid(1,k2) + u_grid(6,k2)
c       vx(3) = u_grid(1,k3) + u_grid(6,k3) 
c       vx(4) = u_grid(1,k4) + u_grid(6,k4)
       
c       vy(1) = u_grid(2,k1) + u_grid(7,k1) 
c       vy(2) = u_grid(2,k2) + u_grid(7,k2)
c       vy(3) = u_grid(2,k3) + u_grid(7,k3)
c       vy(4) = u_grid(2,k4) + u_grid(7,k4)

c       nin = 4
c       do i=1,nin
c        x2(i)=fn180(x(i))*d2r
c        y2(i)=y(i)*d2r
c       enddo

c      call centroid ( nin, x, y, cx, cy, area)
c this is not working!
c      aa1 = sphere01_polygon_area ( nin, y2, x2 ) 
c      area = aa1 * Erad *  Erad

c brute force area
      call distkm( xp(6),yp(6),xp(9),yp(9), d12 )
      call distkm( xp(6),yp(6),xp(7),yp(7), d23 )
      call distkm( xp(7),yp(7),xp(8),yp(8), d34 )
      call distkm( xp(8),yp(8),xp(9),yp(9), d41 )

      area = ( (d12+d34)/two * (d23+d41)/two )

c grid strain file header
c 3-4 lon, lat of grid center
c 5-6 block number and name
c 7 is grid area in km^2
c 8-13 Block strain Exx Eyy Exy SExx SEyy SExy (nanostrain/yr)
c 14-19 Residual block strain and sigmas
c 20-25 Block+Res strain and sigmas
c 26-28 Total strain (not working)
c 29-31 Permanent strain (not working)
c 32 Savage max block strain (max of |Exx| |Eyy| |Exx + Eyy| )
c 33 Savage max res strain
c 34 Savage max block+res strain
c 35 Savage max total strain
c 36 Savage max perm strain
c 37 Max block strain  (larger principle axis)
c 38 Max res strain 
c 39 Max block+res strain  
c 40 Max total strain
c 41 Max perm strain

      write(k20, '(a5,2f10.3,i4,1x,a4,1x,e12.6,34(1x,f9.2))') 
     .    '> -Z ', xpt, ypt, kbl, block_name(kbl), area, 
     .    Bxx, Byy, Bxy, SBxx, SByy, SBxy, 
     .    Rxx, Ryy, Rxy, SRxx, SRyy, SRxy, 
     .    Rxx+Bxx, Ryy+Byy, Rxy+Bxy, SRBxx, SRByy, SRBxy,
     .    Txx*x9, Tyy*x9, Txy*x9, 
     .    Pxx*x9, Pyy*x9, Pxy*x9, 
     .    air_strain(Bxx, Byy, Bxy),
     .    air_strain(Rxx,Ryy,Rxy),
     .    air_strain(Rxx+Bxx, Ryy+Byy, Rxy+Bxy),
     .    air_strain(Txx, Tyy, Txy), 
     .    air_strain(Pxx, Pyy, Pxy),
     .    str_max(Bxx, Byy, Bxy),
     .    str_max(Rxx,Ryy,Rxy),
     .    str_max(Rxx+Bxx, Ryy+Byy, Rxy+Bxy), 
     .    str_max(Txx, Tyy, Txy), 
     .    str_max(Pxx, Pyy, Pxy) 
       
        write(k20,'(2f12.4,2f9.2,i4,1x,a4)')
     .     (xp(i),yp(i),vx(i),vy(i),kb(i),block_name(kb(i)), i=6,9)

c       write(k20, '(2f12.4)') xpt+dx2,ypt+dy2
c       write(k20, '(2f12.4)') xpt+dx2,ypt-dy2
c       write(k20, '(2f12.4)') xpt-dx2,ypt-dy2
c       write(k20, '(2f12.4)') xpt-dx2,ypt+dy2

       endif

       endif

      endif

       enddo
      enddo

      ik = kfclose (k12)
      ik = kfclose (k19)
      if(wstrain) ik = kfclose (k20)

c-- htdp file
      if (htdp) call makehtdp (kg, nxgrid, nygrid)
      
      endif
      
      enddo

      return
      end
c***********************************************************************
      subroutine writefaultinfo

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical flock, dofault, haspoles
      dimension w_tmp(MAX_x,MAX_z), x_tmp(MAX_x), x_Mo(Max_x), 
     .   x_mo_lat(500)
      character block_name*4

      z=0.0d0
      thou =1.0d3
      x99 = 99.0d0
      nseg=0

c*** write faults and nodes to files
      print *, 'Writing fault info '
      
       call fopen(kf2, 1, '.nod ')
       call fopen(k19, 1, '_mid.vec ')
       call fopen(k22, 1, '_fault_Mo.out ')

       if (myflag) call fopen(k29, 1, '_mid_vec.table ')
       call fopen(k20, 1, '.faults ')

       if (scec) call fopen(k19a, 1, '_mid.scec ')
  
      write(k20,*)'No. Fault name Type Length,km  Area,km^2  '//
     . 'Potncy,m^3  Moment,N-m  '//
     . '   Mom_sig    Avslip,m    AvDep,km AvArea,km^2'//
     . '   AvStr ----Mw range---- Footwall  Smoothing  '

      do kf=1, nfault

       dofault = ( flock(kf) .and. fflag(kf,4) )
c       dofault = ( useGF(kf) )
       call cleareal(x_Mo, MAX_x)
       call cleareal(x_Mo_lat, 500)

c* moment stuff
      if ( dofault ) then
       avslip = z
       avdep  = z
       area = fault_sums(kf,1)
       ferr = dsqrt(fault_sums(kf,4))
       pot = fault_sums(kf,2)
       xmo = fault_sums(kf,3)
       if (area.gt.z) avslip = pot/(area*1.0d6)
       sslip = fault_sums(kf,6)
       if (sslip.gt.z) avdep = fault_sums(kf,5)/sslip
       avarea = fault_sums(kf,7)
       if(avarea.gt.z) avslip = pot/(avarea*1.0d6)
       xmw  = fnMw(xmo)
       xmw1 = fnMw(xmo-ferr)
       xmw2 = fnMw(xmo+ferr)
       flength=xwnode(1,nxf(kf),1,kf)

c** write .fault file, moment rates on all faults
       write (k20, 20) 
     .  kf, fault_name(kf), fault_fit_type(kf), flength,
     .  area, pot, xmo, ferr, avslip, avdep, 
     .  avarea, av_fault_strike(kf), xmw1, xmw, xmw2,
     .  block_name(kfw_blk(1, 1, kf)),
     .  int(smooth(kf,1)), (smooth(kf,j),j=2,6)
  20  format(i3,1x,a10,i5,f10.1,7e12.4,f8.1,3f6.2,
     .    2x,a4,1x,i2,1x,5e12.4)
      endif


c***
      call cleareal(w_tmp, MAX_x*MAX_z)
      call cleareal(x_tmp, MAX_x)

      do iz = 1,nzf(kf)

       xstrike=0.0d0

       do ix = 1,nxf(kf)
        xpt = xynode(1,ix,iz,kf)
        ypt = xynode(2,ix,iz,kf)
        zpt = znode(iz,kf)

c along strike distance
        if ( ix.gt.1) then
         call distkm( xpt, ypt,xynode(1,ix-1,iz,kf), 
     .     xynode(2,ix-1,iz,kf), xx)
         xstrike = xstrike+xx
        endif

c downdip distance
        if (iz.gt.1) then
         zz  = zpt - znode(iz-1, kf)
         call distkm( xpt, ypt,xynode(1,ix,iz-1,kf), 
     .     xynode(2,ix,iz-1,kf), xw)
         w = dsqrt( xw*xw + zz*zz)
         w_tmp(ix,iz) = w_tmp(ix,iz-1)+w
         x_tmp(ix) = x_tmp(ix)+xw
        endif
        
c fixed 9-14-20
         xstrike = xwnode(1,ix,iz,kf)
         w_tmp(ix,iz) = xwnode(2,ix,iz,kf)

        kfw = kfw_blk(ix, iz, kf)
        khw = khw_blk(ix, iz, kf)
        haspoles = ( (kfw .ne. 0 .and. khw .ne. 0) .and. 
     .     ( nstrain_block(khw) .ne. nstrain_block(kfw) ) .or.  
     .     ( npole_block(khw) .ne. npole_block(kfw) ) )


        pxE = slip_n(ix,iz,kf,1)
        pxN = slip_n(ix,iz,kf,2)
        p = phi(ix,iz,kf)
        px = pxE * p
        py = pxN * p
        pe = phi_err(ix,iz,kf)
        xmom = tmom_node(ix,iz,kf)*p*xmu

        slipaz = fnstriker(pxE,pxN)
        slipaz = fn360(slipaz)

        ilat = int(2.0d0*ypt)
c        print *, "kf, ix, iz, ilat, ypt ", kf, ix, iz, ilat, ypt
        x_Mo(ix) = x_Mo(ix) + xmom
        x_Mo_lat(ilat) = x_Mo_lat(ilat) + xmom

c** write to fault .nod file
      if ( haspoles .and. (flock(kf) .or. write_all_fault) )  then
        write(kf2, 503)  fault_name(kf), kf, ix, iz, 
     .    block_name(khw), block_name(kfw), xpt, ypt, zpt, p, 
     .    min(pe, 99999.999d0),
     .    (slip_n(ix,iz,kf,k), k=1,5), px, py, slipaz,
     .    xstrike, x_tmp(ix), w_tmp(ix,iz),
     .    fault_strike(ix,iz,kf), fault_dip(ix,iz,kf),
     .    xmom
      endif
  503 format (a10, 3i4, 2(1x,a4), 1x, 3f9.3, 2f10.3, 4f8.1, f8.4, 
     .   8f8.1,1x, 1pe10.3)


       enddo
      enddo



c* get slip vectors at surface points midway between surface nodes
c      if ( yesno ) then


      iz=1
c      if (wusflag) iz=2

      do ix = 1, nxf(kf)-1
       ixx=ix

c- get fault azimuth      
      ddx=0.0d0
      call getnodexy ( xn1, yn1, kf, ixx,   1, ddx)
      call getnodexy ( xn2, yn2, kf, ixx+1, 1, ddx)
      call delaz ( yn2, xn2, yn1, xn1, d, az )
      fault_az = az
      azr = az*d2r

c- get fault dip      
      dip = 0.0d0
      if (nzf(kf).gt.1) then
       ddx=0.0d0
       dz = znode(2,kf) - znode(1,kf)
       call getnodexy ( xn1, yn1, kf, ixx,   1, ddx)
       call getnodexy ( xn2, yn2, kf, ixx+1, 1, ddx)
       call getnodexy ( xn3, yn3, kf, ixx,   2, ddx)
       call getnodexy ( xn4, yn4, kf, ixx+1, 2, ddx)
       call distkm ( xn1, yn1, xn3, yn3, d13 )
       call distkm ( xn2, yn2, xn4, yn4, d24 )
       dp1 = fndipper (d13, dz)
       dp2 = fndipper (d24, dz)
       dip = ( dp1 + dp2 ) / two * r2d
      endif

c get offset nodes for vector calculations
       ddx=0.05
c       ddx=0.01
       call getnodexy ( xn1, yn1, kf, ixx, 1, ddx)
       call getnodexy ( xn2, yn2, kf, ixx+1, 1, ddx)
       xn = (xn1+xn2)/two
       yn = (yn1+yn2)/two
       aphi = ( phi(ixx, iz, kf) + phi(ixx+1, iz, kf) ) /two
       ephi = ( phi_err(ixx, iz, kf) + phi_err(ixx+1, iz, kf) ) /two
       ephi = min(ephi, 99.0d0)

c** which 2 blocks are moving, one is always the foot wall block
       kfw = kfw_blk(ixx, 1, kf)

c** find the other, the point will be within the hanging wall block
      call getblock(xn, yn, khw)

         haspoles = ( kfw .ne. 0 .and. khw .ne. 0 .and. 
     .     ( npole_block(khw) .ne. npole_block(kfw) ) )

c get velocity
      call relvel (3, khw, kfw, xn, yn, Vx, Sx, Vy, Sy, rho)

c       c20=fault_name(kf)//'_'//block_name(khw)//'_'//block_name(kfw)

       call velsig(Vx, Vy, Sx, Sy, rho, Vt, Vsig)
       call azsig (Vx, Vy, Sx, Sy, AzV, AzVer)


c locking depth approximation
       zlock = 0.0d0
       zlocksig = 0.0d0

       if (fault_fit_type(kf) .le. 1) then
        zlock = (f_parm(kf,ix,2)+f_parm(kf,ix+1,2))/two
       endif

       if (fault_fit_type(kf).eq.2) then
        zlock = (f_parm(kf,ix,2)+f_parm(kf,ix+1,2))/two
        zlocksig = (f_parm_err(kf,ix,2)+f_parm_err(kf,ix+1,2))/two
       endif


c** get fault-parallel and fault-normal velocities and sigmas


c* fault-parallel, right-lateral is positive
c* unit vector parallel to fault
       Ux = dsin(azr)
       Uy = dcos(azr)
c* velocity and sigma in this direction
       Velp = -(Vx * Ux + Vy * Uy)
       call velsig(Ux, Uy, Sx, Sy, rho, Ut, Vpsig)

c* fault-normal, extension is postitive, thrust is negative
       azr = (az+90.0d0)*d2r

c* unit vector normal to fault
       Ux = dsin(azr)
       Uy = dcos(azr)

c* velocity and sigma in this direction
       Vn = (Vx * Ux + Vy * Uy)
       call velsig(Ux, Uy, Sx, Sy, rho, Un, Vnsig)

c* _mid.vec file
       if ( nzf(kf).gt.1  .and. haspoles ) 
     .   write(k19, 577)  xn, yn, Vx, Vy, Sx, Sy, rho, 
     .   fault_name(kf), block_name(khw), block_name(kfw), kf,
     .   Vt, Vsig, AzV, AzVer, aphi, ephi, Velp, Vpsig, Vn, Vnsig,
     .   fault_az, dip, zlock, zlocksig
c  577 format(2f8.3, 4f7.1, f7.3, 1x, a10,2(1x,a4),i4,4f6.1,2f7.3,8f6.1)
  577 format(2f8.3, 4f7.2, f7.3, 1x, a10,2(1x,a4),i4,4f6.1,2f7.3,4f7.2,
     .  4f6.1)

        if (haspoles) nseg = nseg + 1

c* _mid_vec.table file, no offset
       if ( myflag .and. nzf(kf).gt.1  .and. haspoles ) then
         ddx=0.00
         call getnodexy ( xn1, yn1, kf, ixx, 1, ddx)
         call getnodexy ( xn2, yn2, kf, ixx+1, 1, ddx)
         xn3 = (xn1+xn2)/two
         yn3 = (yn1+yn2)/two
 
        write(k29, 587) kf, ix, nseg, 
     .   fault_name(kf),block_name(khw),block_name(kfw), 
     .   xn3, yn3, Vx, Vy, Sx, Sy, 
     .   Vt, Vsig, fn360(AzV), AzVer, Velp, Vpsig, Vn, Vnsig,
     .   fn360(fault_az), dip
  587 format(3i5, 1x, a10, 2(1x,a4), 2f8.3, 14f7.1 )
       

cc for SCEC work, no offset from node
       if (scec) then 
        fz=15.0d0
        zlock = aphi*fz
        zlocksig = ephi*fz

       if ( nzf(kf).gt.1  .and. haspoles ) 
     .   write(k19a, 578) nseg, kf, xn3, yn3, Velp, Vpsig, Vn, Vnsig,
     .   zlock, zlocksig, fault_az, dip, aphi, ephi,
     .   fault_name(kf), block_name(khw), block_name(kfw)
       endif

  578 format(2i5, 2f9.4, 2f8.2, 6f8.2, 1x, 2f9.4, 1x, a10, 2(1x,a4) )

      endif

      enddo

c* write out moments by profile
      do ix=1, nxf(kf)
        xx1 = xynode(1,ix,1,kf)
        yy1 = xynode(2,ix,1,kf)
        xx2 = xynode(1,ix,nzf(kf),kf)
        yy2 = xynode(2,ix,nzf(kf),kf)
        call delaz ( yy2, xx2, yy1, xx1, dd, az)
       write (k22, '("P: ",2i5,2f9.3,f7.1, 1pe12.4)') 
     .    kf, ix, xx1, yy1, az, x_Mo(ix)
      enddo
      do ix=1, 500
       if(x_Mo_lat(ix).gt.0.0d0) 
     .  write (k22, '("L: ",i5, f9.2, 1pe12.4)') 
     .    kf, (real(ix-1)+0.5)/2.0d0, x_Mo_lat(ix)
      enddo

c      endif

      enddo

      ik = kfclose (k22)
      ik = kfclose (kf2)
      ik = kfclose (k19)
      if (myflag) ik = kfclose (k29)
      ik = kfclose (k20)
      if (scec) ik = kfclose (k19a)

c  write .blk? files
       call fopen(k20, 1, '_blk0.gmt ')
       call fopen(k21, 1, '_blk1.gmt ')
       call fopen(k24, 1, '_blk4.gmt ')
       call fopen(k25, 1, '_blk5.gmt ')
 
      do kf=1, nfault
        if (nxf(kf).gt.2 ) then
         nmid = int(real(nxf(kf))/two + 0.5d0)
c        print *, "nmid, two ", nmid, two
         xm = xynode(1,nmid,1,kf)
         ym = xynode(2,nmid,1,kf)
        else
         xm = (xynode(1,1,1,kf)+xynode(1,2,1,kf))/two
         ym = (xynode(2,1,1,kf)+xynode(2,2,1,kf))/two
        endif

        do ix = 1, nxf(kf)
        iz=1

        xpt = xynode(1,ix,iz,kf)
        ypt = xynode(2,ix,iz,kf)

c write .blk0 all fault segments 
        if ( ix.eq.1) write (k20, '(a2, i4,2f9.3,1x,a10)' ) '> ', 
     .                       kf,xm,ym, fault_name(kf)
        write(k20, '(2f10.4)' ) xpt, ypt 

c write .blk1 all pseudo-faults 
      if ( nzf(kf).eq.1 ) then
        if ( ix.eq.1) write (k21, '(a2, i4,2f9.3,1x,a10)' ) '> ', 
     .                       kf,xm,ym, fault_name(kf)
        write(k21, '(2f10.4)' ) xpt, ypt 
      endif

c write .blk4 all faults that have locking 
      if (useGF(kf) ) then
        if ( ix.eq.1) write (k24, '(a2, i4,2f9.3,1x,a10)' ) '> ', 
     .                       kf,xm,ym, fault_name(kf)
        write(k24, '(2f10.4)' ) xpt, ypt 
      endif

c write .blk5 all faults that have locking estimated
      if (fflag(kf,1) .and. fflag(kf,3) ) then
        if ( ix.eq.1) write (k25, '(a2, i4,2f9.3,1x,a10)' ) '> ', 
     .                       kf,xm,ym, fault_name(kf)
        write(k25, '(2f10.4)' ) xpt, ypt 
      endif

        enddo
      enddo

      ik = kfclose (k20)
      ik = kfclose (k21)
      ik = kfclose (k24)
      ik = kfclose (k25)

c write .blk3 all faults that have relative slip (ie different poles)
       call fopen(k23, 1, '_blk3.gmt ')
      do kf=1, nfault
       nn = 0
        do ix = 1,nxf(kf) 
        iz=1

        xpt = xynode(1,ix,iz,kf)
        ypt = xynode(2,ix,iz,kf)
        kfw = kfw_blk(ix, iz, kf)
        khw = khw_blk(ix, iz, kf)
        kpfw = 0
        kphw = 0
        if(kfw.gt.0 ) kpfw = npole_block(kfw)
        if(khw.gt.0 ) kphw = npole_block(khw)
 
      if (kpfw .ne. kphw .and. kpfw .ne. 0 .and. kphw .ne. 0 ) then
        nn = nn +1
        if ( nn.eq.1) write (k23, '(a2, 3i4)' ) '> ', kf, kpfw, kphw
        write(k23, '(2f10.4)' ) xpt, ypt
      else
        nn=0
      endif

        enddo
      enddo

      ik = kfclose (k23)

      return
      end

c***********************************************************************
      subroutine centroid ( nin, xi, yi, cx, cy, area)

C*** calculate centroid of polygon
c  http://www.mathopenref.com/coordpolygonarea.html

      implicit real*8 (a-h,o-z)
      include "tdefcons.h"

      dimension xi(nin+1), yi(nin+1), x(nin+1), y(nin+1)
c      dimension v1(3), v2(3), v3(3)

      n=nin

      do i=1,n
       x(i) = xi(i)
       y(i) = yi(i)
      enddo

c*** fill out arrays if necessary
      if ( x(1) .ne. x(n) .or. y(1) .ne. y(n) ) then
        x(n+1) = x(1)
        y(n+1) = y(1)
      else
        n=nin-1
      endif

c*** Area
      a=0.0d0
      do i=1,n
       a = a + (x(i)*y(i+1) - x(i+1)*y(i))
      enddo

      a = 0.5d0*dabs(a)

c*** Centroid
      Cx = 0.0d0
      Cy = 0.0d0
      a6 = dabs(6.0d0*a)
      avx = 0.0d0
      avy = 0.0d0

      do i=1,n
       t2 =  x(i)*y(i+1) - x(i+1)*y(i)
       Cx = Cx + ( x(i) + x(i+1))*t2
       Cy = Cy + ( y(i) + y(i+1))*t2
       avx = avx + x(i)
       avy = avy + y(i)
      enddo

       Cx = Cx/a6
       Cy = Cy/a6

c INSDE = 0 if point outside
c       = +/-1 if inside
c       = 2 if on perimeter
c see if centroid not within block
      call inside ( Cx, Cy, x, y, n, insde) 
      if ( insde.eq.0 .or. insde.eq.2) then
          Cx = avx/ real(n)
          Cy = avy/ real(n)
      endif
 


c now get spherical area
      do i=1,nin
       x(i) = xi(i)*d2r
       y(i) = yi(i)*d2r
      enddo

      tarea = sphere01_polygon_area ( nin, y, x )
      area = tarea*Erad*Erad

       return
       end

c*********************************************************************
c spherical geometry codes from:
c http://people.sc.fsu.edu/~jburkardt/f77_src/geometry/geometry.html
c*********************************************************************
c***********************************************************************
      subroutine sphere01_triangle_vertices_to_sides ( v1, v2, v3, 
     &  as, bs, cs )

c***********************************************************************
c
cc SPHERE01_TRIANGLE_VERTICES_TO_SIDES: spherical triangle sides on unit sphere.
c
c  Discussion:
c
c    We can use the ACOS system call here, but the ARC_COSINE routine
c    will automatically take care of cases where the input argument is
c    (usually slightly) out of bounds.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), V3(3), the vertices of the spherical
c    triangle.
c
c    Output, double precision AS, BS, CS, the (geodesic) length of the sides
c    of the triangle.
c
      implicit none

      double precision arc_cosine
      double precision as
      double precision bs
      double precision cs
      double precision r8vec_dot_product
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      as = arc_cosine ( r8vec_dot_product ( 3, v2, v3 ) )
      bs = arc_cosine ( r8vec_dot_product ( 3, v3, v1 ) )
      cs = arc_cosine ( r8vec_dot_product ( 3, v1, v2 ) )

      return
      end
c*********************************************************************72

      subroutine sphere01_triangle_sides_to_angles ( as, bs, cs, 
     &  a, b, c )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_SIDES_TO_ANGLES; spherical triangle angles on the unit sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision AS, BS, CS, the (geodesic) length of the 
c    sides of the triangle.
c
c    Output, double precision A, B, C, the spherical angles of the triangle.
c    Angle A is opposite the side of length AS, and so on.
c
      implicit none

      double precision a
      double precision as
      double precision asu
      double precision b
      double precision bs
      double precision bsu
      double precision c
      double precision cs
      double precision csu
      double precision ssu
      double precision tan_a2
      double precision tan_b2
      double precision tan_c2

      asu = as
      bsu = bs
      csu = cs
      ssu = ( asu + bsu + csu ) / 2.0D+00

      tan_a2 = sqrt ( ( sin ( ssu - bsu ) * sin ( ssu - csu ) ) / 
     &                ( sin ( ssu ) * sin ( ssu - asu )     ) )

      a = 2.0D+00 * atan ( tan_a2 )

      tan_b2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - csu ) ) / 
     &                ( sin ( ssu ) * sin ( ssu - bsu )     ) )

      b = 2.0D+00 * atan ( tan_b2 )

      tan_c2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - bsu ) ) / 
     &                ( sin ( ssu ) * sin ( ssu - csu )     ) )

      c = 2.0D+00 * atan ( tan_c2 )

      return
      end
c*********************************************************************72

      function arc_cosine ( c )

c*********************************************************************72
c
cc ARC_COSINE computes the arc cosine function, with argument truncation.
c
c  Discussion:
c
c    If you call your system ACOS routine with an input argument that is
c    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant 
c    surprise (I did).
c
c    This routine simply truncates arguments outside the range.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 December 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision C, the argument.
c
c    Output, double precision ARC_COSINE, an angle whose cosine is C.
c
      implicit none

      double precision arc_cosine
      double precision c
      double precision c2

      c2 = c
      c2 = max ( c2, -1.0D+00 )
      c2 = min ( c2, +1.0D+00 )

      arc_cosine = acos ( c2 )

      return
      end

c*********************************************************************72

      subroutine sphere01_triangle_angles_to_area ( a, b, c, area )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_ANGLES_TO_AREA: area of a spherical triangle on unit sphere.
c
c  Discussion:
c
c    A unit sphere centered at 0 in 3D satisfies the equation:
c
c      X*X + Y*Y + Z*Z = 1
c
c    A spherical triangle is specified by three points on the surface
c    of the sphere.
c
c    The area formula is known as Girard's formula.
c
c    The area of a spherical triangle on the unit sphere is:
c
c      AREA = ( A + B + C - PI )
c
c    where A, B and C are the (surface) angles of the triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the angles of the triangle.
c
c    Output, double precision AREA, the area of the spherical triangle.
c
      implicit none

      double precision a
      double precision area
      double precision b
      double precision c
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
c
c  Apply Girard's formula.
c
      area = a + b + c - pi

      return
      end


c*********************************************************************72

      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end

c*********************************************************************72
      function sphere01_polygon_area ( n, lat, lon )

c*********************************************************************72
c
cc SPHERE01_POLYGON_AREA returns the area of a spherical polygon.
c
c  Discussion:
c
c    On a unit sphere, the area of a spherical polygon with N sides
c    is equal to the spherical excess:
c
c      E = sum ( interior angles ) - ( N - 2 ) * pi.
c
c    On a sphere with radius R, the area is the spherical excess multiplied
c    by R * R.
c
c    The code was revised in accordance with suggestions in Carvalho and
c    Cavalcanti.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 October 2010
c
c  Author:
c
c    Original C version by Robert Miller.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Paulo Cezar Pinto Carvalho, Paulo Roma Cavalcanti,
c    Point in Polyhedron Testing Using Spherical Polygons,
c    in Graphics Gems V,
c    edited by Alan Paeth,
c    Academic Press, 1995,
c    ISBN: 0125434553,
c    LC: T385.G6975.
c
c    Robert Miller,
c    Computing the Area of a Spherical Polygon,
c    Graphics Gems, Volume IV, pages 132-138,
c    Edited by Paul Heckbert,
c    Academic Press, 1994, T385.G6974.
c
c    Eric Weisstein,
c    "Spherical Polygon",
c    CRC Concise Encyclopedia of Mathematics,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, integer N, the number of vertices.
c
c    Input, double precision LAT[N], LON[N], the latitudes and longitudes 
c    of the vertices of the spherical polygon.
c
c    Output, double precision SPHERE01_POLYGON_AREA, the area of the 
c    spherical polygon, measured in spherical radians.
c
      implicit none

      integer n

      double precision a
      double precision area
      double precision b
      double precision beta1
      double precision beta2
      double precision c
      double precision cos_b1
      double precision cos_b2
      double precision excess
      double precision hav_a
      double precision haversine
      integer j
      integer k
      double precision lam
      double precision lam1
      double precision lam2
      double precision lat(n)
      double precision lon(n)
      double precision pi_half 
      parameter ( pi_half = 1.5707963267948966192313D+00 )
      double precision s
      double precision sphere01_polygon_area
      double precision t

      area = 0.0D+00

      do j = 1, n + 1

        if ( j.eq.1 ) then
          lam1 = lon(j)
          beta1 = lat(j)
          lam2 = lon(j+1)
          beta2 = lat(j+1)
          cos_b1 = cos ( beta1 )
          cos_b2 = cos ( beta2 )
        else
          k = mod ( j + 1, n + 1 )
c          if (k.gt.n) print *, 'k > n'
          lam1 = lam2
          beta1 = beta2
          lam2 = lon(k)
          beta2 = lat(k)
          cos_b1 = cos_b2
          cos_b2 = cos ( beta2 )
        end if

        if ( lam1 .ne. lam2 ) then

          hav_a = haversine ( beta2 - beta1 ) 
     &      + cos_b1 * cos_b2 * haversine ( lam2 - lam1 )
          a = 2.0D+00 * asin ( sqrt ( hav_a ) )
          b = pi_half - beta2
          c = pi_half - beta1
          s = 0.5D+00 * ( a + b + c )
c
c  Given the three sides of a spherical triangle, we can use a formula
c  to find the spherical excess.
c

          t = tan ( s / 2.0D+00 ) * tan ( ( s - a ) / 2.0D+00 ) 
     &      * tan ( ( s - b ) / 2.0D+00 ) * tan ( ( s - c ) / 2.0D+00 )

          excess = abs ( 4.0D+00 * atan ( sqrt ( abs ( t ) ) ) )

          if ( lam1.lt.lam2 ) then
            lam = lam2 - lam1
          else
            lam = lam2 - lam1 + 4.0D+00 * pi_half
          end if

          if ( 2.0D+00 * pi_half.lt.lam ) then
            excess = -excess 
          end if

          area = area + excess
 
        end if

      end do

      sphere01_polygon_area = abs ( area )

      return
      end
c***********************************************************************

      function haversine ( a )

c*********************************************************************72
c
cc HAVERSINE computes the haversine of an angle.
c
c  Discussion:
c
c    haversine(A) = ( 1 - cos ( A ) ) / 2
c
c    The haversine is useful in spherical trigonometry.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 October 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the angle.
c
c    Output, double precision HAVERSINE, the haversine of the angle.
c
      implicit none

      double precision a
      double precision haversine

      haversine = ( 1.0D+00 - dcos ( a ) ) / 2.0D+00

      return
      end

c***********************************************************************
c make a grid of indices for a fault
c***********************************************************************

      subroutine make_index_grid (nx,nx1,nx2,nxs,nz,nz1,nz2,nzs,iv)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      dimension iv(MAX_nodes) 
      call clearint( iv, MAX_nodes )


      n = 0
      k0 = 1

      nxrow = int((nx2-nx1+1)/nxs)-1
      nzcol = int((nz2-nz1+1)/nzs)

      do iz = 1,nz
       do jx=1,nx
        n = n + 1
        iv(n) = 0

        if (iz .ge. nz1 .and. iz .le. nz2 .and. jx .ge. nx1 
     .     .and. jx .le. nx2 ) then
         rj = real(jx-nx1+1)-0.001d0
         ri = real(iz-nz1+1)-0.001d0
         nzcol = int( (iz-nz1+1)/(real(nzs)+0.001d0 )  )

        k = 1 + int(rj/nxs) + int(ri/nzs) + (nxrow * nzcol)

        if (iz.eq.nz1 .and. jx.eq.nx1) k0 = k

           iv(n) = k - k0 + 1
        endif
        
       enddo
      enddo

      return
      end

c***********************************************************************
c make a new contour at depth znew, index iznew for fault kf
c***********************************************************************

      subroutine new_contour (kf, iznew, znew, zflag )

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

      logical zflag(MAX_z)

      dz1m = 1000.
      dz2m = 1000.
      iz1 = 0
      iz2 = 0

c get depths above and below new depth
      do iz= 1, nzf(kf)
        if ( .not. zflag (iz) ) then
          dz1 = znew - znode(iz,kf)
          dz2 = znode(iz,kf) - znew
          if ( dz1.gt.0.0d0 .and. dz1.lt.dz1m ) then
            iz1 = iz
            dz1m = dz1
          endif
          if ( dz2.gt.0.0d0 .and. dz2.lt.dz2m ) then
            iz2 = iz
            dz2m = dz2
          endif
        endif
      enddo

      if (iz2.eq.0) then
        print *, '*** ND: no contour below ', znew
        stop
      endif

      znode(iznew,kf) = znew

      do ix=1, nxf(kf)
         dz = (znew-znode(iz1,kf))/(znode(iz2,kf)-znode(iz1,kf))
         x = xynode(1,ix,iz1,kf) + 
     .             dz * (xynode(1,ix,iz2,kf)-xynode(1,ix,iz1,kf))
         y = xynode(2,ix,iz1,kf) + 
     .             dz * (xynode(2,ix,iz2,kf)-xynode(2,ix,iz1,kf))
         xynode(1,ix,iznew,kf) = x
         xynode(2,ix,iznew,kf) = y
      enddo

      return
      end


c***********************************************************************
c make kml files
c***********************************************************************

      subroutine make_kml 

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"

c* commons for kml
      character*80 kmlhdr
      common /kml1/ kmlhdr

      character quote*1, comma*1, block_name*4
      character*256 a,b

      quote = char(34)
      comma = char(44)
      zero = 0.0d0

c      print *, kmlhdr

      call fopen (kl, 1, '_faults.kml ' )

c write kml header
       call fopen (k9, 0, kmlhdr )
        do i=1,100
         read (k9, '(a256)', end=8) aline
         write(kl, '(a256)') aline
        enddo
  8    k = kfclose(k9)

c* write used faults
      do kf = 1, MAX_f
       nx = nxf(kf)
       if (nx.gt.0 .and. fflag(kf,4) ) then
      write (kl, *) '<Placemark> <styleUrl>#RLine</styleUrl> '
      write (kl, 12) fault_name(kf)
  12  format ("<name>",a10,"</name>")


      write (kl, '("<description><ul>")' )
      write (kl,'("<li>Name:",a10,"</li>")') fault_name(kf)
      write (kl,'("<li>Number:",i4,"</li>")') kf
      write (kl,'("</ul></description>")')  

      write (kl, *) '<LineString> <coordinates>'
        do ix=1, nx 
          x = xynode(1,ix,1,kf)
          if (x.gt.180.d0) x = x - 360.0d0
          write (a, '(f12.5,a1,f10.5,a1,f4.1)') x, comma, 
     .        xynode(2,ix,1,kf), comma, zero
          call rmblanks (a, b, k)
          write (kl, *) trim(adjustl(b))
        enddo
      write (kl, *) '</coordinates> </LineString> </Placemark>'
       endif
      enddo
      write (kl, *) '</Document> </kml>'
      k = kfclose(kl)

c Block names

      call fopen (kl, 1, '_blocks.kml ' )

c write kml header
       call fopen (k9, 0, kmlhdr )
        do i=1,100
         read (k9, '(a256)', end=9) aline
         write(kl, '(a256)') aline
        enddo
  9    k = kfclose(k9)

       do jb = 1, nblocks
        if (block_flag(jb) ) then
      write(kl,*) "<Placemark>"
      write(kl, 13) block_name(jb)
  13  format ("<name>",a4,"</name><description><ul>")
      write(kl,'("<li>Name:",1x,a4,"</li></ul></description>")')  
     .        block_name(jb)
      write(kl, '(" <styleUrl>#RedStyle</styleUrl>")' )  
          x = block_centroid(jb,1)
          if (x.gt.180.d0) x = x - 360.0d0

      write(a,16) x, block_centroid(jb,2), zero
  16  format (BN, "<Point><coordinates>",f11.6,",",f11.6,",",f6.1,
     .        "</coordinates></Point>")
          call rmblanks (a, b, k)
          write (kl, *) trim(adjustl(b))

      write(kl,*) "</Placemark>"
      endif
      enddo

      write (kl, *) '</Document> </kml>'

      k = kfclose(kl)


c* write unused faults
      if ( myflag ) then
      call fopen (kl, 1, '_faults_unused.kml ' )

c write kml header
       call fopen (k9, 0, kmlhdr )
        do i=1,100
         read (k9, '(a256)', end=10) aline
         write(kl, '(a256)') aline
        enddo
  10    k = kfclose(k9)

      do kf = 1, MAX_f
       nx = nxf(kf)
       if (nx.gt.0 .and. .not. fflag(kf,4) ) then
      write (kl, *) '<Placemark> <styleUrl>#RLine</styleUrl> '
      write (kl, 12) fault_name(kf)

      write (kl, '("<description><ul>")' )
      write (kl,'("<li>Name:",a10,"</li>")') fault_name(kf)
      write (kl,'("<li>Number:",i4,"</li>")') kf
      write (kl,'("</ul></description>")')  

      write (kl, *) '<LineString> <coordinates>'
        do ix=1, nx 
          x = xynode(1,ix,1,kf)
          if (x.gt.180.d0) x = x - 360.0d0
          write (a, '(f12.5,a1,f10.5,a1,f4.1)') x, comma, 
     .        xynode(2,ix,1,kf), comma, zero
          call rmblanks (a, b, k)
          write (kl, *) trim(adjustl(b))
        enddo
      write (kl, *) '</coordinates> </LineString> </Placemark>'
       endif
      enddo
      write (kl, *) '</Document> </kml>'

      k = kfclose(kl)
      endif

      return
      end

c***********************************************************************
c get slip rate and residual
c***********************************************************************
      subroutine getsr (k, Vx, Vy, Vtot, Vaz, Vz, res, rs)

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      logical minmax, vert

        Vz = 0.0d0
        res = 0.0d0
        rs = 0.0d0

        xpt=sr_pos(k,1)
        ypt=sr_pos(k,2)
        az = sr_az(k)*d2r
        sig = sr_sig(k) 
        ktype = ksr_type(k)

        minmax = ( ktype.eq.1 .or. ktype.eq.2 .or. ktype.eq.5)
        vert =   ( ktype.eq.4 .or. ktype.eq.5 )

c np1 fixed block, np2 moving
        np1 = kblk_sr(k,1)  ! fixed block
        np2 = kblk_sr(k,2)  ! moving block

        call relvel (3, np2, np1, xpt, ypt, Vx, Sx, Vy, Sy, rho)

c total slip rate 
       Vtot = dsqrt ( Vx*Vx + Vy*Vy )

c slip rate in specified azimuth
       Vaz  = Vx * sin(az) + Vy * cos(az)

c vertical slip rate, az is the dip in this case  
c also needs the fault normal rate Vz = Vnorm * tan (dip)
      if ( vert ) Vz  = Vtot * tan(az)

c calculated slip rate
      if ( .not. vert ) sr_calc(k) = Vtot
      if (use_sr_az .and. az .ne. 0.0d0 .and. .not. vert ) 
     .      sr_calc(k) = Vaz
      if (vert) sr_calc(k) = Vz

c get residual
      if ( minmax ) then
       c = sr_calc(k)
       o1 = sr_obs(k,1)
       o2 = sr_obs(k,2)
       if ( c.lt.o1) res = o1-c
       if ( c.gt.o2) res = o2-c
      else
        res = sr_obs(k,1) - sr_calc(k) 
      endif

      if (sig.gt.0.0d0) rs = res/sig

      return
      end

c*********************************************************************
      subroutine get_t_slice

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      character sc3*3, tc3*3, cdum*10, c4a*4, c4b*4, c4*4
      character YN*1

      dimension nsrcs(MAX_srce)

      dimension snode(MAX_xin, MAX_zin, 2, 4), 
     .          anode(MAX_xin, MAX_zin, 4),
     .          tnode(MAX_xin, MAX_zin), s(5), u(3),
     .          Ugps(MAX_gps,3,2), 
     .          astf(5000), tstf(5000) 

      dimension xsub(MAX_xin,MAX_zin,MAX_nix,MAX_niy,4,3),
     .          usub(MAX_xin,MAX_zin,MAX_nix,MAX_niy,3,2),
     .          psub(MAX_xin,MAX_zin,MAX_nix,MAX_niy,4)

      max_stf = 5000
      nxz = MAX_nix*MAX_niy
      mxz = MAX_xin*MAX_zin
      nslice = 0

c       print *, 'MAX ', MAX_xin,MAX_zin,MAX_nix,MAX_niy

       

      do its = 1, MAX_t_slice

        t1 = tslice(its, 1)
        t2 = tslice(its, 2)

       if (t2.gt.t1 .and. t2.gt.zero ) then

        nslice = nslice + 1
        if (nslice.eq.1) call fopen (kf5,  1, '_slice.times ')
        if (nslice .ge. 1) write(kf5, '(i5,2f10.3 )' ) its, t1,t2

        call cleareal(u, 3)
        call cleareal(snode, 8*mxz)
        call cleareal(anode, 4*mxz)
        call cleareal(usub,  6*nxz*mxz)
        call cleareal(xsub, 12*nxz*mxz)
        call cleareal(psub,  4*nxz*mxz)
        call cleareal(Ugps,  6*MAX_gps)

        itmax = 0
        jtmax = 0
        ixtmax = 0
        iztmax = 0
        nsrc = 0

       print *, 'Processing time slice ', its

      do nt = 1, MAX_srce  

         kf =  info_source(nt,1)
         
       if ( sflag(nt) .and. kf.eq.kfslice(its) .and. 
     .        tsliceflag(its, nt) ) then

c       print *, 'Doing DW ', its, ' NT ', nt
       ixmax =0
       iymax = 0

       nsrc = nsrc + 1
       nsrcs(nsrc) = nt
c       print *, 'Processing event ', nt

c get total slip at nodes
        call i2c (nt, 3, sc3)
        call fopen (kf2,  1, '_src_'//sc3//'.nod ')
         do  inn = 1, MAX_nodes
          read(kf2, 503, end = 11)  cdum, kff, ix, iz,
     .    c4a, c4b, xpt, ypt, zpt, pp, pe, 
     .    (s(k), k=1,5), px, py, slipaz,
     .    xstrike, xtmp, wtmp, fstrike, fdip, Tarr, xtdelay, xmom 

          tnode(ix, iz)    = Tarr
          anode(ix, iz, 1) = pp
          anode(ix, iz, 2) = px
          anode(ix, iz, 3) = py
          anode(ix, iz, 4) = xmom

        enddo
   11   ik = kfclose(kf2)

c  503 format (a10, 3i4, 2(1x,a4), 1x, 3f9.3, 2f10.1, 4f8.1, f8.4, 
c     .   2f10.1, 6f8.1,f10.4)

  503 format (a10, 3i4, 2(1x,a4), 1x, 3f9.3, 2f10.1, 4f8.1, f8.4, 
     .   2f10.1, 6f8.1, 2f10.4, e12.4)

c get time dependence
       call fopen (kf3,  1, '_src_'//sc3//'.stf ')
       ntimes = 0
       do ii = 1, max_stf
        read (kf3,'(i5, f15.5, 45x, f15.5)', end = 12) k, tst, ast 
         tstf(k) = tst
         astf(k) = ast
       enddo
  12  ik = kfclose(kf3)
      ntimes = k


c process .disp file
       call fopen (katr, 1,  '_src_'//sc3//'.disp ') 

   2  read(katr, 126, end = 13) longname, xpt, ypt, 
     .   ( u(j), j=1,3 ), 
     .   erh, erh, erz, YN, ts0, ts1, ts2,
     .   dum, dum, dum, Tarr, kgps

c get stf multipliers at both times 
      call get_at1_at2 (ntimes, max_stf, tstf, astf, Tarr, 
     .  t1, t2, at1, at2)

      do j=1,3
        Ugps(kgps,j,1) = Ugps(kgps,j,1) + u(j)*at1
        Ugps(kgps,j,2) = Ugps(kgps,j,2) + u(j)*at2
      enddo

      goto 2

c 126  format(a23, 2f10.4, 3f10.2, 3f8.2,  
c     .     1x,a1,1x,3f9.2,3f8.2,f9.3,i6)

 126  format(a23, 2f10.4, 3f10.2, 3f8.2,  
     .     1x,a1,1x,3f9.3,3f10.2,f9.3,i6)

  13  ik = kfclose(katr)


cc read and sum .atr files
       call fopen (katr,  1, '_src_'//sc3//'.atr ')

   1   read(katr, 107, end = 108 ) c4,
     .    kff, uslip, (u(j), j=1,3), Uc1, Vc1, rake, 
     .    xft, yft, zft, ntr, ixt, izt, it, jt, Tarr, Pcent, area

 107  format(a4,1x,i3,9f12.3,f7.2, 5i4, f10.4, f10.1, 1x, 1pe12.4)
c 107  format(a4,1x,i3,9f12.3,f7.2, 5i4, f10.4, 1x, e12.4)

        ixmax = max(it, ixmax)
        iymax = max(jt, iymax)

        itmax = max(it, itmax)
        jtmax = max(jt, jtmax)
        ixtmax = max(ixt, ixtmax)
        iztmax = max(izt, iztmax)

        psub(ixt,izt,it,jt,1) = xft
        psub(ixt,izt,it,jt,2) = yft
        psub(ixt,izt,it,jt,3) = zft
        psub(ixt,izt,it,jt,4) = area

c get stf multipliers at both times 
      call get_at1_at2 (ntimes, max_stf, tstf, astf, Tarr, 
     .  t1, t2, at1, at2)

        do k=1,3
         usub(ixt,izt,it,jt,k,1) = usub(ixt,izt,it,jt,k,1) + u(k)*at1
         usub(ixt,izt,it,jt,k,2) = usub(ixt,izt,it,jt,k,2) + u(k)*at2
        enddo

        do ib = 1,4
          read(katr, 106) (xsub(ixt,izt,it,jt,ib,k), k=1,3)
        enddo

      goto 1

c      enddo

 108  ik = kfclose(katr)

 106  format(3f16.8)
c 107  format(a4,1x,i3,9f10.3,f7.2, 5i4, f10.4)
c 107  format(a4,1x,i3,9f12.3,f7.2, 5i4, f10.4, 1x, e12.4)
c 107  format(a4,1x,i3,9f12.3,f7.2, 5i4, f10.4, f10.1, 1x, 1pe12.4)

c      print *, 'Source ', nt, ' max ', ixmax, iymax


c now interpolate at each node
c get stf multipliers at both times 

       do ix = 1, nxf(kf)        
        do iz = 1, nzf(kf)   
        
       call get_at1_at2 (ntimes, max_stf, tstf, astf, tnode(ix,iz), 
     .  t1, t2, at1, at2)

         do j=1,4
          snode(ix,iz,1,j) = snode(ix,iz,1,j) + at1*anode(ix,iz,j)
          snode(ix,iz,2,j) = snode(ix,iz,2,j) + at2*anode(ix,iz,j)
         enddo

        enddo
       enddo


       endif
       enddo

c ***************************************************
c write out files

       call i2c (its, 3, tc3)

c time .nod file
      kf3 = 40
      kf = kfslice(its)
      tmom = 0.0d0
      call fopen (kf3,  1, '_time_'//tc3//'.nod ')
      write (kf3, '("# ", 2f10.4, 40i3)') t1, t2, (nsrcs(j),j=1,nsrc)

      do ix = 1, nxf(kf)        
       do iz = 1, nzf(kf)   
        xpt = xynode(1,ix,iz,kf)
        ypt = xynode(2,ix,iz,kf)
        zpt = znode(iz,kf)
        write (kf3, '(2i4, 6f12.3, 1pe12.4)' )
     .     ix, iz, xpt, ypt, zpt, 
     .     (snode(ix,iz,2,j) - snode(ix,iz,1,j),j=1,4)
        tmom = tmom + snode(ix,iz,2,4) - snode(ix,iz,1,4)
       enddo
      enddo
      ik = kfclose(kf3)
c      print *, 'Tmom =', tmom

c write time .atr file
      call fopen (kf3,  1, '_time_'//tc3//'.atr ')
      write (kf3, '("# ", 2f10.4, 40i3)') t1, t2, (nsrcs(j),j=1,nsrc)
      c4 = '> -Z'
      do it=1,itmax
       do jt=1,jtmax
        do ixt=1,ixtmax
         do izt=1,iztmax
          uslip = 0.0d0

        do k=1,3 
         u(k) = usub(ixt,izt,it,jt,k,2) - usub(ixt,izt,it,jt,k,1)
         uslip = uslip + u(k)** 2
        enddo
c         xmom = usub(ixt,izt,it,jt,4,2) - usub(ixt,izt,it,jt,4,1)

c rake angle
        frake = fn180(datan2 (u(2),u(1))*r2d)
        uslip = dsqrt(uslip)

        if (uslip .ge. tr_umin ) then

         write(kf3, 110 ) c4, kfslice(its), uslip, 
     .    (u(k),k=1,3), frake, (psub(ixt,izt,it,jt,k),k=1,3),
     .    ixt, izt, it, jt, psub(ixt,izt,it,jt,4)

        do ib = 1,4
         write(kf3, 106) (xsub(ixt,izt,it,jt,ib,k), k=1,3)
        enddo
       endif

         enddo
        enddo
       enddo
      enddo      

 110  format(a4,1x,i3,7f12.3,f10.2, 4i4, 1x, 1pe12.4)
      ik = kfclose(kf3)

c GPS displacement file
       call fopen (kf3,  1, '_time_'//tc3//'.disp ')
       write (kf3, '("# ", 2f10.4, 40i3)') t1, t2, (nsrcs(j),j=1,nsrc)
       do i = 1, num_gps 
       uslip = 0.0d0
       do j=1,3
         u(j) = Ugps(i,j,2) - Ugps(i,j,1) 
         uslip = uslip + u(j)**2
       enddo
        if (uslip.gt.0.0d0 .and. ndx(i,1).gt.0 ) then
         uslip = dsqrt(uslip)

         call gpslongname(i) 
         call gpsinfo2(i, xpt, ypt, xobs, yobs, zobs, 
     .     xcalc, ycalc, zcalc, sigx, sigy, sigz, ss)

c see if site running during this time window
         ts1 = t_disp(ndx(i,1))
         ts2 = t_disp(ndx(i,2))
         YN = 'N'
         if ( ts1 .le. t1 .and. ts2 .ge. t2 ) YN = 'A'
         if ( ts1.gt.t1 .and. ts2.gt.t2 ) YN = 'P'
         if ( ts1 .le. t1 .and. ts2.lt.t2 ) YN = 'P'
         if ( ts1.gt.t1 .and. ts2.lt.t2 ) YN = 'P'
         if ( ts1.lt.t1 .and. ts2.lt.t1 ) YN = 'N'
         if ( ts1.gt.t2 .and. ts2.gt.t2 ) YN = 'N'

         write(kf3, 128) longname, xpt, ypt, 
     .     (u(j), j=1,3 ), YN, ts1, ts2 
        endif

       enddo
       ik = kfclose(kf3)

 128  format(a23, 2f10.4, 3f12.2, 1x,a1,1x, 2f10.4)

       endif


       enddo

       if (nslice .ge. 1) ik = kfclose(kf5)

      return
      end

c*******************************************************
c get amplitude multipliers for start and end of window
      subroutine get_at1_at2 (ntimes, max_stf, tstf, astf, Tarr, 
     .  t1, t2, at1, at2)

      implicit real*8 (a-h,o-z)

      dimension astf(max_stf), tstf(max_stf) 

c get stf multipliers at both times 
         at1 = 0.0d0
         at2 = 0.0d0

         do j=1, ntimes-1
           tb = Tarr + tstf(j)
           ta = Tarr + tstf(j+1)

c find the time and interpolate
          if ( t1 .ge. tb .and. t1.lt.ta ) then
              ab = astf(j)
              aa = astf(j+1)
              at1 = ab + (t1-tb)/(ta-tb) * (aa-ab)
          endif

          if ( t2 .ge. tb .and. t2.lt.ta ) then
              ab = astf(j)
              aa = astf(j+1)
              at2 = ab + (t2-tb)/(ta-tb) * (aa-ab)
          endif

         enddo

         if (t1.lt.Tarr ) at1 = 0.0d0
         if (t2.lt.Tarr ) at2 = 0.0d0
         if (t1.gt.Tarr+tstf(ntimes)) at1 = astf(ntimes)
         if (t2.gt.Tarr+tstf(ntimes)) at2 = astf(ntimes)

      return
      end

c*******************************************************      
c add a new transient parameter or equate with another

      subroutine newtparm (nt, k, kptype)
      implicit real*8 (a-h,o-z)
      
       include "tdefcom1.h"

       nptmp = nparms

c check for equated parameter         
      isequated = 0
      if ( npars_eq.gt.0 ) then
       do i=1, npars_eq
        if ( npar_eq_type(i).eq.3 ) then
         nt1 = min(npar_eq(i,1), npar_eq(i,2))
         nt2 = max(npar_eq(i,1), npar_eq(i,2))
         np1 = npar_eq(i,3)
         if (nt2.eq.nt .and. np1.eq.kptype) then
           isequated = nsource_parm(nt1, k)
           nptmp = isequated
           print *, 'Equating Par',np1,' Srces',nt1,nt2, 'as', nptmp
         endif
        endif
       enddo
      endif
      isequated = 0

c new parameter if not equated
       if ( isequated.eq.0 ) then
         nparms = nparms + 1
         nptmp = nparms
       endif

       parm(nptmp) = transient(nt,k)
       nsource_parm(nt, k) = nptmp
       kparm_type(nptmp) = kptype
       kparm_index(nptmp) = nt

      return
      end
 
c***********************************************************************
c Yang et al subroutines from Alessandro Bonaccorso
c email set by Mike L 2/7/2014

      SUBROUTINE PROLATE(DU, DV, DW, Xs,Ys,Zs, Xpr,Ypr,Zpr, 
     .                   PHI, DIP, A, AB, Pwr)

      implicit real*8 (a-h,o-z)
c      complex*16 Ci

      logical volume

c DU,DV,DW displacements in mm
c Xpr Ypr Zpr location of center of prolate spheroid (lon,lat, km)
c Xs Ys Zs location of observation point (lon,lat, km)
c A is semimajor axis length (km)
c AB is ratio A/B - B semiminor length
c PHI - azimuth of semimajor axis (degrees cwise from N)
c DIP - dip of semimajor axis (degrees from horizontal)
c PN - deltaP/mu (unitless)
c 
      pi   = 3.14159265358979d0 
      one80 = 180.d0
      rpd = pi/one80
      thou = 1.0d3
      xmu = 30.0d10
      xnu = 0.25d0
      xn2 = -2.0d0
      volume = .true.

c      if( DIP .gt. 90.0d0 ) then 
c       DIP = one80-dip
c       PHI = PHI+one80
c      endif
c      if( DIP.lt.0.0d0 ) then 
c       DIP = abs(dip)
c       PHI = PHI+one80
c      endif

      DU=0.
      DV=0.
      DW=0.
      U1=0.
      U2=0.
      U3=0.

c convert to meters
      Ax = A * thou
      B = Ax

c Prolate, ok
      if (AB .ge. 1.001d0 ) then
         B = Ax/AB
         Cx =  dsqrt(Ax*Ax-B*B)

c Oblate, not ok
      elseif ( AB.lt.0.999d0 ) then
c         B = Ax/AB
c         Ci =  dsqrt(Ax*Ax-B*B)
c         Cx =  aimag(Ci)
c         print *, 'AB lt 1 ', AB, Cx, Ci
          DU=1.0d6
          DV=1.0d6
          DW=1.0d6
        return

c Spherical, use Mogi
      else  
c        call MOGI1 (Xpr, Ypr, Zpr, Pwr, Xs, Ys, Zs, du, dv, dw)
        call MOGI2 (Xpr, Ypr, Zpr, Pwr, Xs, Ys, Zs, du, dv, dw)
c        call MOGI3 (Xpr, Ypr, Zpr, Pwr, Xs, Ys, Zs, du, dv, dw)
        return
      endif

c      Cx =  dsqrt( dsqrt((Ax*Ax-B*B)**2))


c conversion
c scale to ML test
c if Volume, use expression from Peter Cervelli's Matlab code
      if(volume) then
       c=Cx
       a=Ax
       xL0 = 1.0d-30
       if(a.gt.c) xL0 = dlog((a-c)/(a+c))
       a2 = a*a
       b2 = b*b
       C0 = 4.0*(1.0-xnu) 
       C3 = C0 + 1.0
       d = 1.0/(b2*c*(2.0*b2-C0*a2)*xL0-
     .        a*c*c*(4.0*b2+C0*a2)+a*b**4*(1.0+xnu)*xL0**2)
       a1 = d*b2*(2.0*c*(a2+2.0*b2)+3.0*a*b2*xL0)
       b1 = d*(a2*c*C0+b2*(c+C3*(c+a*xL0)))

       PN = (3.0*Pwr) / (8.0*b2*pi*(xn2*b1*c**3 - 
     .     a1*(a*xL0*(1.0-2.0*xnu)+c*C3)))

c factor needed to put into units of Mm^3 (Millions of cubic meters)
c this agrees with Mike L's test cases
       PN = PN*4.0d6

c = 4/3 pi R^3 = 4188.787
c       PN = PN*(4.0d0/3.0d0 * pi * 1.0d3)

      else
c if Pressure, from Cervelli
       PN = Pwr  / (4.0d0*xmu)
c       print *, 'P', PN
      endif
c       PN = Pwr  


c-- translate origin and convert observation point lat/lon (Xs,Ys) to Cartesian (X1,Y1) in kms
c   relative to Spheroid (Xpr,Ypr) origin
      call ps_project( Xs, Ys, Xpr, Ypr, X1, X2)

c      if(Ax.eq.B) then
c        Xm = 0.0
c        Ym = 0.0
c        call MOGI (Xm, Ym, Zpr, PN, Ax, X1, Y2, Zs, DU, DV, DW)
c        return
c       endif

c for test
c      eRad=6370997.2d0 
c      eCir=2.0d0*pi*eRad 
c      xmPerDeg=(eCir/360.0d0)
c      X1 = (Xs-Xpr)*xmPerDeg
c      X2 = (Ys-Ypr)*xmPerDeg

c RM negative to match MBattaglia tests
c RM add 180 so that azimuth is in direction of down-dip
      FI=(180.0d0-PHI)*rpd
c Battaglia
c      FI=PHI*rpd
c Cervelli
c       FI= (90.0d0-PHI)*rpd
c Bonnaccorso
c      FI=PHI*rpd

c convert to meters
      XX1= X1*thou
      XX2= X2*thou
      Z0 = Zpr*thou
c observation height, pos or neg?
      X3 = Zs*thou
      X3 = 0.0d0

      X1=XX1*DCOS(FI)+XX2*DSIN(FI)
      X2=XX2*DCOS(FI)-XX1*DSIN(FI)
c      print *, "A",xs,ys,x1,x2

c Battaglia
c      X1=XX1*DCOS(FI)-XX2*DSIN(FI)
c      X2=XX2*DCOS(FI)+XX1*DSIN(FI)
c      print *, "B",xs,ys,x1,x2

c Cervelli
c      X1= XX1*DCOS(FI)+XX2*DSIN(FI)
c      X2=-XX1*DSIN(FI)+XX2*DCOS(FI)
c      print *, "B",xs,ys,x1,x2

c Bonnacorso
c      X1=XX1*DCOS(FI)+XX2*DSIN(FI)
c      X2=XX2*DCOS(FI)-XX1*DSIN(FI)
c      X3=0.D0

      CALL ELLIPU(U1,U2,U3,X1,X2,X3,Z0,DIP,AX,B,CX,PN)
c      print *, U1,U2,U3
 
      UU1=U1
      UU2=U2
      U1=UU1*DCOS(FI)-UU2*DSIN(FI)
      U2=UU1*DSIN(FI)+UU2*DCOS(FI)
c Battaglia
c      U1=UU1*DCOS(FI)+UU2*DSIN(FI)
c      U2=UU1*DSIN(FI)-UU2*DCOS(FI)
c Cervelli
c      U1=UU1*DCOS(FI)-UU2*DSIN(FI)
c      U2=UU1*DSIN(FI)+UU2*DCOS(FI)

c RM change sign of East and North, convert to mm
      DU = -U1*thou
      DV = -U2*thou
c Battaglia
c      DU = U1*thou
c      DV = U2*thou
      DW = U3*thou
c
      RETURN
      END
c
C
      SUBROUTINE ELLIPU(U1,U2,U3,X1,X2,X3,Z0,ST,AX,B,CX,PN)
      IMPLICIT REAL*8(A-H,O-Z)
      pi   = 3.14159265358979d0 
      rpd = pi/180.0d0
      thou = 1.0d3
      X=X1
      Y=X2
      Z=X3
      ZZ0=Z0
      ANG=ST
      A=AX
      C=CX
      P=PN/thou
      AL=1.D0
      AM=1.D0
      SI=AL/2.D0/(AL+AM)
      CA=DCOS(ANG*rpd)
c      B=DSQRT(DSQRT((A*A-C*C)**2))
      CALL PF(PC,PD,P,AL,AM,SI,A,B,C)
      D1=-2.D0*B**2*PD
      D2=3.D0*B**2/C**2*PD+2*(1.D0-2.D0*SI)*PC
C     WRITE(3,15)PC,PD,D1,D2
c   15 FORMAT(1X,'PC=',1X,F10.6,5X,'PD=',1X,F10.6, 
c     .       5X,'D1=',1X,F10.6,5X,'D2=',1X,F10.6//)  
      V=A*B**2/C**3/(16.D0*AM*(1.D0-SI))
      CALL ELLIP(W1,W2,W3,FF1,FF2,FF3,
     .           D1,D2,PD,X,Y,Z,C,ANG,ZZ0,SI)
      UFF1=FF1
      UFF2=FF2
      UFF3=FF3
      U1=W1
      U2=W2
      U3=W3
      CALL ELLIP(W1,W2,W3,FF1,FF2,FF3,
     .           D1,D2,PD,X,Y,Z,-C,ANG,ZZ0,SI)
      UFF1=(UFF1-FF1)/CA**2
      UFF2=(UFF2-FF2)/CA**2
      UFF3=(UFF3-FF3)/CA
      U1=V*(U1-W1+UFF1)*thou
      U2=V*(U2-W2+UFF2)*thou
      U3=V*(U3-W3+UFF3)*thou

C      WRITE(3,20)X,Y,Z,U1,U2,U3
c   20 FORMAT(1X,'X=',1X,F6.2,5X,'Y=',1X,F6.2,5X,'Z=',1X,F6.2,
c     .      10X,'U1=',1X,F10.6,10X,'U2=',1X,F10.6,10X,'U3=',1X,F10.6)
      RETURN
      END
C
C
      SUBROUTINE ELLIP(U1,U2,U3,FF1,FF2,FF3, 
     .                 D1,D2,PD,X,Y,Z,C,ANG,ZZ0,SI)
      IMPLICIT REAL*8(A-H,O-Z)
      pi   = 3.14159265358979d0 
      rpd = pi/180.0d0
      SA=DSIN(ANG*rpd)
      CA=DCOS(ANG*rpd)
      C2=CA*C
      C3=SA*C
      Y2=Y
      Y3=Z-ZZ0
      T2=SA*Y2-CA*Y3
      T3=CA*Y2+SA*Y3
      TT3=T3-C
      YY2=Y
      YY3=Z+ZZ0
      Q2=SA*YY2+CA*YY3
      Q3=-CA*YY2+SA*YY3
      QQ3=Q3+C
      X1=X+.0000001D0
      X2=Y2-C2
      X3=Y3-C3
      XX3=YY3+C3
      C0=ZZ0/SA
      R1=DSQRT(X1*X1+X2*X2+X3*X3)
      R2=DSQRT(X1*X1+X2*X2+XX3*XX3)
      BT1=CA*Q2+(1.+SA)*(R2+QQ3)
      BT2=CA*X1
      A1=C/R1+DLOG(R1+TT3)
      A2=R1-T3*DLOG(R1+TT3)
      A3=C*TT3/R1+R1
      AA1=C/R2-DLOG(R2+QQ3)
      AA2=R2-Q3*DLOG(R2+QQ3)
      AA3=C*QQ3/R2-R2
      B=C*(C+C0)/R2-AA2-C0*DLOG(R2+QQ3)
      F1=-2.D0*SA*Z*(C*(C+C0)/R2**3+(R2+C+C0)/R2/(R2+QQ3)
     .   +4.D0*(1.D0-SI)*(R2+C)/R2/(R2+QQ3))
      F2=-2.D0*SA*Z*(C*(C+C0)*QQ3/R2**3+C0/R2+AA1
     .   +4.D0*(1.D0-SI)*AA1)
      FF1=CA**2*C*X1/(R2+XX3)+3.D0*(SA*X1*DLOG(R2+XX3)
     .    -X1*DLOG(R2+QQ3)+2.D0*Q2*DATAN2(BT1,BT2))
     .    +CA**2*2.D0*X1*DLOG(R2+QQ3)-4.D0*YY3*DATAN2(BT1,BT2)*CA
      FF2=CA**2*C*X2/(R2+XX3)+3.D0*(SA*Q2*DLOG(R2+QQ3)
     .    -Q2*DLOG(R2+XX3)+2.D0*SA*X1*DATAN2(BT1,BT2)
     .    +CA*(R2-XX3))-2.D0*CA**3*AA2
     .    +2.D0*CA*(YY3*DLOG(R2+XX3)-Q3*DLOG(R2+QQ3)) 
      FF3=(Q2*DLOG(R2+QQ3)-Q2*SA*DLOG(R2+XX3)
     .    +2.D0*X1*DATAN2(BT1,BT2))
     .    +(2.D0*SA*AA2+Q3*DLOG(R2+XX3)-C)*CA
      AC=D1/R1/(R1+TT3)+D2*(DLOG(R1+TT3)+(T3+C)/(R1+TT3))
      AAC=-D1/R2/(R2+QQ3)-D2*(DLOG(R2+QQ3)+(Q3-C)/(R2+QQ3))
      BC=(D1/R1+2.D0*D2*A2)+(3.D0-4.D0*SI)*(D1/R2+2.D0*D2*AA2)
      FC1=2.D0*Z*(CA*Q2*(D1*(2.D0*R2+QQ3)/R2**3/(R2+QQ3)**2
     .    -D2*(R2+2.D0*C)/R2/(R2+QQ3)**2)
     .    +SA*(D1/R2**3-2.D0*D2*(R2+C)/R2/(R2+QQ3)))
      FC2=2.D0*Z*(D1*XX3/R2**3
     .    -2.D0*D2*(SA*AA1+CA*Q2*(R2+C)/R2/(R2+QQ3)))
      UC1=(AC+(3.D0-4.D0*SI)*AAC+FC1)*X1
      UC2=SA*(AC*T2+(3.D0-4.D0*SI)*AAC*Q2+FC1*Q2)
     .    +CA*(BC-FC2)+2.D0*SA*CA*Z*AAC
      UC3=-CA*(AC*T2+(3.D0-4.D0*SI)*Q2*AAC-FC1*Q2)
     .    +SA*(BC+FC2)+2.D0*CA**2*Z*AAC
      UD1=X1*A1+X1*((3.D0-4.D0*SI)*AA1+F1)
      UD2=SA*(T2*A1+Q2*((3.D0-4.D0*SI)*AA1+F1))
     .    +4.D0*(1.D0-SI)*CA*(A2+AA2)
     .    +CA*(A3-(3.D0-4.D0*SI)*AA3-F2)
      UD3=CA*(-T2*A1+Q2*((3.D0-4.D0*SI)*AA1+F1))
     .    +4.D0*(1.D0-SI)*SA*(A2+AA2)
     .    +SA*(A3+(3.D0-4.D0*SI)*AA3+F2-2.D0*(3.D0-4.D0*SI)*B)
      U1=UC1+2.D0*PD*UD1
      U2=UC2+2.D0*PD*UD2
      U3=UC3+2.D0*PD*UD3
      FF1=-2.D0*PD*4.D0*(1.D0-SI)*(1.D0-2.D0*SI)*FF1
      FF2=-2.D0*PD*4.D0*(1.D0-SI)*(1.D0-2.D0*SI)*FF2
      FF3= 2.D0*PD*4.D0*(1.D0-SI)*(1.D0-2.D0*SI)*FF3
      RETURN
      END
C

      SUBROUTINE PF(PC,PD,P,AL,AM,SI,A,B,C)
      IMPLICIT REAL*8(A-H,O-Z)
      pi   = 3.14159265358979d0 
      AI=-2.D0*PI*A*B**2*(2.D0/A/C**2
     .   +0.5d0*DLOG(((A-C)/(A+C))**2)/C**3)
      AAI=-2.D0*PI*A*B**2*(2.D0/3.D0/C**2/A**3+2.D0/A/C**4
     .    +0.5d0*DLOG(((A-C)/(A+C))**2)/C**5)
      U=8.D0*PI*(1.D0-SI)
      Q=3.D0/U
      R=(1.D0-2.D0*SI)/U
      A11=2.D0*R*(AI-4.D0*PI)
      A12=-2.D0*R*(AI+4.D0*PI)
      A21=Q*A**2*AAI+R*AI-1
      A22=-(Q*A**2*AAI+(2.D0*R-Q)*AI)
      W=(A11*A22-A12*A21)*(3.D0*AL+2.D0*AM)
      E11=(3.D0*A22-A12)*P/W      
      E22=(A11-3.D0*A21)*P/W
      PD=2.D0*AM*(E11-E22)
      PC=AL*E11+2.D0*(AL+AM)*E22
      RETURN
      END 
C

c**********************************************************************   
c point source Mogi, Pm is Volume change   
      subroutine MOGI1 (Xm, Ym, Zm, Pm, Xs, Ys, Zs, u1, u2, u3)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"

      call delaz(Ym,Xm,Ys,Xs,del,az)
          rkm = del*d2x 
          zkm = Zm + Zs
c 4/24/14 scaling so amplitude (volume change) is in units of millions of m^3 (Mm3)
c    tested against data sent by Mike L.
          aMogi = Pm*3.0d0/(4.0d0*pii)  
          den = dsqrt((rkm*rkm +zkm*zkm)**3) 
          u1 = -aMogi*dsin(az*d2r)*rkm / den
          u2 = -aMogi*dcos(az*d2r)*rkm / den 
          u3 =  aMogi*zkm / den 
      return
      end

c***********************************************************************
c Mogi finite source taken from Peter Cervelli Matlab code
c***********************************************************************
      SUBROUTINE MOGI2 (Xm, Ym, Zm, Pm, Xs, Ys, Zs, Ux, Uy, Uz)
      IMPLICIT REAL*8(A-H,O-Z)
      include "tdefcons.h"
      logical sphcor

      thou = 1.0d3
      Am = 1.0d3
      one = 1.0d0
      six = 6.0d0
      two = 2.0d0
      Zmogi = -Zm*thou
      sphcor = .false.

      C5 = Pm/(4.0d0*pii)  * thou * thou

      C0 = 4.0d0*(one - poisrat )
      C2 = C0 - one
      C4 = C0 - 3.0d0

      call ps_project( Xs, Ys, Xm, Ym, X1, X2)

c convert to meters
      x1 = x1*thou
      x2 = x2*thou
      x3 = Zs*thou

      x3hat = X3 - Zmogi 
      x3bar = X3 + Zmogi 

      s1 = dsqrt(x1**2 +x2**2 + x3hat**2)
      s2 = dsqrt(x1**2 +x2**2 + x3bar**2)
      s13 = s1**3
      s15 = s1**5
      s22 = s2**2
      s23 = s2**3
      s25 = s2**5

c with correction
      if (sphcor) then

       Ux = x1*(one/s13 + C2/s23 + six*x3*x3bar/s25)
       Uy = x2*(one/s13 + C2/s23 + six*x3*x3bar/s25)
       Uz = x3hat/s13 + (-C4*x3bar - two*Zmogi)/s23 + 
     .       six*x3*x3bar/s25
      else
c no correction
       Ux = x1*(one/s13)
       Uy = x2*(one/s13)
       Uz = x3hat/s13 
      endif

c convert to mm
       Ux = C5*Ux*thou
       Uy = C5*Uy*thou
       Uz = C5*Uz*thou

c 
c    s1 = sqrt(x1.^2 +x2.^2 + x3hat.^2);
c    s2 = sqrt(x1.^2 +x2.^2 + x3bar.^2);
c    s13 = s1.^3;
c    s15 = s1.^5;
c    s22 = s2.^2;
c    s23 = s2.^3;
c    s25 = s2.^5;
c    u = C5*[x1.*(1./s13 + C2./s23 + 6*x3.*x3bar./s25)
c            x2.*(1./s13 + C2./s23 + 6*x3.*x3bar./s25)
c            x3hat./s13 + (-C4.*x3bar - 2*X3)./s23 + 6*x3.*x3bar./s25];

      RETURN
      END 
c**********************************************************************   
c point source Mogi from Segall 7.14
c Pm is the volume change, Mm3
c same as MOGI1 if poisratio = 0.25
      subroutine MOGI3 (Xm, Ym, Zm, Pm, Xs, Ys, Zs, u1, u2, u3)
      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcons.h"
  
      thou = 1.0d3

      call delaz(Ym,Xm,Ys,Xs,del,az)
c use meters
      rkm = del*d2x*thou 
      zkm = (Zm + Zs)*thou
c Pm in Mm3, convert to cubic meters
      aMogi = Pm*1.0d6*(one-poisrat)/pii  
      den = dsqrt((rkm*rkm +zkm*zkm)**3) 
c convert to mm
      u1 = -aMogi*dsin(az*d2r)*rkm * thou / den
      u2 = -aMogi*dcos(az*d2r)*rkm * thou / den 
      u3 =  aMogi * zkm * thou / den 
      return
      end

c**********************************************************************   
c below is attemp to use oblate spheroid (ie B>A, C is complex)
      SUBROUTINE comPROLATE(DU, DV, DW, Xs,Ys,Zs, Xpr,Ypr,Zpr, 
     .                   PHI, DIP, A, AoB, Pwr)

c      implicit complex*16 (A-H,O-Z)
c      real*8 DU, DV, DW, Xs,Ys,Zs, Xpr,Ypr,Zpr, 
c     .  PHI, DIP, A, AB, Pwr, X1,X2,thou,pi,one80,rpd,xmu,xnu,xn2
      implicit real*8 (A-H,O-Z)
      complex*16 Ci, Cx

      logical volume

c DU,DV,DW displacements in mm
c Xpr Ypr Zpr location of center of prolate spheroid (lon,lat, km)
c Xs Ys Zs location of observation point (lon,lat, km)
c A is semimajor axis length (km)
c AoB is ratio A/B - B semiminor length
c PHI - azimuth of semimajor axis (degrees cwise from N)
c DIP - dip of semimajor axis (degrees from horizontal)
c PN - deltaP/mu (unitless)
c 
      pi   = 3.14159265358979d0 
      one80 = 180.d0
      rpd = pi/one80
      thou = 1.0d3
      xmu = 30.0d10
      xnu = 0.25d0
      xn2 = -2.0d0
      volume = .true.

c      if( DIP .gt. 90.0d0 ) then 
c       DIP = one80-dip
c       PHI = PHI+one80
c      endif
c      if( DIP.lt.0.0d0 ) then 
c       DIP = abs(dip)
c       PHI = PHI+one80
c      endif

      DU=0.
      DV=0.
      DW=0.
      U1=0.
      U2=0.
      U3=0.

c convert to meters
      Ax = A * thou
      B = Ax
      Cx = 0.0d0
      if (AoB .ge. 1.0d0 ) then
         B = Ax/AoB
         Cx =  sqrt(Ax*Ax-B*B)
      elseif ( AoB.lt.1.0d0 .and. AoB .gt. 0.0d0 ) then
         B = Ax/AoB
         Ci =  sqrt(Ax*Ax-B*B)
         Cx =  Ci
c      else 
c         print *, 'A/B = 0.0 '
      endif

c      Cx =  dsqrt( dsqrt((Ax*Ax-B*B)**2))


c conversion
c scale to ML test
c if Volume, use expression from Peter Cervelli's Matlab code
      if(volume) then
       c=Cx
       a=Ax
c       xL0 = 1.0d-20
c       if(a.gt.c) xL0 = log((a-c)/(a+c))
       xL0 = log((a-c)/(a+c))
       a2 = a*a
       b2 = b*b
       C0 = 4.0*(1.0-xnu) 
       C3 = C0 + 1.0
       d = 1.0/(b2*c*(2.0*b2-C0*a2)*xL0-
     .        a*c*c*(4.0*b2+C0*a2)+a*b**4*(1.0+xnu)*xL0**2)
       a1 = d*b2*(2.0*c*(a2+2.0*b2)+3.0*a*b2*xL0)
       b1 = d*(a2*c*C0+b2*(c+C3*(c+a*xL0)))

       PN = (3.0*Pwr) / (8.0*b2*pi*(xn2*b1*c**3 - 
     .     a1*(a*xL0*(1.0-2.0*xnu)+c*C3)))

c factor needed to put into units of Mm^3 (Millions of cubic meters)
c this agrees with Mike L's test cases
       PN = PN*4.0d3

c = 4/3 pi R^3 = 4188.787
c       PN = PN*(4.0d0/3.0d0 * pi * 1.0d3)

      else
c if Pressure, from Cervelli
       PN = Pwr  / (4.0d0*xmu)
c       print *, 'P', PN
      endif
c       PN = Pwr  


c-- translate origin and convert observation point lat/lon (Xs,Ys) to Cartesian (x1,y1) in kms
c   relative to Spheroid (xpr,ypr) origin
      call ps_project( Xs, Ys, Xpr, Ypr, X1, X2)

c      if(Ax.eq.B) then
c        Xm = 0.0
c        Ym = 0.0
c        call MOGI2 (Xm, Ym, Zpr, PN, X1, Y2, Zs, DU, DV, DW)
c        return
c       endif

c for test
c      eRad=6370997.2d0 
c      eCir=2.0d0*pi*eRad 
c      xmPerDeg=(eCir/360.0d0)
c      X1 = (Xs-Xpr)*xmPerDeg
c      X2 = (Ys-Ypr)*xmPerDeg

c RM PHI negative to match MBattaglia tests
c RM add 180 so that azimuth is in direction of down-dip
      FI=(180.0d0-PHI)*rpd
c Battaglia
c      FI=PHI*rpd
c Cervelli
c       FI= (90.0d0-PHI)*rpd
c Bonnaccorso
c      FI=PHI*rpd

c convert to meters
      XX1= X1*thou
      XX2= X2*thou
      Z0 = Zpr*thou
c observation height, pos or neg?
      X3 = Zs*thou
      X3 = 0.0d0

      X1=XX1*COS(FI)+XX2*SIN(FI)
      X2=XX2*COS(FI)-XX1*SIN(FI)
c      print *, "A",xs,ys,x1,x2

c Battaglia
c      X1=XX1*DCOS(FI)-XX2*DSIN(FI)
c      X2=XX2*DCOS(FI)+XX1*DSIN(FI)
c      print *, "B",xs,ys,x1,x2

c Cervelli
c      X1= XX1*DCOS(FI)+XX2*DSIN(FI)
c      X2=-XX1*DSIN(FI)+XX2*DCOS(FI)
c      print *, "B",xs,ys,x1,x2

c Bonnacorso
c      X1=XX1*DCOS(FI)+XX2*DSIN(FI)
c      X2=XX2*DCOS(FI)-XX1*DSIN(FI)
c      X3=0.D0

      CALL comELLIPU(U1,U2,U3,X1,X2,X3,Z0,DIP,AX,B,CX,PN)
c      print *, U1,U2,U3
 
      UU1=U1
      UU2=U2
      U1=UU1*COS(FI)-UU2*SIN(FI)
      U2=UU1*SIN(FI)+UU2*COS(FI)
c Battaglia
c      U1=UU1*DCOS(FI)+UU2*DSIN(FI)
c      U2=UU1*DSIN(FI)-UU2*DCOS(FI)
c Cervelli
c      U1=UU1*DCOS(FI)-UU2*DSIN(FI)
c      U2=UU1*DSIN(FI)+UU2*DCOS(FI)

c RM change sign of East and North, convert to mm
      DU = -U1*thou
      DV = -U2*thou
c Battaglia
c      DU = U1*thou
c      DV = U2*thou
      DW = U3*thou
c
      RETURN
      END
c
C
      SUBROUTINE comELLIPU(U1,U2,U3,X1,X2,X3,Z0,ST,AX,B,CX,PN)
c      implicit complex*16 (A-H,O-Z)
c      real*8 U1,U2,U3
      implicit real*8 (A-H,O-Z)
      complex*16 C, Cx
      pi   = 3.14159265358979d0 
      rpd = pi/180.0d0
      thou = 1.0d3
      X=X1
      Y=X2
      Z=X3
      ZZ0=Z0
      ANG=ST
      A=AX
      C=CX
      P=PN/thou
      AL=1.D0
      AM=1.D0
      SI=AL/2.D0/(AL+AM)
      CA=COS(ANG*rpd)
c      B=DSQRT(DSQRT((A*A-C*C)**2))
      CALL comPF(PC,PD,P,AL,AM,SI,A,B,C)
      D1=-2.D0*B**2*PD
      D2=3.D0*B**2/C**2*PD+2*(1.D0-2.D0*SI)*PC
C     WRITE(3,15)PC,PD,D1,D2
c   15 FORMAT(1X,'PC=',1X,F10.6,5X,'PD=',1X,F10.6, 
c     .       5X,'D1=',1X,F10.6,5X,'D2=',1X,F10.6//)  
      V=A*B**2/C**3/(16.D0*AM*(1.D0-SI))
      CALL comELLIP(W1,W2,W3,FF1,FF2,FF3,
     .           D1,D2,PD,X,Y,Z,C,ANG,ZZ0,SI)
      UFF1=FF1
      UFF2=FF2
      UFF3=FF3
      U1=W1
      U2=W2
      U3=W3
      CALL comELLIP(W1,W2,W3,FF1,FF2,FF3,
     .           D1,D2,PD,X,Y,Z,-C,ANG,ZZ0,SI)
      UFF1=(UFF1-FF1)/CA**2
      UFF2=(UFF2-FF2)/CA**2
      UFF3=(UFF3-FF3)/CA
      U1=V*(U1-W1+UFF1)*thou
      U2=V*(U2-W2+UFF2)*thou
      U3=V*(U3-W3+UFF3)*thou

C      WRITE(3,20)X,Y,Z,U1,U2,U3
c   20 FORMAT(1X,'X=',1X,F6.2,5X,'Y=',1X,F6.2,5X,'Z=',1X,F6.2,
c     .      10X,'U1=',1X,F10.6,10X,'U2=',1X,F10.6,10X,'U3=',1X,F10.6)
      RETURN
      END
C
C
      SUBROUTINE comELLIP(U1,U2,U3,FF1,FF2,FF3, 
     .                 D1,D2,PD,X,Y,Z,C,ANG,ZZ0,SI)
      implicit real*8 (A-H,O-Z)
      complex*16 C2, C3, C
c      implicit complex*16 (A-H,O-Z)
c      real*8 U1,U2,U3,BT1,BT2
      pi   = 3.14159265358979d0 
      rpd = pi/180.0d0
      SA=SIN(ANG*rpd)
      CA=COS(ANG*rpd)
      C2=CA*C
      C3=SA*C
      Y2=Y
      Y3=Z-ZZ0
      T2=SA*Y2-CA*Y3
      T3=CA*Y2+SA*Y3
      TT3=T3-C
      YY2=Y
      YY3=Z+ZZ0
      Q2=SA*YY2+CA*YY3
      Q3=-CA*YY2+SA*YY3
      QQ3=Q3+C
      X1=X+.0000001D0
      X2=Y2-C2
      X3=Y3-C3
      XX3=YY3+C3
      C0=ZZ0/SA
      R1=SQRT(X1*X1+X2*X2+X3*X3)
      R2=SQRT(X1*X1+X2*X2+XX3*XX3)
      BT1=real(CA*Q2+(1.+SA)*(R2+QQ3))
      BT2=real(CA*X1)
      A1=C/R1+LOG(R1+TT3)
      A2=R1-T3*LOG(R1+TT3)
      A3=C*TT3/R1+R1
      AA1=C/R2-LOG(R2+QQ3)
      AA2=R2-Q3*LOG(R2+QQ3)
      AA3=C*QQ3/R2-R2
      B=C*(C+C0)/R2-AA2-C0*LOG(R2+QQ3)
      F1=-2.D0*SA*Z*(C*(C+C0)/R2**3+(R2+C+C0)/R2/(R2+QQ3)
     .   +4.D0*(1.D0-SI)*(R2+C)/R2/(R2+QQ3))
      F2=-2.D0*SA*Z*(C*(C+C0)*QQ3/R2**3+C0/R2+AA1
     .   +4.D0*(1.D0-SI)*AA1)
      FF1=CA**2*C*X1/(R2+XX3)+3.D0*(SA*X1*LOG(R2+XX3)
     .    -X1*LOG(R2+QQ3)+2.D0*Q2*ATAN2(BT1,BT2))
     .    +CA**2*2.D0*X1*LOG(R2+QQ3)-4.D0*YY3*ATAN2(BT1,BT2)*CA
      FF2=CA**2*C*X2/(R2+XX3)+3.D0*(SA*Q2*LOG(R2+QQ3)
     .    -Q2*LOG(R2+XX3)+2.D0*SA*X1*ATAN2(BT1,BT2)
     .    +CA*(R2-XX3))-2.D0*CA**3*AA2
     .    +2.D0*CA*(YY3*LOG(R2+XX3)-Q3*LOG(R2+QQ3)) 
      FF3=(Q2*LOG(R2+QQ3)-Q2*SA*LOG(R2+XX3)
     .    +2.D0*X1*ATAN2(BT1,BT2))
     .    +(2.D0*SA*AA2+Q3*LOG(R2+XX3)-C)*CA
      AC=D1/R1/(R1+TT3)+D2*(LOG(R1+TT3)+(T3+C)/(R1+TT3))
      AAC=-D1/R2/(R2+QQ3)-D2*(LOG(R2+QQ3)+(Q3-C)/(R2+QQ3))
      BC=(D1/R1+2.D0*D2*A2)+(3.D0-4.D0*SI)*(D1/R2+2.D0*D2*AA2)
      FC1=2.D0*Z*(CA*Q2*(D1*(2.D0*R2+QQ3)/R2**3/(R2+QQ3)**2
     .    -D2*(R2+2.D0*C)/R2/(R2+QQ3)**2)
     .    +SA*(D1/R2**3-2.D0*D2*(R2+C)/R2/(R2+QQ3)))
      FC2=2.D0*Z*(D1*XX3/R2**3
     .    -2.D0*D2*(SA*AA1+CA*Q2*(R2+C)/R2/(R2+QQ3)))
      UC1=(AC+(3.D0-4.D0*SI)*AAC+FC1)*X1
      UC2=SA*(AC*T2+(3.D0-4.D0*SI)*AAC*Q2+FC1*Q2)
     .    +CA*(BC-FC2)+2.D0*SA*CA*Z*AAC
      UC3=-CA*(AC*T2+(3.D0-4.D0*SI)*Q2*AAC-FC1*Q2)
     .    +SA*(BC+FC2)+2.D0*CA**2*Z*AAC
      UD1=X1*A1+X1*((3.D0-4.D0*SI)*AA1+F1)
      UD2=SA*(T2*A1+Q2*((3.D0-4.D0*SI)*AA1+F1))
     .    +4.D0*(1.D0-SI)*CA*(A2+AA2)
     .    +CA*(A3-(3.D0-4.D0*SI)*AA3-F2)
      UD3=CA*(-T2*A1+Q2*((3.D0-4.D0*SI)*AA1+F1))
     .    +4.D0*(1.D0-SI)*SA*(A2+AA2)
     .    +SA*(A3+(3.D0-4.D0*SI)*AA3+F2-2.D0*(3.D0-4.D0*SI)*B)
      U1=UC1+2.D0*PD*UD1
      U2=UC2+2.D0*PD*UD2
      U3=UC3+2.D0*PD*UD3
      FF1=-2.D0*PD*4.D0*(1.D0-SI)*(1.D0-2.D0*SI)*FF1
      FF2=-2.D0*PD*4.D0*(1.D0-SI)*(1.D0-2.D0*SI)*FF2
      FF3= 2.D0*PD*4.D0*(1.D0-SI)*(1.D0-2.D0*SI)*FF3
      RETURN
      END
C

      SUBROUTINE comPF(PC,PD,P,AL,AM,SI,A,B,C)
c      implicit complex*8 (A-H,O-Z)
      implicit real*8 (A-H,O-Z)
      complex*16 C
      pi   = 3.14159265358979d0 
      AI=-2.D0*PI*A*B**2*(2.D0/A/C**2
     .   +0.5d0*LOG(((A-C)/(A+C))**2)/C**3)
      AAI=-2.D0*PI*A*B**2*(2.D0/3.D0/C**2/A**3+2.D0/A/C**4
     .    +0.5d0*LOG(((A-C)/(A+C))**2)/C**5)
      U=8.D0*PI*(1.D0-SI)
      Q=3.D0/U
      R=(1.D0-2.D0*SI)/U
      A11=2.D0*R*(AI-4.D0*PI)
      A12=-2.D0*R*(AI+4.D0*PI)
      A21=Q*A**2*AAI+R*AI-1
      A22=-(Q*A**2*AAI+(2.D0*R-Q)*AI)
      W=(A11*A22-A12*A21)*(3.D0*AL+2.D0*AM)
      E11=(3.D0*A22-A12)*P/W      
      E22=(A11-3.D0*A21)*P/W
      PD=2.D0*AM*(E11-E22)
      PC=AL*E11+2.D0*(AL+AM)*E22
      RETURN
      END 
C
c***********************************************************************
c* stuff for layered structure routines
c***********************************************************************
c      include "tdefedp.h"

c***********************************************************************
c*****************  THE END ********************************************
