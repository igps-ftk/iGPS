c TDEFNODE  2020.09.25
c set array dimensions in parameter statement below
c
c If you are not using the array, set the dimension to 2, not 0 (zero)
c
c********************************************************************
c MAX_block			max number of blocks (BL)
c MAX_corner		max corners defining a single block
c MAX_disp			max number of points in all time series  
c MAX_edits 		max number of points to be edited (MV/AV/DV)
c MAX_f 			max number of faults (FA)
c MAX_fc 			max number of faults that can be locked together (FC)
c MAX_gps 			max number of observed gps 
c MAX_gps_files 	max number of gps files read in (GP)
c MAX_grids  		max number of grids to calculate
c MAX_gridpts 		max total number of grid points in all grids (GR)
c MAX_hc 			max number of hard constraints (HC)
c MAX_insar_pts 	max number of InSAR points  
c MAX_insar_files 	max number of InSAR files (IS)
c MAX_ll 			max number of line length changes (LL)
c MAX_mrlx_files 	max number of mantle relaxation events
c MAX_mrlx_pts 		max number of surface pts in each mantle relaxation event
c MAX_mts 			max number of moment tensors (under development)
c MAX_nodes 		max number of nodes for a single fault (set to MAX_x * MAX_z)
c MAX_num_fs 		max number of points to calculate fault slip vectors (FS)
c MAX_parms 		max number of free parameters (if > 2000 reset NMAX parameter in subroutines)
c MAX_poles 		max rotation poles (PO)
c MAX_pr_lines 		max number of profiles (PR)
c MAX_pr_pts 		max total number of points in all profiles (PR)
c MAX_qfiles 		max number of earthquake files
c MAX_rm 			max number of RM: lines (to remove GPS sites) (RM)
c MAX_rot 			max number of rotation rate data (RO)
c MAX_srce 			max number of transient sources (ES)
c MAX_sr 			max number of slip rate data (SR)
c MAX_sr_files 		max number of slip rate files read in (SR)
c MAX_ss 			max number of surface strain rate data (SS)
c MAX_strain 		max number of uniform strain tensors (ST)
c MAX_sv 			max number of slip vector data (SV)
c MAX_sv_files 		max number of SV files read in (SV)
c MAX_tau 			max number of time elements is a transient time history (ET)
c MAX_t_slice  		max number of time windows for deformation history (DW)
c MAX_tilt 			max number of tilt rate data (TI)
c MAX_tsegs 		max number of segments (offsets) in time series (not used)
c MAX_x 			max x nodes (along strike) per fault
c MAX_z 			max z nodes (down-dip) per fault
c********************************************************************
c
c set dimensions here, be careful of syntax; each number except the last 
c   is followed by a comma
c
       PARAMETER (
     . MAX_block        = 199, 
     . MAX_corner       = 350, 
     . MAX_disp         = 3000, 
     . MAX_edits        = 50,
     . MAX_f            = 100,
     . MAX_fc           = 10,
     . MAX_gps          = 20000, 
     . MAX_gps_files    = 20, 
     . MAX_gridpts      = 1000,
     . MAX_grids        = 5,
     . MAX_hc           = 400,
     . MAX_insar_pts    = 20000,
     . MAX_insar_files  = 100,
     . MAX_ll           = 30,
     . MAX_mrlx_files   = 2,
     . MAX_mrlx_pts     = 2,
     . MAX_mts          = 2,
     . MAX_nodes        = 2100, 
     . MAX_num_fs       = 50,
     . MAX_parms        = 3000, 
     . MAX_poles        = 99,
     . MAX_pr_lines     = 200, 
     . MAX_pr_pts       = 6000, 
     . MAX_qfiles       = 3,
     . MAX_rm           = 300,
     . MAX_rot          = 100,
     . MAX_srce         = 99,
     . MAX_sr           = 600, 
     . MAX_sr_files     = 20,
     . MAX_ss           = 10, 
     . MAX_strain       = 99,
     . MAX_sv           = 500, 
     . MAX_sv_files     = 50,
     . MAX_tau          = 100, 
     . MAX_t_slice      = 30, 
     . MAX_tilt         =  2, 
     . MAX_tsegs        =  5, 
     . MAX_x            = 100, 
     . MAX_z            = 30 ) 

c********************************************************************
c  DON'T EDIT ANYTHING BELOW HERE
c********************************************************************
c
c Points for transient time series
      PARAMETER (
     . MAX_ngc = 15000 )

c tdefcom3 has commons for main programs
      include "tdefcom3.h"
c********************************************************************
