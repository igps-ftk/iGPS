This code works for interpolation of horizontal velocities (or displacements).  It inputs a discretized velocity (or displacement) dataset, and has the options to do the following:
1. check velocity data compatibility
2. interpolate velocity data and estimate velocities at given locations
3. interpolate velocity data and estimate translation, rotation, and strain rates

To compile the software:
% gfortran -o visr visr.f voron.f90

To run the program:
% visr < driver.file

Example 1: checking velocity compatibility  
driver file content (visr_vel.drv) :                    
input file:  velmap.dat         ! station coordinate and velocity data (in degree and mm/yr)
output file: velmap.out         ! checking result 
Parameters:
is_wght                ! distance weighting scheme: 1=gaussian, 2=quadratic
id_wght              ! density weightting scheme: 1=azimuth, 2=voronoi area
min_tau,max_tau,in_tau      ! minimum, maximum, and incremental spatial smoothing constants (km)
wt0                             ! weighting threshold
1                               ! for checking velocity compatibility
lonmin,lonmax,latmin,latmax     ! checking range (in degree) 
ncrp                            ! number of creep faults
crpf                            ! creep fault data file

Example 2: velocity interpolation for velocities at given locations
driver file content (visr_v.drv) : 
input file:  velmap.dat          ! station coordinate and velocity data (in degree and mm/yr)
output file: velmap.out         ! interpolation result
is_wght                ! distance weighting scheme: 1=gaussian, 2=quadratic
id_wght             ! density weightting scheme: 1=azimuth, 2=voronoi area
min_tau,max_tau,in_tau       ! minimum, maximum, and incremental spatial smoothing constants (km)
wt0                             ! weighting threshold
2                               ! for velocity interpolation at sites                              
site.list                       ! site list for interpolation, format(a8,2f10.4) for sitename,longitude and latitude (in degree)
ncrp                            ! number of creep faults
crpf                            ! creep fault data file

Example 3: velocity interpolation for rotation and strain rates 
driver file content (visr.drv): 
input file:  velmap.dat          ! station coordinate and velocity data (in degree and mm/yr)
output file: velmap.out         ! interpolation result
is_wght                ! distance weighting scheme: 1=gaussian, 2=quadratic
id_wght             ! density weightting scheme: 1=azimuth, 2=voronoi area
min_tau,max_tau,in_tau       ! minimum, maximum, and incremental spatial smoothing constants (km)
wt0                             ! weighting threshold
3                               ! for rotation and strain rates                              
lonmin,lonmax,latmin,latmax,dlon,dlat   ! interpolation range and step (degree) 
ncrp                            ! number of creep faults
crpf                            ! creep fault data file

