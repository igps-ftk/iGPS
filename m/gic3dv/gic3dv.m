function gic3dv(file_drv)
%c      program gic3dv
%c --------------------------------------------------------------------
%c   gic3dv (GPS and InSAR combination for 3D velocity): a program to combine GPS
%c   and InSAR data and produce 3D velocity field. Zheng-Kang Shen.  12/02/2019.
%%%
%%%  transformed into Matlab using Deepseek AI
%%%
%c
%c   to run the program:
%c   % gic3dv < gic3dv.drv
%    Under Matlab command line:
%      fx>> gic3dv     %use the defualt driver file name (gic3dv.drv)
%    Or:
%      fx>> gic3dv('gic3dv_othername.drv')
%c
%c   ***TYF**>>>
%c   Modified by tianyf on Thu Dec  7 14:41:59 CST 2023
%c     +increase the maximum values for a few parameters
%c        maximum number of GNSS sites from 2000 to 15000 (maxsit)
%c        maximum number of InSAR files from 5 to 25 (maxsar)
%c        maximum number of pixels in each InSAR image from 12000 to 120000 (maxpxl)
%c        maximum number of creeping fault from 10 to 10 (maxc) ! error occurred when changing its value?
%c
%c     +increase the length of file name variable
%c        length changed from 25 to 1025 (gpsfh,gpsfv,sarf(maxsar),outf,crpf,resf,orbf)
%c        change the method of reading file names (from fixed length to unformatted)
%c        can also handle files in subdirectories (f77 stop reading if it encounters a path separator (/))
%c   **TYF***<<<

% GIC3DV GPS and InSAR combination for 3D velocity field
% Main program - variable declarations and initialization

% clear;

if nargin < 1
  file_drv='./gic3dv.drv';
end


global veldath veldatv crpdat ainv_vh ainv_vv prmtr maxsit

% constants
maxsit = 3500
maxpxl = 12000;
maxc = 10;
maxsar = 5;
maxold = 2000;
maxdat = 3*maxold + 3*maxsar;

% increase the constants for larege data set
% maxsit = 15000;
% maxpxl = 120000;
% maxsar = 25;
% maxold = 15000;
% maxdat = 3*maxold + 3*maxsar;

% input & output gps file names
head = '';
gpsfh = ''; gpsfv = '';
sarf = cell(1, maxsar);
outf = ''; crpf = ''; resf = ''; orbf = '';
tmpstr = '';
gps_h_outf = ''; gps_v_outf = ''; gps_prdf = '';

% gps station names
stnlh = cell(1, maxsit+1);
stnlv = cell(1, maxsit+1);

%number of data lines
nstn = 0; nstnh = 0; nstnv = 0; ncrp = 0;
nsite = 0; ms = 0; nold = 0; ndata = 0;

% geographical coordinates of gps stations
lonlh = zeros(1, maxsit+1); latlh = zeros(1, maxsit+1);
lonlv = zeros(1, maxsit+1); latlv = zeros(1, maxsit+1);
isd = zeros(300, 300);

% local coordinates (X, Y) and horizontal velocity components for gps stations
lonh = zeros(1, maxsit); lath = zeros(1, maxsit);
uxlh = zeros(1, maxsit); sxlh = zeros(1, maxsit);
uylh = zeros(1, maxsit); sylh = zeros(1, maxsit);
cxyh = zeros(1, maxsit); uzlh = zeros(1, maxsit); szlh = zeros(1, maxsit);

% local coordinates (X, Y) and vertical velocity components for gps stations
lonv = zeros(1, maxsit); latv = zeros(1, maxsit);
uxlv = zeros(1, maxsit); sxlv = zeros(1, maxsit);
uylv = zeros(1, maxsit); sylv = zeros(1, maxsit);
cxyv = zeros(1, maxsit); uzlv = zeros(1, maxsit); szlv = zeros(1, maxsit);

% for computation of voronoi areas of gps network
glon = zeros(1, maxsit); glat = zeros(1, maxsit);
qlon = zeros(1, 1); qlat = zeros(1, 1);
posh = zeros(2, maxsit); posv = zeros(2, maxsit);
areah = zeros(1, maxsit); areav = zeros(1, maxsit);
careah = zeros(1, maxsit); careav = zeros(1, maxsit);

%
alon = zeros(1, maxc); alat = zeros(1, maxc);
blon = zeros(1, maxc); blat = zeros(1, maxc);
bbz = zeros(6, 1); dcs = zeros(1, maxc); dsn = zeros(1, maxc);
tx = zeros(1, 1); ty = zeros(1, 1);

%
a = zeros(maxdat, maxdat); aat = zeros(maxdat, maxdat);
indx1 = zeros(1, maxdat);
bb = zeros(6, 1); ainv = zeros(6, 6); b = zeros(maxdat, 1);
bbt = zeros(maxdat, 1); ainvm = zeros(maxdat, maxdat);
v = zeros(maxdat, 1); bbr = zeros(maxdat, 1);
azinv = zeros(6, 6); x = zeros(3, 1); covx = zeros(3, 3);

% InSAR
prd   = zeros(20, 1); prd0 = zeros(20, 1);
asv   = zeros(20, 3); bsv = zeros(20, 1);
isat  = zeros(1, maxsar); f_sar = zeros(1, maxsar);
sazis = zeros(1,maxsar)

% InSAR velocities
tlon = zeros(maxsar, maxpxl); tlat = zeros(maxsar, maxpxl);
sazi = zeros(maxsar, maxpxl); slook = zeros(maxsar, maxpxl);
clos = zeros(maxsar, maxpxl); clos_s = zeros(maxsar, maxpxl);
tlonmean = zeros(1, maxsar); tlatmean = zeros(1, maxsar);
ndat = zeros(1, maxsar);

% output 3D deformation field
vxs = zeros(1, maxsit+1); vys = zeros(1, maxsit+1);
vx_sig = zeros(1, maxsit+1); vy_sig = zeros(1, maxsit+1);
vzs = zeros(1, maxsit+1); vz_sig = zeros(1, maxsit+1);
tmp = zeros(1, maxsit+1);

% whether use GPS components as constraints
use_gps_enu=[1, 1, 1]  %default use all components (e, n, u)


% common variables for interpolation of horizontal gps velocity
veldath.nstnh = 0;
veldath.lonh = [];
veldath.lath = [];
veldath.uxlh = [];
veldath.sxlh = [];
veldath.uylh = [];
veldath.sylh = [];
veldath.uzlh = [];
veldath.szlh = [];
veldath.cxyh = [];
veldath.areah = [];

% common variables for interpolation of vertical gps velocity
veldatv.nstnv = 0;
veldatv.lonv = [];
veldatv.latv = [];
veldatv.uxlv = [];
veldatv.sxlv = [];
veldatv.uylv = [];
veldatv.sylv = [];
veldatv.uzlv = [];
veldatv.szlv = [];
veldatv.cxyv = [];
veldatv.areav = [];

% common variables for creeping faults
crpdat.ncrp = 0;
crpdat.alon = [];
crpdat.alat = [];
crpdat.blon = [];
crpdat.blat = [];
crpdat.dcs = [];
crpdat.dsn = [];

%  common variables for interpolation of horizontal gps velocity
ainv_vh.ainv = zeros(6,6);
ainv_vh.bb = zeros(6,1);
ainv_vh.rtau = 0;
ainv_vh.wtt = 0;
ainv_vh.chisq = 0;
ainv_vh.nslct = 0;

% common variables for interpolation of vertical gps velocity
ainv_vv.azinv = zeros(6,6);
ainv_vv.bbz = zeros(6,1);
ainv_vv.rtauz = 0;
ainv_vv.wttz = 0;
ainv_vv.chisqz = 0;
ainv_vv.nslctz = 0;

%

prmtr.dpi = 4.0 * atan(1.0);
prmtr.dcov = prmtr.dpi / 180.0;
prmtr.cutoff_dis = 0;
prmtr.wt_az = 0;
dpi = prmtr.dpi;
dcov = prmtr.dpi / 180.0;

%
ms = 1;
isl = 0;

%
fprintf('Constants initialized:\n');
fprintf('  dpi = %.8f\n', prmtr.dpi);
fprintf('  dcov = %.8f\n', prmtr.dcov);
fprintf('  ms = %d\n', ms);
fprintf('  isl = %d\n', isl);


fid_drv = fopen(file_drv);

fprintf('Reading input file names and options...\n');

% input horizontal gps data file

tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
gpsfh=tmpstr(1:space_pos-1)

%input vertical gps data file
tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
gpsfv=tmpstr(1:space_pos-1)


%
% 1=use, 0=not to use default GPS vertical velocity uncertainty, and the default vertical uncertainty value (mm/yr)
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%d %f');
idvu0 = tmps(1)
dvu0 = tmps(2)
%
% a priori uncertainties for offset and ramp corrections of SAR data (mm, mm/deg)
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f %f');
offsig = tmps(1)
rmpsig = tmps(2)
%
% default smoothing distances for estimations of horizontal and vertical interpolation velocity uncertainties (km)
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f %f');
rtau_h = tmps(1);
rtau_v = tmps(2);
%
% input interpolation region for GPS only solution
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f %f %f %f');
lonmin0 = tmps(1);
lonmax0 = tmps(2);
latmin0 = tmps(3);
latmax0 = tmps(4);
%
% # of InSAR data input files
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%d');
nsarf = tmps(1)
%
% % 检查InSAR文件数�?�?制
if nsarf > maxsar
  error('number of sar files greater than allowance');
end
%
%insar data input file names
sarf = cell(1, nsarf);
for i = 1:nsarf
  tmpstr = strtrim(fgetl(fid_drv));
  space_pos = find(tmpstr == ' ', 1);
  sarf{i} =tmpstr(1:space_pos-1)
end
%
% input scaling factors of InSAR LOS data input
f_sar = zeros(1, nsarf);
tmpstr = strtrim(fgetl(fid_drv));
f_sar=sscanf(tmpstr,'%f %f %f %f')
%
% 1=use, 0=not to use InSAR default LOS rate uncertainty, default LOS rate uncertainty, minimum uncertainty
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f %f %f');
ilos0 = tmps(1);
dlos0 = tmps(2);
dlos1 = tmps(3);
%
% input search region and increments (deg)
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f %f %f %f %f %f');
lonmin = tmps(1);
lonmax = tmps(2);
latmin = tmps(3);
latmax = tmps(4);
dlon = tmps(5);
dlat = tmps(6);
%
% input InSAR data grid increment for InSAR image ramp estimation (deg)
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f %f');
dlon1 = tmps(1);
dlat1 = tmps(2);
%
% InSAR ramp parameter output file
tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
orbf=tmpstr(1:space_pos-1)
tmpstr = strtrim(fgetl(fid_drv));
%Velocity solution output file
space_pos = find(tmpstr == ' ', 1);
outf=tmpstr(1:space_pos-1)
% outf = strtrim(tmpstr);
%InSAR data postfit residual output file
tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
resf=tmpstr(1:space_pos-1)
% resf = strtrim(tmpstr);

%Interpolated horizontal velocities at GPS sites
tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
gps_h_outf=tmpstr(1:space_pos-1)
% gps_h_outf = strtrim(tmpstr);

%Interpolated vertical velocities at GPS sites
tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
gps_v_outf=tmpstr(1:space_pos-1)
% gps_v_outf = strtrim(tmpstr);

%Interpolated 3D regional GPS velocities and uncertainties
tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
gps_prdf=tmpstr(1:space_pos-1)
% gps_prdf = strtrim(tmpstr);
%
% distance weighting scheme: 1=gaussian, 2=quadratic
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%d');
is_wght = tmps(1);
% spatial weighting scheme: 1=azimuth, 2=voronoi area
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%d ');
id_wght = tmps(1);
% is_wght = input('');
% id_wght = input('');
%
% minimum, maximum, and incremental for search of spatial smoothing constants (km)
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%d %d %d');
% tmps = input('');
min_tau = tmps(1);
max_tau = tmps(2);
in_tau = tmps(3);
%
%
%weighting threshold W for interpolation
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f');
wt0 = tmps(1);
% wt0 = input('');
%
% minimum uncertainty thresholds for GPS horizontal and vertical velocities (mm)
% tmps = input('');
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%f %f');
rsgah = tmps(1);
rsgav = tmps(2);

% number of creeping faults
tmpstr = strtrim(fgetl(fid_drv));
tmps=sscanf(tmpstr,'%d');
ncrp = tmps(1);
% ncrp = input('Number of creep faults: ');
if ncrp > maxc
  fprintf('# of creep faults > maxc %d, please increase maxc ...\n', maxc);
  return;
end
fprintf('#creep faults: %d\n', ncrp);

tmpstr = strtrim(fgetl(fid_drv));
space_pos = find(tmpstr == ' ', 1);
crpf=tmpstr(1:space_pos-1)
%     crpf = input('Creep fault parameter file: ', 's');

tmpstr=fgetl(fid_drv);
if tmpstr ~= -1
  tmpstr = strtrim(tmpstr);
  use_gps_enu=sscanf(tmpstr,'%d %d %d');
end

fprintf('gpsfh: %s\n', gpsfh);
fprintf('gpsfv: %s\n', gpsfv);
fprintf('  use gps e, n, u: %d %d %d\n', use_gps_enu);

% 打开文件
fid4 = fopen(gpsfh, 'r');    %  input, open horizontal gps velocity data
fid8 = fopen(gpsfv, 'r');    %  input, open vertical gps velocity data
fid41 = fopen(gps_h_outf, 'w');  %  output, open file for interpolated horizontal velocities at gps sites
fid42 = fopen(gps_v_outf, 'w');  %  output, open file for interpolated vertical velocities at gps sites
fid79 = fopen(gps_prdf, 'w');    %  output, open file for interpolated 3D regional gps velocities
fid11 = fopen(resf, 'w');        %  output, open file for InSAR data postfit residuals

%
fprintf(fid11, 'Sat#   Long      Lat       Wobsv     Wpred    Wresi     NWobsv    NWpred    NWresi\n');

%
fprintf('reading horizontal gps velocity data...\n');
i = 1;
while true
  head = fgetl(fid4);
  if head == -1
    break;
  end
  if head(1) == '*'
    continue;
  end
 
  % read in horizontal gps velocity data
  tmps = sscanf(head, '%s %f %f %f %f %f %f %f %f %f');
  stnlh{i} = char(tmps(1:8))';
  lonlh(i) = tmps(9);
  latlh(i) = tmps(10);
  uxlh(i) = tmps(11);
  sxlh(i) = tmps(12);
  uylh(i) = tmps(13);
  sylh(i) = tmps(14);
  cxyh(i) = tmps(15);
  %
  uzl0 = tmps(16);
  szl0 = tmps(17);
  
  %
  if lonlh(i) > 180.0
    lonlh(i) = lonlh(i) - 360.0;
  end
  
  %
  if sxlh(i) < rsgah
    sxlh(i) = rsgah;
  end
  if sylh(i) < rsgah
    sylh(i) = rsgah;
  end
  
  i = i + 1;
end
fclose(fid4);
nstnh = i - 1;
fprintf('  %d sites read (stnlh,lonlh,latlh,uxlh,uylh...)\n', nstnh);

%
fprintf('reading vertical gps velocity data...\n');
i = 1;
while true
  head = fgetl(fid8);
  if head == -1
    break;
  end
  if head(1) == '*'
    continue;
  end
  
  % read in vertical gps velocity data
  tmps = sscanf(head, '%s %f %f %f %f %f %f %f %f %f');
  stnlv{i} = char(tmps(1:8))';
  lonlv(i) = tmps(9);
  latlv(i) = tmps(10);
  % uxl0, sxl0, uyl0, syl0, cxy0未使用
  uxl0 = tmps(11);
  sxl0 = tmps(12);
  uyl0 = tmps(13);
  syl0 = tmps(14);
  cxy0 = tmps(15);
  uzlv(i) = tmps(16);
  szlv(i) = tmps(17);
  
  %
  if lonlv(i) > 180.0
    lonlv(i) = lonlv(i) - 360.0;
  end
  
  %
  if szlv(i) < rsgav
    szlv(i) = rsgav;
  end
  
  i = i + 1;
end
fclose(fid8);
nstnv = i - 1;
fprintf('  %d sites read (stnlv,lonlv,latlv,uxlv,uylv...)\n', nstnv);




%

fprintf('Checking horizontal data redundancy by lon&lat ...\n');
iden = 0;
for i = 1:nstnh-1
  for j = i+1:nstnh
    if (latlh(i) == latlh(j)) && (lonlh(i) == lonlh(j))
      fprintf('*ERR* duplicated stations: %d %s %d %s\n', i, stnlh{i}, j, stnlh{j});
      iden = 1;
      return;
    end
  end
end
if iden == 1
  return;
end

fprintf('Checking vertical data redundancy by lon&lat ...\n');
iden = 0;
for i = 1:nstnv-1
  for j = i+1:nstnv
    if (latlv(i) == latlv(j)) && (lonlv(i) == lonlv(j))
      fprintf('*ERR* duplicated stations: %d %s %d %s\n', i, stnlv{i}, j, stnlv{j});
      iden = 1;
      return;
    end
  end
end
if iden == 1
  return;
end

%  compute mass center of network
if (nstnh > maxsit) || (nstnv > maxsit)
  fprintf('*ERR* too many stations (h;v;max): %d %d %d\n', nstnh, nstnv, maxsit);
  fprintf('please increase maxsit ...\n');
  return;
end

%
fprintf('compute mass center of network (center of network) ...\n');
xmean = 0.0;
ymean = 0.0;
for i = 1:nstnh
  xmean = xmean + lonlh(i);
  ymean = ymean + latlh(i);
end
xmean = xmean / nstnh;
ymean = ymean / nstnh;

% convert geodetic coordinates into cartesian coordinates
fprintf('convert station coordinates from WGS84 into cartesian\n');

%
for i = 1:nstnh
  lonh(i) = lonlh(i);
  lath(i) = latlh(i);
end

%
[lath_local, lonh_local] = llxy(ymean, xmean, lath, lonh, nstnh);
lath = lath_local;
lonh = lonh_local;

%
for i = 1:nstnv
  lonv(i) = lonlv(i);
  latv(i) = latlv(i);
end

[latv_local, lonv_local] = llxy(ymean, xmean, latv, lonv, nstnv);
latv = latv_local;
lonv = lonv_local;



% convert geodetic coordinates into cartesian coordinates for creep fault and
%  compute creep fault's azimuth


if ncrp ~= 0
  alon = zeros(1, ncrp);
  alat = zeros(1, ncrp);
  blon = zeros(1, ncrp);
  blat = zeros(1, ncrp);
  dcs = zeros(1, ncrp);
  dsn = zeros(1, ncrp);
  %     crpf = input('Creep fault parameter file: ', 's');
  
  %
  fid = fopen(crpf, 'r');
  if fid == -1
    error(['Cannot open fault file: ' crpf]);
  end
  
  for i = 1:ncrp
    line = fgetl(fid);
    tmps = sscanf(line, '%f %f %f %f');
    alon(i) = tmps(1);
    alat(i) = tmps(2);
    blon(i) = tmps(3);
    blat(i) = tmps(4);
    
    %
    if alon(i) > 180.0
      alon(i) = alon(i) - 360.0;
    end
    if blon(i) > 180.0
      blon(i) = blon(i) - 360.0;
    end
  end
  fclose(fid);
  
  %
  [alat_local, alon_local] = llxy(ymean, xmean, alat, alon, ncrp);
  alat = alat_local;
  alon = alon_local;
  
  [blat_local, blon_local] = llxy(ymean, xmean, blat, blon, ncrp);
  blat = blat_local;
  blon = blon_local;
  
  %
  for i = 1:ncrp
    dx = blon(i) - alon(i);
    dy = blat(i) - alat(i);
    ds = sqrt(dx^2 + dy^2);
    dcs(i) = dx / ds;
    dsn(i) = dy / ds;
  end
end

%
fprintf('setup constants for data reweighting\n');

% setup constants for data reweighting according to options
if is_wght == 1
  cutoff_dis = 5.15;
elseif is_wght == 2
  cutoff_dis = 10.0;
end

%
wt_az = 0.25;           % relative weight of mean azimuth to individual azimuth
np_site = 6;            % number of points to compute mean distance used as diameters of circlar weighting area
cfa1 = 2.0;             % coefficient for circlular area weighing
cfa2 = 2.0;             % coefficient for circular area weighting as substitute of voronoi area weighting

%
prmtr.cutoff_dis = cutoff_dis;
prmtr.wt_az = wt_az;

fprintf('  cutoff_dis = %.2f\n', cutoff_dis);
fprintf('  wt_az = %.2f\n', wt_az);
fprintf('  np_site = %d\n', np_site);
fprintf('  cfa1 = %.1f\n', cfa1);
fprintf('  cfa2 = %.1f\n', cfa2);

%
fprintf('\n=== Data Processing Summary ===\n');
fprintf('Horizontal stations: %d\n', nstnh);
fprintf('Vertical stations: %d\n', nstnv);
fprintf('Network center: (%.4f, %.4f)\n', xmean, ymean);
fprintf('Creep faults: %d\n', ncrp);
fprintf('Weighting scheme: distance=%d, spatial=%d\n', is_wght, id_wght);


veldath.nstnh         = nstnh ;
veldath.lonh  = lonh  ;
veldath.lath  = lath  ;
veldath.uxlh  = uxlh  ;
veldath.sxlh  = sxlh  ;
veldath.uylh  = uylh  ;
veldath.sylh  = sylh  ;
veldath.uzlh  = uzlh  ;
veldath.szlh  = szlh  ;
veldath.cxyh  = cxyh  ;

veldatv.nstnv = nstnv;
veldatv.lonv  = lonv ;
veldatv.latv  = latv ;
veldatv.uxlv  = uxlv ;
veldatv.sxlv  = sxlv ;
veldatv.uylv  = uylv ;
veldatv.sylv  = sylv ;
veldatv.uzlv  = uzlv ;
veldatv.szlv  = szlv ;
veldatv.cxyv  = cxyv ;

crpdat.ncrp = ncrp;
crpdat.alon = alon;
crpdat.alat = alat;
crpdat.blon = blon;
crpdat.blat = blat;
crpdat.dcs  = dcs ;
crpdat.dsn  = dsn ;

% prmtr.dpi         = []        ;
% prmtr.dcov        = []       ;
prmtr.cutoff_dis = cutoff_dis;
prmtr.wt_az      = wt_az     ;



% compute voronoi area
if id_wght == 2
  fprintf('compute voronoi area \n');
  
  %
  for i = 1:nstnh
    posh(1,i) = lonh(i);
    posh(2,i) = lath(i);
  end
  
  %
  pos_xy=posh(:,1:nstnh);
  pos_xy=pos_xy';
  areah = voronoi_area(pos_xy);
  
  %     areah = voron(nstnh, posh);
  
  % assign voronoi cell weights for horizontal gps velocity sites
  for i = 1:nstnh
    careah(i) = cmp_area1(nstnh, posh, np_site, cfa1, i);
    %if areah(i) == -1.0
    if isnan(areah(i)) == 1
      areah(i) = careah(i);
    end
    if areah(i) > cfa2 * careah(i)
      areah(i) = careah(i);
    end
  end
  
  %
  for i = 1:nstnv
    posv(1,i) = lonv(i);
    posv(2,i) = latv(i);
  end
  
  %
  %     areav = voron(nstnv, posv);
  pos_xy=posv(:,1:nstnv);
  pos_xy=pos_xy';
  areav = voronoi_area(pos_xy);
  
  % assign voronoi cell weights for vertical gps velocity sites
  for i = 1:nstnv
    careav(i) = cmp_area1(nstnv, posv, np_site, cfa1, i);
    %         if areav(i) == -1.0
    if isnan(areav(i)) == 1
      areav(i) = careav(i);
    end
    if areav(i) > cfa2 * careav(i)
      areav(i) = careav(i);
    end
  end
  
  fprintf('  voronoi area computed\n');
end


veldath.areah = areah ;
veldatv.areav = areav;

%
fprintf('horizontal gps velocity interpolation at stations ...\n');
perc_i=10;
for i = 1:nstnh
  % for i = 7:7
  % horizontal gps velocity interpolation and uncerstainty estimate
  perc=(i/nstnh*100);
  if perc > perc_i
    fprintf('  %d%%', floor(perc));
    perc_i=perc_i+10;
  end
  [iout, indxx, ainv, bb] = visr_h(lath(i), lonh(i), min_tau, max_tau, ...
    in_tau, is_wght, id_wght, wt0, rtau_h, isl);
  
  vx_sig(i) = sqrt(ainv(1,1));
  vy_sig(i) = sqrt(ainv(2,2));
  vxs(i) = bb(1);
  vys(i) = bb(2);
end
fprintf(' 100%%\n');

%  compute median for sigma of interpolated velocity at GPS site by bootstrapping
for i = 1:nstnh
  tmp(i) = sqrt(vx_sig(i)^2 + vy_sig(i)^2);
end
tmp_sorted = sortit(tmp, nstnh);
v_sig_m = tmp_sorted(floor(nstnh/2) + 1);

%
for i = 1:nstnh
  tmp(i) = sqrt((vxs(i) - uxlh(i))^2 + (vys(i) - uylh(i))^2);
end
tmp_sorted = sortit(tmp, nstnh);
vs_m = tmp_sorted(floor(nstnh/2) + 1);

%median values for formal uncertainty and bootstrapping residual and their ratio for 2D
wt_h = vs_m / v_sig_m;
fprintf('  v_sig_m,vs_m,wt_h: %10.4f %10.4f %10.4f\n', v_sig_m, vs_m, wt_h);

% output horizontal GPS velocity result from bootstrapping
fid41 = fopen(gps_h_outf, 'w');
fprintf(fid41, ' %10.4f%10.4f%10.4f\n', v_sig_m, vs_m, wt_h);
fprintf(fid41, ' 1site,2lon,3lat,4vE,5vE_interp,6vE_interp_sig,7vN,8vN_interp,9vN_interp_sig\n');
for i = 1:nstnh
  fprintf(fid41, '%8s%10.4f%10.4f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f\n', ...
    stnlh{i}, lonlh(i), latlh(i), uxlh(i), vxs(i), vx_sig(i), uylh(i), vys(i), vy_sig(i));
end
fclose(fid41);
% return;

%
fprintf('vertical gps velocity interpolation at stations ...\n');
perc_i=10;
for i = 1:nstnv
  perc=(i/nstnv*100);
  if perc > perc_i
    fprintf('  %d%%', floor(perc));
    perc_i=perc_i+10;
  end
  % horizontal gps velocity interpolation and uncerstainty estimate
  [indxx, iout, ainv, bb] = visr_h(latv(i), lonv(i), min_tau, max_tau, ...
    in_tau, is_wght, id_wght, wt0, rtau_h, isl);
  
  vx_sig(i) = sqrt(ainv(1,1));
  vy_sig(i) = sqrt(ainv(2,2));
  vxs(i) = bb(1);
  vys(i) = bb(2);
  
  % vertical gps velocity interpolation and uncerstainty estimate
  [iout, indxx, azinv, bbz] = visr_v(latv(i), lonv(i), min_tau, max_tau, ...
    in_tau, is_wght, id_wght, wt0, rtau_v, isl);
  
  vz_sig(i) = sqrt(azinv(1,1));
  vzs(i) = bbz(1);
end
fprintf(' 100%%\n');

%
for i = 1:nstnv
  tmp(i) = sqrt(vx_sig(i)^2 + vy_sig(i)^2 + vz_sig(i)^2);
end
tmp_sorted = sortit(tmp, nstnv);
vz_sig_m = tmp_sorted(floor(nstnv/2) + 1);

for i = 1:nstnv
  tmp(i) = sqrt((vxs(i) - uxlh(i))^2 + (vys(i) - uylh(i))^2 + (vzs(i) - uzlv(i))^2);
end
tmp_sorted = sortit(tmp, nstnv);
vzs_m = tmp_sorted(floor(nstnv/2) + 1); %compute median value of 3D GPS bootstrapping velocity residual RMS

wt_v = vzs_m / vz_sig_m;
fprintf('  vz_sig_m,vzs_m,wt_v: %10.4f %10.4f %10.4f\n', vz_sig_m, vzs_m, wt_v);

%
fid42 = fopen(gps_v_outf, 'w');
fprintf(fid42, ' %10.4f%10.4f%10.4f\n', vz_sig_m, vzs_m, wt_v); %median values for formal uncertainty and bootstrapping residual RMS and their ratio for 3D
fprintf(fid42, ' 1site,2lon,3lat,4vU,5vU_interp,6vU_interp_sig\n');
for i = 1:nstnv
  fprintf(fid42, '%8s%10.4f%10.4f%8.2f%8.2f%8.2f\n', ...
    stnlv{i}, lonlv(i), latlv(i), uzlv(i), vzs(i), vz_sig(i));
end
fclose(fid42);

%
fprintf('gps velocity interpolation for grid ...\n');
isl = 1;

fid79 = fopen(gps_prdf, 'w');
fprintf(fid79, '   1long      2lat       3Ve      4dVe       5Vn      6dVn     7Vu      8dVu    9Tau_h   10Tau_v\n');

%
nlon = floor((lonmax0 - lonmin0) / dlon + 1.1);
nlat = floor((latmax0 - latmin0) / dlat + 1.1);
iold = 0;
fprintf('  grid dimension(nx,ny,iold): %d %d %d\n', nlon, nlat, iold);

tic;

for i = 1:nlon
  fprintf('  i of nlon %d / %d\n', i, nlon);
  for j = 1:nlat
    iold = iold + 1;
    qlon(1) = lonmin0 + (i-1) * dlon;
    qlat(1) = latmin0 + (j-1) * dlat;
    zlon = qlon(1);
    zlat = qlat(1);
    
    %
    [qlat_local, qlon_local] = llxy(ymean, xmean, qlat, qlon, ms);
    qlat(1) = qlat_local(1);
    qlon(1) = qlon_local(1);
    
    % horizontal gps velocity interpolation and uncerstainty estimate
    [iout, indxx, ainv, bb, rtau] = visr_h(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_h,  isl);
    
    % vertical gps velocity interpolation and uncerstainty estimate
    [iout_v, indxx_v, azinv, bbz, rtauz] = visr_v(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_v,  isl);
    
    if (iout ~= 1) || (indxx ~= 0) || (iout_v ~= 1) || (indxx_v ~= 0)
      continue;
    end
    
    stdx = sqrt(ainv(1,1));
    stdy = sqrt(ainv(2,2));
    stdz = sqrt(azinv(1,1));
    
    if (stdx > 999.0) || (stdy > 999.0) || (stdz > 999.0)
      continue;
    end
    
    % output GPS only interpolation result
    fprintf(fid79, '%9.3f%9.3f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f\n', ...
      zlon, zlat, bb(1), stdx, bb(2), stdy, bbz(1), stdz, rtau, rtauz);
  end
end

fclose(fid79);
fprintf('  (zlon,zlat,bb(1:2),stdx,stdy,stdz)\n');
fprintf('  gps grid velocity field interpolation completed\n');
time_visr = toc;
fprintf('grid interpolation took: %.4f seconds\n', time_visr);

% return


% sort out sar overlap cells
fid17 = fopen(orbf, 'w');
fid19 = fopen(outf, 'w');

fprintf('iteration over sar input files ...\n');
for i = 1:nsarf %iteration over sar input files
  tmpstr = sarf{i};
  fprintf('reading %s\n', tmpstr);
  fprintf('  reading %d sar file data ...\n', i);
  fprintf('  lon,lat,sat_azi,sat_look,los,los_sig(unit?)\n');
  
  fid3 = fopen(sarf{i}, 'r');
  idat = 0;
  tlonmean(i) = 0;
  tlatmean(i) = 0;
  
  while true
    idat = idat + 1;
    if idat > maxpxl
      fprintf('%d # of data greater than allowance %d %d\n', i, idat, maxpxl);
      return;
    end
    
    line = fgetl(fid3);
    if line == -1
      break;
    end
    
    %  read in i-th sar satellite data
    tmps = sscanf(line, '%f %f %f %f %f %f');
    tlon(i,idat) = tmps(1);
    tlat(i,idat) = tmps(2);
    sazi(i,idat) = tmps(3);
    slook(i,idat) = tmps(4);
    clos(i,idat) = tmps(5);
    clos_s(i,idat) = tmps(6);
    
    %  satellite azimuthal angle
    %    ENVISAT files: descending ~= -77deg; ascending ~= 77deg
    %                                 -167                -13   %counter-clockwise from North (degrees)
    sazi(i,idat) = -90.0 - sazi(i,idat);
    %
    % ascending satellite flight direction
    %              \sazi=-13
    %               \  |
    %                \~|
    %                 \|
    %         --------------------
    %                  |\
    %                    ^
    
    % descending satellite flight direction
    %
    %                | v
    %               _ /
    %         -----( /-------------
    %              |/
    %   sazi=-167  /
    %             /
    %            /
    %
    
    %  LOS angle
    slook(i,idat) = 90.0 - slook(i,idat);  %convert LOS incidence angle to the angle between the LOS and vertical?
    %          |\
    % 90-theta-++\
    %          |  \
    %          |   \
    %          |    \
    %          |     \
    %          |      \
    %          | theta>\
    %          |---------
    
    %
    clos(i,idat) = 10.0 * clos(i,idat);
    clos_s(i,idat) = 10.0 * clos_s(i,idat) * f_sar(i);
    
    ilon = floor((tlon(i,idat) - lonmin) / dlon + 1.1);
    ilat = floor((tlat(i,idat) - latmin) / dlat + 1.1);
    isd(ilon,ilat) = isd(ilon,ilat) + 1;  % accumulate sar data entry count in grid
    
    tlonmean(i) = tlonmean(i) + tlon(i,idat);
    tlatmean(i) = tlatmean(i) + tlat(i,idat);
  end
  
  ndat(i) = idat - 1;
  tlonmean(i) = tlonmean(i) / ndat(i);  % compute mean lat/lon for the i-th sar dataset
  tlatmean(i) = tlatmean(i) / ndat(i);
  fclose(fid3);
end

stdsar = dlos0;
ms = 1;
iold = 0;
idata = 0;
hlfdlon = 0.5 * dlon;
hlfdlat = 0.5 * dlat;

% use grids with gps vertical data entries for ramp estimation
fprintf('use grids with gps vertical data entries for ramp estimation\n');
nlon = floor((lonmax - lonmin) / dlon + 1.1);
nlat = floor((latmax - latmin) / dlat + 1.1);
fprintf('nlon,nlat: %d %d\n', nlon, nlat);

for i = 1:nlon %iterate over lat/lon for ramp estimation
  zlon = lonmin + (i-1) * dlon;
  for j = 1:nlat
    zlat = latmin + (j-1) * dlat;
    
    % select signal InSAR data entry (multiple entries will be entered later)
    if isd(i,j) ~= 1
      continue;
    end
    
    % select grid with gps vertical data entry
    found_gps = false;
    for kk = 1:nstnv
      dlonv = abs(lonlv(kk) - zlon);
      dlatv = abs(latlv(kk) - zlat);
      if (dlonv < hlfdlon) && (dlatv < hlfdlat)
        found_gps = true;
        break;
      end
    end
    
    if ~found_gps
      continue;
    end
    
    iold = iold + 1;
    glon(iold) = zlon;
    glat(iold) = zlat;
    
    % search for InSAR data within grid
    for k = 1:nsarf
      for m = 1:ndat(k)
        dltlon = abs(tlon(k,m) - glon(iold));
        dltlat = abs(tlat(k,m) - glat(iold));
        if (dltlon < hlfdlon) && (dltlat < hlfdlat)
          idata = idata + 1;
          if ilos0 == 0
            stdsar = max(clos_s(k,m), dlos1);
          end
          b(idata) = clos(k,m) / stdsar;
          
          % construct partial derivatives for InSAR data
          a(idata, 3*(k-1)+1) = 1.0 / stdsar;
          a(idata, 3*(k-1)+2) = (glon(iold) - tlonmean(k)) / stdsar;
          a(idata, 3*(k-1)+3) = (glat(iold) - tlatmean(k)) / stdsar;
          
          a(idata, 3*nsarf+3*(iold-1)+1) = cos(dcov*sazi(k,m)) * cos(dcov*slook(k,m)) / stdsar;
          a(idata, 3*nsarf+3*(iold-1)+2) = sin(dcov*sazi(k,m)) * cos(dcov*slook(k,m)) / stdsar;
          a(idata, 3*nsarf+3*(iold-1)+3) = sin(dcov*slook(k,m)) / stdsar;
          break;
        end
      end
    end
    
    qlon(1) = glon(iold);
    qlat(1) = glat(iold);
    
    %
    [qlat_local, qlon_local] = llxy(ymean, xmean, qlat, qlon, ms);
    qlat(1) = qlat_local(1);
    qlon(1) = qlon_local(1);
    
    % horizontal gps velocity interpolation and uncerstainty estimate
    [iout, indxx, ainv, bb] = visr_h(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_h,   isl);
    
    if iout ~= 1
      error('iout not equal to 1 (error in gps horizontal velocity interpolation)');
    end
    
    if indxx ~= 0
      error('GPS velocity interpolation not valid (error in gps horizontal velocity interpolation)\nindxx: %d, glon: %f, glat: %f, 1', ...
        indxx, glon(iold), glat(iold));
    end
    
    % vertical gps velocity interpolation and uncerstainty estimate
    [iout_v, indxx_v, azinv, bbz] = visr_v(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_v,  isl);
    
    if iout_v ~= 1
      error('iout not equal to 1 (error in gps vertical velocity interpolation)');
    end
    
    if indxx_v ~= 0
      error('GPS velocity interpolation not valid (error in gps vertical velocity interpolation)\nindxx: %d, glon: %f, glat: %f, 1', ...
        indxx_v, glon(iold), glat(iold));
    end
    
    stdx = sqrt(ainv(1,1));
    stdy = sqrt(ainv(2,2));
    stdz = sqrt(azinv(1,1));
    
    %  construct partial derivatives for interpolated gps data
    idata = idata + 1;
    b(idata) = bb(1) / stdx;
    a(idata, 3*nsarf+3*(iold-1)+1) = 1.0 / stdx;
    a(idata, 3*nsarf+3*(iold-1)+2) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+3) = 0.0;
    
    idata = idata + 1;
    b(idata) = bb(2) / stdy;
    a(idata, 3*nsarf+3*(iold-1)+1) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+2) = 1.0 / stdy;
    a(idata, 3*nsarf+3*(iold-1)+3) = 0.0;
    
    idata = idata + 1;
    b(idata) = bbz(1) / stdz;
    a(idata, 3*nsarf+3*(iold-1)+1) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+2) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+3) = 1.0 / stdz;
  end
end




% select grids with multiple InSAR data entries for ramp estimation (at coarse grid points)
fprintf('select grids with multiple InSAR data entries for ramp estimation\n');
mlon = floor(dlon1 / dlon + 0.001);
mlat = floor(dlat1 / dlat + 0.001);
nlon = floor((lonmax - lonmin) / dlon1 + 1.1);
nlat = floor((latmax - latmin) / dlat1 + 1.1);
fprintf('nlon,nlat: %d %d\n', nlon, nlat);

for i = 1:nlon %iterate over lat/lon for data selection
  ilon = (i-1) * mlon + 1;
  for j = 1:nlat
    jlat = (j-1) * mlat + 1;
    
    % select InSAR data with multiple entries
    if isd(ilon, jlat) < 2
      continue;
    end
    
    iold = iold + 1;
    glon(iold) = lonmin + (i-1) * dlon1;
    glat(iold) = latmin + (j-1) * dlat1;
    
    %
    for k = 1:nsarf
      for m = 1:ndat(k)
        dltlon = abs(tlon(k,m) - glon(iold));
        dltlat = abs(tlat(k,m) - glat(iold));
        if (dltlon < hlfdlon) && (dltlat < hlfdlat) %search for data within grid
          idata = idata + 1;
          if ilos0 == 0
            stdsar = max(clos_s(k,m), dlos1);
          end
          b(idata) = clos(k,m) / stdsar;
          
          % construct partial derivatives for InSAR data
          a(idata, 3*(k-1)+1) = 1.0 / stdsar;
          a(idata, 3*(k-1)+2) = (glon(iold) - tlonmean(k)) / stdsar;
          a(idata, 3*(k-1)+3) = (glat(iold) - tlatmean(k)) / stdsar;
          
          a(idata, 3*nsarf+3*(iold-1)+1) = cos(dcov*sazi(k,m)) * cos(dcov*slook(k,m)) / stdsar;
          a(idata, 3*nsarf+3*(iold-1)+2) = sin(dcov*sazi(k,m)) * cos(dcov*slook(k,m)) / stdsar;
          a(idata, 3*nsarf+3*(iold-1)+3) = sin(dcov*slook(k,m)) / stdsar;
          break;
        end
      end
    end
    
    qlon(1) = glon(iold);
    qlat(1) = glat(iold);
    
    %
    [qlat_local, qlon_local] = llxy(ymean, xmean, qlat, qlon, ms);
    qlat(1) = qlat_local(1);
    qlon(1) = qlon_local(1);
    
    % horizontal gps velocity interpolation and uncerstainty estimate
    [iout, indxx, ainv, bb] = visr_h(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_h,  isl);
    
    % vertical gps velocity interpolation and uncerstainty estimate
    [iout_v, indxx_v, azinv, bbz] = visr_v(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_v,  isl);
    
    if iout ~= 1
      error('iout not equal to 1');
    end
    
    if indxx ~= 0
      error('GPS velocity interpolation not valid\nindxx: %d, glon: %f, glat: %f, 2', ...
        indxx, glon(iold), glat(iold));
    end
    
    stdx = sqrt(ainv(1,1));
    stdy = sqrt(ainv(2,2));
    stdz = sqrt(azinv(1,1));
    
    % construct partial derivatives for interpolated gps data
    idata = idata + 1;
    b(idata) = bb(1) / stdx;
    a(idata, 3*nsarf+3*(iold-1)+1) = 1.0 / stdx;
    a(idata, 3*nsarf+3*(iold-1)+2) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+3) = 0.0;
    
    idata = idata + 1;
    b(idata) = bb(2) / stdy;
    a(idata, 3*nsarf+3*(iold-1)+1) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+2) = 1.0 / stdy;
    a(idata, 3*nsarf+3*(iold-1)+3) = 0.0;
    
    idata = idata + 1;
    b(idata) = bbz(1) / stdz;
    a(idata, 3*nsarf+3*(iold-1)+1) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+2) = 0.0;
    a(idata, 3*nsarf+3*(iold-1)+3) = 1.0 / stdz;
  end
end

%
nold = iold;
npr = 3*nsarf + 3*nold;

% construct partial derivatives for ramp parameters
for i = 1:nsarf
  idata = idata + 1;
  b(idata) = 0.0;
  a(idata, 3*(i-1)+1) = 1.0 / offsig;
  
  idata = idata + 1;
  b(idata) = 0.0;
  a(idata, 3*(i-1)+2) = 1.0 / rmpsig;
  
  idata = idata + 1;
  b(idata) = 0.0;
  a(idata, 3*(i-1)+3) = 1.0 / rmpsig;
end

ndata = idata;

% aa = a' * a (aa = a^t*a)
for i = 1:npr
  for j = 1:i
    aat(i,j) = 0.0;
    for k = 1:ndata
      aat(i,j) = aat(i,j) + a(k,i) * a(k,j);
    end
    aat(j,i) = aat(i,j);
  end
end

% bb = a' * b (bb = a^t*b)
for i = 1:npr
  bbt(i) = 0.0;
  for j = 1:ndata
    bbt(i) = bbt(i) + a(j,i) * b(j);
  end
end

%solve b=a*x
[aat_lu, indx1, d] = dludcmp(aat, npr, maxdat);
bbt_sol = dlubksb(aat_lu, npr, maxdat, indx1, bbt);
bbt = bbt_sol;

% invert n
for i = 1:npr
  for j = 1:npr
    ainvm(i,j) = 0.0;
  end
  ainvm(i,i) = 1.0;
end

for i = 1:npr
  ainvm_col = dlubksb(aat_lu, npr, maxdat, indx1, ainvm(:,i));
  ainvm(:,i) = ainvm_col;
end

for i = 1:npr
  v(i) = sqrt(ainvm(i,i));
end

% output 3-D velocity solutions at grid points
fprintf('output 3-D velocity solutions at grid points\n');
fprintf(fid17, '  #   1long      2lat      3Ve    4dVe     5Vn   6dVn     7Vu   8dVu \n');

for i = 1:nold
  fprintf(fid17, '%3d%10.4f%8.4f %7.2f%6.2f %7.2f%6.2f %7.2f%6.2f\n', ...
    i, glon(i), glat(i), ...
    bbt(3*nsarf+3*(i-1)+1), v(3*nsarf+3*(i-1)+1), ...
    bbt(3*nsarf+3*(i-1)+2), v(3*nsarf+3*(i-1)+2), ...
    bbt(3*nsarf+3*(i-1)+3), v(3*nsarf+3*(i-1)+3));
end

fprintf(fid17, '      S0      S_sig       Se      Se_sig      Sn      Sn_sig\n');

% output orbital ramp parameters
for i = 1:nsarf
  fprintf(fid17, '%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f\n', ...
    bbt(3*(i-1)+1), v(3*(i-1)+1), ...
    bbt(3*(i-1)+2), v(3*(i-1)+2), ...
    bbt(3*(i-1)+3), v(3*(i-1)+3));
end

fclose(fid17);

%
fprintf(fid19, '   long      lat       Ve      dVe       Vn      dVn     Vu      dVu     Cen      Ceu      Cnu  \n');

% solve for displacements over grid points
fprintf('solve for displacements over grid points\n');
chisq0 = 0;
chisq1 = 0;
ndt = 0;
ngrd = 0;
nlon = floor((lonmax - lonmin) / dlon + 1.1);
nlat = floor((latmax - latmin) / dlat + 1.1);
fprintf('nlon,nlat: %d %d\n', nlon, nlat);

%
%
for i = 1:nlon  % iteration over grid points
  fprintf('  i of nlon %d / %d\n', i, nlon);
  for j = 1:nlat
    fprintf('    j of nlat %d / %d\n', j, nlat);
    zlon = lonmin + (i-1) * dlon;
    zlat = latmin + (j-1) * dlat;
    idata = 0;
    sazis(:)=0;
    
    % iteration over sar satellites
    for k = 1:nsarf
      % iteration over satellite data points
      for m = 1:ndat(k)
        dltlon = abs(tlon(k,m) - zlon);
        dltlat = abs(tlat(k,m) - zlat);
        
        %
        if (dltlon < hlfdlon) && (dltlat < hlfdlat)
          ndt = ndt + 1;
          idata = idata + 1;
          
          if ilos0 == 0
            stdsar = max(clos_s(k,m), dlos1);
          end
          
          % sar data available at the grid point
          clos_c = clos(k,m) - bbt(3*(k-1)+1) - ...
            bbt(3*(k-1)+2) * (zlon - tlonmean(k)) - ...
            bbt(3*(k-1)+3) * (zlat - tlatmean(k));
          
          b(idata) = clos_c / stdsar;
          bsv(idata) = clos_c;
          
          % construct partial derivatives for InSAR data
          a(idata,1) = cos(dcov*sazi(k,m)) * cos(dcov*slook(k,m)) / stdsar;
          a(idata,2) = sin(dcov*sazi(k,m)) * cos(dcov*slook(k,m)) / stdsar;
          a(idata,3) = sin(dcov*slook(k,m)) / stdsar;
          
          asv(idata,1) = cos(dcov*sazi(k,m)) * cos(dcov*slook(k,m));
          asv(idata,2) = sin(dcov*sazi(k,m)) * cos(dcov*slook(k,m));
          asv(idata,3) = sin(dcov*slook(k,m));
          
          isat(idata) = k;
          sazis(idata)=sazi(k,m);
          break;  %   goto 163
        end
      end
    end
    
    
    %         if idata == 0
    if idata < 2
      continue;  %  goto 110
    end
    
    has_asc=find(sazis(1:idata) > -90);
    has_des=find(sazis(1:idata) < -90);
    if isempty(has_asc) || isempty(has_des)
      %           fprintf('  no ascending/descending insar data!\n');
      continue; %NO ascending or descending data
    end
    if idata == 4
      idata  %  goto 110
    end
    
    ngrd = ngrd + 1;
    qlon(1) = zlon;
    qlat(1) = zlat;
    
    %
    [qlat_local, qlon_local] = llxy(ymean, xmean, qlat, qlon, ms);
    qlat(1) = qlat_local(1);
    qlon(1) = qlon_local(1);
    
    % gps horizontal velocity uncertainty estimation
    [iout, indxx, ainv, bb] = visr_h(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_h,   isl);
    % gps vertical velocity uncertainty estimation
    [iout_v, indxx_v, azinv, bbz] = visr_v(qlat(1), qlon(1), min_tau, max_tau, ...
      in_tau, is_wght, id_wght, wt0, rtau_v,   isl);
    
    if iout ~= 1 || iout_v ~=1
      error('iout not equal to 1. failed in horizontal/vertical gps velocity interpolation');
    end
    
    if indxx ~= 0 || indxx_v ~= 0
      error('horizontal/vertical GPS velocity interpolation not valid\nindxx: %d, glon: %f, glat: %f, 3', ...
        indxx, glon(iold), glat(iold));
    end
    
    stdx = sqrt(ainv(1,1));
    stdy = sqrt(ainv(2,2));
    stdz = sqrt(azinv(1,1));
    
    if idvu0 == 1
      stdz = dvu0;
    end
    
    %         fprintf('  interpolated gps stdx,stdy,stdz: %f %f %f\n', stdx, stdy, stdz);
    %         fprintf('  interpolated gps bb,bbz,idata: %f %f %f %d\n', bb(1), bb(2), bbz(1), idata);
    
    % construct partial derivatives for gps data
    %   gps east-west component
    if use_gps_enu(1) == 1
      idata = idata + 1;
      b(idata) = bb(1) / stdx;
      a(idata,1) = 1.0 / stdx;
      a(idata,2) = 0.0;
      a(idata,3) = 0.0;
    end
    
    %   gps north-south component
    if use_gps_enu(2) == 1
      idata = idata + 1;
      b(idata) = bb(2) / stdy;
      a(idata,1) = 0.0;
      a(idata,2) = 1.0 / stdy;
      a(idata,3) = 0.0;
    end
    
    %   gps up-down component
    if use_gps_enu(3) == 1
      idata = idata + 1;
      b(idata) = bbz(1) / stdz;
      a(idata,1) = 0.0;
      a(idata,2) = 0.0;
      a(idata,3) = 1.0 / stdz;
    end
    
    npr = 3;
    ndata = idata;
    
    %  aa = a' * a
    for k = 1:npr
      for nl = 1:k
        aat(k,nl) = 0.0;
        for m = 1:ndata
          aat(k,nl) = aat(k,nl) + a(m,k) * a(m,nl);
        end
        aat(nl,k) = aat(k,nl);
      end
    end
    
    %   bb = a' * b
    for k = 1:npr
      bbr(k) = 0.0;
      for nl = 1:ndata
        bbr(k) = bbr(k) + a(nl,k) * b(nl);
      end
    end
    
    %   b = a * x
    %solve b=a*x.
    [aat_lu, indx1, d] = dludcmp(aat, npr, maxdat);
    bbr_sol = dlubksb(aat_lu, npr, maxdat, indx1, bbr);
    bbr = bbr_sol;
    
    %  invert n
    for k = 1:npr
      for nl = 1:npr
        ainvm(k,nl) = 0.0;
      end
      ainvm(k,k) = 1.0;
    end
    
    %         fprintf('npr,maxdat: %d %d\n', npr, maxdat);
    
    for k = 1:npr
      ainvm_col = dlubksb(aat_lu, npr, maxdat, indx1, ainvm(:,k));
      ainvm(:,k) = ainvm_col;
    end
    
    for k = 1:npr
      v(k) = sqrt(ainvm(k,k));
    end
    
    %
    cv12 = ainvm(1,2) / (v(1) * v(2));
    cv13 = ainvm(1,3) / (v(1) * v(3));
    cv23 = ainvm(2,3) / (v(2) * v(3));
    
    %         fprintf('3d: %f %f %f %f %f %f %f %f %f %f\n', ...
    %             zlon, zlat, bbr(1), v(1), bbr(2), v(2), bbr(3), v(3), cv12, cv13, cv23);
    
    %  output 3-D velocity solutions
    fprintf(fid19, '%9.3f%9.3f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f\n', ...
      zlon, zlat, bbr(1), v(1), bbr(2), v(2), bbr(3), v(3), cv12, cv13, cv23);
    
    % output sar data fitting residuals
    for k = 1:(idata-3)
      prd(k) = 0;
      prd0(k) = 0;
      for m = 1:3
        prd(k) = prd(k) + a(k,m) * bbr(m);
        prd0(k) = prd0(k) + asv(k,m) * bbr(m);
      end
      
      db = b(k) - prd(k);
      db0 = bsv(k) - prd0(k);
      chisq1 = chisq1 + db * db;
      chisq0 = chisq0 + db0 * db0;
      
      fprintf(fid11, '%4d%9.3f%9.3f%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f\n', ...
        isat(k), zlon, zlat, b(k), prd(k), db, bsv(k), prd0(k), db0);
    end
  end
end

fclose(fid19);

%
fprintf(fid11, 'Total weighted postfit residual Chisquare: %10.3f  %6d\n', chisq1, ndt);
fprintf(fid11, 'Reduced weighted postfit residual Chisquare: %10.3f\n', chisq1/ndt);
fprintf(fid11, 'Total postfit residual Chisquare (mm/yr)^2: %10.3f  %6d\n', chisq0, ndt);
fprintf(fid11, 'Reduced postfit residual Chisquare (mm/yr)^2: %10.3f\n', chisq0/ndt);

fclose(fid11);

fprintf('Program completed successfully.\n');
% end