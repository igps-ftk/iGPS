function [iout, indxx, azinv, bbz, rtauz, wttz, chisqz, nslctz] = ...
         visr_v(yy, xx, min_tau, max_tau, in_tau, is_wght, id_wght, ...
                wt0, sigma0, isl)
% VISR_V GPS vertical velocity interpolation subroutine
% Converted from Fortran code

% Declare global variables (corresponding to Fortran COMMON blocks)
global veldatv crpdat ainv_vv prmtr maxsit

% Get data from global variables
nstn = veldatv.nstnv;
lon = veldatv.lonv;
lat = veldatv.latv;
uzl = veldatv.uzlv;
szl = veldatv.szlv;
area = veldatv.areav;

ncrp = crpdat.ncrp;
alon = crpdat.alon(1:ncrp);
alat = crpdat.alat(1:ncrp);
blon = crpdat.blon(1:ncrp);
blat = crpdat.blat(1:ncrp);
dcs = crpdat.dcs(1:ncrp);
dsn = crpdat.dsn(1:ncrp);

dcov = prmtr.dcov;
cutoff_dis = prmtr.cutoff_dis;
wt_az = prmtr.wt_az;

azinv = ainv_vv.azinv;
bbz = ainv_vv.bbz;

% Parameter definition
%maxsit = 2000;

% Initialize output variables
iout = 0;

% Pre-calculate station distances and angles relative to interpolation point
dx_stations = lon - xx;
dy_stations = lat - yy;
dr_stations = sqrt(dx_stations.^2 + dy_stations.^2);
ang_stations = atan2(dy_stations, dx_stations) / dcov;

% Calculate creep fault relationships (vectorized)
dx1 = alon - xx;
dy1 = alat - yy;
ang1 = atan2(dy1, dx1) / dcov;
y1_values = -1.0 * (dy1 .* dcs - dx1 .* dsn);

dx2 = blon - xx;
dy2 = blat - yy;
ang2 = atan2(dy2, dx2) / dcov;

% Initialize creep fault arrays
icrp = zeros(ncrp, 1);
is0 = zeros(ncrp, 1);
crp_strk1 = zeros(ncrp, 1);
crp_strk2 = zeros(ncrp, 1);

for i = 1:ncrp
    if ang1(i) == ang2(i)
        is0(i) = 0;  % Interpolation point on creep fault extension line
        continue;
    end
    
    if y1_values(i) == 0.0
        indxx = 4;  % Interpolation point on creep fault
        return;
    end
    
    is0(i) = sign(y1_values(i));
    
    % Order angles consistently
    if ang2(i) > ang1(i)
        crp_strk1(i) = ang1(i);
        crp_strk2(i) = ang2(i);
    else
        crp_strk1(i) = ang2(i);
        crp_strk2(i) = ang1(i);
    end
    
    % Handle angle wrapping
    dang = crp_strk2(i) - crp_strk1(i);
    if dang > 180.0
        ang_tmp = crp_strk1(i) + 360.0;
        crp_strk1(i) = crp_strk2(i);
        crp_strk2(i) = ang_tmp;
        icrp(i) = 1;
    end
end

% Main tau iteration loop
tau_values = min_tau:in_tau:max_tau;
for itau_idx = 1:length(tau_values)
    itau = tau_values(itau_idx);
    
    % Check if this is the last iteration
    if itau == max_tau
        break;
    end
    
    indxx = 0;
    nslctz = 0;
    rtauz = itau;
    
    % Pre-calculate selection criteria
    rt_ratios = dr_stations / rtauz;
    valid_stations = rt_ratios <= cutoff_dis;
    
    % Apply self-distance check
    if isl == 0
        valid_stations = valid_stations & (dr_stations >= 0.0001);
    end
    
    % Filter stations based on creep fault criteria
    istn = [];
    for i = 1:nstn
        if ~valid_stations(i)
            continue;
        end
        
        skip_station = false;
        ang_s1 = ang_stations(i);
        
        for j = 1:ncrp
            % Handle angle wrapping for this creep fault
            if icrp(j) == 1 && ang_s1 < 0.0
                ang_s1_temp = ang_s1 + 360.0;
            else
                ang_s1_temp = ang_s1;
            end
            
            % Calculate y1 for station relative to creep fault
            dx_crp = alon(j) - lon(i);
            dy_crp = alat(j) - lat(i);
            y1_station = -1.0 * (dy_crp * dcs(j) - dx_crp * dsn(j));
            
            if y1_station == 0.0
                is1 = 0;
            else
                is1 = sign(y1_station);
            end
            
            % Check if station should be skipped
            if is1 ~= is0(j) && ang_s1_temp >= crp_strk1(j) && ang_s1_temp <= crp_strk2(j)
                skip_station = true;
                break;
            end
        end
        
        if ~skip_station
            nslctz = nslctz + 1;
            if nslctz > maxsit
                error(['Number of selected stations > maxsit: ', num2str(maxsit), ...
                       ', please increase maxsit in visr_core...']);
            end
            istn(nslctz) = i;
        end
    end
    
    % Check minimum station count
    if nslctz < 3
        indxx = 1;
        continue;
    end
    
    % Calculate azimuth coverage
    azi = ang_stations(istn);
    azimax = max(azi);
    azimin = min(azi);
    dazim1 = azimax - azimin;
    
    if dazim1 <= 180.0
        indxx = 2;
        continue;
    end
    
    % Adjust azimuths to 0-360 range
    azi(azi < 0.0) = azi(azi < 0.0) + 360.0;
    azimax = max(azi);
    azimin = min(azi);
    dazim2 = azimax - azimin;
    
    if dazim2 <= 180.0
        indxx = 2;
        continue;
    end
    
    iout = 1;
    
    % Calculate spatial density weights
    wght = ones(nslctz, 1);
    
    if id_wght == 1
        % Azimuth-based weighting
        azi_avrg = wt_az * 360.0 / nslctz;
        azi_tot = (1.0 + wt_az) * 360.0;
        
        for i = 1:nslctz
            % Find nearest stations in azimuth (vectorized)
            dazi_all = azi - azi(i);
            dazi_all(dazi_all > 180.0) = dazi_all(dazi_all > 180.0) - 360.0;
            dazi_all(dazi_all < -180.0) = dazi_all(dazi_all < -180.0) + 360.0;
            
            dazi_positive = dazi_all(dazi_all > 0.0);
            dazi_negative = dazi_all(dazi_all < 0.0);
            
            daz1 = min([dazi_positive; 180.0]);
            daz2 = max([dazi_negative; -180.0]);
            
            wght(i) = (0.5 * (daz1 - daz2) + azi_avrg) * nslctz / azi_tot;
        end
        
    elseif id_wght == 2
        % Area-based weighting
        sum_area = sum(area(istn)) / nslctz;
        wght = area(istn) / sum_area;
    end
    
    % Build design matrix and observation vector for vertical component
    wttz = 0.0;
    indr = 0;
    
    % Pre-allocate matrices for vertical component (3 parameters)
    az = zeros(nslctz, 3);
    az_s = zeros(nslctz, 3);
    bz = zeros(nslctz, 1);
    
    for i = 1:nslctz
        station_idx = istn(i);
        dx = dx_stations(station_idx);
        dy = dy_stations(station_idx);
        dr = dr_stations(station_idx);
        
        % Calculate weights
        if is_wght == 1
            wti = exp(-dr / rtauz) * wght(i);
            wti_sig = exp(-dr / sigma0) * wght(i);
        elseif is_wght == 2
            wti = wght(i) / ((dr / rtauz)^2 + 1);
            wti_sig = wght(i) / ((dr / sigma0)^2 + 1);
        end
        
        % Check if station is very close
        if dr < 1.0
            indr = 1;
        end
        
        wttz = wttz + 2 * wti;
        
        % Design matrix for vertical component
        az(i, 1) = wti / szl(station_idx);
        az(i, 2) = wti * dx / szl(station_idx);
        az(i, 3) = wti * dy / szl(station_idx);
        
        % Sigma design matrix
        az_s(i, 1) = wti_sig / szl(station_idx);
        az_s(i, 2) = wti_sig * dx / szl(station_idx);
        az_s(i, 3) = wti_sig * dy / szl(station_idx);
        
        % Observation vector
        bz(i) = wti * uzl(station_idx) / szl(station_idx);
    end
    
    % Check total weight threshold
    if wttz < wt0
        indxx = 3;
        continue;
    end
    
    % Build normal equation matrix using efficient matrix multiplication
    ww = sum(bz.^2);
    aaz = az' * az;
    aaz_s = az_s' * az_s;
    
    bbz_vec = az' * bz;
    
    % Solve linear system
    bbz_sol = aaz \ bbz_vec;
    bbz = bbz_sol;
    
    % Calculate covariance matrix
    azinv = aaz_s \ eye(3);
    
    % Calculate chi-square value
    xaax = bbz' * aaz * bbz;
    chisqz = ww - xaax;
    
    break;  % Successfully found suitable tau value, exit loop
    
end  % tau iteration loop

end  % function end