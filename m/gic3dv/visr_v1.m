function [iout, indxx, azinv, bbz, rtauz, wttz, chisqz, nslctz] = ...
         visr_v(yy, xx, min_tau, max_tau, in_tau, is_wght, id_wght, ...
                wt0, sigma0,  isl)
% VISR_V GPS vertical velocity interpolation subroutine
% Converted from Fortran code

% 声明全局�?��?（对应Fortran的COMMON blocks）
global veldatv crpdat ainv_vv prmtr

% 从全局�?��?获�?�数�?�
nstn = veldatv.nstnv;
lon = veldatv.lonv;
lat = veldatv.latv;
uxl = veldatv.uxlv;
sxl = veldatv.sxlv;
uyl = veldatv.uylv;
syl = veldatv.sylv;
uzl = veldatv.uzlv;
szl = veldatv.szlv;
cxy = veldatv.cxyv;
area = veldatv.areav;

ncrp = crpdat.ncrp;
alon = crpdat.alon(1:ncrp);
alat = crpdat.alat(1:ncrp);
blon = crpdat.blon(1:ncrp);
blat = crpdat.blat(1:ncrp);
dcs = crpdat.dcs(1:ncrp);
dsn = crpdat.dsn(1:ncrp);

dpi = prmtr.dpi;
dcov = prmtr.dcov;
cutoff_dis = prmtr.cutoff_dis;
wt_az = prmtr.wt_az;

azinv=ainv_vv.azinv;
bbz=ainv_vv.bbz;

% �?�数定义
maxsit = 2000;

% �?始化�?��?
iout = 0;

% 计算�?�值点与蠕�?�断层的关系
icrp = zeros(ncrp, 1);
is0 = zeros(ncrp, 1);
crp_strk1 = zeros(ncrp, 1);
crp_strk2 = zeros(ncrp, 1);

for i = 1:ncrp
    icrp(i) = 0;
    
    dx = alon(i) - xx;
    dy = alat(i) - yy;
    ang1 = atan2(dy, dx) / dcov;
    y1 = -1.0 * (dy * dcs(i) - dx * dsn(i));
    
    dx = blon(i) - xx;
    dy = blat(i) - yy;
    ang2 = atan2(dy, dx) / dcov;
    
    if ang1 == ang2
        is0(i) = 0;  % �?�值点�?于蠕�?�断层延长线上？
        continue;
    end
    
    if y1 == 0.0
        indxx = 4;  % �?�值点�?于蠕�?�断层上
        return;
    end
    
    is0(i) = sign(y1);
    
    if ang2 > ang1
        crp_strk1(i) = ang1;
        crp_strk2(i) = ang2;
    else
        crp_strk1(i) = ang2;
        crp_strk2(i) = ang1;
    end
    
    dang = crp_strk2(i) - crp_strk1(i);
    if dang > 180.0
        ang_tmp = crp_strk1(i) + 360.0;
        crp_strk1(i) = crp_strk2(i);
        crp_strk2(i) = ang_tmp;
        icrp(i) = 1;
    end
end

% 开始数�?�选择
for itau = min_tau:in_tau:max_tau
    if itau == max_tau
        break;
    end
    
    indxx = 0;
    nslctz = 0;
    rtauz = itau;
    
    istn = zeros(maxsit, 1);
    
    for i = 1:nstn
        dx = lon(i) - xx;
        dy = lat(i) - yy;
        dr = sqrt(dy^2 + dx^2);
        
        if isl == 0 && dr < 0.0001
            continue;
        end
        
        ang_s1 = atan2(dy, dx) / dcov;
        rt = dr / rtauz;
        
        if rt <= cutoff_dis
            skip_station = false;
            
            for j = 1:ncrp
                if icrp(j) == 1 && ang_s1 < 0.0
                    ang_s1 = ang_s1 + 360.0;
                end
                
                dx = alon(j) - lon(i);
                dy = alat(j) - lat(i);
                y1 = -1.0 * (dy * dcs(j) - dx * dsn(j));
                
                if y1 == 0.0
                    is1 = 0;
                else
                    is1 = sign(y1);
                end
                
                if is1 ~= is0(j)
                    if ang_s1 >= crp_strk1(j) && ang_s1 <= crp_strk2(j)
                        skip_station = true;
                        break;
                    end
                end
            end
            
            if ~skip_station
                nslctz = nslctz + 1;
                if nslctz > maxsit
                    error(['# of selected stations > maxsit', num2str(maxsit), ...
                           ', please increase maxsit (visr_core)...']);
                end
                istn(nslctz) = i;
            end
        end
    end
    
    if nslctz < 3
        indxx = 1;
        continue;
    end
    
    % 估计方�?角覆盖
    azi = zeros(nslctz, 1);
    for i = 1:nslctz
        dx = lon(istn(i)) - xx;
        dy = lat(istn(i)) - yy;
        azi(i) = atan2(dy, dx) / dcov;
    end
    
    azimax = max(azi);
    azimin = min(azi);
    dazim1 = azimax - azimin;
    
    if dazim1 <= 180.0
        indxx = 2;
        continue;
    end
    
    % 调整方�?角到0-360范围
    for i = 1:nslctz
        if azi(i) < 0.0
            azi(i) = azi(i) + 360.0;
        end
    end
    
    azimax = max(azi);
    azimin = min(azi);
    dazim2 = azimax - azimin;
    
    if dazim2 <= 180.0
        indxx = 2;
        continue;
    end
    
    iout = 1;
    
    % 计算数�?�空间密度�?��?
    wght = ones(nslctz, 1);
    
    if id_wght == 1
        azi_avrg = wt_az * 360.0 / nslctz;
        azi_tot = (1.0 + wt_az) * 360.0;
        
        for i = 1:nslctz
            daz1 = 180.0;
            daz2 = -180.0;
            
            for j = 1:nslctz
                if i == j
                    continue;
                end
                
                dazi = azi(j) - azi(i);
                if dazi > 180.0
                    dazi = dazi - 360.0;
                end
                if dazi < -180.0
                    dazi = dazi + 360.0;
                end
                
                if dazi > 0.0 && dazi < daz1
                    daz1 = dazi;
                end
                if dazi < 0.0 && dazi > daz2
                    daz2 = dazi;
                end
            end
            
            wght(i) = (0.5 * (daz1 - daz2) + azi_avrg) * nslctz / azi_tot;
        end
        
    elseif id_wght == 2
        sum_area = sum(area(istn(1:nslctz)));
        sum_area = sum_area / nslctz;
        
        for i = 1:nslctz
            wght(i) = area(istn(i)) / sum_area;
        end
    end
    
    % 计算�?��?矩阵
    indr = 0;
    wttz = 0.0;
    
    az = zeros(nslctz, 3);
    az_s = zeros(nslctz, 3);
    bz = zeros(nslctz, 1);
    
    for i = 1:nslctz
        station_idx = istn(i);
        dy = lat(station_idx) - yy;
        dx = lon(station_idx) - xx;
        dr = sqrt(dx^2 + dy^2);
        
        if is_wght == 1
            wti = exp(-dr / rtauz) * wght(i);
            wti_sig = exp(-dr / sigma0) * wght(i);
        elseif is_wght == 2
            wti = wght(i) / ((dr / rtauz)^2 + 1);
            wti_sig = wght(i) / ((dr / sigma0)^2 + 1);
        end
        
        if dr < 1.0
            indr = 1;
        end
        
        wttz = wttz + 2 * wti;
        
        az(i, 1) = wti / szl(station_idx);
        az(i, 2) = wti * dx / szl(station_idx);
        az(i, 3) = wti * dy / szl(station_idx);
        
        az_s(i, 1) = wti_sig / szl(station_idx);
        az_s(i, 2) = wti_sig * dx / szl(station_idx);
        az_s(i, 3) = wti_sig * dy / szl(station_idx);
        
        bz(i) = wti * uzl(station_idx) / szl(station_idx);
    end
    
    if wttz < wt0
        indxx = 3;
        continue;
    end
    
    % 构建法方程矩阵
    ww = sum(bz.^2);
    
    aaz = zeros(3, 3);
    aaz_s = zeros(3, 3);
    
    for i = 1:3
        for j = 1:i
            aaz(i, j) = sum(az(:, i) .* az(:, j));
            aaz(j, i) = aaz(i, j);
            
            aaz_s(i, j) = sum(az_s(:, i) .* az_s(:, j));
            aaz_s(j, i) = aaz_s(i, j);
        end
    end
    
    aazsv = aaz;
    
    bbz = zeros(3, 1);
    for i = 1:3
        bbz(i) = sum(az(:, i) .* bz);
    end
    
    % 求解线性系统
    bbz_sol = aaz \ bbz;
    bbz = bbz_sol;
    
    % 计算�??方差矩阵
    azinv = aaz_s \ eye(3);
    
    % 计算�?�方值
    xaax = bbz' * aazsv * bbz;
    chisqz = ww - xaax;
    
    break;  % �?功找到�?�适的tau值，退出循环
    
end  % itau循环

end  % 函数结�?�
