function [iout, indxx, ainv, bb, rtau, wtt, chisq, nslct] = ...
  visr_h(yy, xx, min_tau, max_tau, in_tau, is_wght, id_wght, ...
  wt0, sigma0,   isl)
% VISR_H GPS horizontal velocity interpolation subroutine
% Converted from Fortran code

% 声明全局�?��?（对应Fortran的COMMON blocks）
global veldath crpdat ainv_vh prmtr

% 从全局�?��?获�?�数�?�
nstn = veldath.nstnh;
lon = veldath.lonh;
lat = veldath.lath;
uxl = veldath.uxlh;
sxl = veldath.sxlh;
uyl = veldath.uylh;
syl = veldath.sylh;
uzl = veldath.uzlh;
szl = veldath.szlh;
cxy = veldath.cxyh;
area = veldath.areah;

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

% �?�数定义
maxsit = 2000;

% �?始化�?��?
iout = 0;

% 计算�?�值点与蠕�?�断层的关系
icrp = zeros(ncrp, 1);
is0 = zeros(ncrp, 1);
crp_strk1 = zeros(ncrp, 1);
crp_strk2 = zeros(ncrp, 1);

ainv=ainv_vh.ainv;
bb=ainv_vh.bb;

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
  nslct = 0;
  rtau = itau;
  
  istn = zeros(maxsit, 1);
  
  for i = 1:nstn
    dx = lon(i) - xx;
    dy = lat(i) - yy;
    dr = sqrt(dy^2 + dx^2);
    
    if isl == 0 && dr < 0.0001
      continue;
    end
    
    ang_s1 = atan2(dy, dx) / dcov;
    rt = dr / rtau;
    
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
        nslct = nslct + 1;
        if nslct > maxsit
          error(['# of selected stations > maxsit', num2str(maxsit), ...
            ', please increase maxsit (visr_core)...']);
        end
        istn(nslct) = i;
      end
    end
  end
  
  if nslct < 3
    indxx = 1;
    continue;
  end
  
  % 估计方�?角覆盖
  azi = zeros(nslct, 1);
  for i = 1:nslct
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
  for i = 1:nslct
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
  wght = ones(nslct, 1);
  
  if id_wght == 1
    azi_avrg = wt_az * 360.0 / nslct;
    azi_tot = (1.0 + wt_az) * 360.0;
    
    for i = 1:nslct
      daz1 = 180.0;
      daz2 = -180.0;
      
      for j = 1:nslct
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
      
      wght(i) = (0.5 * (daz1 - daz2) + azi_avrg) * nslct / azi_tot;
    end
    
  elseif id_wght == 2
    sum_area = sum(area(istn(1:nslct)));
    sum_area = sum_area / nslct;
    
    for i = 1:nslct
      wght(i) = area(istn(i)) / sum_area;
    end
  end
  
  % 构建设计矩阵和观测�?��?（水平速度：2个分�?）
  npr = 2 * nslct;
  wtt = 0.0;
  
  a = zeros(npr, 6);
  a_s = zeros(npr, 6);
  b = zeros(npr, 1);
  
  for i = 1:nslct
    station_idx = istn(i);
    dy = lat(station_idx) - yy;
    dx = lon(station_idx) - xx;
    dr = sqrt(dx^2 + dy^2);
    
    if is_wght == 1
      wti = exp(-dr / rtau) * wght(i);
      wti_sig = exp(-dr / sigma0) * wght(i);
    elseif is_wght == 2
      wti = wght(i) / ((dr / rtau)^2 + 1);
      wti_sig = wght(i) / ((dr / sigma0)^2 + 1);
    end
    
    wtt = wtt + 2 * wti;
    
    % X分�?设计矩阵
    a(2*i-1, 1) = wti / sxl(station_idx);
    a(2*i-1, 2) = 0.0;
    a(2*i-1, 3) = wti * dx / sxl(station_idx);
    a(2*i-1, 4) = wti * dy / sxl(station_idx);
    a(2*i-1, 5) = 0.0;
    a(2*i-1, 6) = wti * dy / sxl(station_idx);
    
    % Y分�?设计矩阵
    a(2*i, 1) = 0.0;
    a(2*i, 2) = wti / syl(station_idx);
    a(2*i, 3) = 0.0;
    a(2*i, 4) = wti * dx / syl(station_idx);
    a(2*i, 5) = wti * dy / syl(station_idx);
    a(2*i, 6) = -1.0 * wti * dx / syl(station_idx);
    
    % 对应sigma的设计矩阵
    a_s(2*i-1, 1) = wti_sig / sxl(station_idx);
    a_s(2*i-1, 2) = 0.0;
    a_s(2*i-1, 3) = wti_sig * dx / sxl(station_idx);
    a_s(2*i-1, 4) = wti_sig * dy / sxl(station_idx);
    a_s(2*i-1, 5) = 0.0;
    a_s(2*i-1, 6) = wti_sig * dy / sxl(station_idx);
    
    a_s(2*i, 1) = 0.0;
    a_s(2*i, 2) = wti_sig / syl(station_idx);
    a_s(2*i, 3) = 0.0;
    a_s(2*i, 4) = wti_sig * dx / syl(station_idx);
    a_s(2*i, 5) = wti_sig * dy / syl(station_idx);
    a_s(2*i, 6) = -1.0 * wti_sig * dx / syl(station_idx);
    
    % 观测�?��?
    b(2*i-1) = wti * uxl(station_idx) / sxl(station_idx);
    b(2*i) = wti * uyl(station_idx) / syl(station_idx);
  end
  
  if wtt < wt0
    indxx = 3;
    continue;
  end
  
  % 构建法方程矩阵
  ww = sum(b.^2);
  
  aa = zeros(6, 6);
  aa_s = zeros(6, 6);
  
  for i = 1:6
    for j = 1:i
      aa(i, j) = sum(a(:, i) .* a(:, j));
      aa(j, i) = aa(i, j);
      
      aa_s(i, j) = sum(a_s(:, i) .* a_s(:, j));
      aa_s(j, i) = aa_s(i, j);
    end
  end
  
  aasv = aa;
  
  bb_vec = zeros(6, 1);
  for i = 1:6
    bb_vec(i) = sum(a(:, i) .* b);
  end
  
  % 求解线性系统
  bb_sol = aa \ bb_vec;
  bb = bb_sol;
  
  % 计算�??方差矩阵
  ainv = aa_s \ eye(6);
  
  % 计算�?�方值
  xaax = bb' * aasv * bb;
  chisq = ww - xaax;
  
  break;  % �?功找到�?�适的tau值，退出循环
  
end  % itau循环

end  % 函数结�?�
