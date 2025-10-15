function [slat, slon] = llxy(slatm, slonm, slat, slon, m)
% LLXY - Transfer latitude and longitude into local x y coordinates.
%
% Input:
%   slatm: latitude of reference point for local coordinates, in degree.
%   slonm: longitude of reference point for local coordinates, in degree.
%   slat : array coordinate of latitude to be transferred, in degree.
%   slon : array coordinate of longitude to be transferred, in degree.
%   m    : number of points to be transferred.
%
% Output:
%   slat : y array after transformation in km.
%   slon : x array after transformation in km.

% Convert degrees to radians
pi_val = 4.0 * atan(1.0);
rlatc = slatm * pi_val / 180.0;
rlonc = slonm * pi_val / 180.0;

% Calculate local radius of curvature (r) using reference earth NAD 1983
% (semi-major axis is 6378.137, flattening factor is 1/298.2572)
flat = 1.0 / 298.2572;
esq = 2.0 * flat - flat^2;
q = 1.0 - esq * sin(rlatc)^2;
r = 6378.137 * sqrt(1.0 - esq) / q;

% Construct transformation matrix
t = zeros(3,3);
t(1,1) =  sin(rlatc) * cos(rlonc);
t(1,2) =  sin(rlatc) * sin(rlonc);
t(1,3) = -cos(rlatc);
t(2,1) = -sin(rlonc);
t(2,2) =  cos(rlonc);
t(2,3) =  0.0;
t(3,1) =  cos(rlatc) * cos(rlonc);
t(3,2) =  cos(rlatc) * sin(rlonc);
t(3,3) =  sin(rlatc);

% Calculate xl, yl, dis (vector in local coordinate)
for i = 1:m
    % Convert input coordinates to radians
    lat_rad = slat(i) * pi_val / 180.0;
    lon_rad = slon(i) * pi_val / 180.0;
    
    % Calculate unit vector in 3D Cartesian coordinates
    v = zeros(3,1);
    v(1) = cos(lat_rad) * cos(lon_rad);
    v(2) = cos(lat_rad) * sin(lon_rad);
    v(3) = sin(lat_rad);
    
    % Transform to local coordinates
    vp = zeros(2,1);
    for j = 1:2
        vp(j) = 0.0;
        for k = 1:3
            vp(j) = vp(j) + t(j,k) * v(k);
        end
    end
    
    % Assign results (note: slat becomes y, slon becomes x)
    slon(i) =  r * vp(2);  % x coordinate
    slat(i) = -r * vp(1);  % y coordinate
end

end