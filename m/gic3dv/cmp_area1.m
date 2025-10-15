function area_ith = cmp_area1(nstn, pos, np, cfa1, ith)
% CMP_AREA1 Compute alternative occupational area for sites at network edge
% 
% Input:
%   nstn  - number of stations
%   pos   - position matrix (2 x nstn), [x1, x2, ...; y1, y2, ...]
%   np    - number of nearest neighbors to consider
%   cfa1  - scaling factor (currently commented out in original)
%   ith   - index of target station
%
% Output:
%   area_ith - computed area for the target station

% Initialize variables
pi = 4.0 * atan(1.0);
dsmin = zeros(np, 1);
is = 1;
dsmax = 0.0;

% Find np nearest neighbor distances
while true
    dsmin(is) = 1000000.0;
    
    for i = 1:nstn
        if i ~= ith
            dx = pos(1, ith) - pos(1, i);
            dy = pos(2, ith) - pos(2, i);
            ds = sqrt(dx*dx + dy*dy);
            
            if ds < dsmin(is) && ds > dsmax
                dsmin(is) = ds;
            end
        end
    end
    
    dsmax = dsmin(is);
    
    if is == np
        break;
    end
    
    is = is + 1;
end

% Calculate area
area_ith = 0.0;
for i = 1:is
    area_ith = area_ith + dsmin(i);
end

% Average distance and convert to circular area
area_ith = area_ith / is / 2.0;

% Note: The original Fortran code has this line commented out:
% area_ith = cfa1 * area_ith / is / 2.0d0

area_ith = pi * area_ith^2;

end