function [a, indx, d] = dludcmp(a, n, np, indx, d)
% DLUDCMP LU decomposition with partial pivoting
% 
% Input:
%   a   - input matrix (np x np), will be overwritten with LU decomposition
%   n   - actual size of matrix to decompose (n <= np)
%   np  - physical dimension of array a
%   indx - pivot index vector (output)
%   d   - determinant sign (output)
%
% Output:
%   a   - matrix containing LU decomposition
%   indx - pivot index vector
%   d   - determinant sign (+1 or -1)

% Initialize parameters
nmax = 7000;
tiny = 1.0e-20;

% Initialize variables
d = 1.0;
vv = zeros(nmax, 1);

% Find the scaling for each row
for i = 1:n
    aamax = 0.0;
    for j = 1:n
        if abs(a(i,j)) > aamax
            aamax = abs(a(i,j));
        end
    end
    
    if aamax == 0.0
        error('dludcmp:singularMatrix', ...
              'Singular matrix at row %d\nn: %d, np: %d', i, n, np);
    end
    vv(i) = 1.0 / aamax;
end

% LU decomposition with partial pivoting
for j = 1:n
    % Upper triangle
    if j > 1
        for i = 1:j-1
            sum_val = a(i,j);
            if i > 1
                for k = 1:i-1
                    sum_val = sum_val - a(i,k) * a(k,j);
                end
                a(i,j) = sum_val;
            end
        end
    end
    
    % Lower triangle and find pivot
    aamax = 0.0;
    for i = j:n
        sum_val = a(i,j);
        if j > 1
            for k = 1:j-1
                sum_val = sum_val - a(i,k) * a(k,j);
            end
            a(i,j) = sum_val;
        end
        
        dum = vv(i) * abs(sum_val);
        if dum >= aamax
            imax = i;
            aamax = dum;
        end
    end
    
    % Pivot if necessary
    if j ~= imax
        for k = 1:n
            dum = a(imax,k);
            a(imax,k) = a(j,k);
            a(j,k) = dum;
        end
        d = -d;
        vv(imax) = vv(j);
    end
    
    indx(j) = imax;
    
    % Divide by pivot element
    if j ~= n
        if a(j,j) == 0.0
            a(j,j) = tiny;
        end
        dum = 1.0 / a(j,j);
        for i = j+1:n
            a(i,j) = a(i,j) * dum;
        end
    end
end

% Handle zero pivot for last element
if a(n,n) == 0.0
    a(n,n) = tiny;
end

end