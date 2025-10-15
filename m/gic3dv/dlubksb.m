function b = dlubksb(a, n, np, indx, b)
% DLUBKSB Solve system of equations using LU decomposition
%
% Input:
%   a    - LU decomposed matrix from dludcmp (np x np)
%   n    - actual size of matrix to use (n <= np)
%   np   - physical dimension of array a
%   indx - pivot index vector from dludcmp (np x 1)
%   b    - right-hand side vector (np x 1), will be overwritten with solution
%
% Output:
%   b    - solution vector

ii = 0;

% Forward substitution (solve L*y = P*b)
for i = 1:n
    ll = indx(i);
    sum_val = b(ll);
    b(ll) = b(i);
    
    if ii ~= 0
        % Summation for non-zero elements
        for j = ii:i-1
            sum_val = sum_val - a(i,j) * b(j);
        end
    elseif sum_val ~= 0.0
        % First non-zero element encountered
        ii = i;
    end
    b(i) = sum_val;
end

% Backward substitution (solve U*x = y)
for i = n:-1:1
    sum_val = b(i);
    
    if i < n
        for j = i+1:n
            sum_val = sum_val - a(i,j) * b(j);
        end
    end
    
    b(i) = sum_val / a(i,i);
end

end