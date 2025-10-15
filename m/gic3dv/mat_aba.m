function aba = mat_aba(a, b, n)
% MAT_ABA Compute quadratic form a' * B * a
%
% Input:
%   a - vector of length n
%   b - matrix of size n x n
%   n - dimension of vector and matrix
%
% Output:
%   aba - scalar result of a' * B * a
%
% Description:
%   Computes the quadratic form: result = sum_i sum_j a(i) * b(i,j) * a(j)

aba = 0.0;

for i = 1:n
    for j = 1:n
        aba = aba + a(i) * b(i,j) * a(j);
    end
end

end