function array_out = sortit(array,n)
% SORTIT  Sort an array in ascending order using bubble sort
%   SORTED_ARRAY = SORTIT(ARRAY) returns the sorted array in ascending order
%
%   This is a direct conversion from Fortran code

np = length(array);
array_out=array;

for j = 1:n-1
  for k = j+1:n
    if array_out(j) > array_out(k)
      % Swap elements
      temp = array_out(k);
      array_out(k) = array_out(j);
      array_out(j) = temp;
    end
  end
end
end