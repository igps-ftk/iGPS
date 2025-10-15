function areas = voronoi_area(pos_xy)
% CALCULATE_VORONOI_CELL_AREAS Compute areas of Voronoi cells for given pos_xy
%
% Input:
%   pos_xy - Nx2 matrix of generator point coordinates [x, y]
%
% Output:
%   areas - Nx1 vector of Voronoi cell areas (NaN for unbounded cells)

    % Create Voronoi diagram
    [v, c] = voronoin(pos_xy);
    
    % Initialize area array
    areas = zeros(size(pos_xy, 1), 1);
    
    % Calculate area for each Voronoi cell
    for i = 1:length(c)
        % Get vertices of current Voronoi cell
        cell_vertices = v(c{i}, :);
        
        % Check if cell is bounded (finite and non-empty)
        if all(isfinite(cell_vertices(:))) && ~isempty(cell_vertices)
            % Calculate polygon area using shoelace formula
            areas(i) = polyarea(cell_vertices(:, 1), cell_vertices(:, 2));
        else
            % Mark unbounded cells as NaN
            areas(i) = NaN;
        end
    end
end


