function [ w, r, v ] = SamplePortfolios( n, m, C, colour, size )
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
%% Generate 100 random porfolios (random weight combinations)

y = length(m);
w = rand(n,y);
% matrix of sums of each row
norm = w * ones(y,1);
norm = repmat(norm, 1, y);
% divide each row by its sum so each row sums to 1
w = w ./ norm;

%% Calculate E-V

% return value
r = w * m; 

% variance
v = diag((w * C * w') .* diag(ones(n,1),0));

% Make plot
if colour == 'w'
    colour = w;
end
scatter(sqrt(v), r, size, colour, 'filled')
xlabel('Standard Deviation')
ylabel('Expected Return Value')

end

