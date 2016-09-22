function [r, v] = Plot_frontier( w, m, C )
%Plots the frontier from the portfolio weights on that frontier
%   Returns the return and variance of the points on the frontier
%% Calculate E-V

% return value
r = w * m; 

% variance
% REFACTOR: vectorise this
v = zeros(100,1);
for i = 1:length(w)
    v(i) = w(i,:) * C * w(i,:)';
end

plot(sqrt(v),r, 'bx')
xlabel('Variance')
ylabel('return value')

end

