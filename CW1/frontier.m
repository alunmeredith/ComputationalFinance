function [ PRisk, PRoR, Pwts ] = frontier( m, C, n )
%Takes a vector of returns and Covariance matrix for a set of portfolios
%and computes the efficient frontier. 
p = Portfolio('assetmean', m, 'assetcovar', C, ...
'lowerbudget', 1, 'upperbudget', 1, 'lowerbound', 0);
Pwts = p.estimateFrontier(n); % weights on the frontier
[PRisk, PRoR] = p.plotFrontier(n); % plot the frontier
end

