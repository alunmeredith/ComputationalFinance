function [ perf_tr_ret, perf_tr_var, sharpe_tr, VaR_tr, ...
    perf_ret, perf_var, sharpe_ts, VaR_ts, ...
    perf_null_ret, perf_null_var, sharpe_null, VaR_null,...
    perf_tr_null_ret, perf_tr_null_var, sharpe_tr_null, VaR_tr_null] ...
    = Q2_perf( numStocks, train, test, stockNames )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
YearlyInterest = 0.005;
Interest = (1+YearlyInterest)^(1/365) - 1;
%% Select 3 stocks at random
stocks3_ts = [];
stocks3_tr = [];
for i = 1:numStocks
    x = randi(size(train,2) - 1);
    stocks3_tr(:,i) = train(:,x);
    stocks3_ts(:,i) = test(:,x);
    stocks3_names(i) = stockNames(x);
end
%% Calculate variance and expected return for each stock
ret3 = mean(stocks3_tr);
cov3 = cov(stocks3_tr);
ret_ts = mean(stocks3_ts);
cov_ts = cov(stocks3_ts);

%% Find the Maximimum sharpe ratio on the frontier

p = Portfolio('assetmean', ret3, 'assetcovar', cov3, ...
'lowerbudget', 1, 'upperbudget', 1, 'lowerbound', 0, ...
'RiskFreeRate', Interest);
Wts = estimateMaxSharpeRatio(p);

% Calculate return vector for optimal portfolio on training data
% And compute performance metrics
returns_tr = Wts' * stocks3_tr';
perf_tr_ret = mean(returns_tr);
perf_tr_var = var(returns_tr);
sharpe_tr = sharpe(returns_tr, Interest);
VaR_tr = portvrisk(mean(returns_tr), var(returns_tr));

returns_tr_null = (ones(1, numStocks) ./ numStocks) * stocks3_tr';
perf_tr_null_ret = mean(returns_tr_null);
perf_tr_null_var = var(returns_tr_null);
sharpe_tr_null = sharpe(returns_tr_null, Interest);
VaR_tr_null = portvrisk(mean(returns_tr_null), var(returns_tr_null)); 

% Calculate return vector for optimal portfolio on test data
% And compute performance metrics
returns_ts =  Wts' * stocks3_ts';
perf_ret = mean(returns_ts);
perf_var = var(returns_ts);
sharpe_ts = sharpe(returns_ts, Interest);
VaR_ts = portvrisk(mean(returns_ts), var(returns_ts));

% Calculate return vector for 1/N portfolio on test data
% And compute performance metrics
returns_null = (ones(1, numStocks) ./ numStocks) * stocks3_ts';
perf_null_ret = mean(returns_null);
perf_null_var = var(returns_null);
sharpe_null = sharpe(returns_null, Interest);
VaR_null = portvrisk(mean(returns_null), var(returns_null));
end

