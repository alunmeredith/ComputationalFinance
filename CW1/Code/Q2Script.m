%% import data
% The 100 stocks of the FTSE 100 are merged together using R and saved to a
% csv. The values are then imported into matlab as a matrix, with y
% labels(names) and x labels(dates) imported as seperate vectors to
% preserve the matrix type. 
import_script
import_dates
import_names
stockNames = x;
Date = flipud(Date);
joinedstocks = flipud(joinedstocks);
joinedstocks = joinedstocks(:,1:size(joinedstocks,2)-1);
FTSE = joinedstocks(:,size(joinedstocks,2));
%% Calculate return matrix
stocks_shift = circshift(joinedstocks, 1);
stocks_return = (joinedstocks - stocks_shift) ./ stocks_shift;
stocks_return = stocks_return(2:length(stocks_return),:);
% first date in date vector not in stocks_return because caulcated based on
% pairwise differences. 

%% Split into training and testing set
N = length(stocks_return);
stocks_ts = stocks_return(2:ceil(N/2),:);
stocks_tr = stocks_return(ceil(N/2)+1 : N,:);
Date_ts = Date(2:ceil(N/2),:);
Date_tr = Date(ceil(N/2)+1 : N,:);

%%
k = 3:30;
for t = 1:length(k)
    N = 100;
    output = zeros(N,16);
    for i = 1:N
        size(stocks_tr,2)
        [ perf_tr_ret, perf_tr_var, sharpe_tr, VaR_tr, ...
            perf_ret, perf_var, sharpe_ts, VaR_ts, ...
            perf_null_ret, perf_null_var, sharpe_null, VaR_null, ...
            perf_tr_null_ret, perf_tr_null_var, sharpe_tr_null, VaR_tr_null] ...
            = Q2_perf(k(t), stocks_tr, stocks_ts, stockNames);
        output(i,:) = [ perf_tr_ret, perf_tr_var, sharpe_tr, VaR_tr, ...
            perf_ret, perf_var, sharpe_ts, VaR_ts, ...
            perf_null_ret, perf_null_var, sharpe_null, VaR_null, ...
            perf_tr_null_ret, perf_tr_null_var, sharpe_tr_null, VaR_tr_null];
    end
        output_av(t,:) = mean(output);
        output_var(t,:) = var(output);    
end
%% plots
 figure(1)  
 hold on
 title('average return')
 errorbar(k, output_av(:,1), sqrt(output_var(:,1)./N), 'x', 'color', [0.3, 0.5, 0.7])
 errorbar(k, output_av(:,5), sqrt(output_var(:,5)./N), 'x', 'color', [0.2, 0.3, 1])
 errorbar(k, output_av(:,9), sqrt(output_var(:,9)./N), 'x', 'color', [1, 0.3, 0.2])
 errorbar(k, output_av(:,13), sqrt(output_var(:,13)./N), 'x', 'color', [0.7, 0.3, 0.2])

 
 figure(2)  
 hold on
 title('average variance')
 errorbar(k, output_av(:,2), sqrt(output_var(:,2)./N), 'x', 'color', [0.3, 0.5, 0.7])
 errorbar(k, output_av(:,6), sqrt(output_var(:,6)./N), 'x', 'color', [0.2, 0.3, 1])
 errorbar(k, output_av(:,10), sqrt(output_var(:,10)./N), 'x', 'color', [1, 0.3, 0.2])
 errorbar(k, output_av(:,14), sqrt(output_var(:,14)./N), 'x', 'color', [0.7, 0.3, 0.2])

 figure(3)  
 hold on
 title('average Sharpe Ratio')
 errorbar(k, output_av(:,3), sqrt(output_var(:,3)./N), 'x', 'color', [0.3, 0.5, 0.7])
 errorbar(k, output_av(:,7), sqrt(output_var(:,7)./N), '*', 'color', [0.2, 0.3, 1])
 errorbar(k, output_av(:,11), sqrt(output_var(:,11)./N), 'o', 'color', [1, 0.3, 0.2], 'MarkerSize', 2)
 errorbar(k, output_av(:,15), sqrt(output_var(:,15)./N), '.', 'color', [0.7, 0.3, 0.2])

 figure(4)  
 hold on
 title('average Value on Return')
 errorbar(k, output_av(:,4), sqrt(output_var(:,4)./N), 'x', 'color', [0.3, 0.5, 0.7])
 errorbar(k, output_av(:,8), sqrt(output_var(:,8)./N), 'x', 'color', [0.2, 0.3, 1])
 errorbar(k, output_av(:,12), sqrt(output_var(:,12)./N), 'x', 'color', [1, 0.3, 0.2])
  errorbar(k, output_av(:,16), sqrt(output_var(:,16)./N), 'x', 'color', [0.7, 0.3, 0.2])
