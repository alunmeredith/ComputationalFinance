% Define the return and variance of our 3 items in our portfolio. 
m = [0.1 0.2 0.15]';
C = [0.005 -0.010 0.004; -0.010 0.040 -0.002; 0.004 -0.002 0.023];

%% Generate 100 random porfolios (random weight combinations)

% 100 random points between -1 and 1.
n = 100; y = 3;
w = rand(n,y);
% matrix of sums of each row
norm = w * ones(y,1);
norm = repmat(norm, 1, y);
% divide each row by its sum so each row sums to 1
w = w ./ norm;

%%
test = diag((w * C * w') .* diag(ones(n,1),0));

v = zeros(100,1);
for i = 1:length(w)
    v(i) = w(i,:) * C * w(i,:)';
end