% Define the return and variance of our 3 items in our portfolio. 
m = [0.1 0.2 0.15]';
C = [0.005 -0.010 0.004; -0.010 0.040 -0.002; 0.004 -0.002 0.023];

%% Generate 100 random porfolios (random weight combinations)

SamplePortfolios(10000, m, C, 'w', 10);
hold on
SamplePortfolios(10000, m(1:2), C(1:2,1:2), 'k', 20);
SamplePortfolios(10000, m(2:3), C(2:3,2:3), 'k', 20);
SamplePortfolios(10000, m([1,3]), C([1,3],[1,3]), 'k', 20);

c_m = [1 0 0; 0 1 0; 0 0 1];
figure(1);
scatter(sqrt(diag(C)), m, 30, c_m, 'filled');
%% Financial toolbox finding frontier
hold on  
n = 50;
% Three asset model
for i = 1:10
    tic;
    [PRisk123((i-1)*n + 1:i*n,:), PRoR123((i-1)*n + 1:i*n,:), ...
        f123(:,(i-1)*n + 1:i*n)] = frontier(m,C,n);
    time_port(1,i) = toc;
    
    tic;
    [PRisk12((i-1)*n + 1:i*n,:), PRoR12((i-1)*n + 1:i*n,:), ...
    f12(:,(i-1)*n + 1:i*n)] = frontier(m(1:2), C(1:2, 1:2), n);
    time_port(2,i) = toc;
    
    tic;
    [PRisk23((i-1)*n + 1:i*n,:), PRoR23((i-1)*n + 1:i*n,:),...
    f23(:,(i-1)*n + 1:i*n)] = frontier(m(2:3), C(2:3, 2:3), n);
    time_port(3,i) = toc;
    
    tic;
    [PRisk13((i-1)*n + 1:i*n,:), PRoR13((i-1)*n + 1:i*n,:),...
    f13(:,(i-1)*n + 1:i*n)] = frontier(m([1 3]), C([1 3], [1 3]), n);
    time_port(4,i) = toc;
    %% Redraw using NaiveMW
    
    tic;
    [ PRisk_MW123((i-1)*n + 1:i*n,:), PRoR_MW123((i-1)*n + 1:i*n,:),...
        PWts_MW123((i-1)*n + 1:i*n,:) ] = NaiveMW( m, C, n );
    time_MW(1,i) = toc;
    
    tic;
    [ PRisk_MW12((i-1)*n + 1:i*n,:), PRoR_MW12((i-1)*n + 1:i*n,:), ...
        PWts_MW12((i-1)*n + 1:i*n,:) ] = NaiveMW( m(1:2), C(1:2, 1:2), n );
    time_MW(2,i) = toc;
    
    tic;
    [ PRisk_MW23((i-1)*n + 1:i*n,:), PRoR_MW23((i-1)*n + 1:i*n,:), ...
        PWts_MW23((i-1)*n + 1:i*n,:) ] = NaiveMW( m(2:3), C(2:3, 2:3), n );
    time_MW(3,i) = toc;
    
    tic;
    [ PRisk_MW13((i-1)*n + 1:i*n,:), PRoR_MW13((i-1)*n + 1:i*n,:), ...
        PWts_MW13((i-1)*n + 1:i*n,:) ] = NaiveMW( m([1 3]), C([1 3], [1 3]), n);
    time_MW(4,i) = toc;
    
    %% NaiveMW_CVX
    tic;
    [ PRisk_CVX123((i-1)*n + 1:i*n,:), PRoR_CVX123((i-1)*n + 1:i*n,:), ...
        PWts_CVX123((i-1)*n + 1:i*n,:) ] = NaiveMW_CVX( m, C, n );
    time_CVX(1,i) = toc;
    
    tic;
    [ PRisk_CVX12((i-1)*n + 1:i*n,:), PRoR_CVX12((i-1)*n + 1:i*n,:), ...
        PWts_CVX12((i-1)*n + 1:i*n,:) ] = NaiveMW_CVX( m(1:2), C(1:2, 1:2), n );
    time_CVX(2,i) = toc;
    
    tic;
    [ PRisk_CVX23((i-1)*n + 1:i*n,:), PRoR_CVX23((i-1)*n + 1:i*n,:), ...
        PWts_CVX23((i-1)*n + 1:i*n,:) ] = NaiveMW_CVX( m(2:3), C(2:3, 2:3), n );
    time_CVX(3,i) = toc;
    
    tic;
    [ PRisk_CVX13((i-1)*n + 1:i*n,:), PRoR_CVX13((i-1)*n + 1:i*n,:), ...
        PWts_CVX13((i-1)*n + 1:i*n,:) ] = NaiveMW_CVX( m([1 3]), C([1 3], [1 3]), n);
    time_CVX(4,i) = toc;
end

%% Calculate mean and var for each to plot
times = zeros(3,4);
for i = 1:3
    if i == 1 vector = time_port';
    elseif i == 2 vector = time_MW';
    else vector = time_CVX';
    end
    times(i, 1) = mean(vector(1:10));
    times(i, 2) = var(vector(1:10));
    times(i, 3) = mean(vector(11:40));
    times(i, 4) = var(vector(11:30));
end

%% Collect mean/variance differences 
b_m(1,1) = mean(PRisk_CVX12 - PRisk12);
b_m(1,2) = mean(PRoR_CVX12 - PRoR12);
b_m(2,1) = mean(PRisk_CVX13 - PRisk13);
b_m(2,2) = mean(PRoR_CVX13 - PRoR13);
b_m(3,1) = mean(PRisk_CVX23 - PRisk23);
b_m(3,2) = mean(PRoR_CVX23 - PRoR23);
b_m(4,1) = mean(PRisk_CVX123 - PRisk123);
b_m(4,2) = mean(PRoR_CVX123 - PRoR123);

b_v(2,1) = var(PRisk_CVX12 - PRisk12);
b_v(2,2) = var(PRoR_CVX12 - PRoR12);
b_v(3,1) = var(PRisk_CVX13 - PRisk13);
b_v(3,2) = var(PRoR_CVX13 - PRoR13);
b_v(4,1) = var(PRisk_CVX23 - PRisk23);
b_v(4,2) = var(PRoR_CVX23 - PRoR23);
b_v(5,1) = var(PRisk_CVX123 - PRisk123);
b_v(5,2) = var(PRoR_CVX123 - PRoR123);

c_m(1,1) = mean(PRisk_MW12 - PRisk12);
c_m(1,2) = mean(PRoR_MW12 - PRoR12);
c_m(2,1) = mean(PRisk_MW13 - PRisk13);
c_m(2,2) = mean(PRoR_MW13 - PRoR13);
c_m(3,1) = mean(PRisk_MW23 - PRisk23);
c_m(3,2) = mean(PRoR_MW23 - PRoR23);
c_m(4,1) = mean(PRisk_MW123 - PRisk123);
c_m(4,2) = mean(PRoR_MW123 - PRoR123);

c_v(1,1) = var(PRisk_MW12 - PRisk12);
c_v(1,2) = var(PRoR_MW12 - PRoR12);
c_v(2,1) = var(PRisk_MW13 - PRisk13);
c_v(2,2) = var(PRoR_MW13 - PRoR13);
c_v(3,1) = var(PRisk_MW23 - PRisk23);
c_v(3,2) = var(PRoR_MW23 - PRoR23);
c_v(4,1) = var(PRisk_MW123 - PRisk123);
c_v(4,2) = var(PRoR_MW123 - PRoR123);

d_m(1,1) = mean(PRisk_CVX12 - PRisk_MW12);
d_m(1,2) = mean(PRoR_CVX12 - PRoR_MW12);
d_m(2,1) = mean(PRisk_CVX13 - PRisk_MW13);
d_m(2,2) = mean(PRoR_CVX13 - PRoR_MW13);
d_m(3,1) = mean(PRisk_CVX23 - PRisk_MW23);
d_m(3,2) = mean(PRoR_CVX23 - PRoR_MW23);
d_m(4,1) = mean(PRisk_CVX123 - PRisk_MW123);
d_m(4,2) = mean(PRoR_CVX123 - PRoR_MW123);

d_v(1,1) = var(PRisk_CVX12 - PRisk_MW12);
d_v(1,2) = var(PRoR_CVX12 - PRoR_MW12);
d_v(2,1) = var(PRisk_CVX13 - PRisk_MW13);
d_v(2,2) = var(PRoR_CVX13 - PRoR_MW13);
d_v(3,1) = var(PRisk_CVX23 - PRisk_MW23);
d_v(3,2) = var(PRoR_CVX23 - PRoR_MW23);
d_v(4,1) = var(PRisk_CVX123 - PRisk_MW123);
d_v(4,2) = var(PRoR_CVX123 - PRoR_MW123);

b_m = abs(b_m); c_m = abs(c_m); d_m = abs(d_m);
figure(2)
scatter(b_m(:,1),b_m(:,2), 100,'rx', 'linewidth', 2)
hold on
scatter(c_m(:,1),c_m(:,2),100,'bx', 'linewidth', 2)
scatter(d_m(:,1),d_m(:,2),100,'kx', 'linewidth', 2)
fplot(@(x)x, [0 8*10^-6]);

%% Add riskless asset option
m = [m; 0.1];
C = [C zeros(size(C,1),1)];
C = [C; zeros(1, size(C,2))];

figure(1)
p = Portfolio('assetmean', m, 'assetcovar', C, ...
'lowerbudget', 1, 'upperbudget', 1, 'lowerbound', 0);
[A, B, C] = NaiveMW_CVX(m,C,n);
plot(A,B, '--k', 'linewidth', 2)