% test = csvread('plot1.csv', 2);
% scatter3(test(:,1), test(:,2), test(:,3), '.b');
% mesh(test);
% 
% test_model = fitgmdist(test(:,1:2), 4);
% 
% scatter(test(:,1), test(:,2), '.b')
% hold on
% scatter(test_model.mu(:,1), test_model.mu(:,2), 'or')

inputs = csvread('inputs.csv', 1, 1);
targets = csvread('targets.csv', 1, 1);
Design = csvread('Design.csv', 1, 1);

N = size(inputs,1);

options = statset('MaxIter', 1000);
model =  fitgmdist(inputs, 4, 'Options', options, 'SharedCovariance', true);
means = model.mu;
cov = model.Sigma;

Design = zeros(N, 7);
for i = 1:4
    Design(:,i) = Mahalanobis(inputs, means(i,:), cov);
end
Design(:,5:6) = inputs;
Design(:,7) = ones(N,1);

%w = inv(Design'*Design)*Design'*targets;
w = (Design'*Design) \ Design'*targets;

% predicted points
z = zeros(N,1);
for i = 1:N
   z(i) = prediction(inputs(i,:), means, cov, w); 
end
scatter3(inputs(:,1), inputs(:,2), z);

% surface 
npoints = 100;

x_min = min(inputs(:,1));
x_max = max(inputs(:,2));
x_grid = linspace(x_min, x_max, npoints);

y_min = min(inputs(:,1));
y_max = max(inputs(:,2));
y_grid = linspace(y_min, y_max, npoints);

z_grid = zeros(npoints, npoints);
for i = 1:npoints
    for j = 1:npoints
        z_grid(i,j) = prediction([x_grid(i) y_grid(j)], means, cov, w);
    end
end

hold on
surf(x_grid, y_grid, z_grid);