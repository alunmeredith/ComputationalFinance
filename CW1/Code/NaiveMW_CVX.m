function [ PRisk, PRoR, PWts ] = NaiveMW_CVX( ERet, ECov, NPts )
%NaiveMW Summary of this function goes here
%   Detailed explanation goes here

ERet = ERet(:); % Ensures ERet is column vector
NAssets = length(ERet); % Get number of assets

% Create vectors of ones and zeros
V0 = zeros(NAssets, 1);
V1 = ones(1, NAssets); % Row vector

% Set medium scale option (for optimisation functions)
options = optimset('LargeScale', 'off');

% Find the maximum extpected return (independent of variance)
cvx_begin
    variable MaxReturnWeights(NAssets) nonnegative
    maximise(MaxReturnWeights' * ERet)
    subject to
        MaxReturnWeights' * ones(NAssets,1) == 1;
cvx_end
MaxReturn = MaxReturnWeights' * ERet;

% Find the minimum variance (independent of return)
cvx_begin
    variable MinVarWeights(NAssets) nonnegative
    minimise(MinVarWeights' * ECov * MinVarWeights) 
    subject to
        MinVarWeights' * ones(NAssets,1) == 1
cvx_end

MinVarReturn = MinVarWeights' * ERet;
MinVarStd = sqrt(MinVarWeights' * ECov * MinVarWeights);

% Check if there is only one portfolio
if MaxReturn > MinVarReturn 
    % if more than 1 then create a linear boundary from Max return to
    % lowest variance return. 
    RTarget = linspace(MinVarReturn, MaxReturn, NPts);
    NumFrontPoints = NPts;
else % if lowest risk == best return there is only 1 frontier point
    RTarget = MaxReturn;
    NumFrontPoints = 1;
end

% Initialise return portfolio
PRoR = zeros(NumFrontPoints, 1);
PRisk = zeros(NumFrontPoints, 1);
PWts = zeros(NumFrontPoints, NAssets);

%  Store first portfolio (min variance)
PRoR(1) = MinVarReturn;
PRisk(1) = MinVarStd;
PWts(1, :) = MinVarWeights(:)';

%% Trace porfolios on the boundary space calculated

% Constratins
VConstr = ERet'; 
% V0 = Maximum Return constraint
% Constraint A * Weights = B 
%   --> Sum of (Returns * Weights) = Return frontier 
%   --> Sum of Weights = 1                
A = [V1; VConstr];
B = [1; 0]; % B(1) Bias unit, B(2) set to Return target for frontier

for point = 2:NumFrontPoints % Solve weights for each return on frontier
    B(2) = RTarget(point);
    Weights = quadprog(ECov, V0, [], [], A, B, V0, [], [], options);
    PRoR(point) = dot(Weights, ERet)';
    PRisk(point) = sqrt(Weights' * ECov * Weights);
    PWts(point, :) = Weights(:)';
end
end

