function [ distance ] = Mahalanobis( data, mu, cov )
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
N = size(data, 1);

M = repmat(mu, N, 1);
z = data - M;

distance = zeros(N,1);
for i = 1:N
    
    dist_squared = z(i,:) * cov * z(i,:)';
    distance(i) = sqrt(dist_squared);
end

end

