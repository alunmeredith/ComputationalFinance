function [ predicted_C ] = prediction( point, means, cov, weights )
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here
D = zeros(1,7);
for i = 1:4
D(i) = Mahalanobis(point, means(1,:), cov);
end
D(5) = point(1);
D(6) = point(2);
D(7) = 1;

predicted_C = D * weights;

end

