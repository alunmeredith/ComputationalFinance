# Deltas <- function(inputs, means, cov, strike) {
#     N <- nrow(inputs)
#     delta <- vector(length = N)
#     for (i in 1:N) {
#         points <- inputs[i,]
#         S <- strike[i]
#         
#         X <- vector(length = 7L)
#         X[1] <- 0
#         X[6:7] <- points
#         for (j in 1:4) {
#             m <- means[,j]
#             x_m <- points - m
#             dx_dS = points * c(1/S, 0)
#             
#             #a = 2 * t(x_m) %*% cov %*% dx_dS
#             db_dS = 2* cov[1,1] * x_m[1] * points[1]/S +
#                 ( cov[2,1] + cov[1,2] ) * x_m[2] * points[1]/S
#             
#             b = t(x_m) %*% cov %*% x_m
#             X[j + 1] = 1/2 * db_dS / sqrt( b ) 
#         }
#         delta[i] = X %*% weights
#     }
#     return(delta)
# }
# 
# # Deltas <- function(points, means, cov, S) {
# #     X <- matrix(nrow = nrow(points), ncol = 7)
# #     X[,1] <- 0
# #     X[,6:7] <- points
# #     for (j in 1:4) {
# #         m <- means[,j]
# #         x_m <- t(t(points) - m)
# #         
# #         dx_dS = points * matrix(c(1/S, rep(0,5)), ncol = 2)
# #         a = 2 * diag( (x_m) %*% cov %*% t(dx_dS) )
# #         b = diag( (x_m) %*% cov %*% t(x_m) )
# #         X[,j+1] = 1/2 *  a / sqrt(b)
# #     }
# #     delta = X %*% weights
# #     return(delta)
# # }
# 
# 
# Deltas <- function(points, means, cov, S) {
#     N <- nrow(points)
#     cum = rep(0, N)
#     for (j in 1:N) {
#         point <- points[j,]
#         
#         for (i in 1:4) {
#             diff <- point - means[,i]
#             cum[j] = cum[j] + 1/2 * weights[1] * (
#                 cov[1,1] * diff[1] ^ 2 +
#                     (cov[1,2] + cov[2,1]) * diff[1] * diff[2] +
#                     cov[2,2] * diff[2] ^ 2
#             ) ^ (-1/2) *
#                 2*cov[1,1]*diff[1] + (cov[1,2] - cov[2,1])*diff[2]
#         }
#         cum[j] = cum[j] + weights[6]
#     }
#     return(cum)
# }
# 
# 
# Deltas_fin <- function(points, means, cov, strike, weights) {
#     N = nrow(points)
#     delta <- rep(0,N)
#     for (i in 1:N) {
#         point <- points[i,]
#         X <- strike[i]
#         
#         for (j in 1:4) {
#             diff <- point - means[,j]
#             delta[i] <- delta[i] +
#                 weights[j+1] * ( (diff %*% cov) %*% matrix(c(1/X, 0)) %*%  
#                                      ( t(diff) %*% cov %*% diff ) ^ (-1/2) )
#         }
#         delta[i] <- delta[i] + weights[6] * 1/X
#     }
#     return(delta)
# }

# Deltas <- function(points, means, cov, strike, weights) {
#     N = nrow(points)
#     delta <- rep(0,N)
#     for (i in 1:N) {
#         point <- points[i,]
#         X <- strike[i]
#         dx_dS <- c(1, 0)
#         for (j in 1:4) {
#             xm <- point - means[,j]
#             delta[i] <- delta[i] + 
#                 weights[j + 1] * (t(xm) %*% cov %*% dx_dS) * (t(xm) %*% cov %*% xm) ^ (-1/2)
#         }
#         delta[i] <- delta[i] + weights[6:7] %*% dx_dS  
#     }
#     return(delta)
# }

 # Deltas <- function(points, means, cov, strike, weights) {
 #     N = nrow(points)
 #     delta <- rep(0,N)
 #     for (i in 1:N) {
 #         X <- strike[i]
 #         point <- points[i,]
 #         for (j in 1:4) {
 #             xm <- point - means[,j]
 #             delta[i] = delta[i] + 
 #                 weights[j + 1] * (cov[1,1]*xm[1] + cov[2,1]*xm[2])/X / 
 #                 sqrt(mahalanobis(point, means[,j], cov))
 #         }
 #         delta[i] = delta[i] + weights[7]/X
 #     }
 #     return(delta)
 # }

a <- matrix(rnorm(25), 5 ,5)
x <- xtable(x)
print(x, floating=FALSE, tabular.environment="bmatrix", hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE)

a <- matrix(rep("...", 21), 3, 7)
a[1,] <- c("M_1", "M_2", "M_3", "M_4", "S/X", "T-t", "1")
x <- xtable::xtable(a)
xtable::print.xtable(x, floating=FALSE, tabular.environment="bmatrix", hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE)

a <- matrix(c("lambda_1", "lambda_2", "l3", "l4", "w1", "w2", "w0"), 1)
