################################################################
# MK-002-EN Applied Statistics / MK-002 Angewandte Statistik / #
# MK-002-EN-DI Applied Statistics                              #
# *** Matrix algebra ***                                       #
################################################################

# 1 Special matrices
####################

A.m <- matrix(c(3, 0, 0, 1, 0, 0),
              byrow=T,
              ncol=2)
A.m

# Transpose a matrix:
t(A.m)

# Diagonal matrix:
diag(c(2,3,5,11))

# Identity matrix:
diag(1, 4)

# Diagonal elements:
diag(A.m)

# Trace of a quadratic matrix:
sum(diag(A.m))


# 2 Basic matrix operations
###########################

A.m <- matrix(c(3, 0, 1, 0, 1, 2, 1, 2, 4),
              byrow=T,
              ncol=3)

B.m <- matrix(c(1, 0, 0, 0, 2, 0, 0, 0, 4),
              byrow=T,
              ncol=3)
              
C.m <- matrix(c(1, 2, 3, 0, 1, 2, 0, 0, 1),
              byrow=T,
              ncol=3)
              
D.m <- matrix(1:6, byrow=F, ncol=2)

E.m <- matrix(rep(1:4, times=2), byrow=T, ncol=4)


A.m
B.m
C.m
D.m
E.m

# Addition:
A.m + B.m

# Multiplication with a scalar:
3 * C.m

# Multiplication with a matrix:
D.m %*% E.m

# Multiplication with a vector:
C.m %*% c(1,2,3)
C.m %*% c(1,2) # does not work



# 3 Inverse of a matrix
#######################

A.m <- matrix(1:4, byrow=T, ncol=2)
B.m <- matrix(c(2,3,0,0), byrow=T, ncol=2)

A.m
B.m

solve(A.m)

# Multiplication of a matrix with its inverse results in the identity matrix:
solve(A.m) %*% A.m 
A.m %*% solve(A.m)

# Calculating the determinant with R and by hand:
det(A.m)
A.m[1,1]*A.m[2,2]-A.m[1,2]*A.m[2,1]

# Calculating the inverse by hand:
1/det(A.m) * matrix(c(A.m[2,2], -A.m[1,2], -A.m[2,1], A.m[1,1]), byrow=T, ncol=2)

# Matrix B is not invertible:
det(B.m)
solve(B.m)


# 4 Rank of a matrix
####################

A.m <- matrix(c(2, 1, 7, 0, 1 , 3, -3, 2, 0), byrow=T, ncol=3)
B.m <- matrix(c(1, 1, 2, 0, 1, 0, -1, -1, 2), byrow=T, ncol=3)

qr(A.m)$rank
qr(B.m)$rank


# 5 Moore-Penrose Inverse
#########################

A.m <- matrix(c(2, 0, 1, 0), byrow=T, ncol=2)

library(MASS)
ginv(A.m)


# 6 Eigenvalues und eigenvectors
################################

A.m <- matrix(c(3, 0, -9, 6), byrow=T, ncol=2)
A.m

eigen(A.m)$values # determine the eigenvalues

# Is e an eigenvector for the eigenvalue 6 of the matrix A?
e.v <- c(0, 1)
A.m %*% e.v
6 %*% e.v

# The product of the eigenvalues of a matrix equals its determinant.
det(A.m)

# The sum of the eigenvalues of a matrix equals its trace.
sum(diag(A.m))

# The eigenvectors to two different eigenvalues are orthogonal for symmetrical matrices.
A.m <- matrix(c(3, 0, 0, 6), byrow=T, ncol=2)
eigen(A.m)$values
e.v1 <- c(0, 1)
A.m %*% e.v1
6 %*% e.v1
e.v2 <- c(-1, 0)
A.m %*% e.v2
3 %*% e.v2


# 7 System of linear equations
##############################

y.yield <- c(22, 35)
X.d <- matrix(c(1, 1, 2.5, 6.5), byrow=F, ncol=2)  # design matrix

# The system of linear equations can be solved: 
X.d %*% ginv(X.d) %*% y.yield

# Are the inverse and the Moore-Penrose inverse the same?
solve(X.d)
ginv(X.d)

# Determine the vector with the coefficients b:
b.v <- ginv(X.d) %*% y.yield
b.v

# Values for y:
X.d %*% b.v


# 8 Overdetermined systems of linear equations
##############################################

y.yield <- c(22, 17.5, 27, 23, 25, 22.5, 33, 26, 35)
X.d <- matrix(c(rep(1, times=9), seq(2.5, 6.5, 0.5)), byrow=F, ncol=2) # design matrix

# The equation y D Xb cannot be solved:
X.d %*% ginv(X.d) %*% y.yield

# Approximation of the solution with method of least squares:
b.v <- ginv(t(X.d) %*% X.d) %*% t(X.d) %*% y.yield
b.v

# Values for y:
X.d %*% b.v


# 9 Quadratic forms
###################

H.m <- diag(1, nrow(X.d)) - X.d %*% ginv(t(X.d) %*% X.d) %*% t(X.d)
q.h.y <- y.yield %*% H.m %*% y.yield
q.h.y


# 10 Cleaning up
################

rm(list = ls())
