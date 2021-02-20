x <- c(1, 3, 5)
y <- c(2, 4, 6)
z <- c(1, 2,4)

A <- cbind(x, y, z)
rownames(A) <- c("a", "b", "c")
A


is.matrix(A)

d <- c(x, y, z, c(5, 6, 7))

B <- matrix(d, nrow = 4)
B
tB <- t(B)
tB

# Matrix mult operator for nonc-conforming
A %*% tB

#Datframes can be multiplied?
A <- data.frame(A)
A * A

subB <- B[2:4,]

#Dataframe operations handle nonconforming!
3*A
A + subB
A - subB
A + B

# Solve (for system of equations outputting second matrix from first
#needs compatible matrices, and first as square

A1<-matrix(runif(16),4,4)
A2<-matrix(runif(8),4,2)
M<-solve(A1,A2)


#dimnames gives row, col names as list of vectors
M=matrix(c(1:10),nrow=5,ncol=2,
         dimnames=list(c('a','b','c','d','e'),c('A','B')))
M['e', 'A']

N=matrix(c(1:9),nrow=3,ncol=3,
         dimnames=list(c('a','b','c'),c('A','B','C')))

#Diag gives matrix diagonal, or makes diag matrix for dims
print(diag(N))
print(diag(4, 3, 3))

# Note 
M <- matrix(c(1:9),3,3,byrow=T,
            dimnames=list(c('a','b','c'),c('d','e','f')))
M

# tri funs return logicals of values in upper or lower triangles
upper.tri(M)
lower.tri(M)

#Columnwise fill by default
M=matrix(c(1:9),3,3,byrow=T)
N=matrix(c(1:9),3,3)
M
N
M*N

(M+N)^2
M/N

# Matrix mult produces summed products of row i in first 
# term by column j in second. Rows in firts must equal cols in second.
#Output has rows of first and cols of second
M %*% N


#Binding

a <- 1:5 ; b <- 1:5
cbind(a, b)
rbind(a, b)
#Coerces to characrer
a <- 1:5 ; b <- c('1', '2', '3', '4', '5')
cbind(a, b)
rbind(a,b)

#cbind only recycles with multiples, rbind with any number
a <- 1:5 ; b <- 1:4; c <- 1:3
#cbind(a,b)
rbind(a, b, c)

a <- matrix(1:12, ncol=4); b <- matrix(21:35, ncol=5)
cbind(a, b)
a <- matrix(1:12, ncol=4); b <- matrix(21:35, ncol=3)

a <- matrix(1:39, ncol=3); b <- matrix(LETTERS, ncol=2)
cbind(a,b)

#One efficient way of repeating matrix - indexing creates new vales
a <- matrix(1:1089, ncol=33)
a1 <- a[,rep(1:33, 21)]  

#Can't cbind dataframes if differing row numbers
a <- data.frame(v1=1:5, v2=LETTERS[1:5]) ; b <- data.frame(var1=6:10, var2=LETTERS[6:10])
cbind(a, b)
a <- data.frame(v1=1:6, v2=LETTERS[1:6]) ; b <- data.frame(var1=6:10, var2=LETTERS[6:10])
#cbind(a,b)


#rbind fills with NA
 a <- data.frame(v1=1:5, v2=LETTERS[1:5]) ; b <- data.frame(v1=6:10, v2=LETTERS[6:10])
 rbind(a, b)
a <- data.frame(v1=1:6, v2=LETTERS[1:6]) ; b <- data.frame(v2=6:10, v1=LETTERS[6:10])
rbind(a,b)

# To join incompatible matrices column-wise, fill with NAs to legnth of longest
a <- data.frame(v1=1:6, v2=LETTERS[1:6]); b <- data.frame(var1=6:10, var2=LETTERS[6:10])
b <- rbind(b, c(NA, NA))
m <- cbind(a, b)

