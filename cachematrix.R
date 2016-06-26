# CACHING THE INVERSE OF A MATRIX
# This function creates a special "matrix" object that can cache its inverse.
# Concepts are described in CacheMean.R

# 1) Fork Git repository: https://github.com/rdpeng/ProgrammingAssignment2
## saved in my Git Hub Account
## Cloned onto computer: ~/documents/R/Week3/ProgrammingAssignment2

getwd()
setwd(file.path("c:", "Users", "Rachel", "Documents", "R", "Week3", "ProgrammingAssignment2"))

# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.



makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cacheSolve computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

# Only square matrices can be invertable. 
## Not all square matrices are invertible
## If a matrix is invertible, it is called invertible or nonsingular.
## The original matrix returns when it's inverse is multiplied by the identity matrix.

# Make X an invertible matrix

x <- matrix(c(4,3,3,2), 2, 2) # This is an invertible matrix
x

# xi is inverse of x to test if cacheSolve function works (later on).
xi <- matrix(c(-2,3,3,-4), 2,2)
xi

# need to turn makeCacheMatrix into an object because the functions are inside a list and cacheSolve will be accessing children functions through subsetting the list.
m1 <- makeCacheMatrix(x)
m1$get()

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}


cacheSolve(m1)
cacheSolve(makeCacheMatrix(x))

# Test if matrix was inverted
# xi is inverse of x to test if cacheSolve function works (later on).
xi <- matrix(c(-2,3,3,-4), 2,2)
xi
