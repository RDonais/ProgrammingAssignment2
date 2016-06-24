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
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- matrix(data, ...)
        x$setmatrix(m)
        m
}
