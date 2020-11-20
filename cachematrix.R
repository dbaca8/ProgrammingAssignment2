## Assignment: Week 3: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.

## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        cacheMatrix_inv <- NULL
        set <- function(y) {
                x <<- y
                cacheMatrix_inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) cacheMatrix_inv <<- solve
        getsolve <- function() cacheMatrix_inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        cacheMatrix_inv <- x$getsolve()
        if(!is.null(cacheMatrix_inv)) {
                message("getting cached data")
                return(cacheMatrix_inv)
        }
        data <- x$get()
        cacheMatrix_inv <- solve(data, ...)
        x$setsolve(cacheMatrix_inv)
        cacheMatrix_inv
}
