## This code is part of the Coursera course R Programming - Assignment 2
## The goal of the assignment is to write write an R function that is able to cache potentially time-consuming computations.
## The time-consuming computation that we'll try to speed up here through caching is matrix inversion.


## This function creates a special "matrix" object that can cache its inverse
## returns a list of the following functions:
##    set: for setting a new matrix for which the inverse is to be calculated
##    get: for getting the matrix for which the inverse is to be calculated (not really necessary for the assignment but may be useful in the future)
##    setinverse: for setting the inverse matrix (after it has been calculated elsewhere)
##    getinverse: for getting the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(new_matrix){
        x <<- new_matrix
        inverse_matrix <<- NULL
    }
    get <- function() x  #returns the original matrix
    setinverse <- function(inverse){
      inverse_matrix <<- inverse
    } 
    
    getinverse <- function() inverse_matrix
    
    list(set = set,get=get, setinverse = setinverse, getinverse = getinverse)
}




## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)){
            message("getting cached inverse")
            return(inverse_matrix)
        }
        
        matrix <- x$get()
        new_inverse <- solve(matrix)
        x$setinverse(new_inverse)
        new_inverse
}
