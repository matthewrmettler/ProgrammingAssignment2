## R Programming, Programming Assignment #2
## By Matthew Mettler
## Current status: 0% complete, first commit

## Put comments here that give an overall description of what your
## functions do

## This creates a special matrix object that lets you set the matrix,
## get the matrix, set the inverse of the matrix, or get the inverse of 
## the matrix. We do this with the <<- operator.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    # Return a function list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function lets us get the inverse of a matrix using the special matrix
## defined above. It does this by checking to see if it's already cached. If so
## it returns the cached version. Otherwise, it calculates the inverse, stores it
## in the cache of the special matrix, and then returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        inverse <- x$getinverse()
        
        # Check to see if it's cached
        if (!is.null(inverse)) {
            message("Getting cached inverse")
            return(inverse)
        }
        # Need to calculate, then store into the cache
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        
        # Return inverse
        inverse
}
