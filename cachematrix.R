## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## This function returns a list of containing four functions to
##      1. Set the value of matrix
##      2. Get the value of matrix
##      3. Set the inverse of matrix
##      4. Get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initiate a shell inverse matrix
        inverse <- NULL
        
        ## Setting the new matrix & Reseting the new inverse
        set <- function(y){
                x <<- y
                inverse <- NULL
        }
        
        ## Getting the currrent matrix
        get <- function() x
        
        ## Setting the inverse after calculation
        ## Will be used to store inverse that is computed from cacheSolve function
        setInverse <- function(m) {
                inverse <<- m
        }
        
        ## Getting the Inverse matrix
        getInverse <- function() inverse
        
        ## Create a list for the four functions above
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Recall Inverse
        inverse2 <- x$getInverse()
        
        ## Check to see if inverse has already been calculated
        ## *If it has, then return inverse number
        if(!is.null(inverse2)){
                message("getting cached data")
                return(inverse2)
        }
        
        ## *If it hasn't, calculate inverse
        ## First, recall the current data
        data <- x$get()
        
        ## Second, compute inverse, store that into inverse2
        inverse2 <- solve(data, ...)
        
        ## Store that using setmean 
        x$setInverse(inverse2)       
        
        inverse2
}
