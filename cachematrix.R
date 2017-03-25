## -------------------------------------------------------------------------- ##
## Author: Jesús Sánchez de Castro 
## This R code is developed for the second assignment of the R programming 
## course of Coursera (https://github.com/rdpeng/ProgrammingAssignment2)
## -------------------------------------------------------------------------- ##

## makeCacheMatrix is a function that creates a special vector that contains the
## a function to set the matrix, a function to get the matrix, a function to 
## set the inverse (solution) and a function to get the inverse.
## Param x: a matrix
makeCacheMatrix <- function(x = matrix()) {

    # variable m: cache where the inverse is saved
    #initialization of the cache
    m <- NULL
    
    #definition of the functions that will be included in the list:
    
    #<<- is for the parent scope/evironment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    #list with the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function that calculates the inverse of a give matrix.
## If the inverse is already saved in cache (m), it will be fetched and not 
## calculated.
## Param x: special vector/list created by makeCacheMatrix (set,get,setinverse,getinverse)
cacheSolve <- function(x, ...) {
    
    #Save in cache (m) the inverse
    m <- x$getinverse()
    
    if(!is.null(m)) {
        #If it is already calculated just return m
        message("getting cached data")
        return(m)
    }
    #If m does not contain the inverse (is null) get the data from the matrix 
    #and calculate the inverse. Return m 
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
