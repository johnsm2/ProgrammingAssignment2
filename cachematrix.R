## Coursera R Programming
## Assignment 2

## This is based on the vector sample provided in the assignment
## instructions. Primary differences are object type and action 
## function (mean vs inverse).

## ----------------------------------------------------------------------
## makeCacheMatrix() sets up the function closure with a matrix object variable 
## that holds the inverse of a matrix and defines the functions for setting and 
## communicating the value, as well as setting and communicating the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # A matrix variable for holding the inverse of the passed parameter
    mInverse <- NULL
    
    # Private function for setting the value of 
    pri_Set <- function(y) {
        
        # Set the value of x that was passed into the enclosing environment
        x <<- y             # Assigns a value to the uninverted matrix variable
        mInverse <<- NULL   # Clears the previously stored inverted value
    } 
    
    # Private function for simply reporting the value of the enclosing (parent)
    # matrix x
    pri_Get <- function() x
    
    # Private function for setting the enclosing environ var mInverse = the passed param
    pri_SetInverse <- function(mNew) mInverse <<- mNew
    
    # Private function for reporting back the saved value in the parent enclosure
    pri_GetInverse <- function() mInverse
    
    # Accesses the list of the private internal functions and 
    # presents them with their external function member names
    return(list(
        set = pri_Set,
        get = pri_Get,
        setinverse = pri_SetInverse, 
        getinverse = pri_GetInverse))
}


## -----------------------------------------------------------------------
## cacheSolve takes functional object of a matrix defined by makeCacheMatrix() and 
## attempts to efficiently return the inverse of the matrix. If the inverse has 
## already been calculated and stored, it returns the storage. Otherwise it 
## calculates the inverse using the R solve() function and stores it in the
## cache for later efficient retrieval.
##
## This assumes we are using a simple square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    # Will either return a previously calculated inverse or NULL 
    mInverted <- x$getinverse()
    
    # Calculates the inverse if NULL
    if(!is.null(mInverted)) {
        message("getting cached data")
        return(mInverted)
    }
    
    # No inverse exists so one must be created   
    mInitial <- x$get() 
    
	# solve() operates on a simple square matrix
    mInverted <- solve(mInitial, ...)

	# Set the inverse value of the function object so that it does not 
	# need to be recalculated
    x$setinverse(mInverted)
    
    return(mInverted)    
    
}
## -------------------------------------------------------

