## Includes 2 functions, 
## 1) makeCacheMarix that stores a matrix passed into cache
## 2) cacheSolve - solves the inverse of the matrix passed using cached values

## R. Humphrey  

## store the matrix passed in as x so it can be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set x = y to use outside of set function & resets m
    # redefine what is being passed into the main function makeCacheMatrix
    set <- function(y) {
        # can use m and x in makeCacheMatrix function
        x <<- y   
        m <<- NULL
    }
    # can use m outside of makeCacheMatrix    
    get <- function() x

    # store the value of an imput into a variable to use in makeCacheMatrix
    setinv <- function(solve) m <<- solve
    #return the value stored in m
    getinv <- function() m
    
    #store values of functions in a list 
    # this is the result of makeCacheMatrix
    # to use assign makeCacheMatrix to an object, then you can use
    # all 4 functions defined.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Retrun the inverse of a defined matrix
##  must be square in order to use
## If the inverse has already been calculated (and the matrix has not changed),
## then`cacheSolve` will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    #retreive m
    m <- x$getinv()   #$ operator is invalid for atomic vectors
    #  tried
    #m <- x[["getinv", exact=FALSE]]   #Error subscript out of bounds
    #m <- getElement(x,"getinv") #Error in object[[name, exact = TRUE]] : subscript out of bounds
    #m <- x[["getinv"]]   #Error subscript out of bounds
    
    # check if mean is already stored and return value as m if it is
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Return a matrix that is the inverse of 'x'
    #inverse of the matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    # return the inverse of the matrix
    m
    
}


#use
mdat <- matrix(c(1,2, 11,12), nrow = 2, ncol = 2)
cacheSolve(mdat)