## The following functions create an object that can store a matrix and cache its inverse. 

## This function makes a special type of "matrix", which ultimately is a list containing functions that:
## (1) set value of matrix
## (2) get value of matrix
## (3) set value of inverse, and
## (4) get value of inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialising inverse variable (of matrix) to NULL
    inv <- NULL
    
    ## (1) Function that sets the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## (2) Function that gets the value of the matrix
    get <- function() {
        x
    }
    
    ## (3) Function that sets the inverse of the matrix
    setinverse <- function(inv_value) {
        inv <<- inv_value
    }
    
    ## (4) Function that gets the inverse of the matrix
    getinverse <- function() {
        inv
    }
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## This function calculates the inverse of the special matrix created by the makeCacheMatrix() function above.
## If the inverse has already been calculated, then this function will get the inverse of the matrix from the cache and skip further calculation.
## If not, then the inverse is calculated. The function will then update/set the value of the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
    
        inv <- x$getinverse()
        
        ## If the inverse of special matrix is successfully retrieved, then it will use this value and stop further computation.
        if(!is.null(inv)) {
            message("Successfully getting cached data of inverse")
            return(inv)
        }
        
        ## Otherwise, it will retrieve the matrix data to compute the inverse using solve() function
        data <- x$get()
        inv <- solve(data)
        
        ## Setting value of inverse in the cache using setinverse function
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}   
