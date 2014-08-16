## Provides functions for calculating and storing an inverse of a matrix for 
## allowing for quicker computation.

## provides the functions needed for setting  a matrix, getting the matrix,
## setting the inverse of the matrix, and getting the  inverse of the matrix.
## imat is Inverse of MATrix

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL
        
        ## function for setting matrix
        set <- function(y){
                x <<- y
                imat <<- NULL
        }
        
        ## function for getting matrix
        get <- function(){
                x
        }
        
        ## function for setting inverse of matrix
        setinverse <- function(inverse){
                imat <<- inverse
        }
        
        ## function for getting inverse of matrix
        getinverse <- function(){
                imat
        }

}


## This function checks if the inverse of a matrix has been cached and
## returns cached value. If the inverse has not been set, this function
## calculates the inverse of the passed matrix, sets the inverse matrix value
## in the cache, and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        
        ## gets cached value of inverse
        imat <- x$getinverse()
        
        ## checks if inverse has already been set. If yes, returns cached value
        if(!is.null(imat)){
                message("Getting cached data")
                return(imat)
        }
        
        ## If inverse has not been set, sets inverse...
        
        data <- x$get()
        imat <- solve(data,...)
        x$setinverse(imat)
        
        ## And returns the inverse
        imat
}
