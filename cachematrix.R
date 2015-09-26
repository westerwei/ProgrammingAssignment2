## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mData <- x
        mInv <- NULL
        set <- function(xData){
                mData <<- xData
                mInv <<- NULL
        }
        
        get <- function() mData
        
        setInv <- function(inv) mInv <<- inv
        
        getInv <- function() mInv
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInv()
        if(!is.null(mInv)){
                message("Got inverse value from cache!")
                return(mInv)
        }
        
        mInv <- solve(x$get(), ...)
        x$setInv(mInv)
        
        mInv
}
