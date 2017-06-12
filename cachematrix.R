## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    imtx <- NULL
    set <- function(y){
        x <<- y
        imtx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMat) imtx <<- inverseMat
    getinverse <- function() imtx
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imtx <- x$getinverse()
        if(!is.null(imtx)) {
            message("Cached data")
            return(imtx)
        }
        data <- x$get()
        imtx <- solve(data, ...)
        x$setinverse(imtx)
        imtx
}

