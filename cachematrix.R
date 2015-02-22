## The functions could cache the inverse of a matrix rather then computing it repeatedly to 
## reduce a costly computaion of matrix inverse.

## The purpose of function makeCacheMatrix is to cache the inverse.
## It creates a list including "get the matrix", "set the matrix","get the matrix inverse" and "set the matrix inverse".
## "get" is to get the matrix.
## "set" is to invalidate the cache when changing the value of matrix and update the matrix in the scope.
## "setinverse" is to collect the inverse from cacheSolve and make it in the scope. 
## "getinverse" is to get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}

## Function cacheSolve computes the inverse of the matrix from makeCacheMatrix. If the inverse has already been calculcated 
## and the matrix has not been changed,  it just retrieve the inverse from the cache, else calculate the inverse.
## Step1: get the inverse from makeCacheMatrix
## Step2: if inverse is not null, then get the inverse from cache and return m.
## step3: if inverse is null, then get the matrix from makeCacheMatrix, calculate the inverse by solve() and then assign 
## the value of inverse to makeCacheMatrix to cache it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  
}
