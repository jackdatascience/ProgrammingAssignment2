
## makeCacheMatrix function will 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y) {
        x <<- y
        j <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The CacheSolve function will return the inverse of a Matrix
## It will check if the inverse of matrix is already available. If yes, it will
## display this result. If not, it will find the inverse and share the result.

cacheSolve <- function(x, ...) {
    j <- x$getinverse()
    if(!is.null(j)) {
        message("Display earlier cached data.")
        return(j)
    }
    data <- x$get()
    j <- solve(data)
    x$setinverse(j)
    j
}

##> x = rbind(c(1, 9), c(9, 1))
##> m = makeCacheMatrix(x)
##> m$get()
 ##    [,1] [,2]
##[1,]    1    9
##[2,]    9    1
##> cacheSolve(m)
##        [,1]    [,2]
##[1,] -0.0125  0.1125
##[2,]  0.1125 -0.0125
##> cacheSolve(m)
##getting cached data.
##        [,1]    [,2]
##[1,] -0.0125  0.1125
##[2,]  0.1125 -0.0125
##> 