## These two functions will make computations of matrix inversion quicker by caching the inverse. 
## Instead of repeatedly computing the inverse, the inverse can be looked up in the cache.

## The makeCacheMatrix function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            m <- matrix()
            set <- function(y){
                    x <<- y
                    m <<- matrix()
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get, 
                 setinverse = setinverse, 
                 getinverse = getinverse)
  }

## The cacheSolve function computes the inverse of the special matrix returned with the makeCacheMatrix function.
## If the inverse is already calculated this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
            if(all(is.na(m) == FALSE)){
                      message("getting cached data")
                      return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}

