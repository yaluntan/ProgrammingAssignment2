## Caching inverse of a matrix to reduce time-consuming computation
## 

## write a 'makeCacheMatrix'function to cache inverse

makeCacheMatrix <- function(x = matrix()) {
        
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
       
}

## Write a 'cacheSolve' function to retrieve inverse
## If found m previously calculated, return m; otherwise calculate it.

cacheSolve <- function(x, y) {
        ## Return a matrix that is the inverse of 'x'
                x$set(y)
        
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                
                data <- x$get()
                
                m <- solve(data)
                x$setinverse(m)
                m
        
}
