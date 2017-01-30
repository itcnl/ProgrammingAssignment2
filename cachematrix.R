## makeCacheMatrix() assigns a list of functions to be applied
## cacheSolve() returns a cached value from memory, otherwise recalculates.

## The makeCacheMatrix() assigns x and m_inv to the parent environment.
## m_inv will be reset each time x is reassigned a new value.
## the code then goes on to get the value of x from the parent environment
## m_inv is then set using the "solve()" function, and being retrieved using getinverse
## every element is now returned as a list
makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m_inv <<- solve
        getinverse <- function() m_inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve obtains the cached value of the inverse matrix
## if m_inv exists (is not null), the m_inv will be returned from cache without computation
## otherwise, m_inv needs to be calculated using the solve() function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse()
        if (!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinverse(m_inv)
        m_inv
}
