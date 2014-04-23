## These functions need for caching the inverse of a matrix

## Constructor for a cache object which encapsulate data and cache

makeCacheMatrix <- function(x = matrix()) {
		cache <- NULL
        set <- function(new_x) {
                x <<- new_x
                cache <<- NULL
        }
        get <- function() x
        setCache <- function(new_cache) cache <<- new_cache
        getCache <- function() cache
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}

## Solve function which may cache the result

cacheSolve <- function(x, ...) {
		inverse <- x$getCache()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setCache(inverse)
        inverse
}
