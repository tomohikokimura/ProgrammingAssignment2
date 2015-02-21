## MakeCacheMatrix creates a custom matrix object, let's call it CacheMatrix,
## that wraps around a normal R matrix, and caches the matrix's inverse.
## CacheSolve calculates the inverse matrix given a CacheMatrix object. It
## returns a cached inverse if it's already been computed and cached in the
## object, or otherwise calculates one and caches it, and returns the inverse.


## This creates a CacheMatrix object that wraps around the given R matrix passed
## as an argument. "Get" and "set" update the original matrix this wraps around,
## and "getCachedInverse" and "cacheInverse" are getter / setter for the
## internal cached inverse. Upon setting a new matrix to the object via a "set"
## call, the function clears the old cache, setting NULL to cachedInverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    get <- function() x
    set <- function(newMatrix) {
        # Upon setting a new matrix, clears the old cache.
        x <<- newMatrix
        cachedInverse <<- NULL
    }
    getCachedInverse <- function() cachedInverse
    cacheInverse <- function(inv) cachedInverse <<- inv
    list(get = get,
         set = set,
         getCachedInverse = getCachedInverse,
         cacheInverse = cacheInverse)
}

## This takes a CacheMatrix object x, and computes its inverse if it has not
## already been cached yet and caches it with the object. If the object has a
## cached inverse, the function simply returns the cached value.

cacheSolve <- function(x, ...) {
    cachedInv <- x$getCachedInverse()
    if (is.null(cachedInv)) {
        # The inverse is not cached. Calcualtes one and caches it.
        inverse <- solve(x$get(), ...)
        x$cacheInverse(inverse)
        inverse
    } else {
        # The object has a cached inverse. Simply returns it.
        cachedInv
    }
}
