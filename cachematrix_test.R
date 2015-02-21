## Tests makeCacheMatrix and cacheSolve functions

## Test inputs
testMatrix <- matrix(1:4, 2, 2)
testInverseMatrix <- solve(testMatrix)
newMatrix <- matrix(2:5, 2, 2)

testCacheMatrixGetReturnsOriginal <- function() {
    cm <- makeCacheMatrix(testMatrix)
    if (identical(testMatrix, cm$get()))
        message("Success!")
    else
        warning(sprintf("%s != %s", cm$get, testMatrix))
}

testCacheMatrixGetInverseReturnsNull <- function() {
    cm <- makeCacheMatrix(testMatrix)
    if (is.null(cm$getCachedInverse()))
        message("Success!")
    else
        warning("Caches's not been correctly initialized.")
}

testCacheMatrixSetInverse <- function() {
    cm <- makeCacheMatrix(testMatrix)

    cm$cacheInverse(testInverseMatrix)
    if (identical(testInverseMatrix, cm$getCachedInverse()))
        message("Success!")
    else
        warning("Setinverse failed")
}

testCacheMatrixSetUpdatesMatrix <- function() {
    cm <- makeCacheMatrix(testMatrix)

    cm$set(newMatrix)
    if (identical(newMatrix, cm$get()))
        message("Success!")
    else
        warning("Set did not update the matrix properly.")
}

testCacheMatrixSetResetsCache <- function() {
    cm <- makeCacheMatrix(testMatrix)

    cm$cacheInverse(testInverseMatrix)
    cm$set(newMatrix)
    if (is.null(cm$getCachedInverse()))
        message("Success!")
    else
        warning("Caches was not cleared upon setting a new matrix.")
}

testCacheSolveSolvesMatrix <- function() {
    if (identical(testInverseMatrix,
                  cacheSolve(makeCacheMatrix(testMatrix))))
        message("Success!")
    else
        warning("CacheSolve didn't calculate a correct inverse.")
}

testCacheSolveReturnsCachedInverse <- function() {
    cacheMatrix <- makeCacheMatrix(testMatrix)
    cachedInv <- "cachedInv"  # A fake inverse matrix.
    cacheMatrix$cacheInverse(cachedInv)

    if (identical(cachedInv, cacheSolve(cacheMatrix)))
        message("Success!")
    else
        warning("CacheSolve didn't return the cached inverse")
}

testCacheMatrixGetReturnsOriginal()
testCacheMatrixGetInverseReturnsNull()
testCacheMatrixSetInverse()
testCacheMatrixSetUpdatesMatrix()
testCacheMatrixSetResetsCache()
testCacheSolveSolvesMatrix()
testCacheSolveReturnsCachedInverse()
