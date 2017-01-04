## In the following, two functions are presented which are able
## to cache and compute the inverse of a matrix.

## The first function will generate a matrix object which can
## cache the matrice's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx)
        setinv <- function(inv) inverse <<- inv
        getinv <- function() return(inverse)
        return(list(set = set, 
                    get = get, 
                    setinv = setinv, 
                    getinv = getinv))
        }


## The second function generates the inverse of the matrix
## returned by "makeCacheMatrix" (first function). If the
## matrix is not changed and the inverse has already been calculated,
## "cacheSolve" will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        data <- mtx$get()
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}
