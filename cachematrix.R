## makeCacheMatrix creates a matrix, which can store a cached version
## of inverse (Inv)
## cacheSolve calculates the inverse of the matrix (or retrieves the inverse
## from cache (if already calculated).

## creates a cached matrix, with attributes set/get, setInv, getInv
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) i <<- inverse
    getInv <- function() i
    list (set = set, get = get,
        setInv = setInv, getInv = getInv)
}


## returns the inverse of a cachedMatrix x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <-x$getInv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) #solve(a) # returns inv of matrix a
    x$setInv(i)
    i
}
