## makeCacheMatrix
## This function will:
## 1) take a square matrix (e.g., 3x3, 4x4)
## 2) store the matrix in the cache (as "invertedMatrix")
## 3) Return an list of functions to interact with the matrix passed in
makeCacheMatrix <- function(x = matrix()) {
    ## create local var invertedMatrix
    invertedMatrix <- NULL
    ## reassign the matrix to the new one passed in and clear out the inverted matrix
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }

    ## return the current matrix
    get <- function() x
    ## store the inverted matrix in the cache
    setinv <- function(inverse) invertedMatrix <<- inverse
    ## get the current invertedMatrix from cache
    getinv <- function() invertedMatrix
    ## create the list of functions to return to caller
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

## cacheSolve
## This function will:
## 1) Retrieve the inverted matrix from cache
## 2) a: If it exists, just return it.
##    b: If not:
##      - calculate the inverse
##      - store it in the cache
##      - return inverted matrix
cacheSolve <- function(x, ...) { 
    ## retrive the current inverted matrix from cache
    invertedMatrix <- x$getinv()
    ## if there is an inverted matrix, then just return it
    if(!is.null(invertedMatrix)) {
        message("getting cached matrix")            
        return(invertedMatrix)
    }
    ## no inverted matrix so get the current matrix
    data <- x$get()

    ## invert the current matrix
    invertedMatrix <- solve(data)
    
    # store the new inverted matrix
    x$setinv(invertedMatrix)
    
    # return the inverted matrix to the caller
    return(invertedMatrix)
}

