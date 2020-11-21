##Tha aim is to create a function that cache the matrix and gives the inverse of the matrix 
## inverse of the matrix

## the function makeCacheMatrix () creates the matrix

makeCacheMatrix <- function(x = matrix()) {
 I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinvers <- function(solve) I<<- solve
        getinvers <- function() I
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## the function cacheSolve() gives the inverse of the matrix

cacheSolve <- function(x, ...) {
         I <- x$getinvers()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinvers(I)
        I
}
