## This code allows the caching of matrix inversion computations

## makeCacheMartix returns a list of four functions that are needed for cacheSolve to work. 
##      set() which sets the matrix
##      get() which gets the matrix to solve
##      setsolve() which sets the value of the inverse matrix by solve(matrix)
##      getsolve() which gets the value of the inverst matrix is available

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                ## use the <<- operator so we can pass variable to the parent environment
                x <<- y
                s <<- NULL
        }
        ## Create the list
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function returns the inverse of the matrix defined by makeCacheMatrix:
##      If the inverse hasn't been determined, get the matrix and return the inverse
##      If the inverse has been determined, just return the inverse already in memory (cache)

cacheSolve <- function(x, ...) {
        ## Get the inverse if it's in memory and return it
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## else, get the matrix
        data <- x$get()
        ## find the inverse
        s <- solve(data, ...)
        ## set the inverse in memory
        x$setsolve(s)
        ## return the result
        s
}
