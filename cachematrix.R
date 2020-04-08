## This function does calculate the inverse of an input matrix and stores it in the cache
## 

## This function is setting an empty matrix and then getting the respective matrix followed by
## setting the inverse and getting the inverse of the respective matrix. The output of this
## function is a list containing the individual steps as functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function () m
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function first gets the getInverse element of the previous function and prints a matrix,
## if the inverse was already calculated, if not it calculates the inverse and prints it as an
## output.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve (data,...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}