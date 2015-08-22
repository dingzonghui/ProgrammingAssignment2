## The following functions work together to create a square matrix
## and make the inverse of the matrix available in the cache

## makeCacheMatrix function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # store the cached value with initial NULL value
        cache <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                cache <<-NULL
        }
        
        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache
        
        # return the created function to the working environment
        list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}

## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function
## If the inverse has already been calculated and the matrix has not changed, 
## it’ll get the inverse from the cache directly without computation again.
cacheSolve <- function(x, ...) {
        # output of makeCacheMatrix()
        # return: inverse of the original matrix input to makeCacheMatrix()
        
        cache <- x$getInverse()
        # if the inverse matrix has already been calculated
        if(!is.null(cache)){
                # get it from the cache and skips the computation
                message("getting cached data")
                return(cache)
        }
        
        # otherwise, calculates the inverse
        matrix.data <- x$get()
        cache <- solve(matrix.data, ...)
       
         # set the value of the inverted matrix in cache via setMatrix function
        x$setMatrix(cache)
       
        return(cache)
}
