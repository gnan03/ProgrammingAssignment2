## makeCacheMatrix creates a matrix object, initializes it with a given value
## Also caches the inverse of the matrix to avoid computing it again
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If inverse already exists in cache, it rerieves it from the cache

## Creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
     #Initialize inverse to null
     inverse <- NULL
     #Set the value of x
     set <- function(data) 
     {
        x <<- data
        inverse <<- NULL
     }
     #Returns the matrix
     get <- function() x
     #Assigns the inverse of the matrix and stores it in the cache
     setInverse <- function(inv) inverse <<- inv
     #Returns the inverse of the matrix
     getInverse <- function() inverse
     list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function that returns the inverse of a given square matrix
## Checks if the inverse has already been cached

cacheSolve <- function(x, ...) {
        #If the cache is empty, inverse would be null
        inverse <- x$getInverse()
        #Return the cached result
        if(!is.null(inverse)) 
        {
            message("getting cached data")
            return(inverse)
        }
        #Use solve function to calculate inverse
        data <- x$get()
        inverse <- solve(data)
        #Store the inverse in the cache for later use
        x$setInverse(inverse)
        inverse
}
