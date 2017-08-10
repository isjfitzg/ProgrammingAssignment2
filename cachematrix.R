## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){  
        ## a square invertible matrix
        ## returns a list to set and get the matrix; 
        ## and to set and get the inverse
        inv = NULL
        set = function(y){
                ## <<- Assigns the value to an object in another environment
                x <<- y
                inv <<- NULL
        }
        get = function()x
        setInverse = function(inverse) inv <<- inverse
        getInverse <- function() inv
        list (set = set, get = get,
              setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCachematrix ABOVE.  If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the makeCacheMatrix

cacheSolve <- function(x, ...) {  
        inv = x$getInverse()
        if (!is.null(inv)){
                ## if the inverse was calculated, get it from the cache
                message("getting cached data")
                return(inv)
        }
        ## if not already calculated, calculates the inverse
        my.data = x$get()
        inv = solve(my.data, ...)
        ##A sets the value of the inverse in the cache
        x$setInverse(inv)
        return(inv)
}
