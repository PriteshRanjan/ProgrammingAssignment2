
## To save some computations in computing matrix inverses
## this program stores the matrix's inverse. 
## A noramal matrix is transformed into a CacheMatrix 
##CacheSolve calculates the inverse only if the matrix's cache is not available

## Returns a list of functions needed to convert a matrix 
##      into cache enabled matrix

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y)
        {
                x <<- y #store in x of parent env
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        
        # return the list which has the four functions
        list(set = set, get = get, setInv = setInv, getInv= getInv)
        
}


## First checks if the cacheMatrix has an inverse stored
## if yes then returns the cached inverse
## otherwise computes and stores the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if (!is.null(inverse))
        {
                message("returning cached result")
                return (inverse)
        }
        inverse <- solve(x$get())
        x$setInv(inverse)
        
        inverse
}
