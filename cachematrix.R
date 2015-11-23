## This functions calcule the inverse matrix and set it to the cache

## This function sets and gets a Matrix to/from the cache 

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseM) i <<- inverseM
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function calculates the inverse of the Matrix created with 
## makeCacheMatrix and set the result to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        for (a in seq_len(nrow(data)))
                for (b in seq_len(ncol(data)))
                        i[a,b] <- data[b,a]
        
        x$setInverse(i)
        i
        
}
