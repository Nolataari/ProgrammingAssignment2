## 'makeCacheMatrix' creates a 'matrix' object with mutator and accessor functions
## to facilitate caching the inverse of a matrix calculated in 'cacheSolve'


makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
    inv <- NULL # instantiate the inverse of the matrix
    
    set <- function(y){
        x <<- y # mutate the value of y (the new value to be set) in the parent environment
        inv <<- NULL # instantiate the inverse of the matrix
    }
    
    get <- function() x #access the matrix
    
    setinverse <- function(inv){ # mutate the inverse of the matrix
        inv <<- inv ##can do same var since it's a out-of-closure assignment
    }
    
    getinverse <- function() inv #access the currently cached inverse
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
    ## cacheSolve computes the inverse of the special "matrix" returned by 
    ## makeCacheMatrix above. If the inverse has already been calculated 
    ## (and the matrix has not changed), then cacheSolve should retrieve the inverse
    ## from the cache.
    
    #check if the inverse has already been cached. If so, then return it
    inv <- x$getinverse()
    if(!is.null(inv)){
        message('Getting cached data...')
        return(inv)
    }
    
    # [...] If it hasn't been already calculated, then calculate and cache it.
    data <- x$get() # access the matrix value from the 'matrix' object [...]
    inv <- solve(data, ...) # [...] use it compute the inverse
    x$setinverse(inv) # [...] cache it
    inv # return the result
}

## Example
m <- matrix(1:4, 2, 2)
aMatrix <- makeCacheMatrix(m)
aMatrix$get()
aMatrix$getinverse()
cacheSolve(aMatrix)
aMatrix$getinverse()
    
