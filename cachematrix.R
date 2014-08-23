## This file contains two functions: makeCacheMatrix(), and cacheSolve(). 
## makeCacheMatrix() is used to cache the inverse of a matrix;
## cacheSolve() is used to return the inverse of a matrix from the cache.

## makeCacheMatrix() takes a matrix as its argument and creates a list.
## The list contains functions to:
##      1. set the value of the matrix
##      2. get the value of the vector
##      3. set the value of the mean
##      4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        
        ## First, initialize variable 'm'
        m <- NULL
        
        ## Next, create a function, set(), that acts on variable 'y':
        ## 1. variable 'x' in the parent environment -- the makeCacheMatrix() 
        ##    function -- is set to 'y'
        ## 2. variable 'm' in the parent environment -- the makeCacheMatrix() 
        ##    function -- is set to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Create a function, get(), that returns variable 'x' -- the original
        ## argument 
        get <- function() x
        
        ## Create a function, setinverse(), that performs the solve() function
        ## and variable 'm' equal to the output of solve() in the parent
        ## environment [the makeCacheMatrix() function].
        ##    function
        setinverse <- function(solve) m <<- solve
        
        ## Create a function getinverse() that returns the variable 'm'. Variable 
        ## 'm' is null unless setinverse() has been invoked
        getinverse  <- function() m
        
        ## Create a list of the created functions, to identify them by name
        ## The list is the output of the function; the listed functions can then be 
        ## invoked, as a subset of the list object.
        
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
        
        ## Example: For matrix object 'Neo':
        ## ## Create list object
        ## x  <- MakeCacheMatrix(Neo)
        ## ## Pass matrix 'Neo', as a function of 'x', to new variable 't' :
        ## t <- x$get
        
}

## This function, cacheSolve(), takes a list object that resembles a matrix --
## that is, the output of makeCacheMatrix() -- and returns the value from the
## cache (if it exists), or it calculates the inverse.

cacheSolve <- function(x, ...) {
       
        ## Set variable 'm' to the inverse of the argument
        ## Note: This works only if the argument has previously been cached;
        ## otherwise, 'm' is set to NULL
        m <- x$getinverse()
        
        ## Assuming 'm' is not null -- that is, it has been cached -- let
        ## the user know that you're retrieving date from the cache and then
        ## return the (non-null) 'm' and end the function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Set variable 'data' equal to the value of the argument
        data <- x$get()
        
        ## Set variable 'm' equal to the solve() output -- the matrix inverse
        ## -- of variable 'data'
        m <- solve(data, ...)
        
        ## Use the previously defined setinverse() function to cache the 
        ## inverse for later use.
        x$setinverse(m)
        
        ## Return matrix 'm', which was just set to the solve() output
        m
}
