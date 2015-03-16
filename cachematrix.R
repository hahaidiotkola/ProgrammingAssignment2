    ## Programming Assignment 2 is about lexical scoping and caching functions. When
    ## running time consuming computations, it is always wise to cache the results.
    ## So you can retrieve and use them later instead of calculating them again.
    ## Matrix inversion is quite costly to compute, especially when running inside
    ## a loop. The following two functions create a special object that stores a
    ## square invertible matrix, calculates it's inverse and caches the inverse.    


makeCacheMatrix <- function(x = matrix()) {

    ## x is a square invertible matrix
    ## This fuction creats a list containing the following functions:
    ## i) set to set the matrix
    ## ii) get to get the matrix 
    ## iii) setinv to set the inverse matrix
    ## iv) getinv to get the inverse matrix
    ## the list is used as input to cacheSolve()

    inv <- NULL    # where the result of inverse matrix is stored
    set <- function(y) {
        
      ## set function sets a matrix to an object created by makeCacheMatrix()
      ## '<<-' is used to assign a value to an object in an enviroment
      ## different from the current enviroment.
      ## And '<<-' instructs R to continue searching through enviroments until
      ## it finds that variable to set.

    x <<- y
    inv <<- NULL
    }
    get <- function()x     # returns the original input matrix
    setinv <- function(inverse) inv <<- inverse     # set inverse matrix
    getinv <- function() inv     returns the inverse matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
      # returns a list containing the four functions.         
}



cacheSolve <- function(x, ...) {

    ## This function uses the output list of the above function as it's input.
    ## And it calculates the inverse of the input matrix. However, if the inverse
    ## is cached, it will not calculate. But it will retrieve the inverse from the
    ## cache.
    ## Hence, the function returns a matrix that is the inverse of 'x', the original
    ## matrix input to makeCacheMatrix().
    ## x here is the output of makeCacheMatrix()

    inv <- x$getinv()
    if (!is.null(inv)) {

      ## if the inverse has already been calculated
      ## get it from the cache and do not calculate
 
      message("getting cached data")
      return(inv)     # return the cached inverse
    }
    
    ## else calculate the inverse

    mdata <- x$get()             # perform x$get to get the matrix
    inv <- solve(mdata, ...)     # use solve() to calculate the inverse matrix
    x$setinv(inv)                # sets the value of the inverse mstrix in the cache
                                 # via setinv function.
    inv                          # the calculated inverse           
}
