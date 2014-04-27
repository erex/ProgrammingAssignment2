## These functions are derived from the sample functions given as part of the assignment
## makeVector and cachemean. They have been modified in the following way:
## * the correct functionality has been implemented
## * every function has been commented to demonstrate an understanding of what
##   they do
## * minor formatting changes, i.e. return(x) instead of just x on line 25


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
    # we initialize a variable (setting it to null) that we then use to cache the inverse
    m <- NULL
    # this function is used to change the value (x) of the makeCacheMatrix object
    # when the value of the object is changed, we reset the inverse cache variable
    set <- function(y){
        # the use of '<<-' allows us to change the value of a variable in the
        # parent environment - in this case the object created by makeCacheMatrix
        # the set function stores a new matrix 'x' and resets the value of the
        # cached inverse of the matrix 'm' (setting it to null)
        x <<- y
        m <<- NULL
    }
    # the this function returns the value of the makeCacheMatrix object 
    get <- function(){
        return(x)
    }
    # this function stores the inverse matrix in the 'm' variable in the parent environment
    setinverse <- function(inverse){
        m <<- inverse
    }
    # the this function returns the inverse value of the makeCacheMatrix object 
    getinverse <- function(){
        return(m)
    }
    # list exposes the internal functions of the created object 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse
# from the cache.
cacheSolve <- function(x) {
    # return a matrix that is the inverse of 'x' - the previously created object
    m <- x$getinverse()
    # if 'm' (the cached inverse)  that was set in the makeCacheMatrix function
    # is not null, we return 'm'
    if(!is.null(m)) {
        message("got cached data from the object created by makeCacheMatrix")
        return(m)
    }
    # if 'm' is null, we retrieve the matrix value...
    data <- x$get()
    # we calculate the inverse...
    # (lexical scoping means that the 'm' in the cacheSolve function is not the
    # same variable as in the makeCacheMatrix function)
    m <- solve(data)
    # and we store it using the setinverse function in makeCacheMatrix...
    x$setinverse(m)
    # and we return the inverse of the matrix
    return(m)
}
