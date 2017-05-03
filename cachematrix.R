## cacheSolve and makeCacheMatrix work together to create the inverse of a matrix and cache it or to
## retrieve a previously cached inverse. cacheSolve requires makeCacheMatrix to work, i.e. 
## cacheSolve(makeCacheMatrix(x))

## makeCacheMatrix establishes functions and variables necessary for cacheSolve in the parent environment

makeCacheMatrix <- function(x = matrix()) { ## x is initialized, with default of an empty matrix
        i <- NULL ## i is initialized within the function environment and set to NULL
        set <- function (y) { ## set function is defined with anonymous variable y
                x <<- y ## assigns input of x to the parent environment
                i <<- NULL ## clears any previously cached inverse matrices stored in i
        }
        get <- function() x ## defines the get function for x
        setinv <- function(solve) i <<- solve ## defines the set for i as the inverse matrix
        getinv <- function() i ## defines the get function for i
        list( set = set, get = get,
              setinv = setinv, 
              getinv = getinv) ## creates a list of function definitions in the global environment
        
}

## Creates an inverse of a matrix, checking cache first to see if this is already done - used with
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv() ## accesses the getinv function from the parent environment the check value of i
        if (!is.null(i)) { ## checks to see if i is not null
                message( "getting cached data") ## message output
                return (i) ## returns cached value of i (inverse matrix)
        }
        data <- x$get() ## defines variable data using the get function to retrieve x
        i <- solve(data, ...) ## defines i as the inverse matrix of variable data
        x$setinv(i) ## uses function setinv to cache inverse matrix as i
        i ## returns inverse matrix i
}