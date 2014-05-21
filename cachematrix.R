## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The object is really a list that can which is really a list containing a
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # value used for cache starts off as null
        set <- function(y) {  #makes a function to allow setting value of matrix
                x <<- y   #sets the value to the input matrix
                m <<- NULL  #cache NULL when matrix added or changed;using '<<-'
                # insures m instances used in function list changed too.
        }
        get <- function() x  #makes function to return value of the matrix
        setinverse_matrx <- function(the_matrix) m <<- the_matrix
        # above line uses <<- because want to use argument passed in the
        #function to set the value m to what is inside the function
        getinverse_matrx <- function() m #func. returns null or the inverse
        list(set = set, get = get,
             setinverse_matrx = setinverse_matrx,
             getinverse_matrx = getinverse_matrx)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        ##inverse of a square matrix can be done with the solve function in R
        m <- x$getinverse_matrx ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)    #exit function upon returning cached solve result
        }
        data <- x$get() #get the value of the matrix from the special matrix
        m <- solve(data, ...)  #if not previously caclulated, perform solve func.
        x$setinverse_matrx (m) #and cache result so don't need to calculate again
        m
}
