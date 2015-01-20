## Coursera, R Programming, January 2015
## Programming Assignment 2
## Cashing the inverse of a matrix
##
## Description:   Calculating the inverse of a matrix can be very time and resource consuming.
##                Therefor it can be beneficial to store the inverse of a matrix in cache instead 
##                of calculating the inverse repeatedly.  
##                The assignment is to create two functions that are designed to do that.



## Funtion:       makeCacheMatrix
## Description:   Creates an object that is able to store and retrieve an inverse matrix from cache.
##                
## Arguments:     The function has one argument:
##                      x     The matrix you wish to invert.
##
## Returns:       Returns a special "matrix", which is really a list containing a function to: 
##                      1. set the value of the matrix
##                      2. get the value of the matrix
##                      3. set the value of the inverse matrix
##                      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {        
      # Create inverse matrix variable to store the matrix inverse.
      inverse.x <- NULL 
      
      # Create set function that: stores in cache the matrix value and sets the inverse matrix value
      # in cache to NULL.
      set <- function(y) {
            x <<- y
            inverse.x <<- NULL
      }

      # Create get function that: gets the matrix value.
      get <- function() {x}
      
      # Create setinverse function that: stores the inverse matrix value in cache.
      setinverse <- function(inv) {inverse.x <<- inv}
      
      # Create getinverse function that: gets the inverse matrix value.
      getinverse <- function() {inverse.x}
      
      # Return the special "matrix".
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## Funtion:       cacheSolve
## Description:   Calculates the inverse of the matrix using the output of the makeCacheMatrix
##                function. It first checks to see if the inverse has already been stored in cache.
##                If so, it gets the inverse from the cache and skips the computation. Otherwise,
##                it calculates the inverse of the matrix and stores the value of the inverse in 
##                cache via the setinverse function. 
##
## Arguments:     The function has two arguments:
##                      x     The makeCacheMatrix object.
##                      ...   Used to pass additional arguments to the 
##                            solve function
##
## Returns:       Returns the inverse of the matrix that was used as input for the makeCacheMatrix 
##                function. If the inverse was retrieved from cache, "Getting cached data" is
##                printed along with the inverse matrix.

cacheSolve <- function(x, ...) {
      # Gets the cached value for the matrix inverse.
      inverse.x <- x$getinverse()
      
      # If the inverse matrix exists in cache, return it.
      # Send notification to user that the data was retrieved from cache.
      if(!is.null(inverse.x)) {
            message("Getting cached data")
            return(inverse.x)
      }
      
      # Get the matrix to inverse..
      data <- x$get()
      
      # Compute the inverse matrix.
      inverse.x <- solve(data, ...)
      
      # Store the inverse matrix in cache.
      x$setinverse(inverse.x)
      
      # Return the inverse matrix.
      inverse.x
}




## Test:
## To test the working of both functions, use the following lines of code:
##
## Test <- makeCacheMatrix(matrix(1:4, 2, 2))
## cacheSolve(Test)
## cacheSolve(Test)