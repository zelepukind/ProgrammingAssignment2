makeCacheMatrix <- function(x = matrix()) {
  a <- NULL #cash
  set <- function(y) { #local matrix
    x <<- y
    a <<- NULL
  }
  #get the matrix
  get <- function() x 
  #invert to cash
  setinverse <- function(inverse) a <<- inverse 
  #extract form cash
  getinverse <- function() a
  #return functions
  list(set = set,             
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  #get matrix from cash
  a <- x$getinverse()
  if (!is.null(a)) { #is there inverse cash data?
        return(a)    # if so, return it
  }
  #Well, if not - do it
  data <- x$get()
  #inverse matrix
  a <- solve(data, ...)
  #upload to cash
  x$setinverse(a)
  return(a) ## Return a matrix that is the inverse of 'x'
}
