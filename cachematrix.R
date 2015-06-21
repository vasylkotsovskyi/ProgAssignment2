## these functions are designed to extract inverse matrix to the matrix requested. However, instead of computing
## reverse matrix (which is time-concuming operation) it, at first stage, check if there is ready one in the cache
## memory and returns it and, at second stage, if there is none it computes, stores and returns it.

## the first function constitutes of 4 functions 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the reverse matrix
## 4. get the value of the reverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x[,]
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv[,]
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the reverse matrix for the one created with the above function (lets 
## call it consistantly with example - "special" matrix).It proceeds in the following way: firstly, it checks is the 
## reverse matrix to the "special" matrix exists in 
## the cash memory. If it is the case then function returns this matrix together with the message "getting cached data".
## Secondly, if it doesn't exist (explicitly it means the matrix that contains NA's is the reverse matrix to the 
## corresponding "special" one), it calculates reverse matrix using 'solve' command, stores it in
## cash memory and returns it. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!anyNA(inv)) {
    message("getting cached data")
    return(inv[,])
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv[,])
  inv[,] ## Return a matrix that is the inverse of 'x'
}
