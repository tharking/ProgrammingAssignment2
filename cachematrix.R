## [Author: Kamal Choudhary <kamal[dot]choudhary[at]gmail[dot]com]
## [Creation Date: 10/26/2014]
## [Desc: File created as part of Coursera Data Science specilization course 
## 'R programming', programming assignment 2]
## This file can act as library for calculating inverse of large matrix
## It's basically a caching wrapper on the top of "solve" function
## It internally caches the inverse of original matrix and on subsequent 
## calls to solve the matrix, it returns previously cached inverse matrix.
## Two functions 'makeCachematrix' and 'cacheSolve'. 
## User needs to pass original matrix object to makeCachematrix and it 
## returns a special cachematrix object which can be passed on cacheSolve
## function to get the inverse of original matrix. 

## makecachematrix function takes user defined matrix object as input and 
## returns a list object that can be passed on to cacheSolve function
## input: x - user specified matrix object. it defaults to empty NA matrix.
## output: returns a list object containing four functions - "set", "get", 
## "setinv", "getinv" which are used by cacheSolve to cache the inverse 
## matrix and original matrix data

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_x <<- inv
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function takes a 'cached matrix' list object returned by 
## makeCacheMatrix function defined above. it returns inverse matrix of 
## the 'cached matrix' object. If it finds that object doesn't yet have 
## inverse matrix cached in the object, it computes inverse using the 
## solve function and it stores the computed inverse matrix in the original
## cachematrix object. On subsequent calls, it finds the cached inverse 
## in the object and it doesn't need to recompute the inverse matrix which 
## can be a time consuming operation.
## input: x - 'cached matrix' list object returned by makeCacheMatrix.
## output: returns inverse of matrix stored in input object. If it returns 
## cached inverse matrix, it prints a message saying "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinv(inv_x)
  inv_x
}
