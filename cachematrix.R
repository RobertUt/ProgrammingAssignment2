## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function takes an numeric argument x (in this case a matrix), 
## and creates a list with the setter and the getter function. 

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

## The cacheSolve function takes the makeCacheMatrix function as argument. It checks if i is NULL. 
## If not, it returns the value of i. If i is NULL, it takes (gets) the matrix from the cache.
## Note, that whenever the cacheSolve(makeCacheMatrix(x)) function takes a new argument x the value assigned 
## to object i is reset.
## It is within the cacheSolve function were the data (in this case a matrix) gets manipulated.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



## How to run cacheSolve(makeCacheMatrix)?

## Make a matrix

u <- c(1, 2)
v <- c(2, 1)
t <- cbind(u, v)

## Execute makeCacheMatrix and save to aMatrix

aMatrix <- makeCacheMatrix(t)

## Now the getter and the setter are defined. We can use the getter to retrieve the value of x
## (which is the cached matrix t).
## The setter allows us to assign a new value to x.

aMatrix$get()
aMatrix$set()

## aMatrix (which is is in fact the makeCacheMatrix function run with t as argument x) can now be 
## used as an input argument for the cacheSolve function. 

cacheSolve(aMatrix)

## cacheSolve checks if there is a matrix stored in the cache. if so, it calculates the inverse of the matrix.