## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function set the value of matrix and then calculates the inverse of that matrix and then uses getinv function to retrieve the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  # uses set function inside to set the value of the matrix passed as y argument
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  # retrieves the value of the matrix
  get <- function() x
  # sets value of i as the inverse of matrix calculated within the cacheSolve function.
  setinv <- function(inversei) i <<- inversei
  # getinv function retrieves value of inverse of matrix stored in variable i.
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
# This function calls the makeCacheMatrix function and then calls getinv function within makeCacheMatrix function.
# if the value of i already exists, it will not calculate the value of inverse i and simply return a message "getting cached data"
# else it will calculate the value of inverse of matrix and return the value of i 
# and sets the value of i in makeCacheMatrix function by calling the setinv() function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  #if the value is not NULL and was previously calculated then it will cache the value from before and return that and return a message
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #if value of i does not exist, it is calculated and then the value of i is set in the makeCacheMatrix function using setinv() function.
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
