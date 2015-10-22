#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  #defined the variable matr here  
  matr <- NULL
  #set function is to set values of x
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  #get function is to search for values of x
  get <- function() x
  #setinverse is to set the values of the inverse matrix
  setinverse <- function(solve) matr <<- solve
  #getinverse is to get the values of the inverse matrix
  getinverse <- function() matr
  #list function is to list down the existing sub-functions under the 
  #'makeCacheMatrix' function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  #get the inverse matrix that calculated previously
  matr <- x$getinverse()
  #check for the successfulness of the previous calculation
  #if it is not a null value, it will skip the computational steps below 
  #and return the values for matr
  if(!is.null(matr)) {
    message("getting cached data")
    return(matr)
  }
  #if the condition is false, continue the computational steps as below
  data <- x$get()
  matr <- solve(data, ...)
  x$setinverse(matr)
  matr
}