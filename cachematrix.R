#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  #defined the variable matr here  
  matrr <- NULL
  #set function is to set values of x
  set <- function(k) {
    x <<- k
    matrr <<- NULL
  }
  #the purpose of get function is to look for values of x
  get <- function() x
  #setinverse is to set the values of the inverse matrix
  setinverse <- function(solve) matrr <<- solve
  #the purpose of getinverse is to get the values of the inverse matrix
  getinverse <- function() matrr
  #list function is to list down the existing sub-functions under the 
  #'makeCacheMatrix' function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function is used to computes the inverse of the special "matrix"
#returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  #get the inverse matrix that computed previously
  matrr <- x$getinverse()
  #cross check for the successfulness of the previous calculation
  #once it is not a null value, it will ignore the computational steps below 
  #and return the values for matrr
  if(!is.null(matr)) {
    message("getting cached data")
    return(matrr)
  }
  #let say the condition is false, continue the computational steps as below
  data <- x$get()
  matrr <- solve(data, ...)
  x$setinverse(matr)
  matrr
}

