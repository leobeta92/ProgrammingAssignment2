## Put comments here that give an overall description of what your
## functions do

##I generally followed the example given by the instructions.
##At least the principles, I made the variables in makecachematrix global
##so that they can be accessed by cacheSolve to calculate the inverse if 
##there is no matrix in makeCacheMatrix. Or just get the value stored in 
##makeCacheMatrix if there already is a value.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  get <- function(x){
    m <<- x
    i <<- NULL
  }
  
  set <- function(){
    x
  }
  
  getinv <- function(inv) {
    i <<- inv
  }
  
  setinv <- function(){
    i
  }
  
  list(set= set, get= get, setinv= setinv, getinv= getinv)
}


## This first checks to see if a value is stored in the list value stored
## in entry 'getinv'. If there is, yay, it will return mat and not compute the 
## inverse. If not, no worries, the data stored in list value 'get' will be accessed
## and then solve() can be used to get the inverse of the matrix.

cacheSolve <- function(x, ...) {
  
  mat <- x$getinv()
  
  if(!is.null(mat)){
    message('getting cached inverse')
    return(mat)
  }
  
  data= x$get()
  inverse= solve(data)
  
  x$setinv(inverse)
  
        ## Return a matrix that is the inverse of 'x'
}
