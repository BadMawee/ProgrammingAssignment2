## These functions are used to avoid costly computation of matrix inversion

## This function is where the matrix & inverse of the matrix will be stored.
## This also defined the retrieving function for both the matrix & its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<- function(inverse) m<<-inverse
  getInverse<-function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function solves the inverse of the matrix whenever the inverse of the 
## matrix cached. Once the inverse is cached, this will just call the cache
## value instead of recomputing the inverse.

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
