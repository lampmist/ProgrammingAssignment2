## a matrix constructor function and a function calculates the inverse matrix

## makeCacheMatrix() is a matrix constructor function.
## makeCacheMatrix() creates a list containing functions set() and get()
## a matrix, and set and get the inverse matrix by setInv() and getInv().

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y,nrow,ncol) {
    x<<-matrix(y,nrow,ncol)
    inv<<-NULL
  }
  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve() is a function solving for the inverse of a matrix.
## The object constructed by the makeCacheMatrix constructor, containing
## a matrix, is passed to cacheSolve, which checks to see if the inverse
## has already been calculated and return the inverse if so; otherwise
## it proceeds to calculate the inverse and return.  

cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {
    message("not getting cached data")
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv
}
