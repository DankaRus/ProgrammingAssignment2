makeCacheMatrix <-function(x=matrix(numeric(),nrow=0,ncol=0)){
  inv <- NULL
  oldx <- matrix(numeric(),nrow=0,ncol=0)
  compare <- function(){
    flag <<- is.matrix(x) && is.matrix(oldx) && dim(x) == dim(oldx) && all(x == oldx) 
    oldx<<-x
    flag
    }
  set <- function(y){
    oldx <<- x 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, 
       setinv=setinv,
       getinv=getinv,compare=compare)
}

cacheSolve<-function(x,...){
  inv <- x$getinv()
  bulka<-x$compare()
  if(!is.null(inv) && bulka)
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}