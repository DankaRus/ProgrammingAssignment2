makeCacheMatrix <-function(x=matrix(numeric(),nrow=0,ncol=0)){   
  inv <- NULL
  oldx <- matrix(numeric(),nrow=0,ncol=0)            #Matrix needed to compare with previous
  compare <- function(){                            #Function to check if matrix have been changed or not
    flag <<- is.matrix(x) && is.matrix(oldx) && dim(x) == dim(oldx) && all(x == oldx) #Flag is checking dimensions and values of matricies
    oldx<<-x                                                                          #TRUE if matricies are equal, FALSE if not
    flag
    }
  set <- function(y){                         #set matrix
    oldx <<- x 
    x <<- y
    inv <<- NULL
  }
  get <- function() x                          #get matrix
  setinv <- function(solve) inv <<- solve      #solve() function to inverse matrix
  getinv <- function() inv                    #get inverse matrix
  list(set=set, get=get,                      #list of functions
       setinv=setinv,
       getinv=getinv,compare=compare)
}

cacheSolve<-function(x,...){                #Function calculates inverse matrix if there is no inverse matrix in cache
  inv <- x$getinv()                         
  bulka<-x$compare()                         #Getting Flag value
  if(!is.null(inv) && bulka)                 #Checking if matrix has not been changed and is there is inverse matrix in cache
  {
    message("getting cached data")
    return(inv)                             #getting inverse matrix from cache
  }
  data <- x$get()                           #if FALSE - inverse matrix
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
