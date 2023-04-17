## This function makes and stores the matrix into the R environment

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)==nrow(x) && det(x)!=0) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setinverse <- function() m <<- inv(x)
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
  }else{
    return(message("The matrix is'n invertible."))
  }
}

## This function takes the stored matrix from the previous function as inverses
## it

cacheSolve <- function(x, ...) {
  m<-x$getinverse
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get
  m <- inv(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
