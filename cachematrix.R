
makeCacheMatrix <- function(x=matrix()) { 
         inv <- NULL ## initializing inv as NULL;will hold value of matrix inverse
         set <-function(y){ 
               x <<- y  ## value of matrix in parent environment
               inv <<- NULL ## if there is a new matrix,reset  inv to NULL
             }
             get <- function() {x} ## define the get function-returns value of matrix argument
             setInverse <-function(inverse) {inv<<- inverse} ## assigns value of inv in parent environment
             getInverse  <-function() (inv) ## gets the value of inv where called
             list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

cacheSolve <- function(x, ...){
## return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
          if(!is.null(inv)){
                     message("getting cached data")
                     return(inv)
          }
          mat <- x$get()
          inv <- solve(mat,...)
          x$setInverse(inv)
          inv
}
