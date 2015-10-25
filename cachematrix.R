## R Programming (Coursera Course, John Hopkins University)
## Programming Assigment 2: caching potentially time-consuming computations
## Santiago Moral Rubio (Octubre 2015)

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## Ii is really a list containing a function to:
##      (1) set the value of the matrix
##      (2) get the value of the matrix
##      (3) set the value of the inverse
##      (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
        setmatrix<- function(y){
                x<<-y
                m<-NULL
                }
        getmatrix<- function() x
        setinversematrix<- function(inverse) m<<-inverse
        getinversematrix<- function() m
        list(setmatrix=setmatrix, getmatrix=getmatrix, setinversematrix=setinversematrix, 
             getinversematrix=getinversematrix)

}



## Function cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 m<-x$getinversematrix()
        if (!is.null(m)){
                message("getting cached matrix")
                return(m)
                }
        inmatrix<- x$getmatrix()
        m<-solve(inmatrix, ...)
        x$setinversematrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
