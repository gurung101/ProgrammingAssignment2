## There are two functions makeCacheMatrix, makeCacheMatrix
## makeCacheMatrix consists of set, get, setinv, getinv

## library(MASS) is used to calculate inverse for non squared as well as squared matrices
library(MASS)


makecacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){      #initializing inverse as NULL
                  x<<-y
                  inv<<-NULL
  } 
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%%x     #function to obtain inverse of the matrix
  }
  list(set=set, get=get,
       setinv=setinv,
       getinv= getinv)

}



## This is used to get the cache data

cacheSolve <- function(x,...) ##gets cache data
  {
  inv<-x$get()
  if(!is.null(inv)){      #checking whether inverse is NULL
                    message('getting cached data!')
    return(inv)
    #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv     ## Return a matrix that is the inverse of 'x'
}

