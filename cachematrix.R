## Cashing an inverse of a matrix
## to save computing power and speed up computation

## set the value of a matrix
## get the value of a matrix
##get the value of the inverse 


makeCacheMatrix <- function(x = matrix()) {
                    inv<-NULL #NULL to call upon object exists and nothing is there at the beginning
                    f<-NULL #make sure the function f later on is calling upon an empty value
                  
                          set<-function(f){ #defining new function set
                                x<<-f #where x is matrix so is f
  }
                          get<-function() x #get is a bridge between makeCacheMatrix and cacheSolve
                          setinv<-function(solve) inv<<-solve #setting a function solve 
                          getinv<-function() inv #pass on a solve function not a matrix
                           list(set=set, get=get, # output for makeCacheMatrix get is a matrix and 
                                                  # setinv and getinv are functions
                            setinv=setinv,
                            getinv=getinv)

}

cacheSolve <- function(x=matrix(), ...) { #passing on a function
        inv<-x$getinv() #calling upon getinv from a previous function (SOLVE) 
      
       if(!is.null(inv)){ 
          if(x$getinv()==x$getinv()) #check if matrix has changed
          message("getting cached data") #if it hasnt changed call upon computed inverse
          return(inv) #return a value of inverse
          
        }
       #if the matrix changed then calculate the inverse
        y<-x$get() #get the matrix
       inv<-solve(y,...) #solve it and store the value in inv
        x$setinv(inv) #x is a previous function and whereas setinv retreives the value
        return(inv)
        ## Return a matrix that is the inverse of 'x'
}
