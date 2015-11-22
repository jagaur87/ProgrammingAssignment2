## makeCacheMatrix takes a matrix as an input and returns a list of functions
## 1. setmat--sets the value of global variable x to the value of matrix input
## 2. getmat--returns matrix x
## 3. setinv--Sets the global variable invmat to the value of invmatrix
## 4. getinv--Returns the Global variable invmat 

makeCacheMatrix <- function(x = matrix()) {#defining function with attributes x as matrix
        invmat<-NULL                  #Initializing invmat as local variable to NULL
        setmat<-function(y=matrix()){ #defining setmat function to set the matrix
                x<<-y                 #x is initialized as Global variable to y
                invmat<<-NULL         #invmat is initialized as Global variable to NULL
        }
        getmat<-function()x           #Returns matrix x which is input to makeCacheMatrix
        setinv<-function(invmatrix)invmat<<-invmatrix # Sets Global variable invmat to invmatrix variable
        getinv<-function()invmat      #Returns the Global variable invmat 
        list(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv)#Returns the list of functions
}

## The cacheSolve function returns the inverse of the matrix which is input to the function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat<-x$getinv()                    #Initialising local variable invmat to the variable in makeCacheMatrix function 
        if(!is.null(invmat)){                 #If invmat is not NULL condition will return value of invmat
                message("getting cached data")
                return(invmat)
        }
        mat<-x$getmat()                       #Getting matrix from makeCacheMatrix
        invmat<-solve(mat,...)                #Inversing of matrix using Solve function
        x$setinv(invmat)                      #Setting the inverse matrix in global variable invmat
        invmat                                #Returns the value of inverse of matrix
}
