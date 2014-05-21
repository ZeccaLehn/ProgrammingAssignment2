## Put comments here that give an overall description of what your
## functions do

#Functions Overview:

        #The makeCacheMatrix() function created, takes a square matrix as input object 'x' with n elemants--see running scirpt at base.
        #Through set and get methods, this 'special object' is cached by function with a callable environment in list form.
        #The base solve() function is applied to m object in setinverse() and called though getinverse() in proceeding steps. 
        #A matched list is stored for grouping functions together in this function environment.

        #The cachesolve() function created, takes the original 'x' square matrix stored in the special object.
        #Initially, it sets the first inverse matrix solution from getinvers() to 'm' within the function.
        #Next, it checks through the !NULL condition if a new matrix has been loaded, if a new load is not input, it will output the initial inverse matrix.
        #Finally, and assuming this is the first run of the input matrix 'x', the function will take in and return the inverse function of the input square matrix.

## Write a short comment describing this function  

        #This function creates an environment and stores the initial inverse matrix using solve().

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}

## Write a short comment describing this function

        #This function solves for new inputs; it returns the 'm' inverse matrix object.
        
cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'.
        m
}




##Associated code for computing dynamic inverse matrices.

#set.seed(1)                  #Set to off: Used for comparing against other package inverse functions.

start <- proc.time()          #Starts timer

x <- matrix(rnorm(25),5,5)    #The input matrix: Here a 5 X 5
x

matSpec <- makeCacheMatrix(x)  
matSpec                       #The 'special object' is created.

invMat <- cacheSolve(matSpec)                      
invMat                        #Runs the inverse matrix with the new input, and cached if matSpec() is not rerun after 'x' matrix is refreshed.

proofInv <- x %*% invMat
proofInv                      #1s in the diagonal of the square function, prove inverse matrix works!


proc.time() - start          #Ends timer (End - Start = Total)
