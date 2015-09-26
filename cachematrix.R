#First, create an invertible matrix in the global environment.
#
#This first function creates an invertible matrix that will be called by the second function
#for inversion. We are really setting up a database in which we will place the matrix to be
#inverted.

makeCacheMatrix <- function(x = matrix()) {
        #create an object (m) which is NULL
        m <- NULL
        #create an object (set) which is a function of y; set y to the matrix (x) and clear
        #the object (m) to be NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #create the object (get) as a function with no arguments and return (x)
        get <- function() x
        #create the object (setinv) as a function of inv and return (m) as inv
        setinv <- function(inv) m <<- inv
        #creat the object (getinv) as a function with no arguments and return (m)
        getinv <- function() m
        #create a list from the database 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#In the second function, we call the matrix from above from the cache and cause it to be
#inverted.

cacheSolve <- function(x) {
        #create the object (m) calling from matrix (x) the data contained in $getinv
        m <- x$getinv()
        #if (m) is not NULL (from above) issue a message and return (m)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if (m) is NULL, create the object (data) from matrix (x) using the data contained
        #in $get
        data <- x$get()
        #create object (m) from the solve function on the object (data); this inverts the
        #matrix (data) and assigns the inverted matrix to (m)
        m <- solve(data)
        #call from the matrix (x) the data in $setinv
        x$setinv(m)
        #return the inverted matrix (m)
        m
}

#assign makeCacheMatrix('matrix') to some 'variable'
#run cacheSolve('variable')