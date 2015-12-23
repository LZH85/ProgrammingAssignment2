## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## user_mat obj stores the user input matrix. 
## cal_mat obj stores the matrix used to calculate the inv_mat. This is used as comparison to detech if user_mat has changed.
## inv_mat obj stores the calculated inv mat from cal_mat (output).

makeCacheMatrix <- function(user_mat = matrix()) {
        
        inv_mat <- NULL 
        cal_mat <- NULL
        
        
        ## stores user input matrix
        set_user_mat <- function (y) user_mat <<- y
        
        ## display cached matrix
        get_user_mat <- function() user_mat 
        
        ## stores calculated inv of matrix
        set_inv_mat <- function(y) inv_mat <<- y 
        
        ## display cached inv matrix
        get_inv_mat <- function() inv_mat 
        
        ## stores cal_mat 
        set_cal_mat <- function(y) cal_mat <<- y 
        
        ## display cal_mat
        get_cal_mat <- function() cal_mat
        
        list(set_user_mat = set_user_mat,
             get_user_mat = get_user_mat,
             set_cal_mat = set_cal_mat,
             get_cal_mat = get_cal_mat,
             set_inv_mat = set_inv_mat,
             get_inv_mat = get_inv_mat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        
        ## check if a user has input any matrix (assume all input matrix has an inverse)
        new_mat <- x$get_user_mat()
        if (is.null(new_mat)) {
                print (new_mat)
                stop("No input matrix found.")
        }
        
        ## load the previous inv_mat, if any
        old_inv_mat <- x$get_inv_mat()
        
        ## load the previous cat_mat, if any
        old_mat <- x$get_cal_mat()
        
        ## if there is a previous inv_mat and input matrix has not changed, print cached inv_mat
        if (identical(new_mat, old_mat) & (!is.null(old_inv_mat))) {
                print ("Matrix has not changed. Printing cached inverse matrix:")
                return (old_inv_mat)
                
        } else { ## no previous inv_mat calculated, need to calculate and store inv_mat and cal_mat
                new_inv <- solve (new_mat)
                x$set_inv_mat (new_inv)
                x$set_cal_mat (new_mat)
                print ("The calculated inverse matrix is")
                new_inv
        }

}
