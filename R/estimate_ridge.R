#' Estimate missing values using Ridge regression
#'
#' @param data A matrix or data frame object. It should have variables as columns and observations as rows.
#'
#' @return A list object with positions of missing values (positions), estimated values by multiple linear regression with Ridge penalization (est_values) and new data with estimated values (new_data).
#' @export
#'
#' @examples
estimate_ridge <- function(data){

  positions <- pos_miss(data)

  ridge.reg <- function(v){
    complete_data <- data[stats::complete.cases(data),]
    Y = as.matrix(complete_data[positions[v,2]])
    if (length(data[positions[v,1],][is.na(data[positions[v,1],])])!=1){
      X = as.matrix(cbind(c(rep(1, nrow(Y))), complete_data[-c(positions[v,2], c(which(is.na(data[positions[v,1],]))))]))
    }
    else {
      X = as.matrix(cbind(c(rep(1, nrow(Y))), complete_data[-(positions[v,2])]))
    }
    XtX <- t(X)%*%X

    #Ridge penalization
    cv.out=glmnet::cv.glmnet(X,Y,alpha=0)
    bestlam=cv.out$lambda.min
    XtX.1 <- solve(XtX)
    B <- solve(XtX + bestlam*diag(1,nrow(XtX)))%*%t(X)%*%Y #Calculate coefficients with Ridge Lambda penalization

    regresion <- function(b1,b2){   # Regression function
      B[b1]*data[positions[v,1],b2]
    }


    e1 <- B[1] # Value obtained with the regression
    b=2
    for (j in seq_len(ncol(data))){ # Omit the value of the target column, replacing in all columns except where the missing value lies
      if (all(j != c(positions[v,2],c(which(is.na(data[positions[v,1],])))))){ # Check all False
        e1 = e1 + regresion(b,j)
        b=b+1
      }
    }


    # Return e1
    return(e1)

  }

  # Iterate the estimators
  repeat{
    # old estimated values
    old_est_values = vector()
    g = 1
    # Run ridge.reg in each missing value
    for (mv in seq_len(nrow(positions))){
      old_est_values[g] <- ridge.reg(mv)
      g=g+1
    }
    print(old_est_values)

    for(i in seq_len(nrow(positions))){
      data[positions[i,1], positions[i,2]] <- old_est_values[i]
    }


    # new estimated values
    new_est_values = vector()
    g=1

    for (mv in seq_len(nrow(positions))){
      new_est_values[g] <- ridge.reg(mv)
      g=g+1
    }

    print(new_est_values)
    # Catch error, na's in estimation
    tryCatch(if(any(is.na(new_est_values))) stop("na's in estimation"))

    for(i in seq_len(nrow(positions))){
      data[positions[i,1], positions[i,2]] <- new_est_values[i]
    }

    # Exit test
    conv <- old_est_values - new_est_values
    print(conv)
    ifelse(all((lapply(conv, abs) < 1e-5)==TRUE), break, next)



  }

  # List
  my_list <- list("positions" = positions, "est_values" = new_est_values, "new_data" = data)

}
