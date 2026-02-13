#' Estimate missing values using Ridge regression
#'
#' @param data A matrix or data frame object. It should have variables as columns and observations as rows.
#' @param diff A number. It is the minimum difference between the estimated value and the value of the immediately before iteration. It works as an exit test for the iterations, smaller its value, the estimation will be more accurate, however it will take a longer time to be estimated. \code{diff} is by default 1 e-05.
#' @param ridge_alpha It is a specific alpha value applied in the cross validation of Ridge regression. It could give a different penalization and therefore a different estimated value. alpha = 0 by default.
#' @return A list object with positions of missing values (positions), estimated values by multiple linear regression with Ridge penalization (est_values) and new data with estimated values (new_data).
#' @export
#'
#' @examples
estimate_ridge <- function(data, diff = 1e-5, ridge_alpha = 0){
  positions <- empire::pos_miss(data)

  ridge.reg <- function(v){
    complete_data <- data[stats::complete.cases(data),]
    Y = as.matrix(complete_data[positions[v,2]])
    if (length(data[positions[v,1],][is.na(data[positions[v,1],])])!=1){
      X = as.matrix(cbind(c(rep(1, nrow(Y))), complete_data[-c(positions[v,2], c(which(is.na(data[positions[v,1],]))))]))
    } else {
      X = as.matrix(cbind(c(rep(1, nrow(Y))), complete_data[-(positions[v,2])]))
    }
    XtX <- t(X)%*%X

    #Ridge penalization
    cv.out=glmnet::cv.glmnet(X,Y,alpha=ridge_alpha)
    bestlam=cv.out$lambda.min
    B <- solve(XtX + bestlam*diag(1,nrow(XtX)))%*%t(X)%*%Y #Calculate coefficients with Ridge Lambda penalization

    regresion <- function(b1,b2){   # Regression function
      B[b1]*data[positions[v,1],b2]
    }

    estimated_value = B[1]
    b=2
    for (j in seq_len(ncol(data))){ # Omit the value of the target column, replacing in all columns except where the missing value lies
      if (all(j != c(positions[v,2],c(which(is.na(data[positions[v,1],])))))){ # Check all False
        estimated_value = estimated_value + regresion(b,j)
        b=b+1
      }
    }


    # Return estimated value
    return(estimated_value)

  }

  #Apply Ridge
  ridge_est_values <- vector()
  for (mv in seq_len(nrow(positions))){
    ridge_est_values[mv] <- ridge.reg(mv)
  }

  for(i in seq_len(nrow(positions))){
    data[positions[i,1], positions[i,2]] <- ridge_est_values[i]
  }

  estimate.mv <- function(v){
    complete_data <- data[stats::complete.cases(data),]
    Y = as.matrix(complete_data[positions[v,2]])
    if (length(data[positions[v,1],][is.na(data[positions[v,1],])])!=1){
      X = as.matrix(cbind(c(rep(1, nrow(Y))), complete_data[-c(positions[v,2], c(which(is.na(data[positions[v,1],]))))]))
    }
    else {
      X = as.matrix(cbind(c(rep(1, nrow(Y))), complete_data[-(positions[v,2])]))
    }
    XtX <- t(X)%*%X
    XtX.1 <- solve(XtX)
    B <- XtX.1%*%t(X)%*%Y # Calculate coefficients

    regresion <- function(b1,b2){   # Regression function
      B[b1]*data[positions[v,1],b2]
    }


    estimated_value <- B[1]
    b=2
    for (j in seq_len(ncol(data))){ # Omit the value of the target column, replacing in all columns except where the missing value lies
      if (all(j != c(positions[v,2],c(which(is.na(data[positions[v,1],])))))){ # Check all False
        estimated_value = estimated_value + regresion(b,j)
        b=b+1
      }
    }

    # Return estimated value
    return(estimated_value)

  }

  # Iterate the estimators
  repeat{
    # old estimated values
    old_est_values = vector()

    # Run estimate.mv() in each missing value
    for (mv in seq_len(nrow(positions))){
      old_est_values[mv] <- estimate.mv(mv)
    }

    for(i in seq_len(nrow(positions))){
      data[positions[i,1], positions[i,2]] <- old_est_values[i]
    }


    # new estimated values
    new_est_values = vector()

    for (mv in seq_len(nrow(positions))){
      new_est_values[mv] <- estimate.mv(mv)
    }

    # Catch error, na's in estimation
    tryCatch(if(any(is.na(new_est_values))) stop("na's in estimation"))

    for(i in seq_len(nrow(positions))){
      data[positions[i,1], positions[i,2]] <- new_est_values[i]
    }

    # Exit test
    conv <- as.numeric(old_est_values) - as.numeric(new_est_values)
    ifelse(all((lapply(conv, abs) < diff)==TRUE), break, next)



  }

  # List
  my_list <- list("positions" = positions, "est_values" = new_est_values, "new_data" = data)

}

