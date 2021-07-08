#' Estimate missing values using Ridge regression
#'
#' @param data A matrix or data frame object. It should have variables as columns and observations as rows.
#'
#' @return A list object with positions of missing values (positions), estimated values by multiple linear regression with Ridge penalization (est_values) and new data with estimated values (new_data).
#' @export
#'
#' @examples
estimate_ridge <- function(data, diff = 1e-5){

  #Apply Simple Multiple Linear Regression
  mlr_est = empire::estimate_mlr(data, diff=diff)
  data = mlr_est$new_data
  positions = mlr_est$positions

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

  #Apply Ridge
  ridge_est_values <- vector()
  g=1
  for (mv in seq_len(nrow(positions))){
    ridge_est_values[g] <- ridge.reg(mv)
    g=g+1
  }
  print("With Ridge Regularization")

  for(i in seq_len(nrow(positions))){
    data[positions[i,1], positions[i,2]] <- ridge_est_values[i]
  }
  print(ridge_est_values)
  # List
  my_list <- list("positions" = positions, "est_values" = ridge_est_values, "new_data" = data)

}
