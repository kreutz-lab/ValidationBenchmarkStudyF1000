# Extended function to handle errors and warnings as a printed message
msb.tryCatch <- function(expr, errorExpr= NULL, infoToBePrinted="") {
  tryCatch(
    {
      # Attempt to evaluate the expression
      result <- eval(expr)
      return(result)
    },
    error = function(e) {
      # Convert the error message to a standard print message
      print(paste(infoToBePrinted, "An error occurred:", e$message))
      if(!is.null(errorExpr))
        return(eval(errorExpr))
      else
        return(NULL)  # Return NULL or any other appropriate value
    },
    warning = function(w) {
      # Convert the warning message to a standard print message
      print(paste(infoToBePrinted, "A warning occurred:", w$message))
      invokeRestart("muffleWarning")  # Optionally suppress the warning
    }
  )
}
