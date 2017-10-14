# >>>>>>>> SECTION FOUR - INPUT SANITISATION
#
#is.valid.input() - function triages input, returns acceptable
#values as integers, and otherwise rejects input by stopping 
#execution and throwing an error.
#
#Allows: positive integers, doubles (decimals are discarded
#   and leftover integer is used), zero.
#Rejects: negative numbers of any type, complex numbers,
#   any input with letters/symbols eg: "23fhd&jks", 
#   characters with only numbers in them eg: "2", "3.14".
#
#Note: Generating zero variates of a particular distribution or 
#   using a distribution with zero degrees of freedom is
#   a nonsensical request. However, rnorm can make use of mean=0 or 
#   sd=0. Thus, input=zero is not excluded in this function, but will
#   be accounted for in is.nonzero().
#
is.valid.input <- function(argument){ 
  #expects single argument
  if (class(argument) == "integer"){
    return(argument) 
    #case where argument has already been tested once in 
    #caller function
  }
  #test is positive, number
  if (class(argument)!='numeric' || argument < 0) {
    #Note: || -> R stops evaluating if left hand side is true.
    #This avoids an invalid comparison error, for eg: 2+3i <= 0
    stop('invalid arguments')
  } else {
    return(as.integer(argument)) #integer class
  }
} #pass to my.rnorm(), my.chisq(), my.rt()
#
#is.nonzero() - Function rejects numeric input when input 
#is equal to zero, by stopping execution and throwing an error.
#
#Used as follow-up to is.valid.input(). Accounts for cases where
#an input has already been screened, but should also be non-zero.
#
is.nonzero <- function(argument){
  #expects single numeric argument
  if (argument == 0){
    stop('invalid arguments')
  } else {
    return(TRUE) #boolean
  }
} #pass to my.rnorm(), my.chisq(), my.rt()
#
#is.valid.nonzero() - function returns integer when input passes tests,
#else throws error and stops all execution.
#
is.valid.nonzero <- function(n){
  temp.n <- is.valid.input(n)
  if (is.nonzero(temp.n)){
    return(temp.n) #integer
  } #is.nonzero() throws error if not valid
}
temp2 <- function(variable){
  try(is.valid.input(variable), FALSE)
  try(is.nonzero(variable), FALSE)
  return(result)
}




temp <- function(variable){
  tryCatch(
    {
      if (is.valid.input(variable) & is.nonzero(variable)){
        readLines(as.integer(variable))
      }
    }, error=function(e) {
      message("error: invalid arguments")
      # Choose a return value in case of error
      return(NA)
    }, warning=function(w) {
      message("warning: invalid arguments")
      # Choose a return value in case of warning
      return(NULL)
    }, finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message("end script")
    }
  )
}