

GenerateId <- function(currentSeed, type) {
  # Generate an unique id for variables in the model to be referenced by if need be
  # Inputs: 
  #   @currentSeed - value of number of seed being created. This increments to make unique var
  #   @type - where "variable" or "parameter" or "eqn" or "diffeq"
  # Outputs: 
  #   @list: seed - incremented seed Values, id - unique id of variable
  

  #format the seed to a nice format and convert it to a string
  if (currentSeed < 10) {
    seed <- paste0("000", as.character(currentSeed))
  } else if (currentSeed < 100) {
    seed <- paste0("00", as.character(currentSeed))
  } else if (currentSeed < 1000) {
    seed <- paste0("0", as.character(currentSeed))
  } else {
    seed <- as.character(currentSeed)
  }
  
  #create unique id
  id <- paste0(type, seed)
  
  #increment seed
  currentSeed <- currentSeed + 1
  
  # package output
  out <- list(currentSeed, id)
  names(out) <- c("seed", "id")
  return(out)
}

GenerateIdsForOldModel <- function(variables, parameters, equations, diffeqs) {
  seed <- 1
  #create ids for variables
  var.id.df <- data.frame(matrix(ncol = 2, nrow = 0))
  names(var.id.df) <- c("id", "idName")
  i = 1
  for (var in variables) {
    #generate id
    data <- GenerateId(seed, "variable")
    seed <- data$seed
    id <- data$id
    row <- c(id, var)
    var.id.df[i, ] <- row
    i = i + 1
  }
  
  par.id.df <- data.frame(matrix(ncol = 2, nrow = 0))
  names(par.id.df) <- c("id", "idName")
  i = 1
  for (par in parameters) {
    #generate id
    data <- GenerateId(seed, "parameter")
    seed <- data$seed
    id <- data$id
    row <- c(id, par)
    par.id.df[i, ] <- row
    i = i + 1
  }
  
  eqn.id.df <- data.frame(matrix(ncol = 2, nrow = 0))
  names(eqn.id.df) <- c("id", "idName")
  i = 1
  for (eqn in equations) {
    #generate id
    data <- GenerateId(seed, "eqn")
    seed <- data$seed
    id <- data$id
    row <- c(id, eqn)
    eqn.id.df[i, ] <- row
    i = i + 1
  }
  
  dif.id.df <- data.frame(matrix(ncol = 2, nrow = 0))
  names(dif.id.df) <- c("id", "idName")
  i = 1
  for (dif in diffeqs) {
    #generate id
    data <- GenerateId(seed, "diffeq")
    seed <- data$seed
    id <- data$id
    row <- c(id, dif)
    dif.id.df[i, ] <- row
    i = i + 1
  }
  
  # package output
  out <- list(var.id.df, par.id.df, eqn.id.df, dif.id.df, seed)
  names(out) <- c("var", "par", "eqn", "dif", "seed")
  return(out)
}
 



