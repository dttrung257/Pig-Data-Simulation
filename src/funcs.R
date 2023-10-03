# If the directory does not exist, create a new directory
check_path <- function(path) {
  if (!dir_exists(path)) {
    dir_create(path)
  }
}

# Clear all directories and files in folder but don't delete folder
clear_data <- function(path) {
  if (dir_exists((path))) {
    items <- dir_ls(path)
    
    for (item in items) {
      if (is_dir(item)) {
        dir_delete(item, recursive = TRUE, force = TRUE)
      } else {
        file_delete(item)
      }
    }
  }
}

p <- NULL
pp <- NULL

nls.func.0 <- function(X0, ylast){
  pp <- c(X0, ylast)
  #calculation of a and b using these new parameters
  
  b <- target.coef.0(pp)[2]
  a <- target.coef.0(pp)[1]
  
  return (a+b*x)
}

target.coef.0 <- function(P){
  X0 <- P[1]
  ylast <- P[2]
  a <- (ylast*X0)/(X0 - ylast)
  b <- -(ylast)/(X0-xlast)
  
  
  pp <- as.vector(c(a, b))
  return(pp)
}

predict.target.trajectory.0 <- function(pr, age){
  #
  a <- pr[1]
  b <- pr[2]
  
  x <- age
  #calculation of a and b using these new parameters
  #
  results <- list()
  cfi <- a+b*x # cumulative feed intake
  dfi <- b     # daily feed intake
  results[[1]] <- cfi
  results[[2]] <- dfi
  return (results)
}

target.coef.1 <- function(P) {
  X0 <- P[1]
  y2 <- P[2]
  ylast <- P[3]
  
  a <- (xlast * ylast - 4 * y2 * xlast + X0 * ylast) * X0 / (xlast ^ 2 - 2 * X0 * xlast + X0 ^ 2)
  b <- -(xlast * ylast - 4 * y2 * xlast + 3 * X0 * ylast - 4 * y2 * X0)/((xlast - X0) ^ 2)
  c <- 2 * (-2 * y2 + ylast) / (xlast ^ 2 - 2 * X0 * xlast + X0 ^ 2)
  
  pp <- as.vector(c(a, b, c))
  return(pp)
}

nls.func.1 <- function(X0, y2, ylast){
  pp <- c(X0, y2, ylast)
  #calculation of a,b, c and d using these new parameters
  
  c <- target.coef.1(pp)[3]
  b <- target.coef.1(pp)[2]
  a <- target.coef.1(pp)[1]
  
  return (a+b*x+c*x^2)
}

predict.target.trajectory.1 <- function(pr,age){
  #
  a <- pr[1]
  b <- pr[2]
  c <- pr[3]
  
  x <- age
  #calculation of a,b, c and d using these new parameters
  #
  results <- list()
  cfi <- a+b*x+c*x^2 #returns the cubic function
  dfi <- b+2*c*x
  results[[1]] <- cfi
  results[[2]] <- dfi
  return (results)
}

target.coef.2 <- function(P) {
  X0 <- P[1]
  Xs <- P[2]
  DFIs <- P[3]
  CFIs <- P[4]
  
  a <- -X0 * (2 * CFIs * Xs - CFIs * X0 - Xs ^ 2 * DFIs + Xs * DFIs * X0) / (Xs ^ 2 - 2 * X0 * Xs + X0 ^ 2)
  b <- (-Xs ^ 2 * DFIs + DFIs * X0 ^ 2 + 2 * CFIs * Xs) / (Xs ^ 2 - 2 * X0 * Xs + X0 ^ 2)
  c <- -(CFIs - Xs * DFIs + X0 * DFIs) / (Xs ^ 2 - 2 * X0 * Xs + X0 ^ 2)
  
  pp <- as.vector(c(a, b, c))
  return(pp)
}

nls.func.2 <- function(X0, Xs, DFIs, CFIs) {
  pp <- c(X0, Xs, DFIs, CFIs)
  # print(paste0('X0: ', X0))
  # print(paste0('Xs: ', Xs))
  # print(paste0('DFIs: ', DFIs))
  # print(paste0('CFIs: ', CFIs))
  
  #calculation of a, b and c using these new parameters
  a <- target.coef.2(pp)[1]
  b <- target.coef.2(pp)[2]
  c <- target.coef.2(pp)[3]
  
  ind1 <- as.numeric(x < Xs)
  return (ind1 * (a + b * x + c * x ^ 2) + (1 - ind1) * ((a + b * Xs + c * Xs ^ 2)+(b + 2 * c * Xs) * (x - Xs)))
}

predict.target.trajectory.2 <- function(par, age){
  #
  X0 <- par[1]
  Xs <- par[2]
  DFIs <- par[3]
  CFIs <- par[4]
  #
  x <- age
  #calculation of a, b and c using these new parameters
  func.coef <- target.coef.2(par)
  c <- func.coef[3]
  b <- func.coef[2]
  a <- func.coef[1]
  
  #
  brk <- as.numeric(x < Xs)
  #
  result <- list()
  cfi <- brk * (a + b * x + c * x ^ 2) + (1 - brk) * ((a + b * Xs + c * Xs ^ 2) + (b + 2 * c * Xs) * (x - Xs))  #CFI
  dfi <- brk * (b + 2 * c * x) + (1 - brk) * (b+ 2 * c * Xs)                                 #DFI
  result[[1]] <- cfi
  result[[2]] <- dfi
  return (result)
}