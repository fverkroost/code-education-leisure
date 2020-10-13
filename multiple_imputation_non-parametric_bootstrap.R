# -----------------------------------------------------------------------
# Loading data and packages
# -----------------------------------------------------------------------

# Load libraries
library(MASS)
library(robustbase)
library(VIM)

# Set seed for reproducible results
set.seed(1234)

# -----------------------------------------------------------------------
# Functions for multiple imputation via iterative model-based imputation
# -----------------------------------------------------------------------

# Perform multiple imputation on a data set, returning a list of data sets
multiple_imputation <- function(df, M = round(length(which(!complete.cases(df)))/nrow(df)*100), comps = NULL,
                                robust = TRUE, count = NULL, mi = 1, init.method = "kNN") {
  datasets <- replicate(M, irmi(df, mixed = comps), simplify = FALSE)
  return(data_sets = datasets)
}

# Fit regression models for each a list of (imputed) data sets
fit_regression_models <- function(dflist, method = c("OLS", "MM"),
                                  dep_var, indep_vars, weights = NULL, na.action = NULL, 
                                  cntrl = lmrob.control(k.fast.s = 5, best.r.s = 5,
                                                        k.max = 5000, maxit.scale = 5000, k.m_s = 20)) {
  
  frmla <- as.formula(paste0(dep_var, " ~ ", paste0(indep_vars, collapse = " + ")))
  n <- length(dflist)
  int <- sample(1:n, 1)
  
  # For each data set, fit the model
  fitlist <- list()
  for (i in 1:n){
    if (method == "OLS"){
      fitlist[[i]] <- lm(formula = frmla, data = dflist[[i]])
    } else if (method == "MM"){
      fitlist[[i]] <- lmrob(formula = frmla, data = dflist[[i]], control = cntrl)
    }
  }
  
  return(fitList = fitlist)
}

# Pool point estimates and standard errors across the 
# regression results per (imputed) data sets
pool_estimates_errors <- function(fitlist) {
  
  n <- length(fitlist)
  k <- length(fitlist[[(sample(1:n, 1))]]$coefficients)
  
  # Create empty matrices to fill up with results
  coef_mat <- ses <- p_mat <- t_mat <- matrix(NA, nrow = n, ncol = k)
  
  # Compute coefficient estimates, standard errors, p-values and t-values
  for (i in 1:n){
    for (j in 1:k){
      coef_mat[i,j] <- unname((fitlist[[i]]$coefficients)[j])
      ses[i,j] <- unname((coef(summary(fitlist[[i]]))[, "Std. Error"])[j])
      p_mat[i,j] <- unname((coef(summary(fitlist[[i]]))[, "Pr(>|t|)"])[j])
      t_mat[i,j] <- unname((coef(summary(fitlist[[i]]))[, "t value"])[j])
    }
  }
  
  # Compute pooled variance from within and between variances
  var_between <- apply(coef_mat, 2, var)
  var_within <- apply(ses, 2, mean)
  t_stat <- apply(t_mat, 2, mean)
  var_pooled <- gamma <- v_obs <- v_m <- dof <- p_value <- rep(0, k)
  for (j in 1:k){
    var_pooled[j] <- var_within[j] + ((n+1)/n)*var_between[j]
    gamma[j] <- ((n+1)/n)*(var_between[j]/var_pooled[j])
    v_comp <- fitlist[[(sample(1:n, 1))]]$df.residual # the same for all regression fits
    v_obs[j] <- ((v_comp+1)/(v_comp+3))*v_comp*(1-gamma[j])
    v_m[j] <- (n-1)/(gamma[j]^2)
    dof[j] <- (v_m[j]*v_obs[j])/(v_m[j] + v_obs[j])
    p_value[j] <- 2*pt(t_stat[j], dof[j], lower=FALSE)
  }
  
  # Create data frame with results
  final <- cbind(colMeans(coef_mat), sqrt(var_pooled), t_stat, dof, p_value)
  colnames(final) <- c("Estimate", "Std. Error", "t value", "df", "Pr(>|t|)")
  rownames(final) <- names(fitlist[[(sample(1:n, 1))]]$coefficients)
  
  return(list(summary = final))
}

# Multiply impute data amd fit regressions to each imputed data set 
fit_pool <- function(data, regr_method = c("OLS","MM")){
  imp_datas <- multiple_imputation(data)
  fitted <- fit_regression_models(dflist = imp_datas, dep_var = colnames(data)[1],
                                  indep_vars = colnames(data)[-1], method = regr_method)
  return(fit=fitted)
}

# -----------------------------------------------------------------------
# Function for the bootstrap with kNN imputation and linear regression
# -----------------------------------------------------------------------

bootstrap_knn <- function(data, R = 300, k = 5, method = c("OLS", "MM"), dep_var, indep_vars,
                          cntrl = lmrob.control(k.fast.s = 5, best.r.s = 5, setting = "KS2014",
                                                k.max = 5000, maxit.scale = 5000, k.m_s = 20)) {
  
  coef_list <- list()
  frmla <- as.formula(paste0(dep_var, " ~ ", paste0(indep_vars, collapse = " + ")))
  miss <- names(which(sapply(data, function(x) sum(is.na(x))) != 0)) 
  imp <- kNN(data, variable = miss, imp_var = FALSE)
  
  bootstrap_imputation <- function(x){
    boot_sample <- x[sample(x = nrow(x), size = nrow(x), replace = TRUE), ]
    miss_var <- names(which(sapply(boot_sample, function(x) sum(is.na(x)))!=0)) 
    imp_data <- kNN(boot_sample, variable = miss_var, imp_var = FALSE)
    return(imp_data)
  }
  
  for (r in 1:R){
    boot_data <- bootstrap_imputation(data)
    if (method == "OLS"){
      fit <- lm(frmla, boot_data)
      coef_list[[r]] <- unname(fit$coefficients)}
    if (method == "MM"){ # Initialize "fit" in case the first lmrob fit will give an error
      if (r == 1){
        fit <- lm(frmla, boot_data)
      } else { # Ignore and do not consider the fits with errors and warnings
        tryCatch({print(r)
          fit <- lmrob(frmla, boot_data, control = cntrl)},
          error = function(e){cat("ERROR:", conditionMessage(e), "\n")},
          warning = function(w){cat("WARNING:", conditionMessage(w), "\n")})
      }
      coef_list[[r]] <- unname(fit$coefficients)
    }
  }
  
  # Make matrix with coefficient estimates
  coef <- unique(unlist(lapply(coef_list, length)))
  L <- length(unique(coef_list))
  vec <- which(duplicated(coef_list) == FALSE)
  coef_mat <- matrix(NA, nrow = L, ncol = coef)
  for (i in 1:L){
    coef_mat[i,] <- coef_list[[(vec[i])]]
  }
  colnames(coef_mat) <- names(fit$coefficients)
  
  # Compute the variance
  variance <- sqrt(apply(coef_mat, 2, var))

  # Calculate z- and p-value from asymptotic stabdard deviation
  asymp <- matrix(NA, nrow = R, ncol = coef)
  p_value <- rep(NA, coef)
  for (r in 1:R){
    for (i in 1:coef){
      asymp[r, i] <- asymp.test(coef_mat[, i], parameter = "var")$estimate
    }
  }
  
  # Create matrix with coefficients, standard deviation, z-value and p-value
  final <- cbind(colMeans(coef_mat), sqrt(colMeans(asymp)), colMeans(coef_mat) / colMeans(asymp), 2*pnorm(-abs(z_value)))
  colnames(final) <- c("Estimate", "Std. Error", "z value", "Pr(>|t|)")
  rownames(final) <- names(fit$coefficients)
  
  return(list(summary = final, replicates = coef_mat))
}

# -----------------------------------------------------------------------
# Simulation study -- Function to create contaminated data
# -----------------------------------------------------------------------

# Function to create outliers in data (dta) on basis of contamination level (cont.lvl) and type of outliers (type)
contamination <- function(dta, cont.lvl, n, type = c("vertical", "good_leverage", "bad_leverage")){
  contam <- sample(1:n,round(cont.lvl*n))
  contam.up <- contam[1:round(0.5*cont.lvl*n)]
  contam.down <- contam[-which(contam.up == contam)]
  if (type == "vertical"){
    dta[contam.up,] <- mvrnorm(length(contam.up), mu = c(0, rep(5, (ncol(dta)-1))), Sigma = 0.05*diag(ncol(dta)))
    dta[contam.down,] <- mvrnorm(length(contam.down), mu = c(0, rep(-5, (ncol(dta)-1))), Sigma = 0.05*diag(ncol(dta)))
  } else if (type == "good_leverage"){
    dta[contam.up,] <- mvrnorm(length(contam.up), mu = c(5, rep(6, (ncol(dta)-1))), Sigma = 0.05*diag(ncol(dta)))
    dta[contam.down,] <- mvrnorm(length(contam.down), mu = c(-5, rep(-6, (ncol(dta)-1))), Sigma = 0.05*diag(ncol(dta)))
  } else if (type == "bad_leverage"){
    dta[contam.up,] <- mvrnorm(length(contam.up), mu = c(-4, rep(4, (ncol(dta)-1))), Sigma = 0.05*diag(ncol(dta)))
    dta[contam.down,] <- mvrnorm(length(contam.down), mu = c(4, rep(-4, (ncol(dta)-1))), Sigma = 0.05*diag(ncol(dta)))
  } else {
    stop('Chosen contamination type is not valid!')
  }
  dta <- as.data.frame(dta)
  dta["z"] <- NA
  for (i in 1:nrow(dta)){ dta$z[i] = ifelse((i %in% contam.up) | (i %in% contam.down), 1, 2) }
  return(dta)
}

# Function to create missingness in data (dta) on the basis of proportion missingness (prop.m) and type of missingness (type)
missingness <- function(dta, prop.m = 0.2, type = c("MCAR", "MAR", "MNAR")){
  y <- dta[,1]
  x <- dta[,-c(1)]
  
  if (type == "MCAR"){
    size.y <- ceiling(prop.m*length(y))
    y.miss <- ifelse(y %in% sample(y, size = size.y, replace = FALSE), NA, y)
  } else if (type == "MAR"){
    sort.y <- sort(y, decreasing=TRUE)
    int <- runif(1, prop.m, 0.5)
    size.y <- ceiling(length(y)*int)
    sort.y <- dta[order(-dta[,1]),] 
    subset <- sort.y[1:size.y,]  
    sort.col <- sample((2:(ncol(dta))), 1)
    var <- subset[,(sort.col)]
    subset <- subset[order(-var),]
    sort.x <- subset[(1:(length(y)*prop.m)),1]
    y.miss <- ifelse(y %in% sort.x, NA, y)
  } else if (type == "MNAR"){
    sort.y <- sort(y, decreasing=TRUE)
    size.y <- ceiling(prop.m*length(y))
    bigy <- sort.y[size.y]
    y.miss <- ifelse(y >= bigy, NA, y)
  } else {
    stop('Chosen type is not valid!')
  }
  
  simdata <- cbind(y.miss, x)
  return(simdata)
}

# -----------------------------------------------------------------------
# Simulation study -- Create simulation data
# -----------------------------------------------------------------------

# Create random standard normally distributed correlated data
n <- 1000
simdata <- mvrnorm(n, mu = c(0, 0), Sigma = diag(2))
G <- matrix(c(1, 0.7, 0.7, 1), 2, 2) 
simdata_tr <- simdata %*% G 
plot(simdata_tr[, 1],simdata_tr[, 2])

# Create vertical outliers
contdata_vert <- contamination(simdata_tr, 0.2, n, type = "vertical")
plot(contdata_vert[, 1], contdata_vert[, 2], col = c("red", "black")[contdata_vert$z])

# Create good leverage points
contdata_good <- contamination(simdata_tr, 0.2, n, type = "good_leverage")
plot(contdata_good[, 1], contdata_good[, 2], col = c("red", "black")[contdata_good$z])

# Create bad leverage points
contdata_bad <- contamination(simdata_tr, 0.2, n, type = "bad_leverage")
plot(contdata_bad[, 1], contdata_bad[, 2], col = c("red", "black")[contdata_bad$z])

# Create missing data in y according to MCAR
simdata_mcar <- missingness(simdata_tr, type = "MCAR")
plot(simdata_mcar[, 1], simdata_mcar[, 2])

# Create missing data in y according to MNAR
simdata_mnar <- missingness(simdata_tr, type = "MNAR")
plot(simdata_mnar[, 1], simdata_mnar[, 2])

# Create missing data in y according to MAR
simdata_mar <- missingness(simdata_tr, type = "MAR")
plot(simdata_mar[, 1], simdata_mar[, 2])

# -------------------------------------------------------------------------------------------
# Simulation study -- Function for bivariate and multivariate regressions
# with outlier levels varying and missingness proportion constant (purpose = outliers) or
# with missingness proportions varying and outlier level constant (purpose = missingness)
# -------------------------------------------------------------------------------------------

sim_reg <- function(data, cont.level = c(0.05, 0.1, 0.2, 0.3, 0.5), n, purpose = c("outliers", "missingness"),
                    cntrl = lmrob.control(k.fast.s = 5, best.r.s = 5, setting = "KS2014",
                                           k.max = 5000, maxit.scale = 5000, k.m_s = 20)){
  
  data_sets <- list()
  
  # Create contaminated data with missingness and/or outliers
  for (i in 1:length(cont.level)){
    
    data_sets[[paste0("cont", cont.level[i])]] <- list()
    
    pm <- ifelse(purpose == "outliers", 0.2, 
                ifelse(purpose == "missingness", cont.level[i], NA))
    pc <- ifelse(purpose == "outliers", cont.level[i], 
                ifelse(purpose == "missingness", 0.2, NA))
    
    # Generate contaminated datasets
    contdata_vert <- contamination(data, pc, n, type = "vertical")
    contdata_vert <- contdata_vert[, names(contdata_vert) != "z"]
    contdata_good <- contamination(data, pc, n, type = "good_leverage")
    contdata_good <- contdata_good[, names(contdata_good) != "z"]
    contdata_bad <- contamination(data, pc, n, type = "bad_leverage")
    contdata_bad <- contdata_bad[, names(contdata_bad) != "z"]
    contdatas <- list(vertical = contdata_vert, good_leverage = contdata_good, bad_leverage = contdata_bad)
    
    # Generate MCAR, MAR and MNAR data with vertical outliers,
    # and good and bad leverage points
    for (u in 1:length(contdatas)){
      data_sets[[i]][[paste0(names(contdatas[u]), ".MCAR")]] <- missingness(contdatas[[u]], prop.m = pm, type = "MCAR")
      data_sets[[i]][[paste0(names(contdatas[u]), ".MAR")]] <- missingness(contdatas[[u]], prop.m = pm, type = "MAR")
      data_sets[[i]][[paste0(names(contdatas[u]), ".MNAR")]] <- missingness(contdatas[[u]], prop.m = pm, type = "MNAR")
    }
    
  }

  coef_list <- se_list <- list()
  listlength <- unique(unlist(lapply(data_sets, length)))
  
  # Run regressions and create output tables
  for (i in 1:length(cont.level)){
    
    for (j in 1:listlength){
      
      # Create model formula
      dep_var <- colnames(data_sets[[i]][[j]])[1]
      indep_vars <- colnames(data_sets[[i]][[j]])[-1]
      frmla <- reformulate(termlabels = indep_vars, response = dep_var)
      
      # Run regression models and pool results (in case of imputation)
      single_imp <- kNN(data_sets[[i]][[j]], imp_var = FALSE)
      single_OLS <- lm(data = single_imp, formula = frmla)
      single_MM <- lmrob(formula = frmla, data = single_imp, control = cntrl)
      mult_OLS <- pool_estimates_errors(fit_pool(data_sets[[i]][[j]], regr_method = "OLS"))
      mult_MM <- pool_estimates_errors(fit_pool(data_sets[[i]][[j]], regr_method = "MM"))
      
      cnams <- c("Single OLS Coef.", "Single OLS P-val", "Single MM Coef.", "Single MM P-val", 
               "Multiple OLS Coef.", "Multiple OLS P-val", "Multiple MM Coef.", "Multiple MM P-val")
      rnams <- paste0(names(data_sets[[i]][j]), ".", names(data_sets)[i], " - ", c("Intercept", indep_vars))
      
      # Create a matrix with the model coefficients
      ll <- length(single_OLS$coefficients)
      final <- data.frame(matrix(NA, nrow = ll, ncol = length(cnams)))
      colnames(final) <- cnams
      rownames(final) <- rnams
      final$`Single OLS Coef.` <- single_OLS$coefficients
      final$`Single MM Coef.` <- single_MM$coefficients
      final$`Multiple OLS Coef.` <- (mult_OLS$summary)[, 1]
      final$`Multiple MM Coef.` <- (mult_MM$summary)[, 1]
      final$`Single OLS P-val` <- summary(single_OLS)[[4]][,4]
      final$`Single MM P-val` <- coef(summary(single_MM))[, 4]
      final$`Multiple OLS P-val` <- mult_OLS$summary[,5]
      final$`Multiple MM P-val` <- mult_MM$summary[,5]
      
      # Assign result matrices to final results matrix
      if (i == 1 & j == 1){
        res <- final
      } else {
        res <- rbind(res, final)
      }
      
    }
  }
  
  return(res)
  
}

# -------------------------------------------------------------------------------------------
# Simulation study -- Bivariate simulations
# -------------------------------------------------------------------------------------------

# Obtain coefficients for outliers and missingness case (bivariate data)
sim_coefs_outl <- sim_reg(simdata_tr, c(0.05, 0.1, 0.2, 0.3, 0.5), n, purpose = "outliers")
sim_coefs_miss <- sim_reg(simdata_tr, c(0.05,0.1,0.2,0.3,0.5), n, purpose = "missingness")

# -------------------------------------------------------------------------------------------
# Simulation study -- Multivariate simulations
# -------------------------------------------------------------------------------------------

# Multivariate simulation with outlier levels varying and missingness proportion constant
multsim <- mvrnorm(n = 1000, mu = rep(0, 10), Sigma = matrix(.7, nrow = 10, ncol = 10) + diag(10)*.3)

# Multivariate simulation with outlier levels varying and missingness proportion constant
sim_coefs_outl_mult <- sim_reg(multsim, cont.level = c(0.05,0.1,0.2,0.3,0.5), n, purpose = "outliers")
sim_coefs_miss_mult <- sim_reg(multsim, cont.level = c(0.05,0.1,0.2,0.3,0.5), n, purpose = "missingness")

# -----------------------------------------------------------------------
# Simultation results with no contamination or missing data
# -----------------------------------------------------------------------

# Function to perform regressions on data with constant missingness, varying outliers (purpose = missingness), or
# to perform regressions on data with varying missingness, constant outliers (purpose = outliers)
perform_regressions = function(x, purpose = c("outliers", "missingness"), 
                               cntrl = lmrob.control(k.fast.s = 5, best.r.s = 5,
                                                     k.max = 5000, maxit.scale = 5000, k.m_s = 20)){
  
  if (purpose == "outliers"){
    
    # Construct formula and run regressions (single and multiple OLS and MM)
    frmla <- reformulate(termlabels = colnames(x)[-c(1)], response = colnames(x)[1])
    OLS <- lm(data=x, formula = frmla)
    MM <- lmrob(data = x, formula = frmla, control = cntrl)
    
    # Create a data frame with the regression results
    cnams <- c("OLS Coef.", "OLS P-val", "MM Coef.", "MM P-val")
    final <- data.frame(matrix(NA, nrow = ncol(x), ncol = length(cnams)))
    names(final) <- cnams
    final$`OLS Coef.` <- OLS$coefficients
    final$`OLS P-val` <- coef(summary(OLS))[,4]
    final$`MM Coef.` <- MM$coefficients
    final$`MM P-val` <- coef(summary(MM))[,4]

  } else if (purpose == "missingness"){
    
    # Construct formula and run regressions (single and multiple OLS and MM)
    frmla <- reformulate(termlabels = colnames(x)[-1], response = colnames(x)[1])
    single_imp <- kNN(x, imp_var = FALSE)
    single_OLS <- lm(data = single_imp, formula = frmla)
    single_MM <- lmrob(data = single_imp, control = cntrl, formula = frmla)
    mult_OLS <- pool_estimates_errors(fit_pool(x, regr_method = "OLS"))
    mult_MM <- pool_estimates_errors(fit_pool(x, regr_method = "MM"))
    
    # Create a data frame with the regression results
    cnams <- c("Single OLS Coef.", "Single OLS P-val", "Multiple OLS Coef.", "Multiple OLS P-val", 
               "Single MM Coef.", "Single MM P-val", "Multiple MM Coef.", "Multiple MM P-val")
    final <- data.frame(matrix(NA, nrow = ncol(x), ncol = length(cnams)))
    names(final) <- cnams
    final$`Single OLS Coef.` <- single_OLS$coefficients
    final$`Single OLS P-val` <- coef(summary(single_OLS))[,4]
    final$`Single MM Coef.` <- single_MM$coefficients
    final$`Single MM P-val` <- coef(summary(single_MM))[,4]
    final$`Multiple OLS Coef.` <- (mult_OLS$summary)[,1]
    final$`Multiple OLS P-val` <- mult_OLS$summary[,5]
    final$`Multiple MM Coef.` <- (mult_MM$summary)[,1]
    final$`Multiple MM P-val` <- mult_MM$summary[,5]
    
  } else {
    stop("Option 'purpose' should be either 'outliers' or 'missingness'!")
  }
  
  return(final)
}

# Create data with varying outliers and constant missingness
# and perform regressions on these data
df_mcar <- as.data.frame(missingness(multsim, prop.m = .2, type = "MCAR"))
df_mar <- as.data.frame(missingness(multsim, prop.m = .2, type = "MAR"))
df_mnar <- as.data.frame(missingness(multsim, prop.m = .2, type = "MNAR"))
names(df_mcar) <- c("y.mcar",  paste0("V", 2:10))
names(df_mar) <- c("y.mar",  paste0("V", 2:10))
names(df_mnar) <- c("y.mnar",  paste0("V", 2:10))
dfmlist <- list(df_mcar, df_mar, df_mnar)
lapply(dfmlist, perform_regressions, purpose = "missingness")

# Create data with constant outliers and varying missingness
# and perform regressions on these data
df_vert <- as.data.frame(contamination(multsim, .2, n, type = "vertical"))[, 1:10]
df_good <- as.data.frame(contamination(multsim, .2, n, type = "good_leverage"))[, 1:10]
df_bad <- as.data.frame(contamination(multsim, .2, n, type = "bad_leverage"))[, 1:10]
names(df_vert) <- c("y.mcar",  paste0("V", 2:10))
names(df_good) <- c("y.mar",  paste0("V", 2:10))
names(df_bad) <- c("y.mnar",  paste0("V", 2:10))
dfolist <- list(df_vert, df_good, df_bad)
lapply(dfolist, perform_regressions, purpose = "outliers")
