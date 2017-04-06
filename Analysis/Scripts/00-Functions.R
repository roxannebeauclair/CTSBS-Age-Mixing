# =================
# Importing dataset
# =================

# Author: Roxanne Beauclair
# Description: This scripts creates all of the functions that will be used in the 
# CTSBS age mixing analysis

# ====================
# Loading dependencies
# ====================
InstallLoad <- function(package1, ...) {
  # This function will load, and install if necessary, libraries needed for script
  
  # Convert arguments to vector
  pkgs <- c(package1, ...)
  
  # Start loop to determine if each package is installed
  for(p in pkgs) {
    
    # If package is installed locally, load
    if(p %in% rownames(installed.packages()))
      do.call("library", list(p))
    
    # If package is not installed locally, download, then load
    else {
      install.packages(p)
      do.call("library", list(p))
    }
  }
}

# ====================
# Function definitions
# ====================

# Remove libraries
RemoveLibraries <- function(package1, ...) {
  # This function will remove all libraries that have been loaded
  # Packages must be entered as character strings
  
  # Convert argument into a vector
  pkgs <- c(package1, ...)
  
  #Start loop
  for(p in pkgs) {
    item <- paste("package", p, sep = ":")
    
    #Check to see that package is loaded before detaching
    while(item %in% search()) {
      detach(item, character.only = TRUE)
    }
  }
}

# Missing Table
MissingTable <- function (x, y) {
  # This function creates a ?? x 2 table, with missing values
  # Returns the table
  # y must be binary
  
  count <- table(x, y, useNA = "ifany")
  freq <- round(prop.table(count, 2)*100, 1)
  tbl <- cbind(count[,1], freq[,1], count[,2], freq[,2])
  cnames <- character(0)
  for (i in 1:ncol(count)) {
    cnames <- c(cnames, colnames(count)[i], "%")
  }
  colnames(tbl) <- cnames
  tbl
}

# Get T1 Stat
GetT1Stat <- function(var, byvar, continuous_fn = describeMedian, prop_fn = describeFactors){
  # This function gets description stats for building the htmlTable
  # You use this function when you have a 'by' variable
  require(Gmisc)
  
  getDescriptionStatsBy(var, 
                        byvar, 
                        html = TRUE,
                        continuous_fn = continuous_fn,
                        prop_fn = prop_fn,
                        header_count = TRUE,
                        digits = 1)
}

# Rounding vectors in dataframe
RoundDf <- function(df, digits) {
  # This function rounds all the numeric vectors in a dataframe to the specified
  # number of digits. This is useful when creating tables for RMarkdown
  
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[, nums] <- round(df[, nums], digits = digits)
  
  (df)
}

#To save the legend of a plot as an external graphical element
get_legend<-function(myggplot){
  require(ggplot2)
  
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}

# Convert clmm() model components to a tidy dataframe
TidyCLMM <- function(m) {
  
  # This function takes the model output from the clmm() funciton and
  # outputs it as a tidy dataframe, which can then be used in ggplot
  # or dwplot() from the dotwhisker package.
  # The tidy dataframe has 'term', 'or', 'lb' and 'ub'
  # It also calculates profile confidence intervals, which
  # are supposed to be more accurate with prop odds
  
  require(dplyr)
  
  thresh <- length(m$y.levels)
  estimate <- exp(coef(m))
  lb <- exp(confint(m)[, 1])
  ub <- exp(confint(m)[, 2])
  
  ordf <- cbind(estimate, lb, ub) %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    filter(between(row_number(), thresh, n()))
  
  return(ordf)
  
}


# Convert coxph() model components to a tidy dataframe
TidyCoxph <- function(m) {
  
  # This function takes the model output from the coxph() funciton and
  # outputs it as a tidy dataframe, which can then be used in ggplot
  # or dwplot() from the dotwhisker package.
  # The tidy dataframe has 'term', 'or', 'lb' and 'ub'
  
  require(dplyr)
  
  estimate <- exp(coef(m))
  lb <- exp(confint(m)[, 1])
  ub <- exp(confint(m)[, 2])
  
  ordf <- cbind(estimate, lb, ub) %>%
    data.frame() %>%
    rownames_to_column("term") 
  
  return(ordf)
  
}

# Creating a linear mixed model with random intercept for person
# This is specifically for age-mixing patterns

ampmodel <- function(df) {
  lme(agep ~ agemean, 
      data = df, 
      random = ~1 | id,
      method = "REML")
}

# Extracts the between subject SD from agemixing pattern model
# Also upper and lower CI limits

bvar <- function(model) {
  
  # Must take an nlme model object
  # Outputs a dataframe 
  
  bsd <- VarCorr(model)[1, 2] %>%
    as.numeric() %>%
    round(2)
  
  lwrbsd <- intervals(model)$reStruct$id$lower  %>%
    round(2)
  
  uprbsd <- intervals(model)$reStruct$id$upper  %>%
    round(2)
  
  data.frame(bsd = bsd, lwrbsd = lwrbsd, uprbsd = uprbsd)
}

# Extracts the within subject SD from agemixing pattern model
# Also upper and lower CI limits

wvar <- function(model) {
  
  # Must take an nlme model object
  # Outputs a dataframe
  
  wsd <- VarCorr(model)[2, 2] %>%
    as.numeric() %>%
    round(2)
  
  lwrwsd <- (intervals(model)$sigma[1])  %>%
    round(2)
  
  uprwsd <- intervals(model)$sigma[3]  %>%
    round(2)
  
  data.frame(wsd = wsd, lwrwsd = lwrwsd, uprwsd = uprwsd)
}


# ===========================================================
# Functions developed by Mike Li to take clmm() model objects
# and derive fitted values with 95%CIs
# ===========================================================

# Provides predictions for CLMM
VarPred <- function(mod, varname, frame, isolate=FALSE, isoValue=NULL, level=0.05, steps=101, dfspec=100, vv=NULL){
  
  # Takes a clmm-class model object (mod), predictor variable name (varname), 
  # and data.frame(frame)containing only observations and variables used 
  # in model and returns a data.frame containing fitted values with 95% 
  # upper and lower confidence limits. The fitted values are on the scale of
  # the linear predictor.
  
  # Service functions
  eff <- function(mod){
    if (inherits(mod, "lm")) return (coef(mod))
    if (inherits(mod, "mer")) return (fixef(mod))
    if (inherits(mod, "clmm"))
    {
      ef <- c(0, mod$beta)
      names(ef) <- c("(Intercept)", names(mod$beta))
      return (ef)
    }
    stop("Don't recognize model type")
  }
  
  varfun <- function(vcol, steps){
    if(is.numeric(vcol)){
      if(is.null(steps)){return(sort(unique(vcol)))}
      return(seq(min(vcol), max(vcol), length.out=steps))
    }
    return(unique(vcol))
  }
  
  # Stats
  df <- ifelse(
    grepl("df.residual", paste(names(mod), collapse="")), 
    mod$df.residual, dfspec
  )
  mult <- qt(1-level/2, df)
  
  # Eliminate rows that were not used
  rframe <- frame[rownames(model.frame(mod)), ]
  
  # Find variable in data frame
  # ISSUE: Can't have a variable name that's a subset of another name
  pat <- paste("\\b", varname, sep="")
  fnames <- names(rframe)
  
  # print(pat); print(fnames)
  fCol <- grep(pat, fnames)
  
  #print(paste("Selected variable", fnames[fCol]))
  if(length(fCol)<1) {
    stop(paste("No matches to", varname, "in the frame", collapse=" "))
  }
  if(length(fCol)>1) {
    stop(paste("Too many matches:", fnames[fCol], collapse=" "))
  }
  
  if(is.null(vv)) {vv <- varfun(rframe[[fCol]], steps)}
  steps <- length(vv)
  
  # Mean row of model matrix
  modTerms <- delete.response(terms(mod))
  mm <- model.matrix(modTerms, rframe)
  rowbar<-matrix(apply(mm, 2, mean), nrow=1)
  mmbar<-rowbar[rep(1, steps), ]
  
  # Find variable in model matrix
  mmNames <- colnames(mm)
  mmCols <- grep(pat, mmNames)
  #print(paste(c("Selected columns:", mmNames[mmCols], "from the model matrix"), collapse=" "))
  if (length(mmCols)<1) 
    stop(paste("No matches to", varname, "in the model matrix", collapse=" "))
  
  # Model matrix with progression of focal variable 
  varframe <- rframe[rep(1, steps), ]
  varframe[fCol] <- vv
  mmvar <- mmbar
  mmnew <- model.matrix(modTerms, varframe)
  
  for(c in mmCols){
    mmvar[, c] <- mmnew[, c]
  }
  
  ef <- eff(mod)
  vc <- vcov(mod)
  if (inherits(mod, "clmm")){
    f <- c(names(mod$alpha)[[1]], names(mod$beta))
    vc <- vc[f, f]
  }
  
  if(!identical(colnames(mm), names(ef))){
    print(setdiff(colnames(mm), names(ef)))
    print(setdiff(names(ef), colnames(mm)))
    stop("Effect names do not match: check for empty factor levels?")
  }
  pred <- mmvar %*% eff(mod)
  
  # (Centered) predictions for SEs
  if (isolate) {
    if(!is.null(isoValue)){
      rframe[fCol] <- 0*rframe[fCol]+isoValue	
      mm <- model.matrix(modTerms, rframe)
      rowbar<-matrix(apply(mm, 2, mean), nrow=1)
      mmbar<-rowbar[rep(1, steps), ]
    }
    mmvar <- mmvar-mmbar
  }
  
  pse_var <- sqrt(diag(mmvar %*% tcrossprod(vc, mmvar)))
  
  df <- data.frame(
    var = vv,
    fit = pred,
    lwr = pred-mult*pse_var,
    upr = pred+mult*pse_var
  )
  names(df)[[1]] <- varname
  return(df)
}

# Transforms the predictions in the data.frame returned by varpred()

OrdTrans <- function(v, a){
  # This function takes a vector (v) from the data frame returned by varpred—
  # either the fitted value, or one of the confidence limits, and the intercepts 
  # for each of the ordinal outcomes. It returns transformed values by accounting for 
  # the threshold parameters and averaging through all levels.
  
  sapply(v, function(n){
    return(sum(plogis(n-a)))
  })
}

# Gets predictions from clmm object

OrdPred <- function(mod, n, modAns){
  # This function takes the model object (mod), predictor(n), and dataframe(modAns)
  # and returns a data.frame that has the original predictor values, fitted value
  # and upr and lower 95% intervals. The returned fitted value takes into account the
  # threshold values.
  
  
  v <- VarPred(mod, n, modAns, isolate=TRUE)
  v$fit <- OrdTrans(v$fit, mod$alpha)
  v$lwr <- OrdTrans(v$lwr, mod$alpha)
  v$upr <- OrdTrans(v$upr, mod$alpha)
  return(v)
}

