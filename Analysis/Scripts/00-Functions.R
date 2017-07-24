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
                        digits = 1,
                        hrzl_prop = TRUE,
                        add_total_col = FALSE)
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

# ===========================
# Functions for SIHR analysis
# ===========================


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



# =========================
# Functions for CTSBS study
# =========================

# Creating a linear mixed model with random intercept for person
# This is specifically for age-mixing patterns

# ampmodel <- function(df, s, h) {
#  
#   if(s == "Male" | h == "Negative") {
#     
#     lme(agep ~ age0, 
#         data = df, 
#         random = ~1 | id,
#         weights = varPower(value = 0.5, form = ~age0 + 1),
#         method = "REML")
#     
#   } else {
#     
#     lmer(agep ~ age0 + (1 | id),
#          data = df,
#          REML = TRUE)
#   }
# }

# ampmodel <- function(df) {
#   
#   df2 <- df %>%
#     arrange(agegroup)
#   
#   df3 <- df2 %>%
#     distinct(id) %>%
#     mutate(id2 = row_number())
#   
#   df4 <- left_join(df2, df3, by = "id")
#   
#   lme(agep ~ age0,
#       data = df4,
#       random = ~ 1 | id2,
#       weights = varIdent(form = ~1|agegroup),
#       method = "REML")
#   
# }

ampmodel <- function(df) {
    
  tryCatch({
    
    lmer(agep ~ age0 + (1 | id),
         data = df,
         REML = TRUE)
    
  }, error = function(e) {
    print(paste("My error: ", e)); NA
  })
  
}

# Extracts the between subject SD from agemixing pattern model
# Also upper and lower CI limits

bvar <- function(model) {
  
  # Outputs a df with between-subject variance, upr & lwr limits
  
  if(class(model)[1] == "lmerMod") {
    
    # Must take an merMod  object
    
    bsd <- as.data.frame(VarCorr(model))[1, 5] %>%
      round(3)
    
    # The tryCatch function is used so that the function will keep going even 
    # if there is an error for one of the models. If there is an error, it
    # will just make the value missing
    lwrbsd <- tryCatch({
      confint(model)[1, 1] %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    uprbsd <- tryCatch({
      confint(model)[1, 2] %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
  } else {
    
    # Must take an nlme model object
    
    bsd <- VarCorr(model)[1, 2] %>%
      as.numeric() %>%
      round(3)
    
    lwrbsd <- tryCatch({
      intervals(model)$reStruct$id$lower  %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    uprbsd <- tryCatch({
      intervals(model)$reStruct$id$upper  %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
  }
  
  data.frame(bsd = bsd, lwrbsd = lwrbsd, uprbsd = uprbsd)
}


# Extracts the within subject SD and power coefficient
# from agemixing pattern model
# Also upper and lower CI limits

# wvar <- function(model) {
#   
#   # Outputs a df with within-subject variance, upr & lwr limits
#   
#   if(class(model)[1] == "lmerMod") {
#     
#     # Takes merMod model object
#     
#     wsd <- as.data.frame(VarCorr(model))[2, 5] %>%
#       round(3)
#     
#     # The tryCatch function is used so that the function will keep going even 
#     # if there is an error for one of the models. If there is an error, it
#     # will just make the value missing
#     lwrwsd <- tryCatch({
#       confint(model)[2, 1] %>%
#         round(3)
#     }, error = function(e) {
#       print(paste("My error: ", e)); NA
#     })
#     
#     uprwsd <- tryCatch({
#       confint(model)[2, 2] %>%
#         round(3)
#     }, error = function(e) {
#       print(paste("My error: ", e)); NA
#     })
#     
#     p <- NA
#     lwrpsd <- NA
#     uprpsd <- NA
#     
#   } else {
#     
#     # Must take an nlme model object
#     
#     wsd <- VarCorr(model)[2, 2] %>%
#       as.numeric() %>%
#       round(3)
#     
#     lwrwsd <- tryCatch({
#       (intervals(model)$sigma[1])  %>%
#         round(3)
#     }, error = function(e) {
#       print(paste("My error: ", e)); NA
#     })
#     
#     uprwsd <- tryCatch({
#       (intervals(model)$sigma[3])  %>%
#         round(3)
#     }, error = function(e) {
#       print(paste("My error: ", e)); NA
#     })
#   
#     # Power coefficient
#     p <- tryCatch({
#       (attributes(model$apVar)$Pars["varStruct.power"])  %>%
#         round(3)
#     }, error = function(e) {
#       print(paste("My error: ", e)); NA
#     })
#     
#     # Lower interval
#     lwrpsd <- tryCatch({
#       (intervals(model)$varStruct[, 1]) %>%
#         round(3)
#     }, error = function(e) {
#       print(paste("My error: ", e)); NA
#     })
#     
#     # Upper interval
#     uprpsd <- tryCatch({
#       (intervals(model)$varStruct[, 3]) %>%
#         round(3)
#     }, error = function(e) {
#       print(paste("My error: ", e)); NA
#     })
#   }
# 
#   data.frame(wsd = wsd, lwrwsd = lwrwsd, uprwsd = uprwsd,
#              p = p, lwrpsd = lwrpsd, uprpsd = uprpsd)
# }

wvar <- function(model) {
  
  # Outputs a df with within-subject variance, upr & lwr limits
  
  if(class(model)[1] == "lmerMod") {
    
    # Takes merMod model object
    
    wsd <- as.data.frame(VarCorr(model))[2, 5] %>%
      round(3)
    
    # The tryCatch function is used so that the function will keep going even 
    # if there is an error for one of the models. If there is an error, it
    # will just make the value missing
    lwrwsd <- tryCatch({
      confint(model)[2, 1] %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    uprwsd <- tryCatch({
      confint(model)[2, 2] %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
  } else {
    
    # Must take an nlme model object
    
    wsd <- VarCorr(model)[2, 2] %>%
      as.numeric() %>%
      round(3)
    
    lwrwsd <- tryCatch({
      (intervals(model)$sigma[1])  %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    uprwsd <- tryCatch({
      (intervals(model)$sigma[3])  %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    # Variance factor from varIdent
    vf <- tryCatch({
      intervals(model)$varStruct[, 2] %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    # Lower interval
    lwrvf <- tryCatch({
      (intervals(model)$varStruct[, 1]) %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    # Upper interval
    uprvf <- tryCatch({
      (intervals(model)$varStruct[, 3]) %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
    
    # wsd for old age groups
    wsdold <- tryCatch({
      (wsd * vf) %>%
        round(3)
    }, error = function(e) {
      print(paste("My error: ", e)); NA
    })
  }
  
  data.frame(wsd = wsd, lwrwsd = lwrwsd, uprwsd = uprwsd)
}


# Creating a  negative binomial model for hiv status as outcome
# Calculates RR
# Adjusts for race, age 
# Need to adjust the df for spline appropriately
# For both male and female models the EDF was 2

bwmodel <- function(df) {
  
  gam(bridgewidth ~ hiv + s(age),
    data = df,
    family = nb())

}


# This function creates a model that regresses age differences
# on HIV.
# If it is a male df, then the df = 4 for age
# If it is a female df, then linear age term

admodel <- function(df, gendervar) {
  
  if(gendervar == "Male") {
    lmer(agedif ~ hiv + splines::ns(age, df = 4) + race + (1 | id), 
         data = df)
  } else {
    lmer(agedif ~ hiv + splines::ns(age, df = 2) + race + (1 | id), 
         data = df)
  }
}

# This function creates a model that regresses age differences on HIV.
# Uses a GA mixed model with smooth term for age
# Random intercept for participant
# Using gamm4 because it uses lmer instead of lme
# Return only the gam part of the model object (this is the fixed effects)

adgammodel <- function(df) {
  
  mod <- gamm4(agedif ~ s(age) + hiv + race, 
               data = df, 
               random = ~(1 | id))
  
  mod[[2]]

}

# This function creates a model that regresses the condom frequency
# outcome variable on HIV
# Mixed model with random intercept for participant
# with bridgewidth as a mediator
# There are smooth terms for bw and age
cfmodel <- function(df, med = FALSE) { 
  
  # Checks to see if we want a model with bw as a mediator
  if(med) {
    
    mod <- gamm4(cf ~ bridgewidth + s(age) + hiv + race,
                 family = binomial,
                 data = df,
                 random = ~(1 | id))
    
  } else {
    
    mod <- gamm4(cf ~ s(age) + hiv + race,
                 family = binomial,
                 data = df,
                 random = ~(1 | id))
    
  }
  
  mod[[2]]
  
}

cfbwmodel <- function(df) { 
  
  mod <- gamm4(cf ~ bridgewidth + s(age) + race,
               family = binomial,
               data = df,
               random = ~(1 | id))

  mod[[2]]
  
}

# This function creates a model that regresses the binary concurrency
# outcome variable on HIV
# with bridgewidth as a mediator
# There are smooth terms for bw and age
# 
mcpmodel <- function(df, med = FALSE) { 
  
  # The yvar is the specific outocme variable
  # df$yvar <- eval(substitute(yvar), df)
  
  # Checks to see if we want a model with bw as a mediator
  if(med) {
    
    gam(partconcur ~ bridgewidth + s(age) + hiv + race,
        family = binomial,
        data = df)
    
  } else {
    
    gam(partconcur ~ s(age) + hiv + race,
        family = binomial,
        data = df)
    
  }
 
}

# This function regresses mcp on bridgewidth
# No HIV in this model
mcpbwmodel <- function(df) { 
    
  gam(partconcur ~ bridgewidth + s(age) + race,
      family = binomial,
      data = df)
  
}

# This function creates a model that regresses sex frequency (count)
# outcome variable on HIV
# with bw as a mediator
# there are smooth terms for bw and age

sfmodel <- function(df, med = FALSE) {
  
  # Checks to see if we want a model with bw as a mediator
  if(med) {
    
    mod <- gamm4(relsf ~ bridgewidth + s(age) + hiv + race,
                 family = poisson,
                 data = df,
                 random = ~(1 | id))
    
  } else {
    
    mod <- gamm4(relsf ~ s(age) + hiv + race,
                 family = poisson,
                 data = df,
                 random = ~(1 | id))
    
  }
  
  mod[[2]]

}

sfbwmodel <- function(df) {
  
  mod <- gamm4(relsf ~ bridgewidth + s(age) + race,
               family = poisson,
               data = df,
               random = ~(1 | id))

  
  mod[[2]]
  
}

# Function to tidy output of gamm4 models
# These will have a sexual behaviour as the outcome
tidygam <- function(mod) {
  
  
  df <- data.frame(term = names(coef(mod)),
             estimate = coef(mod),
             std.error = summary(mod)$se) %>%
    mutate(lwr = estimate - 2 * std.error,
           upr = estimate + 2 * std.error) %>%
    remove_rownames()
  
  if(mod$family[1] == "binomial" |
     mod$family[1] == "poisson" |
     grepl("Negative Binomial", mod$family[1])) {
    
    df <- df %>%
      mutate(ratio = exp(estimate),
           rlwr = exp(lwr),
           rupr = exp(upr)) 
  }
  
  return(df)
}



# This function takes the model object form glm.nb and produces
# predictions for 50 values of age and then stores and returns
# as a tidy df

nbsplinepreds <- function(df, mod) {
  
  grid <- df %>%
    data_grid(age = seq_range(age, 50), 
              .model = mod) 
  
  grid %>%
    mutate(pred = predict(mod, 
                          newdata = .,
                          type = "response"),
           se = predict(mod,
                        newdata = .,
                        type = "response",
                        se.fit = T)[[2]])
  
}

# This function takes the model object from lme4 and
# produces predictions for the specified ages and then
# stores and returns as a tidy df

lmesplinepreds <- function(mod) {

  grid <- expand.grid(age = 15:70, 
                hiv = "Negative", # Mode hiv value
                race = "Black") # Mode race value
  
  grid %>%
    mutate(pred = predict(mod, 
                          newdata = .,
                          re.form = NA))

}

# This function takes the model object from a gam or gamm4 model,
# a dataframe, and the explanatory variable that is a smooth
# term in the model and produces predictions for that particular
# smooth term. It stores and returns the result as a tidy df.


gampredsage <- function(mod) {
  
  # Determine the model family, so that we can do the 
  # predictions on the right scale
  if(mod$family[1] == "binomial" |
     mod$family[1] == "poisson" |
     grepl("Negative Binomial", mod$family[1])) { 
    
    type <- "response"
    
  } else {
    
    type <- "link"
    
  }
  
    
  # grid <- data %>%
  #   modelr::data_grid(age = seq_range(age, 50, pretty = TRUE),
  #                     .model = mod)
  
  grid <- expand.grid(hiv = "Negative",
                      race = "Black",
                      bridgewidth = 5, 
                      age = seq(15, 70, length.out = 50))
    
  # Take the grid from above and create predictions.  
  grid %>%
    mutate(pred = predict(mod,
                          newdata = .,
                          type = type),
           se = predict(mod, 
                        newdata = ., 
                        type = type,
                        se.fit = TRUE)[[2]])
}

# =====================
# Cluster bootstrapping
# =====================

clusboot <- function(df) {

  cluster_sample <- data.frame(id = sample(unique(df$id), replace = TRUE))
  
  df_sample <- df %>%
    inner_join(cluster_sample, by = "id")
  
  return(df_sample)
}

bootstrap_clus <- function(data, n, id = ".id") {
  
  bootstrap <- purrr::rerun(n, clusboot(data))
  
  df <- tibble::data_frame(strap = bootstrap) %>%
    mutate(.id = row_number())
  
  df
  
} 