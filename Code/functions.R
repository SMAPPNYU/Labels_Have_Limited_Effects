display <- function(num, pct=FALSE){
  if (!pct) return(sprintf("%.3f", num))
  if (pct) return(sprintf("%.1f", num*100))
}

extract_covariates <- function(mod){
  coefs <- names(coef(mod[[1]]))
  coefs <- gsub("_c$|Firefox_c", "", coefs)
  coefs <- coefs[coefs %in% names(svy)]
  return(coefs)
}

estimate_cace <- function(Y, D, Z, X, trt, control="Control", data = svy){
  form <- formula(paste0(Y, " ~ ", D, " + ", paste(X, collapse=" + "),
                         " | ", Z, " + ", paste(X, collapse=" + ")))
  dt <- filter(data, Treated %in% c(control, trt))
  mod <- iv_robust(form, data=dt)
  return(mod)
}

# to determine whether we need to run multiple imputation, compute the
# % of observations with at least one missing value in the covariates.
# Note that missing values in DV are not included in the estimation.
# If % of missing values is above 20%, then we will do multiple imputation.
compute_proportion_missing_covars <- function(mod, more_info=FALSE){
  # mod = output from run_model()
  
  # identify labels for covariates and dv
  covars <- attr(mod[[1]]$terms, "term.labels")
  dv <- mod[[1]]$outcome
  
  # replacing state dummies with state factor variable
  if (sum(grepl('state', covars))>0){
    toreplace <- grep('state', covars)
    covars[toreplace] <- 'state'
    covars <- unique(covars)
  }
  # replacing race dummies with race factor variable
  if (sum(grepl('raceeth', covars))>0){
    toreplace <- grep('raceeth', covars)
    covars[toreplace] <- 'raceeth'
    covars <- unique(covars)
  }  
  
  # extract relevant variables from svy file
  dd <- svy[,c(dv, covars)]
  
  # identify rows where DV is not missing
  no_miss_dv <- which(!is.na(dd[,dv]))
  
  # identify rows where at least one covariate has a missing value
  miss_covars <- apply(svy[no_miss_dv,covars], 1, function(x) any(is.na(x)))
  
  # print % of missing values
  message(display(mean(miss_covars), pct=TRUE), "% missing")
  
  if (more_info){
    print(summary(svy[no_miss_dv,covars]))
  }
  
  return(invisible(mean(miss_covars)))
}




# Compute power analysis for DIM, ITT and CACE
power2 <- function(dv, dim, itt, cace, covariates = TRUE, cace_mde = TRUE, D, trt, 
                   trt_var = "Treated"){
  message("MDE without covariate adjustment:")
  pwr_dim <- power.t.test(n = dim$nobs, 
                          sd = sd(unlist(svy[dv]), na.rm=TRUE),
                          sig.level = 0.05, power=0.80)
  message(display(pwr_dim$delta), " (",
          display(pwr_dim$delta / sd(unlist(svy[,dv]),
                                     na.rm=TRUE), pct=TRUE),
          "% of a 1-SD increase in DV)")
  if (covariates == TRUE){
    message("MDE with covariate adjustment:")
    message("Formulas: ", itt[[3]])
    reg <- summary(lm(itt[[3]], data=svy))
    ( pwr_itt <- power.t.test(n = itt[[1]]$nobs, 
                              sd = reg$sigma,
                              sig.level = 0.05, power=0.80
    ) )
    message(display(pwr_itt$delta), " (",
            display(pwr_itt$delta / sd(unlist(svy[,dv]),
                                       na.rm=TRUE), pct=TRUE),
            "% of a 1-SD increase in DV)")
  }
  if(cace_mde == TRUE){
    message("MDE for CACE:") # following Bansak 2020
    # compute proportion of variation in D/Y left unexplained by Z, 
    # that is explained by covariates W
    if(covariates == TRUE){
      d_z_df <- na.omit(svy[,c(D, extract_covariates(itt))])
      d_y_df <- na.omit(svy[,c(dv, extract_covariates(itt))])
      out_d_z <- lm(paste0(D, "~", trt_var), data = d_z_df)
      out_d_y <- lm(paste0(dv, "~", trt_var), data = d_y_df)
      out_d_zw <- lm(paste0(D, "~", paste0(c(extract_covariates(itt)), 
                                           collapse = "+")), data = d_z_df)
      out_d_yw <- lm(paste0(dv, "~", paste0(c(extract_covariates(itt)), 
                                            collapse = "+")), data = d_y_df)
      r2dw <- max(0, ((summary(out_d_z)$sigma)^2 - 
                        (summary(out_d_zw)$sigma)^2)/((summary(out_d_z)$sigma)^2))
      r2yw <- max(0, ((summary(out_d_y)$sigma)^2 - 
                        (summary(out_d_yw)$sigma)^2)/((summary(out_d_y)$sigma)^2))
    }
    # compute compliance rate / average causal effect of treatment assignment
    # on treatment uptake
    d_z_df <- svy[,c(D, trt_var)]
    out_d_z <- lm(paste0(D, "~", trt_var), data = d_z_df)
    compliance_rate <- out_d_z$coefficients[2]
    # compute conservative (upper) bound for minimum detectable effect size
    if(covariates == TRUE) {
      if(length(extract_covariates(itt)) > 1) {
        pwr_cace <- try(powerLATE::powerLATE.cov(pZ = .5, 
                                                 pi = compliance_rate, 
                                                 N = cace$nobs,
                                                 sig.level = 0.05,
                                                 power = 0.8,
                                                 r2dw = r2dw,
                                                 r2yw = r2yw,
                                                 effect.size = TRUE,
                                                 assume.ord.means = TRUE,
                                                 verbose = FALSE)$output.parameter)
      }else{
        pwr_cace <- try(powerLATE::powerLATE(pZ = .5, 
                                             pi = compliance_rate, 
                                             N = cace$nobs,
                                             sig.level = 0.05,
                                             power = 0.8,
                                             effect.size = TRUE,
                                             assume.ord.means = TRUE,
                                             verbose = FALSE)$output.parameter)    
      }
    }else{
      pwr_cace <- try(powerLATE::powerLATE(pZ = .5, 
                                           pi = compliance_rate, 
                                           N = cace$nobs,
                                           sig.level = 0.05,
                                           power = 0.8,
                                           effect.size = TRUE,
                                           assume.ord.means = TRUE,
                                           verbose = FALSE)$output.parameter)    
    }
    if(class(pwr_cace) == "try-error") {
      pwr_cace <- NA
    }
    message(display(pwr_cace), " (",
            display(pwr_cace / sd(unlist(svy[,dv]),
                                  na.rm=TRUE), pct=TRUE),
            "% of a 1-SD increase in DV)")
  }
  if(covariates == FALSE & cace_mde == TRUE){
    return(data.frame(pwr_itt = c(display(pwr_dim$delta), display(pwr_dim$delta / sd(unlist(svy[,dv]),
                                                                              na.rm=TRUE))),
               pwr_cace = c(display(pwr_cace), display(pwr_cace / sd(unlist(svy[,dv]),
                                                                     na.rm=TRUE)))))
  }
  if(covariates == TRUE & cace_mde == TRUE){
    return(data.frame(pwr_itt = c(display(pwr_itt$delta), display(pwr_itt$delta / sd(unlist(svy[,dv]),
                                                                                     na.rm=TRUE))),
                      pwr_cace = c(display(pwr_cace), display(pwr_cace / sd(unlist(svy[,dv]),
                                                                            na.rm=TRUE)))))
  }
  if(covariates == FALSE & cace_mde == FALSE){
    return(data.frame(pwr_itt = c(display(pwr_dim$delta), display(pwr_dim$delta / sd(unlist(svy[,dv]),
                                                                              na.rm=TRUE)))))
  }
  if(covariates == TRUE & cace_mde == FALSE){
    return(data.frame(pwr_itt = c(display(pwr_itt$delta), display(pwr_itt$delta / sd(unlist(svy[,dv]),
                                                                              na.rm=TRUE)))))   
  } 
}


#Create a function using glmnet lasso that chooses the variables to use:
Lasso <- function(data_for_analysis) {
  set.seed(938)
  lasso_select <- cv.glmnet(x=data.matrix(data_for_analysis[,-1]),
                            y=as.vector(data_for_analysis[,1]),
                            alpha=1)
  
  coef.out <- coef(lasso_select, exact = TRUE)
  indices <- which(coef.out != 0)
  names_of_columns <- c(rownames(coef.out)[indices],colnames(data_for_analysis)[1])
  names_of_columns_3 <- names_of_columns[!names_of_columns %in% "(Intercept)"]
  return(names_of_columns_3)
}

#Clean Data:

Clean <- function(data_for_analysis) {
  #Replace Infinite values in data with NA
  data_for_analysis <- do.call(data.frame,                      
                               lapply(data_for_analysis,
                                      function(x) replace(x, is.infinite(x), NA)))
  #Remove NA values:
  data_for_analysis <- na.omit(data_for_analysis)
  
  return(data_for_analysis)
}




heterogeneous_effect2 <- function(dv, dv_pre=NULL, trt, control, moderator){
  itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, verbose=FALSE)
  vars <- unique(c(extract_covariates(itt), moderator))
  # now compute itt but adding moderator (if it wasn't included)
  lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
  lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
  res <- lm_lin(lin_formula, covariates = lin_covars,
                data=svy[svy$W3_PATA306_treatment_w3 %in% c(control, trt),])
  # extract t stat for interaction
  interaction_t <- res$statistic[grep(paste0(":", moderator), names(res$statistic))]
  sig <- ifelse(
    interaction_t > 1.96, "+",
    ifelse(interaction_t < (-1.96), "-", "n.s.")
  )
  return(sig)
}