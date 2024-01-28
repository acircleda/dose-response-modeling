# Model functions
# These functions utilize the drc package as well as others and are meant to be used by the Shiny app


# paremter_names serves as a method to select the appropriate loglinear function with pre-supplied names from the shiny app
model_names <- dplyr::tribble(~choice_name, ~fct, ~argument,
        "2 Paramaeter Log-logistic", "LL.2", 'LL.2(names=c("Slope", "EC50"))',
        "3 Paramaeter Log-logistic", "LL.3", 'LL.3(names=c("Slope", "Einf", "EC50"))',
        "4 Paramaeter Log-logistic", "LL.4", 'LL.4(names=c("b Slope", "c E0", "d Einf", "e EC50"))',
        "4 Paramaeter Weibull", "W1.4", 'W1.4(names=c("b Slope", "c E0", "d Einf", "e EC50"))',
        
)

# This function is a wrapper that supplies input options into the drm function
fit_drm <- function(y, x, data, groups=NULL, selected_fct, ...){
  
  groups <-  if(!is.null(groups)){
    groups
  } else { NULL }
  
  
  # set formula
  formula <- as.formula(paste0(y, " ~ ", x))
  
  # fit model
  model <- drm(formula, curveid = groups,
               data = data, fct = eval(parse(text=selected_fct)),
               ...)
  
  # update call in model object for correct printing
  model[["call"]] <- formula
  return(model)
}

# this is a wrapper to present a summary with standard errors or robust standard errors
summarize_drm <- function(model, robust=FALSE){
  if(robust==TRUE){
    registerS3method("estfun", "drc", drc::estfun.drc)
    registerS3method("bread", "drc", drc::bread.drc)
    
    lmtest::coeftest(model, vcov = sandwich::sandwich)
  } else {
    summary(model)
  }
}


# this produces model output for multiple models for comparison
compare_models_summary <- function(models_to_compare, data, y, x, groups=NULL, ...){
  
  model_output_list <- list()
  for(m in 1:length(models_to_compare)){
    
    selected_model <- model_names[model_names$choice_name == models_to_compare[m], ]$argument
    model_object <- fit_drm(y=y, x=x, data=data, selected_fct=selected_model, ...)
    model_output <- capture.output(summary(model_object))
    model_output_formatted <- c("\n\n",
                                rep("-", 10),
                                models_to_compare[m],
                                "\n\n",
                                paste(model_output, collapse = "\n"))
    
    model_output_list[[paste("Model", models_to_compare[m])]] <- model_output_formatted
    
  }
  
  all_model_outputs <- unlist(model_output_list, recursive = FALSE)
  
  return(all_model_outputs)
}


# this calculates and makes a table for AIC/BIC for multiple models for comparison
compare_model_fit <- function(models_to_compare, data, y, x, groups=NULL, ...){
  
  fit_df <- data.frame(matrix(ncol = 3, nrow = 0))
  names(fit_df) <- c("model", "AIC", "BIC")
  
  for(m in 1:length(models_to_compare)){
    
    selected_model <- model_names[model_names$choice_name == models_to_compare[m], ]$argument
    model_object <- fit_drm(y=y, x=x, data=data, selected_fct=selected_model, ...)
    data_fit <- data.frame(
      model = models_to_compare[m],
      AIC = AIC(model_object),
      BIC = BIC(model_object)
    )
    
    fit_df <- rbind(fit_df, data_fit)
    
  }
    fit_df <- arrange(fit_df, AIC)
    return(fit_df)
}
   
# This is a function that attempts to place key model paramaters into context to remind the reader of how they relate to the curve being analyzed
model_interpret <- function(model){
  
  E0 <- model$coefficients[2]
  Einf <- model$coefficients[3]
  EC50 <- model$coefficients[4]
  H <-  -model$coefficients[1]
  H_type <- ifelse(H < 0, "negative", "positive")
  H_type <- ifelse(H == 0, "zero", H_type)
  H_meaning <- ifelse(H < 0, "is decreasing", "is increasing")
  H_meaning <- ifelse(H == 0, "has no clear relationship", H_type)

  
  E0_ex <- paste0("An E0 of ", round(E0,2), " represents the response in absence of the dose. This is the lowest part of the curve.")
  
  Einf_ex <- paste0("An Einf of ", round(Einf,2), " represents the response with the maximum amount of the dose. This is the highest part of the curve.")
  
  H_ex <- paste0(round(H, 2), " is the slope, which indicates the steepness of your curve. The slope models how the response is affected by increasing (or decreasing) dosage. This is also known as a Hill slope. Your Hill slope is ", H_type, " which means increased dosage ", H_meaning, " your response.")
  
  EC50_ex <- paste0("The EC50 of ", round(EC50,2), " indicates dosage amount where 50% of the maximum response is seen. This is also the inflection point (on the x/dose axis) halfway up the curve")

  listify <- function(term, explanation){
    if(is.na(term)){ "" } else { paste('<li>',explanation,'</li>') }
  }
  
  output <- stringr::str_glue(
    '<p>Your model parameters can genrally be interpreted as follows:</p>
<ul>
	{listify(H, H_ex)}
	{listify(E0, E0_ex)}
  {listify(Einf, Einf_ex)}
	{listify(EC50, EC50_ex)}
</ul>
'
  )
  
  return(output)
}




