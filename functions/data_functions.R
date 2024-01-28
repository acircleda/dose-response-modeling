# data functions
#TODO - add in function to repeat low doses, max doses - ghost starts
#To do generate 
simulate_data <- function(type,
                          nobs,
                          dose_label = 'dose',
                          response_label = 'response',
                          dose_range = NULL,
                          control=NULL,
                          experiment=NULL,
                          response_range,
                          seed = 1983,
                          expected_response_design = 'inhibition'){
  
  # sort based on the desiign
  
groups = c(control, experiment)

design_sort <- function(x, type){
  if(type == 'inhibition'){
    sort(x, decreasing=TRUE)
  } else if(type == 'stimulation'){
    sort(x, decreasing=FALSE)
  } else {x}
}

# dose simulation
make_dose <- function(dose_range, nobs){
  min_dose = dose_range[1] # Include minimum dose
  max_dose = dose_range[length(dose_range)] # Include maximum dose
  # Get the mid range values of the dose range * random noise, and ensure max > mid_range > min
  mid_range = pmax(
    pmin(
      suppressWarnings(
        sample(dose_range[-c(1, median(dose_range), max(dose_range+1))], 
               nobs-2, replace=TRUE) * runif(nobs-2, .8, 1.1)
      ), 
      max_dose),
    min_dose)
  
  dose = round(sort(c(min_dose, mid_range, max_dose), decreasing = FALSE), 2)
  
  return(dose)
}

# response simulation


make_response <- function(response_range, expected_response_design, nobs, type){
  

  sim_min = min(response_range)+1 * runif(1, -1,1)
  sim_max = max(response_range)+1 * runif(1, 0,1)
  slope_val <- log(sim_max)* runif(1, 1,2)
  slope = ifelse(expected_response_design == "stimulation", -slope_val, slope_val)
  inflection = (sim_max*.1)+(sqrt(sim_max) * runif(1, -1,1))
  sim_data <- rdrm(1, fct=LL.4(), c(slope, sim_min, sim_max, inflection), xpar = 1,
                   yerror = "rnorm", xerror = dose, onlyY=TRUE)
  
  sim_data <- if(type == "binomial"){
                     scales::rescale(as.vector(sim_data$y), to0=c(0,1)) } else {
                     as.vector(sim_data$y) }
  return(sim_data)
  
}

if(is.null(groups)){
  dose = make_dose(dose_range=dose_range, nobs=nobs)

  response = make_response(response_range=response_range,
                           expected_response_design=expected_response_design,
                           type=type,
                           nobs=nobs)

  simulated_data <- data.frame(dose=dose, response=response)
  names(simulated_data) <- c(dose_label, response_label)
  return(simulated_data)
} else {
  
  dose = rep(make_dose(dose_range=dose_range, nobs=nobs), length(groups))
  experiment_response = make_response(response_range=response_range,
                                      expected_response_design=expected_response_design,
                                      type=type,
                                      nobs=nobs)
  control_response = make_response(response_range=response_range,
                                   expected_response_design=expected_response_design,
                                   type=type,
                                   nobs=nobs)

    if(expected_response_design == "inhibition"){
      experiment_response <- pmin(experiment_response, control_response*.8)
    } else if(expected_response_design == "stimulation") {
      control_response <- pmin(control_response, experiment_response*.8)
    }  
  
  response = c(control_response, experiment_response)
  groups = c(rep(control, nobs), rep(experiment, nobs))
  simulated_data <- data.frame(dose=dose, response=response, groups=groups)
  names(simulated_data) <- c(dose_label, response_label, "groups")
  return(simulated_data)
  }
  

}
