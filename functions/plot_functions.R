# plot functions


# This is a simple EDA plot that plots the inputted data (without smoothing)
eda_plot <- function(data, x, y){
  
  data |>
    ggplot(aes_string(x=x,
               y=y))+
    geom_line()+
    geom_point()+
    scale_x_log10()
}


# This plots the observed vs predicted values to see how well the model fits
plot_model <- function(data, model, x, y){
  
  original_data <- data
  newdata <- expand.grid(x=exp(seq(log(0.5), 
                                      log(max(original_data[[x]])), 
                                      length=nrow(original_data))))
  
  pm <- predict(model, newdata=newdata, interval="confidence")
  newdata$p <- pm[,1]
  newdata$pmin <- pm[,2]
  newdata$pmax <- pm[,3]
  original_data$x_adjusted <- original_data[[x]]
  original_data$x_adjusted[original_data$x_adjusted == 0] <- 0.5
  
  
  ggplot(original_data, aes_string(x = "x_adjusted", y = y)) +
    geom_point() +
    geom_ribbon(data=newdata,
                aes_string(x="x", y="p", ymin="pmin", ymax="pmax"), 
                alpha=0.2) +
    geom_line(data=newdata, aes_string(x="x", y="p"))+
    geomtextpath::geom_textvline(xintercept = model$coefficients[4], label="EC50",
                                 linetype="dashed", color="grey50",
                                 hjust = .2)+
    scale_x_log10()
}
