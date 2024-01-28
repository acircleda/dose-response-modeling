# Dose-Response Analysis

This is a Shiny-based app that allows the user to perform basic dose-response analysis using uploaded or simulated data. The app is an interface with the `drc` package and allows basic EDA, model comparison, model fitting, and post-hoc analysis.

The app can be accessed [here](https://as-test.shinyapps.io/dose-response-modeling/).

## Key Files

`app.R` - The Shiny app itself with ui and server. Open this file to run the app locally if all required packages are installed.

`functions/data_functions.R` - A function to simulate data suitable for dose-response analysis. Arguments include:

-   `type` - The type of distribution the data should represent (continuous, binomial, Poisson)

-   `nobs` - The number of observations

-   `dose_label` - The name of the dose column

-   `dose_range` - The numeric range of the dose

-   `response_label` - The name of the response column

-   `response_range` - The numeric range of the response

-   `seed` - A number for random value generation

-   `expected_response_design` - Whether the data should depict inhibition (a decrease in response as dose increases) or stimulation (the opposite effect).

-   There are also 2 group arguments but due to time constraints, group analysis was not implemented in the app.

`functions/model_functions.R` - These are key functions that allow interaction between the UI/user input and `drc`. The functions include:

-   `fit_drm` - A direct wrapper of `drm` to fit models

-   `summarize_drm` - A function to summarize the `drc` model with the option to use robust standard errors to correct p-values

-   `compare_models_summary` - A basic function that captures multiple summary-type output of multiple models for comparison

-   `compare_models_fit` - A function that captures AIC and BIC values for model comparison

-   `model_interpret` - A function to explain parameters in simple to understand terms

`functions/plot_funtions.R` - `ggplot`-based functions to visualize data. Due to time constraints, no theming was applied

-   `eda_plot` - A function to plot uploaded or simulated data using point and line geometries and a log rescale of the x-axis

-   `plot_model` - A function to plot observed (points) vs fitted (line) values to understand how well the model fits the data
