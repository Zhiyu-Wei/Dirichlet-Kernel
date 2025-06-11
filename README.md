# Dirichlet Kernel Ternary Viewer

An interactive R Shiny application for semi-parametric analysis and visualization of compositional data using Dirichlet kernel smoothing and backfitting.

## 🧠 Features

- Upload your own dataset or use built-in simulated data
- Interactive ternary plots of compositional data
- Dirichlet kernel smoothing with user-defined or cross-validated bandwidth
- Semi-parametric regression via backfitting
- Bootstrap inference for linear covariates
- Parallel computation support

## 📦 Installation

```r
# Required packages
install.packages(c("shiny", "DT", "ggplot2", "shinycssloaders", "MCMCpack", "foreach", "doParallel"))
```
![screenshot](1.png) 
![screenshot](2.png) 
![screenshot](3.jpg) 

## App Structure

- **Tab 1: Data Preparation**
  - Upload CSV file or use built-in example data.  
  - Select three compositional variables (must sum to 1).  
  - Choose covariates and response variable.  
    *Note: Currently, at least two covariates must be selected to ensure proper functioning of the app.*

- **Tab 2: Ternary Plot**
  - Visualize the smoothed response surface using Dirichlet kernel smoothing.
  - Adjust the smoothing bandwidth (`h`) manually or via cross-validation.  
    *Note: The cross-validation interval must be carefully chosen. The default range may not always be suitable.  
    If the app closes unexpectedly, try expanding the interval range to improve convergence.*
  - Customize the color scale and display options:  
    - **Scale color low / high**: Define the gradient colors for the ternary plot (e.g., yellow to blue).  
    - **Color bar title**: Specify the label for the color scale.  
    - **Plot title**: Set a custom title for the plot.  
    - **Resolution slider**: Control the grid density; higher resolution provides a smoother plot but increases computation time.  
    - **Show boundary**: Optionally display boundaries based on the observed data range.

- **Tab 3: Semi-parametric Estimation**
  - Run the backfitting algorithm to separate linear covariate effects and nonlinear compositional effects.
  - Perform bootstrap inference for covariates to obtain standard errors and p-values.  
    *Note: Bootstrap is computationally intensive. Parallel processing is supported to reduce execution time.  
    It is recommended to use no more than half of the available CPU cores. You may check your core count using `parallel::detectCores()` in R.*
  - Set the **tolerance** parameter to control convergence in backfitting.  
    Smaller values result in more accurate estimates but require longer computation.
  - Visualize the residual surface after removing linear effects using Dirichlet kernel smoothing on the ternary plot.


