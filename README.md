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

- **Tab 1: Data Preparation**
  - Upload CSV file or use built-in example data  
  - Select 3 compositional variables (must sum to 1)  
  - Choose covariates and response variable    
    ⚠️ *Note: Currently, at least two covariates must be selected to ensure proper functioning of the app.*

- **Tab 2: Ternary Plot**
  - Visualize smoothed response using Dirichlet kernel smoothing
  - Adjust bandwidth (`h`) manually or via cross-validation  
    ⚠️ *Note: The cross-validation interval must be carefully chosen. The default range may not always work.*  
    *If the app crashes or closes automatically, try expanding the interval range, as convergence may have failed.*
  - Customize color scale and display options
    🎨 *Scale color low / high: Set the gradient colors for the smoothed ternary plot (e.g., yellow → blue).*  
    🏷 *Color bar title: Customize the label shown on the color bar (e.g., "Predicted Y").*  
    🖼 *Plot title: Add a custom title to the ternary plot for display or export.*  
    🧱 *Resolution slider: Controls the grid density of smoothed values. Higher resolution = smoother but slower.*  
    🔲 *Show boundary: Toggle to display boundary lines based on observed data limits.*  


- **Tab 3: Semi-parametric Estimation**
  - 🧮 Run backfitting algorithm: Decomposes the response into linear (covariate) and nonlinear (compositional) effects.
  - 📊 Bootstrap inference for covariates: Resamples the dataset and estimates coefficient uncertainty via standard error and p-values.
     ⚠️ *Bootstrap involves high computation; parallel processing is supported to reduce runtime.*
     💡 *We recommend using at most half of your system's CPU cores. You can check your core count via `parallel::detectCores()` in R.*
     🔁 *Tolerance: The stopping threshold for the iterative backfitting loop. Smaller values lead to more accurate convergence but longer runtime.*
  - 🌊 Visualize residual nonparametric effects: The residual surface after removing linear covariate effects is visualized using Dirichlet kernel smoothing on the ternary plot.



