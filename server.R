library(shiny)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(MCMCpack)
library(foreach)
library(doParallel)


#local functions
d.kernel <- function(x, u, alpha, h, p = 3) {
  # 確保 x 和 u 都在 (0,1) 區間內
  if (all(x > 0 & x < 1) & all(u > 0 & u < 1)) {
    
    result <- lgamma(1/h + p * alpha) - sum(lgamma(x/h + alpha)) + sum((x/h + alpha - 1) * log(u))
    
    return(exp(result))  # 返回計算結果
  } else {
    stop("All values of x and u must be in (0,1) interval. Please check your input values.")
  }
}

draw_composition_plot <- function(data, pred, lab.title = "", original.data=NULL,
                                  show_boundary = FALSE,
                                  boundary.color="red",
                                  scale_color = c("yellow", "blue"),
                                  scale.bar.title=NA) {
  # Convert input to matrix and set column names
  datapoints <- as.matrix(data)
  if (!is.null(original.data)) {
    observed.data<- as.matrix(original.data)
  }
  pred <- as.numeric(pred)
  colnames(datapoints) <- colnames(data)
  
  # Create barycentric transformation matrix
  Bmatrix <- matrix(c(0,1, 0.5,0, 0,sqrt(3)/2), 3,2)
  
  # Minimum values for each dimension (used for axis setup)
  Lmin <- min(datapoints[,1])
  Rmin <- min(datapoints[,2])
  Tmin <- min(datapoints[,3])
  sep <- (1 - Lmin - Rmin - Tmin) / 10
  
  # Triangle border in composition space
  bord.matrix <- matrix(c(1-Rmin-Tmin, Lmin, Lmin,
                          Rmin, 1-Lmin-Tmin, Rmin,
                          Tmin, Tmin, 1-Lmin-Rmin), 3, 3)
  
  # Limits based on observed data range (used for boundary lines)
  if (!is.null(original.data)) {
    limit.matrix <- matrix(c(
      max(original.data[,1]), Rmin, 1-max(original.data[,1])-Rmin,
      max(original.data[,1]), 1-max(original.data[,1])-Tmin, Tmin,
      min(original.data[,1]), Rmin, 1-min(original.data[,1])-Rmin,
      min(original.data[,1]), 1-min(original.data[,1])-Tmin, Tmin,
      Lmin, max(original.data[,2]), 1-Lmin-max(original.data[,2]),
      1-max(original.data[,2])-Tmin, max(original.data[,2]), Tmin,
      Lmin, min(original.data[,2]), 1-Lmin-min(original.data[,2]),
      1-min(original.data[,2])-Tmin, min(original.data[,2]), Tmin,
      Lmin, 1-Lmin-max(original.data[,3]), max(original.data[,3]),
      1-Rmin-max(original.data[,3]), Rmin, max(original.data[,3]),
      Lmin, 1-Lmin-min(original.data[,3]), min(original.data[,3]),
      1-Rmin-min(original.data[,3]), Rmin, min(original.data[,3])
    ), 12, 3, byrow = TRUE)
  }
  
  # Project compositions to 2D coordinates
  Bbord <- bord.matrix %*% Bmatrix
  if (!is.null(original.data)) {
    Blimit <- limit.matrix %*% Bmatrix
  }
  Bdata <- datapoints %*% Bmatrix
  
  # Main triangle and expanded outer triangle for plotting
  tar.triangle <- data.frame(
    x = c(Bbord[1,1], Bbord[2,1], Bbord[3,1], Bbord[1,1]),
    y = c(Bbord[1,2], Bbord[2,2], Bbord[3,2], Bbord[1,2])
  )
  exp.triangle <- data.frame(
    x = c(Bbord[1,1]-sep/2*sqrt(3)*2, Bbord[2,1]+sep/2*sqrt(3)*2, Bbord[3,1], Bbord[1,1]-sep/2*sqrt(3)*2),
    y = c(Bbord[1,2]-sep, Bbord[2,2]-sep, Bbord[3,2]+sep*2, Bbord[1,2]-sep)
  )
  
  # Data points projected to 2D
  point_df <- data.frame(x = Bdata[,1], y = Bdata[,2], pred = pred)
  
  # Helper function for axis tick labels
  make_axis_labels <- function(axis, coord_mat, axis_label) {
    data.frame(
      x = coord_mat[,1],
      y = coord_mat[,2],
      label = round(axis_label, 2)
    )
  }
  
  # Generate axis grid positions (L, R, T axes)
  Lpos <- matrix(c(seq(1-Rmin-Tmin, Lmin, by=-sep),
                   rep(Rmin, 11),
                   seq(Tmin, 1-Lmin-Rmin, by=sep)), 11, 3)
  Rpos <- matrix(c(seq(1-Rmin-Tmin, Lmin, by=-sep),
                   seq(Rmin, 1-Lmin-Tmin, by=sep),
                   rep(Tmin, 11)), 11, 3)
  Tpos <- matrix(c(rep(Lmin, 11),
                   seq(1-Lmin-Tmin, Rmin, by=-sep),
                   seq(Tmin, 1-Lmin-Rmin, by=sep)), 11, 3)
  
  B.Lpos <- Lpos %*% Bmatrix
  B.Rpos <- Rpos %*% Bmatrix
  B.Tpos <- Tpos %*% Bmatrix
  
  Lmarkpos <- cbind(B.Lpos[,1] - sep/10, B.Lpos[,2] + sep/10 * sqrt(3))
  Rmarkpos <- cbind(B.Rpos[,1] - sep/10, B.Rpos[,2] - sep/10 * sqrt(3))
  Tmarkpos <- cbind(B.Tpos[,1] + sep/10 * 2, B.Tpos[,2])
  
  # Tick labels for each axis
  Lname <- make_axis_labels("L", Lmarkpos, Lpos[,1])
  Rname <- make_axis_labels("R", Rmarkpos, Rpos[,2])
  Tname <- make_axis_labels("T", Tmarkpos, Tpos[,3])
  
  # Axis names (e.g., sleep / exercise / other)
  axis_labels <- data.frame(
    x = c(Bbord[1,1], Bbord[2,1], Bbord[3,1]),
    y = c(Bbord[1,2], Bbord[2,2], Bbord[3,2]),
    label = colnames(data),
    hjust = c(-1, 1, -1),
    vjust = c(-3, 3, -3),
    angle = c(54, 0, -60)
  )
  
  # Begin plot construction
  p <- ggplot() +
    geom_polygon(data = tar.triangle, aes(x, y), fill = NA, color = "steelblue1", alpha = 0.3) +
    geom_polygon(data = exp.triangle, aes(x, y), fill = NA, color = "white") +
    geom_point(data = point_df, aes(x = x, y = y, color = pred), size = 2, alpha = 0.8) +
    scale_color_gradient(low = scale_color[1], high = scale_color[2]) +
    guides(color = guide_colorbar(title = scale.bar.title)) +
    theme_minimal() +
    labs(title = lab.title, x = "", y = "") +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
    geom_text(data = axis_labels, aes(x = x, y = y, label = label),
              size = 5, hjust = axis_labels$hjust,
              vjust = axis_labels$vjust, angle = axis_labels$angle) +
    geom_segment(aes(x = B.Lpos[,1], y = B.Lpos[,2], xend = Lmarkpos[,1], yend = Lmarkpos[,2]), color = "lightblue") +
    geom_segment(aes(x = B.Rpos[,1], y = B.Rpos[,2], xend = Rmarkpos[,1], yend = Rmarkpos[,2]), color = "lightblue") +
    geom_segment(aes(x = B.Tpos[,1], y = B.Tpos[,2], xend = Tmarkpos[,1], yend = Tmarkpos[,2]), color = "lightblue") +
    geom_text(data = Lname, aes(x, y, label = label), size = 2, hjust = 1, vjust = 0, angle = -60) +
    geom_text(data = Rname, aes(x, y, label = label), size = 2, hjust = 1, vjust = 0, angle = 60) +
    geom_text(data = Tname, aes(x, y, label = label), size = 2, hjust = 0, vjust = 0, angle = 0) +
    geom_segment(aes(x = B.Lpos[2:10,1], y = B.Lpos[2:10,2], xend = B.Rpos[2:10,1], yend = B.Rpos[2:10,2]),
                 color = "lightblue", linetype = 2, alpha = 0.3) +
    geom_segment(aes(x = B.Rpos[2:10,1], y = B.Rpos[2:10,2], xend = B.Tpos[10:2,1], yend = B.Tpos[10:2,2]),
                 color = "lightblue", linetype = 2, alpha = 0.3) +
    geom_segment(aes(x = B.Tpos[2:10,1], y = B.Tpos[2:10,2], xend = B.Lpos[2:10,1], yend = B.Lpos[2:10,2]),
                 color = "lightblue", linetype = 2, alpha = 0.3)
  
  # Optionally show data bounds
  if (!is.null(original.data) && show_boundary) {
    boundary_df <- data.frame(
      x = Blimit[seq(1, 11, 2), 1],
      y = Blimit[seq(1, 11, 2), 2],
      xend = Blimit[seq(2, 12, 2), 1],
      yend = Blimit[seq(2, 12, 2), 2]
    )
    p <- p + geom_segment(data = boundary_df,
                          aes(x = x, y = y, xend = xend, yend = yend),
                          color = boundary.color)
  }
  
  return(p)
}

composition_kernel_smoother=function(data, pred, resolution, 
                                     h, alp){
  # Minimum values for each dimension (used for axis setup)
  Lmin <- min(data[,1])
  Rmin <- min(data[,2])
  Tmin <- min(data[,3])
  
  
  
  grid_points <- expand.grid(
    x = seq(0, 1, by = resolution),
    y = seq(0, 1, by = resolution)
  )
  grid_points$z <- 1 - grid_points$x - grid_points$y
  
  grid_points <- subset(grid_points,x > Lmin & x < 1-Rmin-Tmin&y > Rmin & y < 1-Lmin-Tmin&z > Tmin & z < 1-Lmin-Rmin)
  
  
  output <- apply(grid_points, 1, function(x) {
    distances = apply(data,1,function(u){
      d.kernel(x, u, 1, h)
    })
    weighted_sum <- sum(distances * pred) / sum(distances)
    return(weighted_sum)
  })
  colnames(grid_points) <- colnames(data)
  return(cbind(grid_points, output))
}

CV.Best.h=function(data,pred,alp,opt.interval=c(0.1,0.00001)){
  data=as.matrix(data)
  pred=as.vector(pred)
  CV1=function(h){
    
    n <- nrow(data)
    S <- matrix(0, n,n)
    
    for (i in 1:n) {
      for (j in 1:n) {
        S[i, j] <- d.kernel(data[i,], data[j,], alp, h)
      }
    }
    
    #create an n*n identity matrix
    I <- diag(n) #I_n
    one=matrix(1,n,1)
    S_0=S
    diag(S_0)=0 #S_0
    W=diag(n) #W
    diag(W)=apply(I,2,function(i) (t(i)%*%S%*%one-S[i,i]))
    Wiv=solve(W) #W^-1
    
    y_i= matrix(0,n,1)
    y_i[,1]= pred
    
    (1/n)*t(y_i)%*%t(I-Wiv%*%S_0)%*%(I-Wiv%*%S_0)%*%y_i
    
  }
  result=optimize(CV1, interval = opt.interval)
  result$minimum
}

bootstrapping <- function(data2, data1, obj, alp, h, loops = 1000, B = 1000, cores = 1, tolerance = 1e-6) {
  safe_data1 <- data.frame(lapply(data1, function(x) {
    if (is.factor(x) || is.character(x)) as.numeric(as.factor(x)) else x
  }))
  
  n <- nrow(data2)
  I <- diag(n)
  Wmatrix <- model.matrix(~ . - 1, data = safe_data1)
  
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  on.exit({
    try(stopCluster(cl), silent = TRUE)
  }, add = TRUE)
  
  bootstrap_betas <- foreach(b = 1:B, .combine = rbind,
                             .packages = "base", .export = "d.kernel") %dopar% {
                               tryCatch({
                                 sample_idx <- sample(1:n, n, replace = TRUE)
                                 W_boot <- Wmatrix[sample_idx, , drop = FALSE]
                                 y_boot <- obj[sample_idx]
                                 X_prop_boot <- as.matrix(data2[sample_idx, , drop = FALSE])
                                 n_boot <- nrow(X_prop_boot)
                                 
                                 betahatlist <- matrix(0, loops, ncol(W_boot))
                                 S <- outer(1:n_boot, 1:n_boot, Vectorize(function(i, j) d.kernel(X_prop_boot[i, ], X_prop_boot[j, ], alp, h)))
                                 rs <- rowSums(S)
                                 rs[rs == 0] <- 1e-8  # 防止除以0
                                 Sij <- S / rs
                                 
                                 i <- 0
                                 oldbetahat <- rep(Inf, ncol(W_boot))
                                 backfit.y <- matrix(0, n_boot, 1)
                                 
                                 while (i < loops) {
                                   i <- i + 1
                                   residual.1 <- y_boot - Sij %*% backfit.y
                                   betahat <- solve(t(W_boot) %*% W_boot) %*% t(W_boot) %*% residual.1
                                   residual.2 <- y_boot - W_boot %*% betahat
                                   backfit.y <- residual.2 - mean(residual.2)
                                   betahatlist[i, ] <- betahat
                                   diff <- max(abs(betahat - oldbetahat))
                                   oldbetahat <- betahat
                                   if (diff < tolerance) break
                                 }
                                 
                                 betahatlist[i, ]
                               }, error = function(e) {
                                 cat("Bootstrap iteration", b, "failed: ", e$message, "\n")
                                 rep(NA, ncol(Wmatrix))
                               })
                             }
  
  colnames(bootstrap_betas) <- colnames(Wmatrix)
  
  beta_mean <- colMeans(bootstrap_betas, na.rm = TRUE)
  beta_sd <- apply(bootstrap_betas, 2, sd, na.rm = TRUE)
  T_values <- beta_mean / beta_sd
  p_values <- 2 * pnorm(-abs(T_values))
  significant <- p_values < 0.05
  
  result <- data.frame(
    Variable = colnames(bootstrap_betas),
    Estimate = round(beta_mean, 6),
    SD = round(beta_sd, 6),
    T = round(T_values, 6),
    p_value = round(p_values, 6),
    significant = toupper(significant)
  )
  rownames(result) <- NULL
  result
}

# shiny
shinyServer(function(input, output, session) {
  
  opt.h <- reactiveVal(NULL)
  
  showPlot <- reactiveVal(FALSE)
  observeEvent(input$drawPlot, {
    showPlot(TRUE)
  })
  observeEvent(input$load_example, {
    showPlot(FALSE)
  })
  observeEvent(input$show_upload, {
    showPlot(FALSE)
  })
  observeEvent(input$best.h, {
    showPlot(FALSE)
  })
  
  output$dataInputUI <- renderUI({
    if (input$show_upload) {
      tagList(
        fileInput("file", "Upload CSV File"),
        uiOutput("choose_comp"),
        uiOutput("choose_covariates"),
        uiOutput("choose_y")
      )
    } else {
      actionButton("load_example", "Load Example Data")
    }
  })
  
  output$previewUI <- renderUI({
    if (input$show_upload) {
      tagList(
        h4("Preview of Uploaded Data", style = "font-size: 16px; color: #003366;font-weight: bold;"),
        DT::dataTableOutput("preview"),
        h4("Preview of Selected Data", style = "font-size: 16px; color: #003366;font-weight: bold;"),
        DT::dataTableOutput("choose.preview")
      )
    } else {
      tagList(
        h4("Preview of Example Data", style = "font-size: 16px; color: #003366;font-weight: bold;"),
        DT::dataTableOutput("Exp.preview")
      )
    }
  })
  
  output$ternaryUI <- renderUI({
    if (input$show_upload) {
      tagList(
        h3("Selected Data", style = "font-size: 20px; color: #003366;font-weight: bold;"),
        tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),
        h4("Origninal Data", style = "font-size: 16px; color: #003366;font-weight: bold;"),
        withSpinner(plotOutput("ternaryPlot"), type = 4, color = "#0dc5c1"),
        h4("Kernel Density Estimation", style = "font-size: 16px; color: #003366;font-weight: bold;"),
        withSpinner(plotOutput("K.ternaryPlot"), type = 4, color = "#0dc5c1")
      )
    } else {
      tagList(
        h3("Example Data", style = "font-size: 20px; color: #003366;font-weight: bold;"),
        tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),
        h4("Origninal Data", style = "font-size: 16px; color: #003366;font-weight: bold;"),
        withSpinner(plotOutput("ternaryPlot"), type = 4, color = "#0dc5c1"),
        h4("Kernel Density Estimation", style = "font-size: 16px; color: #003366;font-weight: bold;"),
        withSpinner(plotOutput("K.ternaryPlot"), type = 4, color = "#0dc5c1")
      )
    }
  })
  
  output$hUI <- renderUI({
    if (input$best.h) {
      tagList(
        numericInput("h", "Bandwidth (h):", value = 0.001, min = 1e-10, max = 1, step = 0.00001)
        
      )
    } else {
      tagList(
        h4("Cross-Validation for Best h", style = "color: #003366;"),
        numericInput("top", "upper bound:", value = 0.01, min = 1e-10, max = 1, step = 0.00001),
        numericInput("bot", "lower bound:", value = 0.00001, min = 1e-10, max = 1, step = 0.00001),
        tags$strong("Best Bandwidth (h)"),
        div(
          style = "display: inline-block; vertical-align: top; margin-right: 15px;",
          actionButton("CV.h", "Find Best h")
        ),
        
        div(
          style = "display: inline-block; vertical-align: top;
             border: 2px solid #ccc; padding: 15px; width: 80px;
             border-radius: 6px; background-color: #f9f9f9;",
          textOutput("h_value_display")
        )  
      )
      
    }
  })
  
  output$h_value_display <- renderText({# 確保 h 有值
    if (input$best.h) {
      req(input$h,input$h>0)
      paste("h =", input$h)
    } else {
      req(opt.h())  # 確保 opt.h 有值
      round(opt.h(),6)
    }
  })
  
  CV.Data <- eventReactive(input$CV.h, {
    
    if (input$show_upload) {
      req(selectedData())
      list(
        data = selectedData()[, input$comp_cols],
        pred = selectedData()$Y
      )
    } else {
      req(exampleData())
      list(
        data = exampleData()[, 1:3],
        pred = exampleData()[, 4]
        
      )
    }
  })
  
  
  
  observeEvent(input$CV.h, {
    req(CV.Data())
    showModal(modalDialog("Calculating optimal bandwidth h...", footer = NULL))
    
    #result <- CV.Best.h(CV.Data()$data,
    #CV.Data()$pred,alp=input$alpha,opt.interval=c(input$top,input$bot))
    result<-CV.Best.h(CV.Data()$data,CV.Data()$pred,alp=as.numeric(input$alpha),opt.interval=c(input$top,input$bot))
    
    opt.h(result)
    removeModal()
  })
  
  final.h <- reactive({
    if (input$best.h) {
      req(input$h > 0)
      input$h
    } else {
      req(opt.h())
      opt.h()
    }
  })
  
  
  
  
  
  
  
  #example data
  exampleData <- eventReactive(input$load_example, {
    req(input$show_upload==F)
    X <- rdirichlet(50, c(5, 5, 5))
    x1 <- X[, 1]
    x2 <- X[, 2]
    x3 <- X[, 3]
    f_true = cos(20 * pmin(x1, x2, x3))
    z1 <- rnorm(50, 0.5, 1)  
    z2 <- rnorm(50, 0, 1.5)
    y = 2*z1 + 1*z2 + 5*f_true + rnorm(50, 0, 0.1)
    df <- cbind(x1, x2, x3,y,z1 ,z2)
    colnames(df)<-c("X1","X2","X3","Y","covariate1", "covariate2")
    df <- as.data.frame(df)
    return(df)
  })
  output$Exp.preview <- DT::renderDataTable({
    req(exampleData())
    DT::datatable(head(exampleData(), 10), options = list(scrollX = TRUE))
  })
  
  
  
  # Reactive data
  rawData <- reactive({
    req(input$file,input$show_upload==T)
    read.csv(input$file$datapath,,fileEncoding = "big5")
  })
  
  # 資料預覽
  output$preview <- DT::renderDataTable({
    req(rawData())
    DT::datatable(head(rawData(), 10), options = list(scrollX = TRUE))
  })
  
  # UI: 比例資料欄位選擇
  output$choose_comp <- renderUI({
    req(rawData())
    checkboxGroupInput("comp_cols", "Select Compositional Columns (should sum to 1):",
                       choices = names(rawData()))
  })
  
  
  # UI: 非比例欄位選擇
  output$choose_covariates <- renderUI({
    req(rawData())
    checkboxGroupInput("covariate_cols", "Select Covariate Columns (optional):",
                       choices = names(rawData()))
  })
  
  # UI: 應變數 Y 選擇
  output$choose_y <- renderUI({
    req(rawData())
    selectInput("y_col", "Select Response Variable (Y):",
                choices = names(rawData()))
  })
  
  # 將所選的欄位轉換為數據框
  selectedData <- reactive({
    req(input$comp_cols, input$covariate_cols, input$y_col)
    validate(
      need(length(input$comp_cols) == 3, "You must select 3 compositional variables."),
      need(all(abs(rowSums(rawData()[, input$comp_cols, drop = FALSE]) - 1) < 1e-6),
           "Selected compositional variables do not sum to 1.")
    )
    df <- rawData()
    data.frame(
      cbind(df[, input$comp_cols, drop = FALSE],
            df[, input$covariate_cols, drop = FALSE],
            Y=df[, input$y_col])
    )
  })
  
  # 資料預覽
  output$choose.preview <- DT::renderDataTable({
    req(selectedData())
    DT::datatable(head(selectedData(), 10), options = list(scrollX = TRUE))
  })
  
  plotData <- reactive({
    
    if (input$show_upload) {
      req(selectedData())
      list(
        data = selectedData()[, input$comp_cols],
        pred = selectedData()$Y,
        covariate = selectedData()[, input$covariate_cols, drop = FALSE]
      )
    } else {
      req(exampleData())
      list(
        data = exampleData()[, 1:3],
        pred = exampleData()[, 4],
        covariate = exampleData()[, 5:6, drop = FALSE]
        
      )
    }
  })
  
  output$ternaryPlot <- renderPlot({
    req(showPlot())
    req(plotData())# 確保資料已準備好
    
    if ((input$show_upload && is.null(selectedData())) || 
        (!input$show_upload && is.null(exampleData()))) {
      return(NULL)
    }
    
    draw_composition_plot(
      data = plotData()$data,
      pred = plotData()$pred,
      lab.title = input$title,
      scale_color = c(input$color1, input$color2),
      scale.bar.title = input$scale.bar.title
    )
  })
  
  drawParams <- eventReactive(input$drawPlot, {
    req(plotData())
    h <- final.h()
    req(h > 0)
    
    list(
      h = h,
      res = input$resolution,
      alpha = input$alpha,
      boundary = input$boundary,
      color1 = input$color1,
      color2 = input$color2,
      title = input$scale.bar.title,
      data = plotData()$data,
      pred = plotData()$pred
    )
  })
  output$K.ternaryPlot <- renderPlot({
    req(showPlot(), drawParams())
    
    params <- drawParams()
    
    plotpoints <- composition_kernel_smoother(
      data = params$data,
      pred = params$pred,
      resolution = params$res,
      h = params$h,
      alp = params$alpha
    )
    
    draw_composition_plot(
      plotpoints[, 1:3], plotpoints[, 4],
      original.data = params$data,
      show_boundary = params$boundary,
      scale_color = c(params$color1, params$color2),
      scale.bar.title = params$title
    )
  })
  
  
  
  backfitting <- eventReactive(input$semi.go, {
    req(plotData(),final.h())
    showModal(modalDialog("Running bootstrap analysis, this may take a moment...", footer = NULL))
    boot.data <- plotData()
    boot.h <- final.h()
    df=bootstrapping(
      data2 = boot.data$data,
      data1 = boot.data$covariate,
      obj = boot.data$pred,
      alp = as.numeric(input$alpha),
      h = boot.h,
      B = as.numeric(input$bootstrap.n),
      cores = as.numeric(input$cores),
      tolerance = as.numeric(input$tolerance)
    )
    removeModal()
    return(df)
    
    
  })
  
  
  output$backfittingTable <- DT::renderDataTable({
    req(backfitting())
    DT::datatable(backfitting(), options = list(scrollX = TRUE))
  })
  
  residual.plot <- eventReactive(input$semi.go, {
    req(backfitting())
    
    data1 = plotData()$covariate
    safe_data1 <- data.frame(lapply(data1, function(x) {
      if (is.factor(x) || is.character(x)) as.numeric(as.factor(x)) else x
    }))
    Wmatrix <- model.matrix(~ . - 1, data = safe_data1)
    
    # 確保維度一致
    beta_hat <- backfitting()$Estimate
    if (ncol(Wmatrix) != length(beta_hat)) {
      showNotification("Covariate dimensions do not match estimated coefficients.", type = "error")
      return(NULL)
    }
    
    linear.pred <- Wmatrix %*% as.matrix(beta_hat)
    linear.residual <- plotData()$pred - linear.pred
    
    plotpoints <- composition_kernel_smoother(
      plotData()$data,
      linear.residual,
      resolution = input$semi.resolution,
      h = final.h(),
      alp = input$alpha
    )
    
    draw_composition_plot(
      data = plotpoints[, 1:3],
      pred = plotpoints[, 4],
      lab.title = input$semi.title,
      original.data = plotData()$data,
      show_boundary = input$semi.boundary,
      scale_color = c(input$semi.color1, input$semi.color2),
      scale.bar.title = input$semi.scale.bar.title
    )
  })
  
  
  output$residualPlot <- renderPlot({
    req(residual.plot())
    residual.plot()
  })
  
  
})
