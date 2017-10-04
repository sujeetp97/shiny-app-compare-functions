library(shiny)
library(compareFunctions)
library(data.table)
library(formatR)

num_box_plots <- 6

shinyServer(function(input, output, session){
  
  num_funcs <- reactiveVal(0)
  summary_table <- NULL
  
  output$function_txt_areas <- renderUI({
    lapply(X = 1:input$num_funcs, FUN = function(X){
      column(width = 6, 
             shiny::textAreaInput(inputId = paste0("func_text_", X), label = paste0("Function ", X), placeholder = "Enter function here", 
                                  value = paste0("function_", X, " <- function(param){ }"), width = '100%', height = '100%')
             )
      
    })
  })
  
  observeEvent(input$load_param, {
    # run parameter expression
    eval(parse(text = input$param_expr), envir = .GlobalEnv)
    # create slider for increments and the run comparison button
    output$num_increments_ui <- renderUI({
      tagList(
        sliderInput(inputId = "num_increments", label = "Choose number of incrememnts", min = 2, max = NROW(param), step = 1, value = 2),
        br(),
        actionButton(inputId = "run_comparison", label = "Run Comparison")
      )
    })
    
  })
  
  observeEvent(input$run_comparison, {
    lapply(X = 1:input$num_funcs, FUN = function(X){
      eval(parse(text = input[[paste0("func_text_", X)]]), envir = .GlobalEnv)
    })
    function_vector <- lapply(X = 1:input$num_funcs, function(X){
      get(paste0("function_", X))
    })
    output$status_text <- renderText("Running Comparison")
    comp_func_res <<- compare_functions(functions = function_vector, param = param, increments = input$num_increments)
    output$status_text <- renderText("Comparison Done!")
    
    get_summary() # This function also sets the value for reactive val num_funcs
    
  })
  
  measure_vars <- c("user_time", "user_rate", "sys_time", "sys_rate", "elapsed_time", "elapsed_rate")
  
  output$summary_ui <- renderUI({
    
    if(num_funcs() != 0){
        tagList(
          h4("Summary Table"),
          dataTableOutput(outputId = "summ_table"),
          h4("Box Plots"),
          fluidRow(
              lapply(X = measure_vars, FUN = function(X){
                column(width = 2, plotOutput(outputId = paste0(X, "_box_plot")))
               
              })
              
          )
          
        )
      }
    
  })
  
  # Rendering all the box plot
  for(index in 1:num_box_plots){
    local({
      i <- index
      output[[paste0(measure_vars[i], "_box_plot")]] <- renderPlot(
        {
          if(num_funcs() != 0){
            get_boxplot(measure_vars[i])
          }
        }
      )
    })
  }
  

  
  
  
  output$summ_table <- renderDataTable({
    get_summary()
  })
  
  
  get_summary <- function(){
    num_funcs(NROW(unique(comp_func_res$function_index)))
    summary_table <- rbind(
      cbind(comp_func_res[, lapply(.SD, mean), by = function_index], as.data.table(list("summ_type" = rep("mean", num_funcs())))),
      cbind(comp_func_res[, lapply(.SD, max), by = function_index], as.data.table(list("summ_type" = rep("max", num_funcs())))),
      cbind(comp_func_res[, lapply(.SD, min), by = function_index], as.data.table(list("summ_type" = rep("min", num_funcs()))))
    )
    summary_table <- summary_table[, .(summ_type, function_index, size, user_time, user_rate, sys_time, sys_rate, elapsed_time, elapsed_rate)]
    setnames(summary_table, old = c("summ_type", "function_index", "size", "user_time", "user_rate", "sys_time", "sys_rate", "elapsed_time", "elapsed_rate"),
             new = c("Type", "Function Index", "Sample Size", "User Time", "User Rate", "System Time", "System Rate", "Elapsed Time", "Elapsed Rate"))
    return(summary_table)
    
  }
  
  get_boxplot <- function(y){
    ggplot(data = comp_func_res, aes(factor(function_index), get(y))) + 
      geom_boxplot() + coord_flip() + 
      geom_boxplot(varwidth = TRUE, fill = "#4D8FAC", color = "black", outlier.shape = 16, outlier.color = "#F62459") +
      theme(panel.background = element_rect(fill = "white")) +
      ylab(label = make_label(y)) + xlab(label = "Function Index")
    
  }
  
  make_label <- function(name){
    return(
      paste(unlist(lapply(X = unlist(strsplit(x = name, split = "_", fixed = TRUE)), FUN = function(X){
        substr(X, 1, 1) <- toupper(substr(X, 1, 1))
        return(X)
      })), 
      collapse = " ")
    )
  }
  
})


