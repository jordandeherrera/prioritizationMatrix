#' Read Values
#'
#' This is a simple function that provides a prompt to the user to enter in values. These will be 
#' the names of columns for the priority matrix.  No parameters are taken for this function and all input
#' values are taken from the console.
#' 
#' @return This function does not return any values.  Instead, it saves an array character object based
#' on the user's input to the global environment for use in \link{valuesImportance}.
#'
#' @examples
#' \dontrun{
#' readValues()
#' }
#'
#' @export
readValues <- function()
{
  assign("values",NULL,envir = .GlobalEnv)
  
  cat ("Enter in names of values separated by commas and spaces (i.e., Goal1, Goal2, Goal3, etc.")
  assign("values",readline(),envir = .GlobalEnv)
  assign("values",unlist(strsplit(values, split=", ")),envir = .GlobalEnv)
}

#' Read Priorities
#'
#' This is a simple function that provides a prompt to the user to enter in strategic priorities These will be 
#' the names of rows for the priority matrix.  No parameters are taken for this function and all input
#' values are taken from the console.
#' 
#' @return This function does not return any values.  Instead, it saves an array character object based
#' on the user's input to the global environment for use in \link{priorityImportance}.
#'
#' @examples
#' \dontrun{
#' readPriorities()
#' }
#' 
#' @export
readPriorities <- function()
{
  assign("priorities",NULL,envir = .GlobalEnv)
  
  cat ("Enter in names of priorities separated by commas and spaces (i.e., Priority1, Priority2, etc.")
  assign("priorities",readline(),envir = .GlobalEnv)
  assign("priorities",unlist(strsplit(priorities, split=", ")),envir = .GlobalEnv)
}

#' Rank Importance of Values
#'
#' This function assigns relative rank and importance to the values defined in \link{readValues}.  These 
#' numerical assignments are then used by \link{createPriorityMatrix} and \link{createHeatmap}.
#' 
#' @param arrayList a list of values to assign numeric importance.  Can be taken from \link{readValues}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' \dontrun{
#' valuesImportance()
#' }
#'
#' @export
valuesImportance <- function(arrayList = c("Contribution","Certainty","Connection")){
  
  assign("vImport",NULL,envir = .GlobalEnv)
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    vImportNewLine <- readline()
    assign("vImport",as.numeric(c(vImport,vImportNewLine)),envir = .GlobalEnv)
  }
}

#' Rank Importance of Priorities
#'
#' This function assigns relative rank and importance to the priorities defined in \link{readPriorities}.  These 
#' numerical assignments are then used by \link{createPriorityMatrix} and \link{createHeatmap}.
#' 
#' @param arrayList a list of values to assign numeric importance.  Can be taken from \link{readPriorities}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' \dontrun{
#' priorityImportance()
#' }
#'
#' @export
priorityImportance <- function(arrayList = c("Professional certifications", 
                                             "On the job learning", "Mentorship")){
  assign("pImport",NULL,envir = .GlobalEnv)
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    pImportNewLine <- readline()
    assign("pImport",as.numeric(c(pImport,pImportNewLine)),envir = .GlobalEnv)
  }
}

#' Create Priority Matrix
#'
#' This function creates a priority matrix that is a result of \link{priorityImportance} and 
#' \link{valuesImportance} for numeric values of \link{readValues} and \link{readPriorities}.
#' 
#' @param vImport a numeric list assigning importance to values.  
#' Can be taken from \link{valuesImportance}.
#' 
#' @param pImport a numeric list assigning importance to priorities  
#' Can be taken from \link{priorityImportance}.
#' 
#' @param priorities a character array of priorities.  
#' Can be taken from \link{readPriorities}.
#' 
#' @param values a character array of values  
#' Can be taken from \link{readValues}.
#' 
#' @return This function returns a priority matrix as a data frame (values as columns and priorities as rows).  
#' Values in matrix are the result of cross-multiplication.
#'
#' @examples
#' createPriorityMatrix(vImport,pImport,priorities,values)
#'
#' @import dplyr
#'
#' @export
createPriorityMatrix <- function(vImport = c(3,3,5),pImport = c(2,5,5),
                                 priorities = c("Professional certifications", 
                                                "On the job learning", "Mentorship"),
                                 values = c("Contribution","Certainty","Connection")){
  requireNamespace("dplyr")
  
  mat <- matrix(rep(vImport,length(pImport)),nrow = length(pImport),
         ncol = length(vImport), 
         byrow = TRUE,dimnames = c(
           list(priorities),
           list(values)
         ))*
    matrix(rep(pImport,length(vImport)),nrow = length(pImport),
           ncol = length(vImport),
           byrow = FALSE)
  
  df <- as.data.frame(mat) %>% 
    mutate(Total = rowSums(.))
  
  row.names(df) <- priorities
  
  df
}

#' Create Heatmap
#'
#' This function creates a heatmap of the priority matrix available in \link{createPriorityMatrix}.
#' 
#' @param vImport numeric importance assigned to values from \link{valuesImportance} 
#' 
#' @param pImport numeric importance assigned to priorities from \link{priorityImportance}
#' 
#' @param priorities a list of priorities from \link{readPriorities}
#' 
#' @param values a list of values from \link{readValues}
#' 
#' @return This function returns a highcharter object (heatmap).
#'
#' @examples
#' createHeatmap(vImport,pImport,priorities,values)
#'
#' @import highcharter
#' 
#' @import lazyeval
#' 
#' @import tidyr
#'
#' @export
createHeatmap <- function (vImport,pImport,priorities,values){
  requireNamespace("highcharter")
  requireNamespace("lazyeval")
  requireNamespace("tidyr")
  
  hcaes_string <- function(x, y, ...) {
    mapping <- list(...)
    if (!missing(x)) 
      mapping["x"] <- list(x)
    
    if (!missing(y)) 
      mapping["y"] <- list(y)
    
    mapping <- lapply(mapping, function(x) {
      if (is.character(x)) {
        parse(text = x)[[1]]
      }
      else {
        x
      }
    })
    mapping <- structure(mapping, class = "uneval")
    mapping <- mapping[names(mapping) != ""]
    class(mapping) <- c("hcaes", class(mapping))
    mapping
  }
  
  hcaes_ <- hcaes_string
  
  data <- createPriorityMatrix(vImport,pImport,priorities,values) %>% 
    mutate(Name = row.names(.)) %>% 
    gather(.,Values,Value,1:length(values)+1)
  
  hc <- hchart(data, "heatmap", hcaes_(x = "Values", y = "Name", 
                                       value = "Value")) %>% 
    hc_xAxis(title = list(text = "Values")) %>% 
    hc_yAxis(title = list(text = "Priorities"), reversed = TRUE, 
             offset = -20, tickLength = 0, gridLineWidth = 0, 
             minorGridLineWidth = 0, labels = list(style = list(fontSize = "8px"))) %>% 
    hc_plotOptions(series = list(boderWidth = 0, dataLabels = list(enabled = TRUE, 
                                                                   format = "{point.value:,.0f}")), stacking = "normal") %>% 
    hc_title(text = "Priority Matrix") %>% 
    hc_subtitle(text = "A priority matrix shows relative importance of priorities aligned with values") %>% 
    hc_legend(layout = "vertical", verticalAlign = "top", 
              align = "right", valueDecimals = 0)
  return(hc)
}

#' Evaluate a Strategic Option Against Priority Matrix
#'
#' This function determines whether a given strategic option is aligned with priorities as defined by the
#' \link{createPriorityMatrix} and \link{createHeatmap}.
#' 
#' @param arrayList a list of priorities to evaluate numeric significance for a given strategic option.  
#' Can be taken from \link{readPriorities}.
#' 
#' @return This function returns a data frame to evaluate strategic option alignment with a given set of priorities
#' and values.
#' 
#' @import dplyr
#' 
#' @import lazyeval
#' 
#' @import stringr
#' 
#' @importFrom stats na.omit
#' 
#' @importFrom stats setNames
#' 
#' @examples
#' \dontrun{
#' optionImportance(priorities)
#' }
#' 
#' @export
optionImportance <- function(arrayList = c("Professional certifications", 
                                             "On the job learning", "Mentorship")){
  # Load required libraries
  requireNamespace("dplyr")
  requireNamespace("lazyeval")
  requireNamespace("stringr")
  
  # Enter in option name
  cat ("Enter the name of the option being considered:")
  optionName <- str_replace_all(readline()," ",".")
  
  # Set option importance equal to NULL prior to loop
  oImport <- NULL
  
  # Loop through priority list to evaluate how much the option under evaluation satisfies each criteria
  for(i in (1:length(arrayList))){
    cat("On a scale of 1 to 5, how much does",optionName,"fulfill",arrayList[i],"?")
    oImportNewLine <- readline()
    oImport <- as.numeric(c(oImport,oImportNewLine))
  }
  
  # Remove NA values
  oImport <- as.numeric(na.omit(oImport))
  
  # Create a priority matrix and determing total for option being evaluated
  options <- createPriorityMatrix(vImport,pImport,priorities,values) %>% 
    mutate(Names = row.names(.)) %>% select(Names,Total)
  
  # Create lazy eval option
  mutate_call1 = lazyeval::interp(~a*b, 
                                  a = as.name("Total"), b = oImport)
  
  # Modify data frame for evaluation criteria according to prioritzation matrix
  options <- options %>%
    mutate_(.dots = setNames(list(mutate_call1), optionName))
  
  # Return options data frame and add a total at the bottom
  rbind(options,data.frame(options %>% 
                             summarise_if(is.numeric,sum,na.rm = T) %>% 
          ungroup() %>% 
          mutate(Names = "Total") %>% 
          select(3,1,2))
  )
}

#' Read Pareto Options
#'
#' This is a simple function that provides a prompt to the user to enter in Pareto strategic options. These
#' will be the x-axis of a Pareto chart in \link{createParetoChart}.
#' 
#' @return This function does not return any values.  Instead, it saves an array character object based
#' on the user's input to the global environment for use in \link{paretoImportance}.
#'
#' @examples
#' \dontrun{
#' readParetoOptions()
#' }
#'
#' @export
readParetoOptions <- function()
{
  assign("pOptions",NULL, envir = .GlobalEnv)
  
  cat ("Enter in names of Pareto options separated by commas and spaces (i.e., Option1, Option2, Option3, etc.")
  assign("pOptions",readline(),envir = .GlobalEnv)
  assign("pOptions",unlist(strsplit(pOptions, split=", ")),envir = .GlobalEnv)
}

#' Rank Importance of Pareto Options
#'
#' This function assigns relative rank and importance to the Pareto options defined in \link{readParetoOptions}.  
#' These numerical assignments are then used by \link{createParetoChart}.
#' 
#' @param arrayList a list of values to assign numeric importance.  Can be taken from \link{readParetoOptions}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' \dontrun{
#' paretoImportance(pOptions)
#' }
#'
#' @export
paretoImportance <- function(arrayList = c("Customer 1","Customer 2","Customer 3")){
  assign("poImport",NULL,envir = .GlobalEnv)
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    poImportNewLine <- readline()
    assign("poImport",as.numeric(c(poImport,poImportNewLine)),envir = .GlobalEnv)
  }
}

#' Create Pareto Chart
#'
#' This function creates a Pareto chart of the Pareto options evaluated in \link{paretoImportance}.
#' 
#' @param pOptions The names of the Pareto options in \link{readParetoOptions}.
#' 
#' @param poImport The numeric values assigned to each Pareto option in \link{paretoImportance}.
#' 
#' @return This function returns a Pareto chart as a highcharter object (column and line charts).
#'
#' @examples
#' createParetoChart(pOptions,poImport)
#'
#' @import dplyr
#' 
#' @import highcharter
#'
#' @export
createParetoChart <- function(pOptions = c("Customer 1","Customer 2","Customer 3"),poImport = c(5,3,1)){
  options(warn=-1)
  requireNamespace("dplyr")
  requireNamespace("highcharter")
  
  data <- data.frame(Names = pOptions,
                    Values = poImport) %>%
    mutate(Values = Values / 5 * 100) %>%
    arrange(desc(Values)) %>%
    mutate(CumTotal = cumsum(Values)) %>%
    mutate(Total = sum(Values)) %>%
    mutate(Pct = CumTotal / Total*100)
  
  hc <- hchart(data, "column", hcaes(x = Names, y = Values)) %>% 
    hc_xAxis(title = list(text = "Priorities")) %>% 
    hc_yAxis(title = list(text = "Importance"), reversed = FALSE, 
             offset = -20, tickLength = 0, gridLineWidth = 0, 
             minorGridLineWidth = 0, labels = list(style = list(fontSize = "8px"))) %>% 
    hc_plotOptions(series = list(boderWidth = 0, dataLabels = list(enabled = TRUE, 
                                                                   format = "{point.y:,.0f}")), stacking = "normal") %>% 
    hc_title(text = "Pareto Chart") %>% 
    hc_subtitle(text = "A pareto chart shows relative importance of competing priorities") %>% 
    hc_add_series_df(data = data,x = Names, y = Pct,type="line")
  return(hc)
}

#' Read Strategy Options
#'
#' This is a simple function that provides a prompt to the user to enter in strategic options. These
#' will be the x-axis of a strategy canvas in \link{createStrategyCanvas}.
#' 
#' @return This function does not return any values.  Instead, it saves an array character object based
#' on the user's input to the global environment for use in \link{strategyOptionImportance}.
#'
#' @examples
#' \dontrun{
#' readStrategyOptions()
#' }
#' 
#' @export
readStrategyOptions <- function()
{
  assign("sOptions",NULL,envir = .GlobalEnv)
  
  cat ("Enter in names of strategy options separated by commas and spaces (i.e., Option1, Option2, Option3, etc.")
  assign("sOptions",readline(),envir = .GlobalEnv)
  assign("sOptions",unlist(strsplit(sOptions, split=", ")),envir = .GlobalEnv)
}

#' Rank Importance of Strategy Options
#'
#' This function assigns relative rank and importance to the strategy options defined in \link{readStrategyOptions}.  
#' These numerical assignments are then used by \link{createStrategyCanvas}.
#' 
#' @param arrayList a list of strategy options to assign numeric importance.  Can be taken from \link{readStrategyOptions}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' strategyOptionImportance(sOptions)
#' @export
strategyOptionImportance <- function(arrayList = c("Option 1","Option 2","Option 3")){
  assign("soImport1",NULL,envir = .GlobalEnv)
  assign("soImport2",NULL,envir = .GlobalEnv)
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, what are the industry standards for",arrayList[i],"?")
    soImportNewLine <- readline()
    assign("soImport1",as.numeric(c(soImport1,soImportNewLine)),envir = .GlobalEnv)
    cat ("On a scale of 1 to 5, what are the ideal differentiated standards for",arrayList[i],"?")
    soImportNewLine <- readline()
    assign("soImport2",as.numeric(c(soImport2,soImportNewLine)),envir = .GlobalEnv)
  }
}

#' Create Strategy Canvas
#'
#' This function creates a strategy canvas of the strategy options evaluated in \link{strategyOptionImportance}.
#' 
#' @param sOptions The names of the strategy options in \link{readStrategyOptions}.
#' 
#' @param soImport1 The numeric values assigned to each strategy option relative to industry standards
#' in \link{strategyOptionImportance}.
#' 
#' @param soImport2 The numeric values assigned to each strategy option relative to differentiated standards
#' in \link{strategyOptionImportance}.
#' 
#' @return This function returns a strategy canvas as a highcharter object (two line charts).
#'
#' @examples
#' createStrategyCanvas(sOptions,soImport1,soImport2)
#'
#' @import dplyr
#' 
#' @import highcharter
#' 
#' @import tidyr
#'
#' @export
createStrategyCanvas <- function(sOptions = c("Option 1","Option 2","Option 3"),soImport1 = c(5,3,1),
                                 soImport2 = c(1,3,5)){
  requireNamespace("dplyr")
  requireNamespace("highcharter")
  requireNamespace("tidyr")
  
  data <- data.frame(Names = sOptions,
                     Industry = soImport1,
                     Blue.Ocean = soImport2) %>%
    arrange(desc(Blue.Ocean)) %>%
    gather(Category,Value,c(Industry,Blue.Ocean))
  
  hc <- hchart(data, "line", hcaes(x = Names, y = Value, group = Category)) %>% 
    hc_xAxis(title = list(text = "Strategic Options")) %>% 
    hc_yAxis(title = list(text = "Importance"), reversed = FALSE, 
             offset = -20, tickLength = 0, gridLineWidth = 0, 
             minorGridLineWidth = 0, labels = list(style = list(fontSize = "8px"))) %>% 
    hc_plotOptions(series = list(boderWidth = 0, dataLabels = list(enabled = TRUE, 
                                                                   format = "{point.y:,.0f}")), stacking = "normal") %>% 
    hc_title(text = "Strategy Canvas") %>% 
    hc_subtitle(text = "A strategy canvas shows points of differentiation and purposeful strategic variance") 
  return(hc)
}

#' Sample Priorities
#'
#' An example of priority inputs.
#' 
#' @examples
#' \dontrun{
#'  priorities
#' }
"priorities"

#' Sample Values
#'
#' An example of value inputs.
#' 
#' @examples
#' \dontrun{
#'  values
#' }
"values"

#' Sample Priority Values
#'
#' An example of priority numeric values.
#' 
#' @examples
#' \dontrun{
#'  pImport
#' }
"pImport"

#' Sample Pareto Options
#'
#' An example of Pareto options.
#' 
#' @examples
#' \dontrun{
#'  pOptions
#' }
"pOptions"

#' Sample Pareto Option Values
#'
#' An example of Pareto option values.
#' 
#' @examples
#' \dontrun{
#'  poImport
#' }
"poImport"

#' Sample Strategic Options
#'
#' An example of strategic options.
#' 
#' @examples
#' \dontrun{
#'  sOptions
#' }
"sOptions"

#' Sample Strategic Options - Industry Importance
#'
#' An example of strategic options importance according to industry.
#' 
#' @examples
#' \dontrun{
#'  soImport1
#' }
"soImport1"

#' Sample Strategic Options - Differentiated Importance
#'
#' An example of strategic options importance according to a differentiated competitor.
#' 
#' @examples
#' \dontrun{
#'  soImport2
#' }
"soImport2"

#' Sample Value Importance
#'
#' An example of values importance.
#' 
#' @examples
#' \dontrun{
#'  vImport
#' }
"vImport"
