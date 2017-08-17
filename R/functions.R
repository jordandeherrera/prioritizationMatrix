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
#' readValues()
#' Contribution, Certainty, Connection
#' #Value available in global variable values = c("Contribution","Certainty","Connection")
#'
#' @export
readValues <- function()
{
  values <<- NULL
  
  cat ("Enter in names of values separated by commas and spaces (i.e., Goal1, Goal2, Goal3, etc.")
  values <<- readline()
  values <<- unlist(strsplit(values, split=", "))
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
#' readPriorities()
#' Professional certifications, On the job learning, Mentorship
#' #Value available in global variable priorities = c("Professional certifications","On the job learning","Mentorship")
#'
#' @export
readPriorities <- function()
{
  priorities <<- NULL
  
  cat ("Enter in names of priorities separated by commas and spaces (i.e., Priority1, Priority2, etc.")
  priorities <<- readline()
  priorities <<- unlist(strsplit(priorities, split=", "))
}

#' Rank Importance of Values
#'
#' This function assigns relative rank and importance to the values defined in \link{readValues}.  These 
#' numerical assignments are then used by \link{createPriorityMatrix} and \link{createHeatmap}.
#' 
#' @param arrayList a list of values to assign numeric importance.  Can be taken from \link{readValues} output object of \code{values}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' valuesImportance()
#' # On a scale of 1 to 5, how important is Contribution ?
#' 5
#' # On a scale of 1 to 5, how important is Certainty ?
#' 2
#' # On a scale of 1 to 5, how important is Connection ?
#' 5
#' #Value available in global variable vImport = c(5,2,5)
#'
#' @export
valuesImportance <- function(arrayList = c("Contribution","Certainty","Connection")){
  vImport <<- NULL
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    vImportNewLine <- readline()
    vImport <<- as.numeric(c(vImport,vImportNewLine))
  }
}

#' Rank Importance of Priorities
#'
#' This function assigns relative rank and importance to the priorities defined in \link{readPriorities}.  These 
#' numerical assignments are then used by \link{createPriorityMatrix} and \link{createHeatmap}.
#' 
#' @param arrayList a list of values to assign numeric importance.  Can be taken from \link{readPriorities} output object of \code{priorities}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' priorityImportance()
#' # On a scale of 1 to 5, how important is Certifications ?
#' 1
#' # On a scale of 1 to 5, how important is Mentorship ?
#' 4
#' # On a scale of 1 to 5, how important is On the job learning ?
#' 5
#' #Value available in global variable pImport = c(1,4,5)
#'
#' @export
priorityImportance <- function(arrayList = c("Professional certifications", 
                                             "On the job learning", "Mentorship")){
  pImport <<- NULL
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    pImportNewLine <- readline()
    pImport <<- as.numeric(c(pImport,pImportNewLine))
  }
}

#' Create Priority Matrix
#'
#' This function creates a priority matrix that is a result of \link{priorityImportance} and 
#' \link{valuesImportance} for numeric values of \link{readValues} and \link{readPriorities}.
#' 
#' @param vImport a numeric list assigning importance to values.  
#' Can be taken from \link{valuesImportance} output object of \code{vImport}.
#' 
#' @param pImport a numeric list assigning importance to priorities  
#' Can be taken from \link{priorityImportance} output object of \code{pImport}.
#' 
#' @param priorities a character array of priorities.  
#' Can be taken from \link{readPriorities} output object of \code{priorities}.
#' 
#' @param values a character array of values  
#' Can be taken from \link{readValues} output object of \code{values}.
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
  require(dplyr)
  
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
#' @return This function returns a highcharter object (heatmap).
#'
#' @examples
#' createHeatmap()
#'
#' @import highcharter
#' 
#' @import lazyeval
#' 
#' @import tidyr
#'
#' @export
createHeatmap <- function (){
  require(highcharter)
  require(lazyeval)
  require(tidyr)
  
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
  
  endValue <- sum(length(values),1)
  
  data <- createPriorityMatrix(vImport,pImport,priorities,values) %>% 
    mutate(Name = row.names(.)) %>% 
    gather(.,Values,Value,1:endValue)
  
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
#' Can be taken from \link{readPriorities} output object of \code{priorities}.
#' 
#' @return This function returns a data frame to evaluate strategic option alignment with a given set of priorities
#' and values.
#'
#' @examples
#' optionImportance(priorities)
#' # Enter the name of the option being considered:
#' Job from Company A
#' # On a scale of 1 to 5, how much does Job.from.Company.A fulfill Professional certifications ?
#' 1
#' # On a scale of 1 to 5, how much does Job.from.Company.A fulfill On the job learning ?
#' 5
#' # On a scale of 1 to 5, how much does Job.from.Company.A fulfill Mentorship ?
#' 5
#' 
#' @import dplyr
#' 
#' @import lazyeval
#' 
#' @import stringr
#' 
#' @export
optionImportance <- function(arrayList = c("Professional certifications", 
                                             "On the job learning", "Mentorship")){
  # Load required libraries
  require(dplyr)
  require(lazyeval)
  require(stringr)
  
  # Enter in option name
  cat ("Enter the name of the option being considered:")
  optionName <<- str_replace_all(readline()," ",".")
  
  # Set option importance equal to NULL prior to loop
  oImport <<- NULL
  
  # Loop through priority list to evaluate how much the option under evaluation satisfies each criteria
  for(i in (1:length(arrayList))){
    cat("On a scale of 1 to 5, how much does",optionName,"fulfill",arrayList[i],"?")
    oImportNewLine <- readline()
    oImport <<- as.numeric(c(oImport,oImportNewLine))
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
#' on the user's input to the global environment for use in \link{paretoImportance}.  The object is accessible at
#' \code{pOptions}.
#'
#' @examples
#' readParetoOptions()
#' Job 1, Job 2, Job 3
#' #Value available in global variable pOptions = c("Job 1","Job 2","Job 3")
#'
#' @export
readParetoOptions <- function()
{
  pOptions <<- NULL
  
  cat ("Enter in names of Pareto options separated by commas and spaces (i.e., Option1, Option2, Option3, etc.")
  pOptions <<- readline()
  pOptions <<- unlist(strsplit(pOptions, split=", "))
}

#' Rank Importance of Pareto Options
#'
#' This function assigns relative rank and importance to the Pareto options defined in \link{readParetoOptions}.  
#' These numerical assignments are then used by \link{createParetoChart}.
#' 
#' @param arrayList a list of values to assign numeric importance.  Can be taken from \link{readParetoOption} 
#' output object of \code{pOptions}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' paretoImportance(pOptions)
#' # On a scale of 1 to 5, how important is Job 1 ?
#' 1
#' # On a scale of 1 to 5, how important is Job 2 ?
#' 5
#' # On a scale of 1 to 5, how important is Job 3 ?
#' 3
#' #Value available in global variable poImport = c(1,5,3)
#'
#' @export
paretoImportance <- function(arrayList = c("Customer 1","Customer 2","Customer 3")){
  poImport <<- NULL
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    poImportNewLine <- readline()
    poImport <<- as.numeric(c(poImport,poImportNewLine))
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
  require(dplyr)
  require(highcharter)
  
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
#' The object is accessible at \code{sOptions}.
#'
#' @examples
#' readStrategyOptions()
#' Bookkeeping, Internal Controls Analysis, Financial and Business Review
#' #Value available in global variable sOptions = c("Bookkeeping","Internal Controls Analysis", Financial and Business Review")
#'
#' @export
readStrategyOptions <- function()
{
  sOptions <<- NULL
  
  cat ("Enter in names of strategy options separated by commas and spaces (i.e., Option1, Option2, Option3, etc.")
  sOptions <<- readline()
  sOptions <<- unlist(strsplit(sOptions, split=", "))
}

#' Rank Importance of Strategy Options
#'
#' This function assigns relative rank and importance to the strategy options defined in \link{readStrategyOptions}.  
#' These numerical assignments are then used by \link{createStrategyCanvas}.
#' 
#' @param arrayList a list of strategy options to assign numeric importance.  Can be taken from \link{readStrategyOptions} 
#' output object of \code{sOptions}.
#' 
#' @return This function does not return any values.  Instead, it saves an array numeric object based
#' on the user's input to the global environment.
#'
#' @examples
#' strategyOptionImportance(sOptions)
#' @export
strategyOptionImportance <- function(arrayList = c("Option 1","Option 2","Option 3")){
  soImport1 <<- NULL
  soImport2 <<- NULL
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, what are the industry standards for",arrayList[i],"?")
    soImportNewLine <- readline()
    soImport1 <<- as.numeric(c(soImport1,soImportNewLine))
    cat ("On a scale of 1 to 5, what are the ideal differentiated standards for",arrayList[i],"?")
    soImportNewLine <- readline()
    soImport2 <<- as.numeric(c(soImport2,soImportNewLine))
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
  require(dplyr)
  require(highcharter)
  require(tidyr)
  
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
