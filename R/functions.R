#' Read Values
#'
#' This is a simple function that provides a prompt to the user to enter in values. These will be 
#' the names of columns for the priority matrix.  No parameters are taken for this function.
#'
#' 
#' @return This function does not return any values.  Instead, it save an array character object based
#' on the user's input.  
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
#' This is a simple function that provides a prompt to the user to enter in priorities. These will be 
#' the names of rows for the priority matrix.  No parameters are taken for this function.
#'
#' 
#' @return This function does not return any values.  Instead, it save an array character object based
#' on the user's input.  
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

valuesImportance <- function(arrayList = c("Contribution","Certainty","Connection")){
  vImport <<- NULL
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    vImportNewLine <- readline()
    vImport <<- as.numeric(c(vImport,vImportNewLine))
  }
}

priorityImportance <- function(arrayList = c("Professional certifications", 
                                             "On the job learning", "Mentorship")){
  pImport <<- NULL
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    pImportNewLine <- readline()
    pImport <<- as.numeric(c(pImport,pImportNewLine))
  }
}

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
  
  data <- createPriorityMatrix(vImport,pImport,priorities,values) %>% 
    mutate(Name = row.names(.)) %>% gather(.,Values,Value,1:(length(values)+1))
  
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

readParetoOptions <- function()
{
  pOptions <<- NULL
  
  cat ("Enter in names of Pareto options separated by commas and spaces (i.e., Option1, Option2, Option3, etc.")
  pOptions <<- readline()
  pOptions <<- unlist(strsplit(pOptions, split=", "))
}

paretoImportance <- function(arrayList = c("Customer 1","Customer 2","Customer 3")){
  poImport <<- NULL
  
  for(i in (1:length(arrayList))){
    cat ("On a scale of 1 to 5, how important is",arrayList[i],"?")
    poImportNewLine <- readline()
    poImport <<- as.numeric(c(poImport,poImportNewLine))
  }
}

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

readStrategyOptions <- function()
{
  pOptions <<- NULL
  
  cat ("Enter in names of strategy options separated by commas and spaces (i.e., Option1, Option2, Option3, etc.")
  sOptions <<- readline()
  sOptions <<- unlist(strsplit(sOptions, split=", "))
}

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
