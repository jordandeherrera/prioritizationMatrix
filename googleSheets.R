
# Create Heatmap Function -------------------------------------------------


createHeatmap <- function (vImport, pImport, priorities, values) {
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
  data <- createPriorityMatrix(vImport, pImport, priorities, 
                               values) %>% mutate(Name = row.names(.)) %>% gather(., 
                                                                                  Values, Value, -contains("Name"))
  hc <- hchart(data, "heatmap", hcaes(x = "Values", y = "Name", 
                                      value = "Value")) %>% hc_xAxis(title = list(text = "Values")) %>% 
    hc_yAxis(title = list(text = "Priorities"), reversed = TRUE, 
             offset = -20, tickLength = 0, gridLineWidth = 0, 
             minorGridLineWidth = 0, labels = list(style = list(fontSize = "8px"))) %>% 
    hc_plotOptions(series = list(boderWidth = 0, dataLabels = list(enabled = TRUE, 
                                                                   format = "{point.value:,.0f}")), stacking = "normal") %>% 
    hc_title(text = "Priority Matrix") %>% hc_subtitle(text = "A priority matrix shows relative importance of priorities aligned with values") %>% 
    hc_legend(layout = "vertical", verticalAlign = "top", 
              align = "right", valueDecimals = 0)
  return(hc)
}


# Create Screenshot -------------------------------------------------------

createScreenshot <- function(htmlWidget,directoryName){
  require(highcharter)
  require(webshot)
  require(dplyr)
  require(htmltools)
  
  tagList(htmlWidget) %>%
    html_print %>%
    # get forward slash on windows
    normalizePath(.,winslash="/") %>%
    # replace drive:/ with drive:// so C:/ becomes C://
    gsub(x=.,pattern = ":/",replacement="://") %>%
    # appends file:/// to make valid uri
    paste0("file:///",.) %>%
    # screenshot it for lots of good reasons
    webshot(file = paste0(directoryName,"/image.png"), delay = 3)
}


# Create Prioritization Matrix --------------------------------------------

createPrioritizationMatrix <- function(sheetName, directoryName){

  requireNamespace("priorityMatrix")
  requireNamespace("googlesheets")
  requireNamespace("tidyverse")
  requireNamespace("googledrive")
  
  gs_url("https://docs.google.com/spreadsheets/d/1mZkcyQ02R1UnGZXOSmfPBbcfmQm0tx8elbeTFbLf1jA/edit#gid=0") ->
    inputSheet
  
  inputSheet %>%
    gs_read(ws = sheetName) ->
    ss
  
  ss %>%
    filter(Category == "Priority") ->
    p
  
  ss %>%
    filter(Category == "Value") ->
    v
  
  createHeatmap(vImport = as.numeric(v$Value),
                pImport = as.numeric(p$Value),
                priorities = as.character(p$Name),
                values = as.character(v$Name)) ->
    hc
  
  createScreenshot(hc, directoryName)
  
  googledrive::drive_upload(paste0(directoryName,"/image.png"),
                            path = "Framework/img",
                            name = paste0(sheetName,".png"))
  
  googledrive::drive_find(pattern = paste0(sheetName,".png"),
                            n_max = 1) %>%
    collect %>% .[["id"]] ->
    picID
  
  gs_edit_cells(inputSheet, ws = sheetName, anchor = "E12",
                input = paste0('=IMAGE("https://drive.google.com/uc?export=download&id=',
                               picID,'",2)')
  )
  
  return(hc)
}


# Sample Work -------------------------------------------------------------

createPrioritizationMatrix("Example","sample")
createPrioritizationMatrix("Business","sample")
createPrioritizationMatrix("Move","sample")

createScreenshot(hc, "sample")

x <- gs_edit_cells(ss=inputSheet, ws="Example", anchor = "E1",
                   input='=IMAGE("https://i.stack.imgur.com/FrDZc.jpg", 4, 800, 1200)'
                   input='=IMAGE("https://i.stack.imgur.com/FrDZc.jpg", 4, 800, 1200)'
                   )
