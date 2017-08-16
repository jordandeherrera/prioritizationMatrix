# List out names of 
readValues()
readPriorities()

# Determine priority values
valuesImportance(values)
priorityImportance(priorities)

# Visualize Priority Matrix with Heatmap
createHeatmap()

# Evaluation option
elearning <- optionImportance(priorities)
saas <- optionImportance(priorities)
realEstate <- optionImportance(priorities)

# Evaluate all options
elearning %>% inner_join(saas) %>% inner_join(realEstate)

# Customer evaluation with Pareto charts
readParetoOptions()
paretoImportance(pOptions)
createParetoChart(pOptions,poImport)

# Create a strategy canvas
readStrategyOptions()
strategyOptionImportance(sOptions)
createStrategyCanvas(sOptions,soImport1,soImport2)
