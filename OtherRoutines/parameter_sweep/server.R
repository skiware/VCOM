#############################################
######VCOM Model Output Web Application######
######Sean Wu 8/19/2016######################
#############################################

library(shiny)
library(htmlwidgets)
library(D3TableFilter)

tbl <- readRDS("tmp.rds")
tbl <- tbl[,-grep("time",colnames(tbl))]
tbl <- tbl[1:500,]

shinyServer(function(input,output,session){
  output$vcom <- renderD3tf({
    
    table_Props <- list(
      #appearance
      btn_reset = TRUE,
      btn_reset_text = "Reset",
      #behavior
      on_change = TRUE,  
      btn = FALSE,  
      enter_key = TRUE,  
      on_keyup = TRUE,  
      on_keyup_delay = 1500,
      highlight_keywords = TRUE,  
      loader = TRUE,  
      loader_text = "Filtering data...",
      # sorting
      sort = TRUE,
      sort_config = list(
        sort_types = c("String","String",rep("US",16))
      ),
      # paging
      paging = FALSE
    )
    
    bgColScales <- list(
      col_0 = "auto:white",
      col_1 = "auto:white",
      col_2 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#edf8fb","#ccece6","#99d8c9","#66c2a4","#2ca25f","#006d2c"]);
                  return color(i);
      }'),
      col_3 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8856a7","#810f7c"]);
                  return color(i);
      }'),
      col_4 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#f0f9e8","#ccebc5","#a8ddb5","#7bccc4","#43a2ca","#0868ac"]);
                  return color(i);
      }'),
      col_5 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#fef0d9","#fdd49e","#fdbb84","#fc8d59","#e34a33","#b30000"]);
                  return color(i);
      }'),
      col_6 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#f1eef6","#d0d1e6","#a6bddb","#74a9cf","#2b8cbe","#045a8d"]);
                  return color(i);
      }'),
      col_7 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#f6eff7","#d0d1e6","#a6bddb","#67a9cf","#1c9099","#016c59"]);
                  return color(i);
      }'),
      col_8 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#f1eef6","#d4b9da","#c994c7","#df65b0","#dd1c77","#980043"]);
                  return color(i);
      }'),
      col_9 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                  .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                  .range(["#feebe2","#fcc5c0","#fa9fb5","#f768a1","#c51b8a","#7a0177"]);
                  return color(i);
      }'),
      col_10 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                 .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                 .range(["#ffffcc","#d9f0a3","#addd8e","#78c679","#31a354","#006837"]);
                 return color(i);
      }'),
      col_11 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                 .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                 .range(["#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494"]);
                 return color(i);
      }'),
      col_12 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                 .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                 .range(["#ffffd4","#fee391","#fec44f","#fe9929","#d95f0e","#993404"]);
                 return color(i);
      }'),
      col_13 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                 .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                 .range(["#ffffb2","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026"]);
                 return color(i);
      }'),
      col_14 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                 .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                 .range(["#eff3ff","#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c"]);
                 return color(i);
      }'),
      col_15 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                 .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                 .range(["#edf8e9","#c7e9c0","#a1d99b","#74c476","#31a354","#006d2c"]);
                 return color(i);
      }'),
      col_16 = JS('function colorScale(tbl,i){
                  var color = d3.scale.ordinal()
                 .domain([0.0,0.2,0.4,0.6,0.8,1.0])
                 .range(["#f7f7f7","#d9d9d9","#bdbdbd","#969696","#636363","#252525"]);
                 return color(i);
      }'),
      col_17 = JS('function colorScale(tbl,i){
                  var color = d3.scale.linear()
                  .domain([0,50])
                  .range(["white","orangered"]);
                  return color(i);
      }')
    )
    
    d3tf(tbl, table_Props, enableTf = TRUE,
         showRowNames = FALSE, tableStyle = "table table-condensed", 
         bgColScales = bgColScales)
    
  })
})