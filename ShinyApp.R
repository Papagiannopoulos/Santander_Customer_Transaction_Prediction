
#Satander Costumers
options(digits = 8)
#### Libraries & Path ####
library(shiny); library(shinyWidgets); #library(shinydashboard); 
library(shinythemes); library(leaflet); #library(shinyjs); 
library(ggplot2); library(ggpubr)
library(tidyverse)
library(stringr);
library(xlsx); library(readxl)
library(corrplot)

#### Functions ####
data_overview <- function(x, N_outliers){
  continuous <- x[sapply(x[1:100,], is.numeric)]
  categorical <- x[!sapply(x[1:100,], is.numeric)]
  digit <- 2
  
  #Metrics for Continuous features
  unscaled_continuous <- continuous
  scaled_continuous <- as.data.frame(apply(continuous, 2, scale))
  continuous <- data.frame(feature = names(scaled_continuous),
                           `Length of unique values` = apply(scaled_continuous, 2, function(x){length(unique(x))}),
                           Skew = apply(scaled_continuous, 2, skewness, na.rm = T),
                           MeanValues = apply(unscaled_continuous, 2, mean, na.rm = T),
                           Median = apply(scaled_continuous, 2, median, na.rm = T),
                           SD = apply(unscaled_continuous, 2, sd, na.rm = T),
                           Q2.2 = apply(scaled_continuous, 2, quantile, na.rm = T, 0.0225),
                           Q97.8 = apply(scaled_continuous, 2, quantile, na.rm = T, 0.9775),
                           Min = apply(scaled_continuous, 2, min, na.rm = T),
                           Max = apply(scaled_continuous, 2, max, na.rm = T),
                           `Mean of 2000 min values` = apply(scaled_continuous, 2, function(x){mean(sort(x, decreasing = F)[1:N_outliers])}),
                           `Mean of 2000 max values` = apply(scaled_continuous, 2, function(x){mean(sort(x, decreasing = T)[1:N_outliers])}),
                           `Number of Missing` = apply(scaled_continuous, 2, function(x){sum(is.na(x))/length(x)*100})
  )
  continuous <- continuous %>% arrange(desc(Max), desc(Min))
  continuous[sapply(continuous, is.numeric)] <- apply(continuous[sapply(continuous, is.numeric)], 2, round, digit)
  
  #Metrics for Categorical features
  categorical <- data.frame(feature = names(categorical),
                            `Number of Categories` = apply(categorical, 2, function(x){length(unique(x))}),
                            Categories = apply(categorical, 2, function(x){paste0(unique(x), collapse = ", ")}),
                            
                            `Number of Missing` = apply(categorical, 2, function(x){sum(is.na(x))/length(x)*100})
  )
  #Return list
  return(list(scaled_cont = continuous, cat = categorical))
}
violin_plot <- function(data, x = "0", y = y, col = "orange1"){
  val <- case_when(x %in% "0" ~ mean(data[, which(names(data) %in% y)]),
                   .default = 0)
  ggplot(data, aes_string(x = x, y = y)) + 
    geom_violin(fill = col)+
    geom_abline(slope = 0, intercept = val, lty = 2, lwd = 0.8, col = "red")+
    geom_abline(slope = 0, intercept = c(0,-2,2,-3.23,3.23), lty = 2, lwd = 0.8)+
    labs(y="Scaled Values", x = y)+ theme_bw()+
    theme(axis.text = element_text(size=12, face = "bold"), axis.title=element_text(size=14))
}
histogram <- function(data, x, fill){
  ggplot(data, aes_string(x=x))+
    geom_density(aes_string(fill=fill),alpha=0.7)+
    labs(x=x,y="Density")+
    theme_bw()+
    theme(axis.text= element_text(size=12, face = "bold"),axis.title=element_text(size=14),
          legend.text = element_text(size = 12, face = "bold"))
}

#### Import Data & Processing ####
train <- read.csv("train.csv")
test <- read.csv("test.csv")
test <- test %>% mutate(target = NA, .after = ID_code)
satander <- rbind(train, test)
satander <- satander %>% mutate(target = as.character(target))

#Descriptives
descr_all <- satander %>% data_overview(2000)
descr_tr <- satander %>% filter(!is.na(target)) %>% data_overview(2000)
descr_ts <- satander %>% filter(is.na(target)) %>% data_overview(2000)
descr_tr_0 <- satander %>% filter(target == 0) %>% data_overview(2000)
descr_tr_1 <- satander %>% filter(target == 1) %>% data_overview(2000)
descr_all <- descr_all$scaled_cont
descr_tr <- descr_tr$scaled_cont
descr_ts <- descr_ts$scaled_cont
descr_tr_0 <- descr_tr_0$scaled_cont
descr_tr_1 <- descr_tr_1$scaled_cont

# Shiny App ####
#Create descriptive table for Shiny
data <- list(All = descr_all, Train = descr_tr, Test = descr_ts, Train_0 = descr_tr_0, Train_1 = descr_tr_1)

dataset <- names(data)
compare <- c("Train VS Test", "Train (target = 0) VS Train (target = 1)")
vars <- descr_all$feature[order(as.numeric(str_remove(descr_all$feature, "var_")))]
metrics <- names(descr_all)[-1]

#Frontend
ui <- fluidPage(theme = shinytheme("united"),
                sidebarLayout(position = "left",
                  sidebarPanel(width = 2,
                               #Search Dataset
                               pickerInput("dataset", "Dataset",
                                           choices = dataset, multiple = F,selected = dataset[1],
                                           options=pickerOptions(liveSearch=F,liveSearchNormalize=F,
                                                                 liveSearchStyle="contains",
                                                                 noneResultsText="No results found")),
                               #Choose Variables
                               pickerInput("vars",label = "Features",
                                           choices = vars, multiple = T, selected = "var_0",
                                           options=pickerOptions(liveSearch=TRUE,liveSearchNormalize=F,
                                                                 liveSearchStyle="contains",
                                                                 noneResultsText="No results found")),
                               #Compare Datasets
                               pickerInput("compare","Compare",
                                           choices = compare,  multiple = F, selected = compare[1],
                                           options=pickerOptions(liveSearch=F,liveSearchNormalize=F,
                                                                 liveSearchStyle="contains",
                                                                 noneResultsText="No results found")),
                               #Metrics
                               selectInput("select", "Metrics",
                                           choices = metrics, multiple = F, selected = "Mean"),
                               #Sort of Table
                               radioButtons("sort", h3("Sort"),
                                            choices = c("Descending", "Ascending"),selected = "Descending")),
                  
                  mainPanel(width=10, fluidRow(tabsetPanel(tabPanel("EDA - Total Descriptives",
                                                                    column(6, splitLayout(plotOutput("figures1", height = "720px"))),
                                                                    column(2, tableOutput("DescTable1"))),
                                                          tabPanel("EDA - Dataset Comparisons",
                                                                   column(7, splitLayout(plotOutput("figures2", height = "720px"))),
                                                                   column(3, tableOutput("DescTable2"))),
                                                          tabPanel("EDA - Variable Comparisons",
                                                                   column(5, splitLayout(plotOutput("figures3a"))),
                                                                   column(5, splitLayout(plotOutput("figures3b"))),
                                                                   br(),
                                                                   plotOutput("figures3c")),
                                                          tabPanel("EDA - Variable Correlations",
                                                                   splitLayout(plotOutput("figures4",width = "99%", height = "780px")))
                                                          ))
                  )
                )
)

#Back-end
server <- function(input, output, session){
  #Search Dataset
  observeEvent(input$dataset,{
    updatePickerInput(session=session, inputId = "dataset",label="Dataset",
                      choices = dataset, selected = input$dataset, clearOptions=F)
  }, ignoreInit = T, ignoreNULL = FALSE)
  
  #Choose Variables
  observeEvent(input$compare,{
    updatePickerInput(session=session, inputId = "compare",label="Compare",
                      choices = compare,  selected = input$compare, clearOptions=F)
  }, ignoreInit = T, ignoreNULL = FALSE)
  
  #Compare Datasets
  observeEvent(input$vars,{
    if(is.null(input$vars)){
      input$vars <- vars[1]
    }
    updatePickerInput(session=session, inputId = "vars",label="Features",
                      choices = c(input$vars, vars[!(vars %in% input$vars)]), selected = input$vars, clearOptions=F)
  }, ignoreInit = T, ignoreNULL = FALSE)

  #Metrics
  observeEvent(input$select,{
    choice1 <- input$select
    updateSelectInput(session=session,
                      inputId = "select",label="Metrics",
                      choices = metrics,
                      selected = choice1)
  }, ignoreInit = T, ignoreNULL = FALSE)
  
  #Sort of Table 2
  observeEvent(input$sort,{
    choice1 <- input$sort
    updateRadioButtons(session=session,
                      inputId = "sort",label="Sort",
                      choices =  c("Descending", "Ascending"),
                      selected = choice1[1])
  }, ignoreInit = T, ignoreNULL = FALSE)
  
  Rempty <- reactive(({list()}))
  R1 <- reactive({list(input$dataset)})
  R2 <- reactive({list(input$compare, input$select, input$sort)})
  R3a <- reactive({list(input$compare)})
  R3b <- reactive({list(input$vars)})
  
  #Panel 1
  #Plot
  observeEvent(R1(), {
    d <- data[[which(names(data) %in% input$dataset)]]
    output$figures1 <- renderPlot({
      ggarrange(violin_plot(d, y = "Median"), violin_plot(d, y = "Q2.2") + theme(axis.title.y = element_blank()),
                violin_plot(d, y = "Q97.8") + theme(axis.title.y = element_blank()),
                violin_plot(d, y = "SD") + labs(y = "Values"),
                violin_plot(d, y = "Skew"), 
                violin_plot(d, y = "Mean.of.2000.min.values") + theme(axis.title.y = element_blank()),
                violin_plot(d, y = "Mean.of.2000.max.values") + theme(axis.title.y = element_blank()), 
                violin_plot(d, y = "Length.of.unique.values") + theme(axis.title.y = element_blank()), nrow = 2, ncol = 4)
    })
  })
  #Descriptive table
  observeEvent(R1(), {
    d <- data[[which(names(data) %in% input$dataset)]]
    output$DescTable1 <- renderTable({
      d[d$feature %in% input$vars, ]
    })
  })
  
  #Panel 2
  #Plot
  observeEvent(R2(), {
    d1 <- data.frame(rbind(descr_tr, descr_ts)) %>% 
      mutate(cat= as.factor(rep(c("Train", "Test"), times = 1, each = nrow(descr_tr))))
    d2 <- data.frame(rbind(descr_tr_0, descr_tr_1)) %>% 
      mutate(cat= as.factor(rep(c("Train_0", "Train_1"), times = 1, each = nrow(descr_tr_0))))
    d <- case_when(input$compare %in% compare[1] ~ d1, .default = d2)
    
    output$figures2 <- renderPlot({
      ggarrange(violin_plot(d, y = "Median", x = "cat"),
                violin_plot(d, y = "Q2.2", x = "cat") + theme(axis.title.y = element_blank()),
                violin_plot(d, y = "Q97.8", x = "cat") + theme(axis.title.y = element_blank()),
                violin_plot(d, y = "SD", x = "cat") + labs(y = "Values"),
                violin_plot(d, y = "Skew", x = "cat"),
                violin_plot(d, y = "Mean.of.2000.min.values", x = "cat") + theme(axis.title.y = element_blank()),
                violin_plot(d, y = "Mean.of.2000.max.values", x = "cat") + theme(axis.title.y = element_blank()), 
                violin_plot(d, y = "Length.of.unique.values", x = "cat") + theme(axis.title.y = element_blank()), nrow = 2, ncol = 4)
    })
  })
  #Descriptive table
  observeEvent(R2(), {
    d1 <- merge(descr_tr, descr_ts, by = "feature", all = T, sort = F) %>% data.frame()
    d2 <- merge(descr_tr_0, descr_tr_1, by = "feature", all = T, sort = F) %>% data.frame()
    d <- case_when(input$compare %in% compare[1] ~ d1, .default = d2)
    cols <- case_when(input$compare %in% compare[1] ~ c("Train", "Test"), .default = c("Train_0", "Train_1"))
    d <- d[, c(1, which(grepl(input$select, names(d), fixed = T)))]
    names(d) <- c("Feature", cols)
    d$Differences <- d[,2] - d[,3]

        output$DescTable2 <- renderTable({
      case_when(input$sort %in% "Descending" ~ d %>% arrange(desc(Differences)), .default = d %>% arrange(Differences))
    }, rownames = F)
  })
  
  #Panel 3
  #Plot
  observeEvent(R3a(), {
    d1 <- satander %>% mutate(cat = factor(case_when(is.na(target) ~ 1, .default = 0), c(0,1), c("Train", "Test")))
    d2 <- satander %>% filter(!is.na(target)) %>% mutate(cat = factor(target, c(0, 1), c("No", "Yes")))
    if(input$compare %in% compare[1]){d <- d1}else{d <- d2}

    output$figures3a <- renderPlot({
      histogram(d, x = input$vars[length(input$vars)], "cat") + labs(fill = "")
    })
  })
  #Plot
  observeEvent(R3b(), {
    var <- input$vars[length(input$vars)]
    d <- train
    names(d)[which(names(d) %in% var)] <- "var"
    d1 <- as.data.frame(d %>% group_by(target, var) %>% summarise(count = n())) %>% arrange(var)
    d <- reshape(d1, direction = "wide", timevar = "target", idvar = "var")
    d[is.na(d)] <- 0
    d$count <- d$count.0 + d$count.1
    d$perc.0 <- d$count.0/d$count; d$perc.1 <- d$count.1/d$count
    d_tr <- d %>% dplyr::select(var, count, perc.1)
    
    logit.mod <- glm(target ~ var, data = d1, family = "binomial")
    d_tr$pred <- predict(logit.mod, newdata = d_tr, type = "response")
    logit.mod.inter <- glm(target ~ var*count, data = d1, family = "binomial")
    d_tr$pred.inter <- predict(logit.mod.inter, newdata = d_tr, type = "response")
    
    output$figures3b <- renderPlot({
      ggplot(d_tr, aes(y = perc.1, x = var)) +
        geom_smooth(se = F, method = "loess", span = 0.1)+
        labs(x = var, y = paste0("Prob(target=1/",var, ")"))+
        theme(axis.text = element_text(size = 12, face = "bold"), axis.title = element_text(size = 14))
      })
    
    output$figures3c <- renderPlot({
      g1 <- ggplot(d_tr, aes(x = var, y = 0, fill= pred)) +
        geom_tile(aes(width = 1, height = 1)) +
        labs(y = "", x = var,  fill = paste0("Prob(target = 1/", var,")"))+
        theme_minimal()+guides(fill = "none")+
        theme(axis.text.y = element_blank(), axis.ticks = element_blank(),panel.grid = element_blank(),
              axis.text = element_text(size = 12, face = "bold"), axis.title = element_text(size = 14))

      g2 <- ggplot(d_tr, aes(var, count, fill= pred.inter)) + 
        geom_tile(aes(width = 1, height = 1))+
        labs(y = "Count of Values", x = var, fill = paste0("Prob(target = 1/", var,")"))+
        theme_minimal()+guides(fill = "none")+
        theme(axis.ticks = element_blank(), panel.grid = element_blank(),
              axis.text = element_text(size = 12, face = "bold"), axis.title = element_text(size = 14))
        
      ggarrange(g1, g2, nrow = 1, ncol = 2)
    })
  })
  
  #Panel 4
  #Plot
  observeEvent(Rempty(), {
    corr <- train %>% dplyr::select(-c(ID_code, target)) %>% cor(method = "pearson")
    corr <- corr - diag(1, nrow = nrow(corr))
    
      output$figures4 <- renderPlot({
        corrplot(corr = corr, type = "full", order = "AOE", tl.cex = 1, tl.col = "black", method = "color", diag = F,
                 col.lim = c(min(corr), max(corr)), is.corr = T, col = colorRampPalette(c("blue", "white", "red3"))(100))
      })
  })
}

#Run App
shinyApp(ui = ui, server = server)
