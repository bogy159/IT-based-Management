library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)
library(V8)

#necessary for remote box-collapsing
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

sqlite <- dbConnect(SQLite(), "db.sqlite")

server <- function(input, output, session) {
  
  observeEvent(input$ab_Initial_Pricing2, {
    js$collapse("box_Do2")
    hide(id = "box_Initial_Pricing2", anim = FALSE)
    
    temp_db_Stock_Derivative_Static <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative2,
        input$ti_Stock_ISIN2,
        input$ti_Exercise_Or_Forward_Price2,
        as.character(input$ti_Contracting_Date2),
        as.character(input$ti_Expiration_Date2),
        input$ti_Contract_Size2,
        input$ti_Number_Of_Contracts2,
        input$ti_Stock_Volatility2,
        input$ti_Interest_Rate2,
        input$ti_Mark_To_Model2
      )
    names(temp_db_Stock_Derivative_Static) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static,
                 append = TRUE)
    

    d1 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2))/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2))))
    d2 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 - ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2))/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2))))
    Nd1 <- pnorm(d1,lower.tail = TRUE)
    Nd2 <- pnorm(d2,lower.tail = TRUE)


    PresentValue <- 100 #???
    assetOrLiability<- 1 #???

    temp_db_Economic_Resource_Risky_Income <-
      cbind.data.frame(
        as.character(input$ti_Contracting_Date2),
        Nd1,
        input$ti_Do_Stock_Price2,
        assetOrLiability
      )
    names(temp_db_Economic_Resource_Risky_Income) <-
      c(
        "timestamp",
        "Nd1t",
        "Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(sqlite,
                 "Economic_Resource_Risky_Income",
                 temp_db_Economic_Resource_Risky_Income,
                 append = TRUE)



    temp_db_Economic_Resource_Fixed_Income <-
      cbind.data.frame(
        as.character(input$ti_Do_timestamp2),
        PresentValue,
        assetOrLiability
      )
    names(temp_db_Economic_Resource_Fixed_Income) <-
      c(
        "timestamp",
        "Present_Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(sqlite,
                 "Economic_Resource_Fixed_Income",
                 temp_db_Economic_Resource_Fixed_Income,
                 append = TRUE)
    
    
  })
  observeEvent(input$button_Do2, {
    temp_db_Stock_Pricing_Dynamic <-
      cbind.data.frame(
        input$ti_Stock_ISIN2,
        input$ti_Do_Stock_Price2,
        as.character(input$ti_Do_timestamp2)
      )
    names(temp_db_Stock_Pricing_Dynamic) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic,
                 append = TRUE)
    
    js$collapse("box_Plan2")
  })
  
  observeEvent(input$button_Plan2, {
    
    # d1 <- (log(100/100) + (0.05 + (0.2^2)/2) * 1)/(0.2*(1^-2))
    # d1 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2))/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2))))
    # d2 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 - ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2))/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2))))
    # neshtotam <- as.numeric(input$ti_Do_Stock_Price2) + as.numeric(input$ti_Exercise_Or_Forward_Price2)
    # pnorm(d1,lower.tail = TRUE)
    # Nd1 <- pnorm(d1,lower.tail = TRUE)
    # Nd2 <- pnorm(d2,lower.tail = TRUE)
    
    PresentValue <-100#???
    
    t <- as.numeric(difftime(as.Date(input$ti_Expiration_Date2), as.Date(input$ti_Do_timestamp2), unit="weeks"))/52.25
    t <- round(t, digits = 2)
    
    d1 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * t)/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(t)))
    Nd1 <- pnorm(d1,lower.tail = TRUE)
    d2 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 - ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * t)/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(t)))
    Nd2 <- pnorm(d2,lower.tail = TRUE)
    
    output$to_Plan2 <- renderText(({paste("N(d1) =", Nd1)}))
    
    assetOrLiability<-1 #???
    
    
    temp_db_Economic_Resource_Risky_Income <-
      cbind.data.frame(
        as.character(input$ti_Do_timestamp2),
        Nd1,
        input$ti_Do_Stock_Price2,
        assetOrLiability
      )
    names(temp_db_Economic_Resource_Risky_Income) <-
      c(
        "timestamp",
        "Nd1t",
        "Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(sqlite,
                 "Economic_Resource_Risky_Income",
                 temp_db_Economic_Resource_Risky_Income,
                 append = TRUE)
    
    
    
    temp_db_Economic_Resource_Fixed_Income <-
      cbind.data.frame(
        as.character(input$ti_Do_timestamp2),
        PresentValue,
        assetOrLiability
      )
    names(temp_db_Economic_Resource_Fixed_Income) <-
      c(
        "timestamp",
        "Present_Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(sqlite,
                 "Economic_Resource_Fixed_Income",
                 temp_db_Economic_Resource_Fixed_Income,
                 append = TRUE)
    
    
    
    js$collapse("box_Check2")
  })
  
  observeEvent(input$button_Check2, {
    
    # d1t1 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2)*0.75)/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2)*0.75)))
    # d1t2 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2)*0.5)/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2)*0.5)))
    # d1t3 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2)*0.25)/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2)*0.25)))
    # d1t4 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * as.numeric(input$ti_Contract_Size2)*0)/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2)*0)))
    
    # d1t1 <- (log(as.numeric(input$ti_Do_Stock_Price2)/as.numeric(input$ti_Exercise_Or_Forward_Price2)) + (as.numeric(input$ti_Interest_Rate2)/100 + ((as.numeric(input$ti_Stock_Volatility2)/100)^2)/2) * (as.numeric(input$ti_Contract_Size2) - as.numeric(input$ti_Contract_Size2) / 4))/((as.numeric(input$ti_Stock_Volatility2)/100)*(sqrt(as.numeric(input$ti_Contract_Size2)*0.75)))
    
    
    # d12 <- (log(100/100) + (0.05 + (0.2^2)/2) * 0.75)/(0.2*(sqrt(0.75)))
    # Nd1t1 <-pnorm(d1t1,lower.tail = TRUE)
     
    baseERRI <- dbReadTable(sqlite, "Economic_Resource_Risky_Income")
    lastValues <- as.numeric(tail(baseERRI[,3],2))
    
    Nd1t<- as.numeric(lastValues[2])
    Nd1tm1<- as.numeric(lastValues[1])
    
    
    
    
    output$to_Check2 <- renderText(({paste("Delta N(d1) =", Nd1t - Nd1tm1)}))
  
    # output$to_Check2 <- renderText(d12)
    js$collapse("box_Act2")
  })
  
  observeEvent(input$button_Act2, {
    output$to_Act2 <- renderText("Forward: No action possible")
    v$doCalcAndPlot <- input$button_Act2 #CalcAndPlot
  })
  
  observeEvent(input$button_Act_Continue2, {
    js$collapse("box_Act2")
    js$collapse("box_Plan2")
    js$collapse("box_Check2")
    
    output$to_Plan2 <- renderText("")
    output$to_Check2 <- renderText("")
    output$to_Act2 <- renderText("")
    
  })
  
  
  
  
  
  
  observeEvent(input$ab_Initial_Pricing, {
    js$collapse("box_Do")
    hide(id = "box_Initial_Pricing", anim = FALSE)
    
    temp_db_Stock_Derivative_Static <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative,
        input$ti_Stock_ISIN,
        input$ti_Exercise_Or_Forward_Price,
        as.character(input$ti_Contracting_Date),
        as.character(input$ti_Expiration_Date),
        input$ti_Contract_Size,
        input$ti_Number_Of_Contracts,
        input$ti_Stock_Volatility,
        input$ti_Interest_Rate,
        input$ti_Mark_To_Model
      )
    names(temp_db_Stock_Derivative_Static) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static,
                 append = TRUE)
  })
  observeEvent(input$button_Do, {
    temp_db_Stock_Pricing_Dynamic <-
      cbind.data.frame(
        input$ti_Stock_ISIN,
        input$ti_Do_Stock_Price,
        as.character(input$ti_Do_timestamp)
      )
    names(temp_db_Stock_Pricing_Dynamic) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic,
                 append = TRUE)
    
    
    js$collapse("box_Plan")
  })
  
  observeEvent(input$button_Plan, {
    
    output$to_Plan <- renderText("N(d1) = 1")
    js$collapse("box_Check")
  })
  
  #https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
  
  
  observeEvent(input$button_Check, {
    output$to_Check <- renderText("Delta N(d1) = 0")
    js$collapse("box_Act")
  })
  
  observeEvent(input$button_Act, {
    output$to_Act <- renderText("Forward: No action possible")
    v$doCalcAndPlot <- input$button_Act #CalcAndPlot
  })
  
  observeEvent(input$button_Act_Continue, {
    js$collapse("box_Act")
    js$collapse("box_Plan")
    js$collapse("box_Check")
    
    output$to_Plan <- renderText("")
    output$to_Check <- renderText("")
    output$to_Act <- renderText("")
    
  })
  
  observeEvent(input$reset_db, {
    dbSendStatement(sqlite, "DELETE from Stock_Derivative_Static")
    dbSendStatement(sqlite, "DELETE from Stock_Pricing_Dynamic")
    dbSendStatement(sqlite, "DELETE from Derivative_Instrument_Dynamic")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Risky_Income")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Fixed_Income")
    dbSendStatement(sqlite, "DELETE from Asset")
    dbSendStatement(sqlite, "DELETE from Liability")
    dbSendStatement(sqlite, "DELETE from Off_Balance")
  })
  

  
  v <- reactiveValues(doCalcAndPlot = FALSE) #recalc and redraw
  
  output$timeline <- renderDygraph({
    if (v$doCalcAndPlot == FALSE)
      return()
    isolate({
      temp_db_draw <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
      temp_db_draw$Pricing_Date <-
        as.Date(as.POSIXct(temp_db_draw$timestamp))
      
      #legacy calc
      temp_db_draw$TtM <-
        as.numeric(difftime(
          as.Date(isolate(input$ti_Expiration_Date)),
          as.Date(temp_db_draw$Pricing_Date),
          unit = "weeks"
        )) / 52.1775
      temp_db_draw$Interest_Rate <-
        as.numeric(input$ti_Interest_Rate) / 100
      temp_db_draw$Interest_Rate_Cont <-
        log(1 + temp_db_draw$Interest_Rate)
      temp_db_draw$F_Price <-
        temp_db_draw[1, 3] * (1 + as.numeric(input$ti_Interest_Rate) / 100) ^ (as.numeric(difftime(
          as.Date(input$ti_Expiration_Date),
          as.Date(input$ti_Contracting_Date),
          unit = "weeks"
        )) / 52.1775)
      temp_db_draw$Liability <-
        -temp_db_draw$F_Price * exp(-temp_db_draw$Interest_Rate_Cont * temp_db_draw$TtM)
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      temp_db_draw$'Forward Value' <-
        round(temp_db_draw$Liability + temp_db_draw$Stock_Price, 1)

      #Composing XTS
      temp_xts_draw <-
        xts(x = temp_db_draw[, c("Asset", "Liability", "Forward Value")], order.by =
              temp_db_draw[, 5])
      
      #Derivative_Instrument_Dynamic entry
      temp_Stock_Derivative_Static <-
        dbReadTable(sqlite, "Stock_Derivative_Static")
      temp_db_Derivative_Instrument_Dynamic <-
        cbind.data.frame(
          tail(temp_Stock_Derivative_Static$Stock_Derivative_Static_ID, 1),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Forward Value', 1)
        )
      names(temp_db_Derivative_Instrument_Dynamic) <-
        c("Stock_Derivative_Static_ID",
          "timestamp",
          "Fair_Value")
      dbWriteTable(
        sqlite,
        "Derivative_Instrument_Dynamic",
        temp_db_Derivative_Instrument_Dynamic,
        append = TRUE
      )
      
      #Economic_Resource_Risky_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Risky_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          1,
          tail(temp_db_draw$'Asset', 1),
          1
        )
      names(temp_db_Economic_Resource_Risky_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Nd1t",
          "Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Risky_Income",
        temp_db_Economic_Resource_Risky_Income,
        append = TRUE
      )
      
      #Economic_Resource_Fixed_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Fixed_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Liability', 1),
          1
        )
      names(temp_db_Economic_Resource_Fixed_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Present_Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Fixed_Income",
        temp_db_Economic_Resource_Fixed_Income,
        append = TRUE
      )
      
      #Asset, Liability of Off Balance
      if (tail(temp_db_draw$'Forward Value', 1) > 0) {
        #Asset
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_asset <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_asset) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Asset", temp_db_asset, append = TRUE)
      } else if (tail(temp_db_draw$'Forward Value', 1) < 0) {
        #Liability
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_liability <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_liability) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Liability", temp_db_liability, append = TRUE)
      }
      else {
        # Off_Balance
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_off_balance <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp)
          )
        names(temp_db_off_balance) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp")
        dbWriteTable(sqlite, "Off_Balance", temp_db_off_balance, append = TRUE)
      }
      
      #Plotting XTS
      dygraph(temp_xts_draw) %>%
        dyRangeSelector()
    })
  })
  
  observeEvent(
    input$load_table_Stock_Pricing_Dynamic,
    output$table_Stock_Pricing_Dynamic <- renderDataTable({
      dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Information_Static,
    output$table_Stock_Information_Static <- renderDataTable({
      dbReadTable(sqlite, "Stock_Information_Static")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Derivative_Static,
    output$table_Stock_Derivative_Static <-
      renderDataTable({
        dbReadTable(sqlite, "Stock_Derivative_Static")
      })
  )
  
  observeEvent(
    input$load_table_Derivative_Instrument_Dynamic,
    output$table_Derivative_Instrument_Dynamic <-
      renderDataTable({
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Risky_Income,
    output$table_Economic_Resource_Risky_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Risky_Income")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Fixed_Income,
    output$table_Economic_Resource_Fixed_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Fixed_Income")
      })
  )
  
  observeEvent(input$load_table_Asset,
               output$table_Asset <- renderDataTable({
                 dbReadTable(sqlite, "Asset")
               }))
  
  observeEvent(input$load_table_Liability,
               output$table_Liability <- renderDataTable({
                 dbReadTable(sqlite, "Liability")
               }))
  
  observeEvent(input$load_table_Off_Balance,
               output$table_Off_Balance <- renderDataTable({
                 dbReadTable(sqlite, "Off_Balance")
               }))
}