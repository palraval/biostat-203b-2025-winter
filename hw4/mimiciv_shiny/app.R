# Loading Necessary Packages

library(shiny)
library(tidyverse)


# Loading Data 

mimic_icu_cohort = readRDS("mimic_icu_cohort.rds") |>
  mutate(insurance = as.factor(insurance),
         marital_status = as.factor(marital_status),
         gender = as.factor(gender))

satoken <- "../biostat-203b-2025-winter-4e58ec6e5579.json"

bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)




# Defined UI for application that makes plots

ui <- fluidPage(
  titlePanel("MIMIC-III ICU Cohort"),
  tabsetPanel(
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable",
                             label = "Choose a variable to display",
                             choices = c("demographics", "lab measurements",
                                         "vitals"),
                             selected = "demographics"),
                 selectInput("Specificied_variable",
                            label = "Choose a specific variable to display",
                            choices = NULL)
    ),
              mainPanel(
                plotOutput("plot"),
                tableOutput("table")
      )
    )
  ),
    tabPanel("Patient Info",
           sidebarLayout(
             sidebarPanel(
               numericInput("patient_id_value",
                           label = "Input Patient ID",
                           value = ""),
               actionButton("updated_patient_id", "Update Patient ID"),
               selectInput("Selected_plot",
                           label = "Select a Plot:",
                           choices = c("ADT", "ICU"))
             ),
             mainPanel(
               plotOutput("second_tab_plot")
                      )
                    )
                )
            )
        )


# Defined server logic required to make plots


server <- function(input, output, session){
  
  observeEvent(input$variable, {
    if (input$variable == "demographics") {
      updateSelectInput(session, "Specificied_variable",
                         label = "Choose a specific variable to display",
                         choices = c("insurance", "marital_status", "race",
                                     "gender", "age_intime", "language"))
      }
    else if (input$variable == "lab measurements") {
      updateSelectInput(session, "Specificied_variable",
                         label = "Choose a specific variable to display",
                         choices = c("potassium", "white_blood_cell_count",
                                     "glucose", "chloride", "hematocrit",
                                     "sodium", "creatinine", "bicarbonate"))
      }
    else if (input$variable == "vitals") {
      updateSelectInput(session, "Specificied_variable",
                         label = "Choose a specific variable to display",
                         choices = c("heart_rate", "body_temperature", 
                                     "diastolic_non-invasive_blood_pressure",
                                     "respiratory_rate", 
                                     "systolic_non-invasive_blood_pressure"))
      }
    })
  
  output$plot <- renderPlot({
    selected_variable = mimic_icu_cohort[[input$Specificied_variable]]
    
    if (is.numeric(selected_variable)) {
          quantile_value = quantile(selected_variable, probs = c(0.01, 0.99),
                                    na.rm = TRUE)
          hist(selected_variable, 
           main = paste("Distribution of", input$Specificied_variable),
           xlab = input$Specificied_variable, col = "darkviolet",
           xlim = quantile_value,
           breaks = 20)
      }
    else {
    barplot(table(mimic_icu_cohort[[input$Specificied_variable]]),
            main = paste("Distribution of", input$Specificied_variable),
            xlab = input$Specificied_variable,
            ylab = "Frequency", col = rainbow(10))
    }
  })

  output$table <- renderTable({
    selected_variable = mimic_icu_cohort[[input$Specificied_variable]]
    if (is.numeric(selected_variable)) {
      summary_values = summary(selected_variable)
      data.frame(Statistics = names(summary_values),
                 Values = as.numeric(summary_values))
    }
  
  }, caption = "Statistics Table", caption.placement = "top")


  
# SECOND TAB  
  
  
  reactive_patient_id = reactiveVal(NULL)
  
  observeEvent(input$updated_patient_id, {
    reactive_patient_id(input$patient_id_value)
  })
  
  
  patientInput = reactive({
    
    patient_id = reactive_patient_id()
    
    demographic = tbl(con_bq, "patients") |>
      filter(subject_id == patient_id) |>
      collect()
    
    adt = tbl(con_bq, "admissions") |>
      filter(subject_id == patient_id) |>
      collect()
    
    color_info = tbl(con_bq, "transfers") |>
      filter(subject_id == patient_id) |>
      collect()
    
    shape_info = tbl(con_bq, "procedures_icd") |>
      filter(subject_id == patient_id) |>
      collect()
    
    labevents_subset = tbl(con_bq, "labevents") |>
      filter(subject_id == patient_id) |>
      collect()
    
    chartevents_subset = tbl(con_bq, "chartevents") |>
      filter(subject_id == patient_id) |>
      collect()
    
    diagnoses = tbl(con_bq, "diagnoses_icd") |>
      filter(subject_id == patient_id) |>
      collect()
    
    diagnoses = diagnoses |>
      filter(subject_id == patient_id) |>
      head(3)
    
    diagnoses_codes = tbl(con_bq, "d_icd_diagnoses") |>
      filter(subject_id == patient_id)
    
    diagnoses_codes = diagnoses_codes %>% 
      filter(icd_code %in% diagnoses$icd_code)
    
    items = tbl(con_bq, "d_items.csv.gz") |> 
      filter(abbreviation %in% c("HR", "NBPd","NBPs","RR", 
                                 "Temperature F")) |>
      collect()
    
    chartevents_subset = chartevents_subset %>% 
      filter(itemid %in% items$itemid)
    
    list(demographic = demographic,
         adt = adt,
         color_info = color_info,
         shape_info = shape_info,
         labevents_subset = labevents_subset,
         chartevents_subset = chartevents_subset,
         diagnoses_codes = diagnoses_codes,
         diagnoses = diagnoses,
         items = items)
    
    
  })
  
  
  
  
  output$second_tab_plot = renderPlot({
    
    data = patientInput()
    
    demographic = data$demographic
    
    adt = data$adt
    
    color_info = data$color_info
    
    shape_info = data$shape_info
    
    labevents_subset = data$labevents_subset
    
    chartevents_subset = data$chartevents_subset 
    
    diagnoses_codes = data$diagnoses_codes 
    
    diagnoses = data$diagnoses
    
    items = data$items
  
    
    
    if (input$Selected_plot == "ADT") {
      ggplot() +
        geom_segment(data = adt, aes(x = admittime, 
                                     xend = dischtime, y = "lab"),
                                     linewidth = 0.1) +
        geom_point(data = labevents_subset, aes(x = charttime, y = "lab"), 
                   shape = 3, size = 2) + 
        geom_segment(data = adt, aes(x = admittime, xend = dischtime, 
                                     y = "adt")) + 
        geom_segment(data = color_info, aes(x = intime, xend = outtime, 
                                            y = "adt", 
                                            colour = careunit, 
                                            linewidth = careunit)) + 
        geom_point(data = shape_info, aes(x = as.POSIXct(chartdate), 
                                          y = "procedure",
                                          shape = icd_code),
                   size = 4) +
        labs(title = paste0("Patient ", demographic$subject_id, ", ",  
                            demographic$gender, ", ", demographic$anchor_age, 
                            " years old ", tolower(adt$race)),
             subtitle = paste(diagnoses_codes$long_title[1],
                              diagnoses_codes$long_title[2],
                              diagnoses_codes$long_title[3],
                              sep = "\n"),
             x = "Calendar Time",
             y = "")
    
    } else {
        
      labels = c("220045" = "HR",
                 "220179" = "NBPd",
                 "220180" = "NBPs",
                 "220210" = "RR",
                 "223761" = "Temperature F")
      
      ggplot(data = chartevents_subset) + 
        geom_line(mapping = aes(x = charttime, y = valuenum, 
                                colour = as.factor(itemid))) +
        geom_point(mapping = aes(x = charttime, y = valuenum, 
                                 colour = as.factor(itemid))) + 
        facet_grid(itemid~stay_id, scales = "free", 
                   labeller = labeller(itemid = as_labeller(labels))) + 
        scale_x_datetime(date_breaks = "8 hours",
                         date_labels = "%Y-%m-%d\n%H:%M") + 
        theme(axis.text.x = element_text(size = 6), legend.position = "None") +
        labs(title = paste("Patient", chartevents_subset$subject_id, 
                           "ICU stays - Vitals"),
             x = "", 
             y = "")
      }
    
 
  })
  
}



# Run the application

shinyApp(ui = ui, server = server)



















