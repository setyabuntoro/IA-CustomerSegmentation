# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyright IBM Corp. 2019. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

clientButton <- function(id, name, image) {
  tags$p(
    actionButton(paste0('client-btn-', id),
                 fluidRow(
                   column(3, 
                          tags$img(src = image, width = "100px", height = "100px")
                   ),
                   column(9,
                          tags$h3(name),
                          tags$h4(paste('Customer ID: ', id))
                   )
                 ),
                 style="width:100%"
    )
  )
}

homePanel <- function() {
  
  tabPanel(
    "Dashboard",
    tags$head(
      tags$style(HTML("
        .datatables {
          width: 100% !important;
        }
      "))
    ),
    shinyjs::useShinyjs(),
    
    fluidRow(
      div(id = "topActionClients",
      column(4, panel(
        h2("Top Action Clients"),
        br(),
        
        # List of client buttons
        lapply(clientIds, function(id){
          client <- clients[[toString(id)]]
          clientButton(id, client$name, paste0("profiles/", client$image))
        })
        
      ))),
      div(id = "rightPanel",
      column(8, 
             panel(
               
               h2("Customer Segmentation"),
               br(),
               
               tags$div(
                 id = "authPanel",
                 column(6,
                        panel(
                          h4("Connect to ICP for Data API"),
                          br(),
                          textInput("hostname", "ICP4D Hostname"),
                          textInput("username", "ICP4D Username"),
                          passwordInput("password", "ICP4D Password"),
                          actionButton("authBtn", "Authenticate API", class = "btn-primary btn-lg btn-block", style = "max-width:300px", disabled = TRUE),
                          tags$head(tags$style("#authError{color:red;}")),
                          verbatimTextOutput("authError")
                        ),
                        style = "max-width:360px;"
                 )
               ),
               
               hidden(
                 tags$div(
                   id = "deploymentPanel",
                   
                   panel(
                   
                   fluidRow(
                     column(6,
                            tags$div(id = "scoreBtnSection",
                                     h4("Input JSON:"),
                                     verbatimTextOutput("pipelineInput"),
                                     br(),
                                     actionButton(
                                       "scoreBtn",
                                       "Segment Customers",
                                       class = "btn-primary btn-lg btn-block",
                                       disabled = TRUE
                                     ),
                                     tags$head(tags$style("#scoringError{color:red;}")),
                                     verbatimTextOutput("scoringError")
                            )),
                     column(6,
                            
                              tags$h4("Model Scoring Pipeline Deployment"),
                              pickerInput(
                                inputId = 'deploymentSelector',
                                label = 'Deployment Name:',
                                choices = list(),
                                options = pickerOptions(width = "auto", style = "btn-primary")
                              ),
                              tags$p(
                                tags$strong("Scoring URL: "),
                                textOutput(outputId = "scoring_url", inline = TRUE),
                                style = "word-wrap: break-word"
                              ),
                              tags$p(
                                tags$strong("Project Release: "),
                                textOutput(outputId = "release_name", inline = TRUE)
                              ),
                              tags$p(
                                tags$strong("Script Name: "),
                                textOutput(outputId = "script_name", inline = TRUE)
                              ),
                              tags$p(
                                tags$strong("Engine: "),
                                textOutput(outputId = "runtime", inline = TRUE)
                              )
                      )
                     )))),
               tags$div(id = "scoringResponse")
              )
     ))),
     div(id = "clientAssignments")
  )
}


# server variables (reactive)
serverVariables = reactiveValues()

pipelineInput <- list(cust_ids = as.list(clientIds), dataset_name = 'customer_full_summary_latest.csv', sc_end_date = '2018-09-30')

homeServer <- function(input, output, session, sessionVars) {
  
  sessionVariables = reactiveValues()
  
  # Set default hostname for ICP4D API
  observeEvent(session$clientData$url_hostname, {
    updateTextInput(session, "hostname", value = session$clientData$url_hostname)
  })
  
  # Enable buttons when inputs are provided
  observe({
    toggleState("authBtn", nchar(input$hostname) > 0 && nchar(input$username) > 0 && nchar(input$password) > 0)
    toggleState("scoreBtn", nchar(input$endpoint) > 0 && nchar(input$token) > 0 && length(input$allCustomers_rows_selected) > 0)
  })
  
  # Handle ICP4D API authentication button
  observeEvent(input$authBtn, {
    shinyjs::disable("authBtn")
    
    tryCatch({
      serverVariables$deployments <- collectDeployments(input$hostname, input$username, input$password, "Segmentation_Scoring_Pipeline.py")
    }, warning = function(w) {
      output$authError <- renderText(w$message)
    }, error = function(e) {
      output$authError <- renderText(e$message)
    })
    
    shinyjs::enable("authBtn")
  })
  
  observe({
    if(length(serverVariables$deployments) > 0) {
      updateSelectInput(session, "deploymentSelector", choices = names(serverVariables$deployments))
      shinyjs::hide(id = "authPanel")
      shinyjs::show(id = "deploymentPanel")
    }
  })
  
  # Handle model deployment dropdown switching
  observeEvent(input$deploymentSelector, {
    selectedDeployment <- serverVariables$deployments[[input$deploymentSelector]]
    output$release_name <- renderText(selectedDeployment$release_name)
    output$scoring_url <- renderText(selectedDeployment$scoring_url)
    output$script_name <- renderText(selectedDeployment$deployment_asset_name)
    output$runtime <- renderText(selectedDeployment$runtime_definition_name)
    toggleState("scoreBtn", nchar(selectedDeployment$deployment_url) > 0 && nchar(selectedDeployment$deployment_token) > 0)
  })
  
  # Handle model deployment scoring button
  observeEvent(input$scoreBtn, {
    shinyjs::disable("scoreBtn")
    
    selectedDeployment <- serverVariables$deployments[[input$deploymentSelector]]
    
    response <- scoreModelDeployment(selectedDeployment$scoring_url, selectedDeployment$deployment_token, pipelineInput)
    
    if(length(response$error) > 0) {
      output$scoringError <- renderText(toString(response$error))
    }
    else if(length(response$result) > 0) {
      shinyjs::hide(id = "deploymentPanel")
      
      sessionVariables$clusterMapping <- response$result$assigments
      clusterFeatures <- response$result$features
      
      names(sessionVariables$clusterMapping) <- pipelineInput$cust_ids
      
      # Generate table of cluster mapping
      df <- data.frame(clusterID=t(t(sessionVariables$clusterMapping)))
      names(df) <- c("Segment ID")
      clusterMappingTable <- tibble::rownames_to_column(df, "Customer ID") %>%
        mutate_all(as.integer)
      
      # Generate tables of cluster features
      sessionVariables$featureTables <- list()
      for(clusterID in names(clusterFeatures)) {
        cluster <- clusterFeatures[[clusterID]]
        clusterTable <- list()
        for(feature in names(cluster)) {
          newFeatureName <- sub("CUSTOMER_", "", feature)
          newFeatureName <- sub("SUMMARY_", "", newFeatureName)
          clusterTable[[feature]] <- list(feature=newFeatureName, min=cluster[[feature]]$min, max=cluster[[feature]]$max)
        }
        sessionVariables$featureTables[[clusterID]] <- do.call(rbind.data.frame, clusterTable) %>%
          mutate_at(vars(min, max), funs(ifelse(grepl("NUMBER|MONTHS",feature), as.integer(.), dollar(.)))) %>%
          mutate(feature=sub("NUMBER_OF_|TOTAL_AMOUNT_OF_", "", feature))
        names(sessionVariables$featureTables[[clusterID]]) <- c("Feature", "Min", "Max")
      }
      
      insertUI(
        selector = "#scoringResponse",
        where = "beforeEnd",
        ui = panel(
          h3("Segment Assignments:"),
          br(),
          div(id = "assignTableContainer",
            fluidRow(
              column(4,
                tableOutput("clusterMapping")
              ),
              column(8,
                actionBttn("vizAssign", "Visualize Segments"),
                br(),br(),
                h3("Segment Features:"),
                lapply(names(sessionVariables$featureTables), function(id){
                  div(
                       h4(paste("Segment",id), style="text-align:center"),
                       tableOutput(paste0("features-",id))
                      )
                })
              )
            )
          )
        )
      )
      
      lapply(names(sessionVariables$featureTables), function(id){
        output[[paste0("features-",id)]] <- renderTable(sessionVariables$featureTables[[id]], bordered = TRUE)
      })
      
      output$clusterMapping <- renderTable(clusterMappingTable, bordered = TRUE)
      
    } else {
      output$scoringError <- renderText(response)
    }
    
    shinyjs::enable("scoreBtn")
  })
  
  observeEvent(input$vizAssign, {
    
    clusters = list()
    for(custID in names(sessionVariables$clusterMapping)){
      clusterID <- sessionVariables$clusterMapping[custID]
      clusters[[toString(clusterID)]] <- c(clusters[[toString(clusterID)]], custID)
    }
    
    removeUI("#topActionClients")
    shinyjs::hide(id = "rightPanel")
    
    insertUI(
      selector = "#clientAssignments",
      where = "beforeEnd",
      ui = panel(
        h2("Top Action Clients"),
        br(),
        h3("Client Segments:"),
        fluidRow(
        lapply(names(clusters), function(clusterID) { 
          column(4, 
                 panel(
                   h3(paste("Segment",clusterID), style = "text-align:center"),
                   tableOutput(paste0("features2-",clusterID)),
                   lapply(clusters[[clusterID]], function(custID) {
                     client <- clients[[custID]]
                     clientButton(id, client$name, paste0("profiles/", client$image))
                   })
                 )
                )
          })
        )
      )
    )
    
    lapply(names(sessionVariables$featureTables), function(id){
      output[[paste0("features2-",id)]] <- renderTable(sessionVariables$featureTables[[id]], bordered = TRUE, width = "100%")
    })
    
  })
  
  output$pipelineInput <- renderText(toJSON(pipelineInput, indent = 2))
  
}
