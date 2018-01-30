# This is the combined ui, server and app call for the SyNC (Synergy.Networking.Communication) Rshiny e-mail tool
# This tool may be used to select an audience and construct an e-mail to that audience
# To edit the values available in this tool, see the SyNClists tool
# Author: Matthew Robert Jones
# Version 1: 1/24/2018

library(shiny)
library(shinyjs)
library(shinyBS)
library(sendmailR)
library(shinyAce)

# Define UI to allow user to select audience and craft message
ui <- fluidPage(
  
    # Main panel for displaying outputs ----
    mainPanel(
      #App logo
      img(src = "SyNC_logo.png", height = 108, width = 461),
      
      #Instructions
      helpText("This section of the tool is designed to help you determine WHO to communicate to based on WHAT you want to communicate. 
Please note that the Communication Owner is accountable to Protect Lilly and to share the appropriate level of information with only those who 'need to know'. Refer to Protect Lilly for further guidance on how to classify and manage information."),
      
      # Input communications owner.  This currently has no functionality, because the from line is spoofable.
      textInput("text", h3("Communication Owner"), 
                       value = "label"),
      helpText("Only the person listed as the Communication Owner will be able to send the email. This person is also accountable to the content of the communication as it relates to Protect Lilly."),
      
      # Select topic.  This changes pulls the highly recommended functions list from the back end.
      selectInput("topic", h3("Communication Topic"),c("Please click 'Update Topic Dropdown'")),
      helpText("Open the drop down list to choose the topic that best fits your need. Selecting a topic will generate a list of highly recommended and other possible communication recipients. If your topic isn't in the list, you have two options:> Do not select a topic. This will not give you a recommended list of recipients, however you will be able to select recipients from the 'DSS Recipients', 'GSS Recipients', and 'Other Recipients' sections; or > Select a topic that closely aligns with your topic. This will populate the 'Highly Recommended Communication Recipients' section and help you choose the best recipients."),
      
      
      #Testing input fields
      #selectInput("variable", "Variable:",c("Cylinders" = "cyl","Transmission" = "am","Gears" = "gear")),
      
      # Get current topics
      # Removed end-user ability to update topics for simplicity.  User must start a new session of the app to get any updates.
      #actionButton("refreshTopic", "Update Topic Dropdown"),
      
      # Recommended groups, dynamically generated in a loop over the lists recommended for the selected topic, each wrapped in their own conditional
      # Defaultly shown with checkbox yes (indicating they will be mailed unless the user ops them out)
      conditionalPanel(
        condition = "recommended == true",
        radioButtons("Matt", "Title here",
               c("Yes" = "yes",
                 "No" = "no"
                 ))
      ),
      checkboxInput("somevalue", "Some value", FALSE),
      verbatimTextOutput("value"),
      lapply(1:5, function(i) {
        checkboxInput(paste0('a', i), paste0('SelectA', i),TRUE)
      }),
      
      # Non-recommended groups, dynamically generated in a loop over all lists associated with any topic, excluding the recommended groups
      # Defaultly hidden with checkbox no (indicating they will not be mailed unless the user ops them in)
      # Shown via user button click
      
      # Show GSS groups
      actionButton("GSSShow", "Show non-recommended GSS groups"),
      
      # Show DSS groups
      actionButton("DSSShow", "Show non-recommended DSS groups"),
      
      # Show Other groups
      actionButton("otherShow", "Show non-recommended other groups"),
      
      
      # Mail-based input values
      textInput("from", "From:", value="jones_matthew_robert@gmail.com"),
      textInput("to", "To:", value="jones_matthew_robert@lilly.com"),
      textInput("to2", "To:", value="matthew_robert_jones@lilly.com"),
      textInput("subject", "Subject and Message", value="label"),
      
      #Message
      #textInput("message", "message", value="Double email"),
      aceEditor("aceMessage", "Ace Message", value="Write message here."),
      
      # Execute e-mail button (send)
      actionButton("send", "Send mail")
  )
)

# Define server logic to send email as specified by the GUI
server <- function(input, output, session) {
  
  #Have some strings to work with
  #tabledata<-read.table("../SyNClists/mail_list.csv")
  # unlisted<-unlist(tabledata)
  readIn <- read.table("../SyNClists/topic_list.csv") # Fetching the current topic list
  choiceVector <- unique(c(t(readIn)))
  updateSelectInput(session, "topic", label = NULL, choices = choiceVector,  selected = NULL)
  
  # Showing and hiding recommended
  recommended<- TRUE

  # Removed end-user ability to update topics for simplicity.  User must start a new session of the app to get any updates.
  # Watches the Update Topic Dropdown (refreshTopic) button.  When pressed, reads in the latest dataset.
  # observe({
  #   if(is.null(input$refreshTopic) || input$refreshTopic==0) return(NULL)
  #   readIn <- read.table("../SyNClists/topic_list.csv") # Fetching the current topic list
  #   choiceVector <- unique(c(t(readIn)))
  #   updateSelectInput(session, "topic", label = NULL, choices = choiceVector,  selected = NULL)
  #   showModal(modalDialog(
  #        title = "Topic list updated",
  #        "You may now select a topic.",
  #        easyClose = TRUE,
  #        footer = NULL
  #       ))
  # })
  
  #Send mail button
  observe({
    if(is.null(input$send) || input$send==0) return(NULL)
    from <- isolate(input$from)
    to <- isolate(input$to)
    to2 <- isolate(input$to2)
    subject <- isolate(input$subject)
    msg <- isolate(input$aceMessage)
    recipients <- c(to,to2)
    sendmail(from, recipients, subject, msg)

    
  })
  
}

shinyApp(ui = ui, server = server)