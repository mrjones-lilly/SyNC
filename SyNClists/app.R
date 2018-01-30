# This is the combined ui, server and app call for the list managemement of SyNC (Synergy.Networking.Communication) Rshiny e-mail tool
# This tool may be used to edit the selectable audiences in SyNC
# Author: Matthew Robert Jones
# Version 1: 1/24/2018

library(shiny)
library(shinyjs)
library(shinyBS)

# Define UI for list editing
# You may select an existing topic for viewing or editing
# Alternatively, you may add a new topic
# Topics are associated with functions
# Functions may also be viewed, edited, or expanded.  
# The default for new functions is to be associated with no topics.

ui <- fluidPage(
  
    # Main panel for displaying outputs ----
    mainPanel(
      #App logo
      img(src = "SyNC_logo.png", height = 108, width = 461),
      
      #Instructions
      helpText("This list editor allows the editing of the back end data such as mailing lists available to the SyNC tool."),
      
      # Existing topic selection and actions
      selectInput("topic", h3("Existing Communication Topics"),c("Please click 'Update Topic Dropdown'")),
      helpText("Select the topic you would like to view or edit with the above drop down."),
      actionButton("refreshTopic", "Update Topic Dropdown"),
      actionButton("viewTopic", "View Current Topic"),
      actionButton("editTopic", "Save Edited Topic"),
      
      # Input new topics
      textInput("newTopicName", h3("New Communication Topic"), value="If the topic dropdown list does not have your topic, enter your topic here"),
      helpText("Here you may define a new topic.  Once defined, use the edit existing topic functionality to change associated groups."),
      actionButton("newTopic", "Save New Topic"), 
      
      #Delete topics
      selectInput("topicToDelete", h3("Select Topic for Deletion"),c("Please click 'Update Topic Dropdown'")),
      actionButton("deleteTopic", "Delete Current Topic"),
      
      # View/input existing highly recommended group edits (note to self, loopify)
      textInput("groupName1", h3("Highly Recommended Groups"), value=""),
      textInput("groupName2", label = NULL, value=""),
      textInput("groupName3", label = NULL, value=""),
      textInput("groupName4", label = NULL, value=""),
      textInput("groupName5", label = NULL, value=""),
      helpText("If you need more than 25 highly recommended groups, consider reducing your topic's scope or creating a group that has broad reach"),
      
      # Existing group selection and actions
      selectInput("group", h3("Existing Groups"),c("Please click 'Update Groups Dropdown'")),
      #textInput("useGroup",label = NULL, value="") #Use this selected group in an open highly recommended group slot for the selected topic
      helpText("Select the group you would like to view, edit or use with the above drop down."),
      actionButton("refreshGroup", "Update Group Dropdown"),
      actionButton("viewGroup", "View Current Group"),
      actionButton("editGroup", "Save Edited Group"),
      
      # Input new groups
      textInput("newGroupName", h3("New Communication Group"), value="If the group dropdown list does not have your group, enter your group here"),
      actionButton("newGroup", "Save New Group"), 
      helpText("Here you may define a new group.  Once defined, use the edit existing group functionality to change associated data."),
      
      # Delete groups
      # actionButton("deleteGroup", h3("Delete Communication Group"), "Delete Current Group"),
      
      # Input existing group/function edits
      textInput("distList1", h3("Distribution Lists"), value=""),
      textInput("distList2", label = NULL, value="") #, here
      #textInput("newTopicName", h3("New Communication Topic"), value="CLUWE"),
      #textInput("newTopicName", h3("New Communication Topic"), value="CLUWE"),
      #textInput("newTopicName", h3("New Communication Topic"), value="CLUWE"),
      #textInput("newTopicName", h3("New Communication Topic"), value="CLUWE"),
      #textInput("newTopicName", h3("New Communication Topic"), value="CLUWE"),
      #textInput("newTopicName", h3("New Communication Topic"), value="CLUWE")
      
      # Find and replace a single distribution list across all groups (even 'deleted' groups)
      
  )
)

# Define server logic for list editing
server <- function(input, output, session) {
  
############ Begin topic edit code  
  
  # Watches the Update Topic Dropdown (refreshTopic) button.  When pressed, reads in the latest dataset.
  observe({
    if(is.null(input$refreshTopic) || input$refreshTopic==0) return(NULL)
    readIn <- read.table("topic_list.csv") # Fetching the current topic list
    choiceVector <- unique(c(t(readIn)))
    updateSelectInput(session, "topic", label = NULL, choices = choiceVector,  selected = NULL)
    showModal(modalDialog(
         title = "Topic list updated",
         "You may now select a topic.",
         easyClose = TRUE,
         footer = NULL
        ))

    
  })  
  
  # Watches the View Current Topic (viewTopic) button.  When pressed, reads in the latest dataset.
  observe({
    if(is.null(input$viewTopic) || input$viewTopic==0) return(NULL)
    selectedTopic <- isolate(input$topic)
    selectedTopicCSV <- paste(selectedTopic,".csv",sep="")
    readIn <- read.table(selectedTopicCSV) # Fetching the current associated list data
    choiceVector <- unique(c(t(readIn)))
    updateTextInput(session, "groupName1", label = NULL, value=choiceVector[1])
    updateTextInput(session, "groupName2", label = NULL, value=choiceVector[2])
    showModal(modalDialog(
         title = "Groups updated",
         "You may now edit groups associated with this topic.",
         easyClose = TRUE,
         footer = NULL
        ))

    
  }) 
    
  # Watches the Save Edited Topic (editTopic) button.  When pressed, writes out the edited dataset.
  observe({
    if(is.null(input$editTopic) || input$editTopic==0) return(NULL)
    #combined <- isolate(input$newTopicName) #Old
    #write.table(combined, "topic_list.csv",col.names=F,row.names=F) #Old
    selectedTopic <- isolate(input$topic)
    selectedTopicCSV <- paste(selectedTopic,".csv",sep="")
    group1<- isolate(input$groupName1)
    group2<- isolate(input$groupName2)
    groupList <- list(c(group1,group2))
    write.table(groupList, selectedTopicCSV,col.names=F,row.names=F)
    showModal(modalDialog(
         title = "Topic entries saved",
         "The lists for the selected topic's highly recommended groups and associated e-mail lists have been saved.",
         easyClose = TRUE,
         footer = NULL
        ))

    
  })
  
  # Watches the Save New Topic (newTopic) button.  When pressed, writes out the edited dataset.
  # Checks that the new topic does not share a key value with any other topic, halts writing and delivers popup in that case
  observe({
    if(is.null(input$newTopic) || input$newTopic==0) return(NULL)
    new_topic <- isolate(input$newTopicName) # Fetch the entry entered by the user
    duplicate<-F 
    duplicateFinder<-c(F,F) # Initializing variables and vectors
    readIn <- read.table("topic_list.csv") # Fetching the current topic list
    for (element in 1:length(readIn[[1]])){
       duplicateFinder[element]<- readIn[[1]][element] == new_topic
       duplicate<-duplicate|duplicateFinder[element]
    } # Checking for duplicates
    if (duplicate==T){
        showModal(modalDialog(
         title = "Duplicate entry",
         "The list was not edited because the new value is a duplicate entry.  Please either edit the existing entry or use a different name for your new entry.",
         easyClose = TRUE,
         footer = NULL
        ))} # Showing a popup if a duplicate is detected
     
    else{
      showModal(modalDialog(
         title = "New topic added",
         "The list was edited to include your new topic entry.  You may now edit associated values.",
         easyClose = TRUE,
         footer = NULL
        ))
     readString<-lapply(unlist(readIn),toString)
     newList<-list(c(new_topic,readString))
     write.table(newList, "topic_list.csv",col.names=F,row.names=F)
     } # Editing the table when a duplicate is not detected
    
  })

############ Begin group edit code  
  
  # Watches the Update Group Dropdown (refreshGroup) button.  When pressed, reads in the latest dataset.
  observe({
    if(is.null(input$refreshGroup) || input$refreshGroup==0) return(NULL)
    readIn <- read.table("group_list.csv") # Fetching the current topic list
    choiceVector <- unique(c(t(readIn)))
    updateSelectInput(session, "group", label = NULL, choices = choiceVector,  selected = NULL)
    showModal(modalDialog(
         title = "Group list updated",
         "You may now select a group.",
         easyClose = TRUE,
         footer = NULL
        ))

    
  })  
  
  # Watches the View Current Group (viewGroup) button.  When pressed, reads in the latest dataset.
  observe({
    if(is.null(input$viewGroup) || input$viewGroup==0) return(NULL)
    selectedGroup <- isolate(input$group)
    selectedGroupCSV <- paste(selectedGroup,".csv",sep="")
    readIn <- read.table(selectedGroupCSV) # Fetching the current associated list data
    choiceVector <- unique(c(t(readIn)))
    updateTextInput(session, "distList1", label = NULL, value=choiceVector[1])
    updateTextInput(session, "distList2", label = NULL, value=choiceVector[2])
    showModal(modalDialog(
         title = "Groups updated",
         "You may now edit distribution lists associated with this group.",
         easyClose = TRUE,
         footer = NULL
        ))

    
  }) 
    
  # Watches the Save Edited Group (editGroup) button.  When pressed, writes out the edited dataset.
  observe({
    if(is.null(input$editGroup) || input$editGroup==0) return(NULL)
    selectedGroup <- isolate(input$group)
    selectedGroupCSV <- paste(selectedGroup,".csv",sep="")
    dist1<- isolate(input$distList1)
    dist2<- isolate(input$distList2)
    distList <- list(c(dist1,dist2))
    write.table(distList, selectedGroupCSV,col.names=F,row.names=F)
    showModal(modalDialog(
         title = "Group entries saved",
         "The distribution lists for the selected group have been saved.",
         easyClose = TRUE,
         footer = NULL
        ))

    
  })
  
  # Watches the Save New Group (newGroup) button.  When pressed, writes out the edited dataset.
  # Checks that the new topic does not share a key value with any other topic, halts writing and delivers popup in that case
  observe({
    if(is.null(input$newGroup) || input$newGroup==0) return(NULL)
    new_group <- isolate(input$newGroupName) # Fetch the entry entered by the user
    duplicate<-F 
    duplicateFinder<-c(F,F) # Initializing variables and vectors
    readIn <- read.table("group_list.csv") # Fetching the current group list
    for (element in 1:length(readIn[[1]])){
       duplicateFinder[element]<- readIn[[1]][element] == new_group
       duplicate<-duplicate|duplicateFinder[element]
    } # Checking for duplicates
    if (duplicate==T){
        showModal(modalDialog(
         title = "Duplicate entry",
         "The list was not edited because the new value is a duplicate entry.  Please either edit the existing entry or use a different name for your new entry.",
         easyClose = TRUE,
         footer = NULL
        ))} # Showing a popup if a duplicate is detected
     
    else{
      showModal(modalDialog(
         title = "New topic added",
         "The list was edited to include your new topic entry.  You may now edit associated values.",
         easyClose = TRUE,
         footer = NULL
        ))
     readString<-lapply(unlist(readIn),toString)
     newList<-list(c(new_group,readString))
     write.table(newList, "group_list.csv",col.names=F,row.names=F)
     } # Editing the table when a duplicate is not detected
    
  })
  
}

shinyApp(ui = ui, server = server)