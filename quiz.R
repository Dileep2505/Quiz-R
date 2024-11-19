library(shiny)

# Define UI
ui <- fluidPage(
  navbarPage(
    title = "Quiz App",
    
    # Quiz Page
    tabPanel("Quiz",
             h2("10-Question Quiz"),
             uiOutput("quizUI"),
             actionButton("submit", "Submit Answers", class = "btn-primary"),
             actionButton("restart", "Restart Quiz", class = "btn-warning")
    ),
    
    # Results Page
    tabPanel("Results",
             h2("Your Results"),
             verbatimTextOutput("scoreOutput")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Questions and Answers
  questions <- list(
    list(question = "What is the capital of France?",
         options = c("Berlin", "Paris", "Madrid", "Rome"),
         correct = "Paris"),
    list(question = "What is 5 + 3?",
         options = c("6", "7", "8", "9"),
         correct = "8"),
    list(question = "Who wrote 'Romeo and Juliet'?",
         options = c("Charles Dickens", "William Shakespeare", "Mark Twain", "Jane Austen"),
         correct = "William Shakespeare"),
    list(question = "What is the largest planet in our Solar System?",
         options = c("Earth", "Mars", "Jupiter", "Saturn"),
         correct = "Jupiter"),
    list(question = "Which element has the chemical symbol 'O'?",
         options = c("Oxygen", "Gold", "Osmium", "Mercury"),
         correct = "Oxygen"),
    list(question = "What is the square root of 49?",
         options = c("5", "6", "7", "8"),
         correct = "7"),
    list(question = "Who painted the Mona Lisa?",
         options = c("Vincent van Gogh", "Leonardo da Vinci", "Pablo Picasso", "Claude Monet"),
         correct = "Leonardo da Vinci"),
    list(question = "What is the smallest prime number?",
         options = c("0", "1", "2", "3"),
         correct = "2"),
    list(question = "Which country is known as the Land of the Rising Sun?",
         options = c("China", "Japan", "Thailand", "India"),
         correct = "Japan"),
    list(question = "What is the boiling point of water at sea level (in Celsius)?",
         options = c("50", "100", "150", "200"),
         correct = "100")
  )
  
  # Reactive value to hold the shuffled questions and answers
  shuffled_questions <- reactiveVal()
  
  # Shuffle the questions and answer options
  shuffle_questions <- function() {
    shuffled <- sample(questions, length(questions))
    shuffled <- lapply(shuffled, function(q) {
      q$options <- sample(q$options)  # Shuffle the answer choices
      return(q)
    })
    shuffled_questions(shuffled)
  }
  
  # Initial shuffle when app starts
  shuffle_questions()
  
  # Generate UI for Questions
  output$quizUI <- renderUI({
    questions_to_display <- shuffled_questions()
    tagList(
      lapply(seq_along(questions_to_display), function(i) {
        question <- questions_to_display[[i]]
        div(
          h4(paste0(i, ". ", question$question)),
          radioButtons(
            inputId = paste0("q", i),
            label = NULL,
            choices = question$options,
            selected = character(0),  # Default to no selection
            inline = TRUE
          )
        )
      })
    )
  })
  
  # Calculate and Display Results
  observeEvent(input$submit, {
    score <- 0
    questions_to_display <- shuffled_questions()
    
    for (i in seq_along(questions_to_display)) {
      user_answer <- input[[paste0("q", i)]]
      correct_answer <- questions_to_display[[i]]$correct
      if (!is.null(user_answer) && user_answer == correct_answer) {
        score <- score + 1
      }
    }
    
    # Switch to Results Page immediately after submission
    updateNavbarPage(session, "quiz", selected = "Results")
    
    output$scoreOutput <- renderText({
      paste0("You scored ", score, " out of ", length(questions_to_display), ".")
    })
  })
  
  # Restart the quiz and shuffle questions and answers again
  observeEvent(input$restart, {
    # Reset the UI to the Quiz page and shuffle questions again
    shuffle_questions()  # Shuffle questions when restarting the quiz
    updateNavbarPage(session, "quiz", selected = "Quiz")
    
    # Reset the radio button selections
    questions_to_display <- shuffled_questions()
    lapply(seq_along(questions_to_display), function(i) {
      updateRadioButtons(session, paste0("q", i), selected = character(0))
    })
    
    output$scoreOutput <- renderText("")  # Clear previous results
  })
}

# Run the App
shinyApp(ui = ui, server = server)
