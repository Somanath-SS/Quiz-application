library(jsonlite)

user <- list()
play <- function() {
  cat("\n==========QUIZ START==========\n")
  score <- 0
  
  # Read the JSON file and convert it to a data frame
  questions <- as.data.frame(jsonlite::fromJSON("E:/code/project/Quiz-Application/assets/questions.json", flatten = TRUE))
  
  for (i in 1:10) {
    no_of_questions <- nrow(questions)
    ch <- sample(1:no_of_questions, 1)
    current_question <- questions[ch, ]
    
    cat(paste("\nQ", i, " ", current_question[["question"]], "\n", sep = ""))
    cat(paste(current_question[["options"]], collapse = "\n"), "\n")
    answer <- tolower(substr(readline("\nEnter your answer (e.g., A, B, C, D): "), 1, 1))
    
    # Compare the first character of the user's answer (converted to lowercase) with the correct answer
    if (answer == tolower(substr(current_question[["answer"]], 1, 1))) {
      cat("\nYou are correct\n")
      score <- score + 1
    } else {
      cat("\nYou are incorrect\n")
    }
    questions <- questions[-ch, ]
  }
  cat(paste("\nFINAL SCORE: ", score, "\n", sep = ""))
}

quizQuestions <- function() {
  if (length(user) == 0) {
    cat("You must first login before adding questions.\n")
  } else if (length(user) == 2 && user[[2]] == "ADMIN") {
    cat('\n==========ADD QUESTIONS==========\n')
    ques <- readline("Enter the question that you want to add:\n")
    opt <- character(4)
    cat("Enter the 4 options with character initials (A, B, C, D)\n")
    for (i in 1:4) {
      opt[i] <- readline()
    }
    ans <- tolower(readline("Enter the answer:\n"))
    
   questions <- jsonlite::fromJSON("E:/code/project/Quiz-Application/assets/questions.json")
    new_question <- data.frame(
      question = ques,
      options = opt,
      answer = ans,
      stringsAsFactors = FALSE
    )
    questions <- rbind(questions, new_question)
    jsonlite::write_json(questions, "E:/code/project/Quiz-Application/assets/questions.json")
    cat("Question successfully added.\n")
  } else {
    cat("You don't have access to adding questions. Only admins are allowed to add questions.\n")
  }
}

createAccount <- function() {
  cat("\n==========CREATE ACCOUNT==========\n")
  username <- readline("Enter your USERNAME: ")
  password <- readline(prompt = 'Enter your PASSWORD: ', secret = TRUE)
  
  user_accounts <- fromJSON("E:/code/project/Quiz-Applicationassets/user_accounts.json")
  
  if (username %in% names(user_accounts)) {
    cat("An account of this Username already exists.\nPlease enter the login panel.\n")
  } else {
    user_accounts[[username]] <- list(password, "PLAYER")
    write_json(user_accounts, "E:/code/project/Quiz-Applicationassets/user_accounts.json")
    cat("Account created successfully!\n")
  }
}

loginAccount <- function() {
  cat('\n==========LOGIN PANEL==========\n')
  username <- readline("USERNAME: ")
  password <- readline(prompt = 'PASSWORD: ', secret = TRUE)
  
  user_accounts <- fromJSON("E:/code/project/Quiz-Applicationassets/user_accounts.json")
  
  if (!username %in% names(user_accounts)) {
    cat("An account of that name doesn't exist.\nPlease create an account first.\n")
  } else if (username %in% names(user_accounts) && user_accounts[[username]][[1]] != password) {
    cat("Your password is incorrect.\nPlease enter the correct password and try again.\n")
  } else if (username %in% names(user_accounts) && user_accounts[[username]][[1]] == password) {
    cat("You have successfully logged in.\n")
    user <<- list(username, user_accounts[[username]][[2]])
  }
}

logout <- function() {
  if (length(user) == 0) {
    cat("You are already logged out.\n")
  } else {
    user <<- list()
    cat("You have been logged out successfully.\n")
  }
}

rules <- function(){
  cat("
==========RULES==========
1. Each round consists of 10 random questions. To answer, you must press A/B/C/D (case-insensitive).
Your final score will be given at the end.
2. Each question consists of 1 point. There's not a negative point for wrong answers.
      3. You can create an account from ACCOUNT CREATION panel.
      4. You can log in using the LOGIN PANEL. Currently, the program can only log in and not do anything more.
      \n ")}

about <- function() {
  cat("
==========ABOUT US==========
This project has been created by Somanath Shyamsundar.
It is a basic R Project for my portfolio.\n")
}

choice <- 1

while (choice != 7) {
  cat('\n=========WELCOME TO QUIZ MASTER==========\n')
  cat('-----------------------------------------\n')
  cat('1. PLAY QUIZ\n')
  cat('2. ADD QUIZ QUESTIONS\n')
  cat('3. CREATE AN ACCOUNT\n')
  cat('4. LOGIN PANEL\n')
  cat('5. LOGOUT PANEL\n')
  cat('6. SEE INSTRUCTIONS ON HOW TO PLAY THE GAME\n')
  cat('7. EXIT\n')
  cat('8. ABOUT US\n')
  
  choice <- as.integer(readline('ENTER YOUR CHOICE: '))
  
  if (choice == 1) {
    play()
  } else if (choice == 2) {
    quizQuestions()
  } else if (choice == 3) {
    createAccount()
  } else if (choice == 4) {
    loginAccount()
  } else if (choice == 5) {
    logout()
  } else if (choice == 6) {
    rules()
  } else if (choice == 7) {
    break
  } else if (choice == 8) {
    about()
  } else {
    cat('WRONG INPUT. ENTER THE CHOICE AGAIN\n')
  }
}
