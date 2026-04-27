#' @title Semantic Word Counter
#' @description This is a package to process text and calculate a sentiment score based on
#' the presence of positive and negative words in the text.
#' The core functionality is divided into three parts:
#' FUNCTION 1: Preprocessing
#'          ## Cleans and tokenizes raw text.
#' FUNCTION 2: Semantic Comparison
#'          ## Handles the matching logic.
#' FUNCTION 3: Summarizing and Automation
#'          ## Runs the full analysis.
#' @docType package
#' @name SemanticAlkinoos
#' @author Panagiotis Alkinoos Polychronopoulos
NULL

# --- FUNCTION 1: Preprocessing ---

#' Preprocess Text Data
#' @description Loads a file using read.delim and converts text to a cleaned word vector.
#' @param file_path A string path to the .txt file.
#' @export
#' @examples
#' # words <- preprocess_text("diary.txt")
preprocess_text <- function(file_path) {
  # Use read.delim()
  # We set sep = "\n" so it treats each line as a row, ensuring we get the whole file.
  # We set quote = "" to prevent it from failing if there are single/double quotes in the diary.
  raw_data <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE, sep = "\n", quote = "")

  # Transform all uppercase letters to lowercase
  # raw_data[[1]] accesses the column of text
  clean_text <- tolower(raw_data[[1]])

  # Create a vector with all words as separate elements
  # We combine all lines and then split by spaces, tabs (\t), and punctuation
  all_text <- paste(clean_text, collapse = " ")

  # Use strsplit to remove spaces and stopwords
  # Using [.,; \t]+ catches dots, commas, semicolons, spaces, and tabs (\t)
  words_vector <- unlist(strsplit(all_text, split = "[.,; \t]+", fixed = FALSE))

  # Remove empty elements
  words_vector <- words_vector[words_vector != ""]

  return(words_vector)
}

# --- FUNCTION 2: Semantic Comparison ---

#' Compare Word Vectors
#' @description Compares document words to sentiment vectors using loops and wildcards.
#' @param doc_vector A character vector of words from the document.
#' @param sentiment_vector A character vector of sentiment keywords (can use *).
#' @export
#' @examples
#' # count <- compare_sentiment(c("nauseated", "happy"), c("nause*", "happ*"))
compare_sentiment <- function(doc_vector, sentiment_vector) {
  count <- 0

  # Instruction: This requires at least one loop
  for (word in doc_vector) {
    for (s_word in sentiment_vector) {

      # Instruction: identify the character "*" as suffix or prefix
      # Using startsWith for suffix wildcards (nause*)
      # Using endsWith for prefix wildcards (*nause)
      if (grepl("\\*$", s_word)) { # Suffix check
        root <- gsub("\\*", "", s_word)
        if (startsWith(word, root)) {
          count <- count + 1
          break
        }
      } else if (grepl("^\\*", s_word)) { # Prefix check
        root <- gsub("\\*", "", s_word)
        if (endsWith(word, root)) {
          count <- count + 1
          break
        }
      } else {
        # Exact match
        if (word == s_word) {
          count <- count + 1
          break
        }
      }
    }
  }
  return(count)
}

# --- FUNCTION 3: Summarizing and Automation ---

#' Summarize Sentiment
#' @description Summarizes results of positive and negative word matching.
#' @param file_path A string path to the .txt file.
#' @export
#' @examples
#' # results <- summarize_sentiment("diary.txt")
summarize_sentiment <- function(file_path) {
  # Create a vector of negative words and positive words. We are fetching them automatically.
  message("Downloading sentiment lexicons...")

  get_clean_web_list <- function(url_in) {
    con <- url(url_in, encoding = "latin1")
    lines <- readLines(con, warn = FALSE)
    close(con)
    lines <- lines[!grepl("^;", lines)]
    return(lines[lines != ""])
  }

  pos_lexicon <- get_clean_web_list("https://ptrckprry.com/course/ssd/data/positive-words.txt")
  neg_lexicon <- get_clean_web_list("https://ptrckprry.com/course/ssd/data/negative-words.txt")

  # Process document
  doc_words <- preprocess_text(file_path)

  # For an absolute number of positive and negative words
  pos_hits <- compare_sentiment(doc_words, pos_lexicon)
  neg_hits <- compare_sentiment(doc_words, neg_lexicon)

  # For a value/ratio
  total_words <- length(doc_words)
  ratio <- if (neg_hits > 0) pos_hits / neg_hits else pos_hits

  return(list(
    absolute_positive = pos_hits,
    absolute_negative = neg_hits,
    sentiment_ratio = round(ratio, 2),
    total_words = total_words,
    verdict = ifelse(pos_hits > neg_hits, "Positive", "Negative")
  ))
}
