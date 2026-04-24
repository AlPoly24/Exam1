# Example code for Semantic Word Counter

# 1. Load the devtools library
library(devtools)

# 2. Load and Check
# Load the package functions properly
devtools::load_all()

# 3. Run the Semantic Analysis
# Select your diary text file (e.g., Example_negative.txt)
file_to_test <- file.choose()

# 4. Execute the full automated algorithm
results <- summarize_sentiment(file_to_test)

# 5. View Results
cat("\n--- SENTIMENT RESULTS ---\n")
print(results)

# 6. Verification
# ee whether the preprocessing works as intended
words <- preprocess_text(file_to_test)
cat("\n--- FIRST 20 PREPROCESSED WORDS ---\n")
print(head(words, 20))
