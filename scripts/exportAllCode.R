

# create the output file you will be storing text in. 
sink(file = "allcodeastext.txt")

# list all utilities 
utils <- list.files("utilities",
                    pattern = ".R",
                    full.names = TRUE)

# list all functions 
functions <- list.files("functions",
                        pattern = ".R",
                        recursive = TRUE,
                        full.names = TRUE)


# store data to file 
cat("Utilities Section", sep = "\n")
for(i in utils){
  # convert a script to plain text 
  file <- readLines(i)
  cat(file, sep = "\n")
}
cat("Functions Section", sep = "\n")
for(i in functions){
  # convert a script to plain text 
  file <- readLines(i)
  cat(file, sep = "\n")
}

# export the feature to disk
sink()

