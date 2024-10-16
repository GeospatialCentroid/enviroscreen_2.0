

# create the output file you will be storing text in. 
sink(file = "allcodeastext.txt")

# list all utilities 
utils <- list.files("utilities/")

# list all functions 




# convert a script to plain text 
file <- readLines("path to R script")

# compile that text into the .txt file 
cat(file, sep = "\n")

# export the feature to disk
sink()

