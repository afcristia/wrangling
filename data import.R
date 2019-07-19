########
# Getting data from the dslabs package
#
#
######

path <- system.file("extdata", package = "dslabs")
list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath    # just printing the full path of the file we want to copy
file.copy(fullpath, getwd())   # actual copy
file.exists(filename) # double checking the file was indeed copied over

