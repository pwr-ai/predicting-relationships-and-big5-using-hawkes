library(stringr)

read_all_users <- function(datasetFolder) {
  files = list.files(path=datasetFolder, pattern = "telcodata.*")
  users <- str_extract(files, "[0-9]{4,6}")
  return(users)
}
