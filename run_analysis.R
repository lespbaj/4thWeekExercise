
C_tbl <- function (folder){
    current_project <- "./"
    x <-  read.delim(file.path(current_project, folder, paste("x_", folder , ".txt", sep="")),header = FALSE, sep = "")
    activity  <- read.delim(file.path(current_project, folder, paste("y_", folder , ".txt", sep="")),header = FALSE, sep = "")
    vars <- read.delim(file.path(current_project, "features.txt"),header = FALSE, sep="")
    subject  <-  read.delim(file.path(current_project, folder, paste("subject_", folder , ".txt", sep="")),header = FALSE, sep = "")
    names(subject) <- "volunteers"
    names(activity) <- "activity"
    names(x) = vars[,2]
    tbl_a <- cbind(subject, activity , x)
    
   
}

M_tbl  <-  function (A, B) {
  
  rbind(A, B)
  
}

Scol_tbl <-  function (X, ...) {
  
      library(dplyr)    
      Searchterms <-  paste(list(...), collapse = "|")
      fields  <-  grep(Searchterms, names(X) )
      Colresult  <-   names(X)[fields]
      Colresult   <-   c("volunteers","activity",  Colresult)
      Scol_Tbl    <-   X[,Colresult]
}
Label_tbl <-  function (tbl_a) {
  library(dplyr)  
  current_project <- "./"
  labels   <-   read.delim(file.path(current_project, "activity_labels.txt"),header = FALSE, sep="")
  names(labels) <- c("ID","activity_label")
  Label_Tbl <- inner_join(tbl_a, labels, c("activity" = "ID"))
}
Mean_tbl_field <- function(X, field1, field2){

  Mean_tbl_field <- aggregate(select_if(X,is.numeric), list(X[,field1], X[,field2]), FUN=mean)
  
  
}
Expor_txt <- function(X, name){
    current_project <- "./"
    file  <-  paste( name , ".txt")
    write.table(X, file, append = FALSE, sep = " ", dec = ".",
                row.names = TRUE, col.names = TRUE)
  
}