---
title: "CodeBook"
output: html_notebook
---

# **Getting and Cleaning Data Course Project**

## 1 Merges the training and the test sets to create one data set

For the first exercise, I have used the following txt files:

1.  **features.txt** table that contains the name of each variable recorded

2.  **Test Tables:**

    1.  ***x_test.txt:*** table that contains the variable results from the test

    2.  ***y_test.txt:*** table that contains the activity performed for each observation from the test

    3.  ***subject_test.txt:*** table that identify the volunteer from each observation in the test data frame

3.  **Train Tables:**

    1.  ***x_train.txt:*** table that contains the variable results from the train

    2.  ***y_train.txt:*** table that contains the activity performed for each observation from the train

    3.  ***subject_train.txt:*** table that identify the volunteer from each observation in the training data frame

This exercise aims to add activity and subject columns to the observation table from both directories (train and test folder). I also labelled each column with its descriptive name ( this was supposed to be done in the fourth exercise, but, from my point of view, it makes more sense to do it since the beginning.

For this purpose, I wrote two functions in run_analysis.R called C_tbl and M_tbl . The first function C_tbl just need one parameter, the folder name (test or train), and its objective is to create both data frames:

```{r}
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
```

M_tbl is in charge of merging both tables; it only needs two parameters: the data frames resulting from the previous function

```{r}
M_tbl  <-  function (A, B) {
  
  rbind(A, B)
  
}
```

Calling both functions as it follows, we get the desired result

```{r}
result  <-  M_tbl(C_tbl("test"), C_tbl("train") )
```

     dim(result)
    [1] 10299   563

## 2 Extracts only the measurements on the mean and standard deviation for each measurement.

for extracting the measurements on the mean and standard deviation for each measurement, I created a function in run_analysis.R called Scol_tbl

```{r}
Scol_tbl <-  function (X, ...) {
  
      library(dplyr)    
      Searchterms <-  paste(list(...), collapse = "|")
      fields  <-  grep(Searchterms, names(X) )
      Colresult  <-   names(X)[fields]
      Colresult   <-   c("volunteers","activity",  Colresult)
      Scol_Tbl    <-   X[,Colresult]
}

```

Scol_tbl needs at least two parameters, the data frame (X) that in our case is the result from the previous exercise (result) and the ... parameters, which are the search term we need that in this case is "mean" and "std".

```{r}
result2 <- Scol_tbl (result, "mean", "std")

```

```{r}
 dim (result2)
[1] 10299    81

```

## 3. Uses descriptive activity names to name the activities in the data set

For the third exercise, I wrote a new function (Label_tbl)

```{r}
Label_tbl <-  function (tbl_a) {
  library(dplyr)  
  current_project <- "./"
  labels   <-   read.delim(file.path(current_project, "activity_labels.txt"),header = FALSE, sep="")
  names(labels) <- c("ID","activity_label")
  Label_Tbl <- inner_join(tbl_a, labels, c("activity" = "ID"))
}

```

This function matches the results from the previous exercise with the table labels from the activity_labels.txt . the only parameter it needs is the data frame from exercise 2 (result2)

```{r}
result3 <- Label_tbl(result2)

```

```{r}
dim(result3)
[1] 10299    82
```

As you can see, we have added one more column.

## 4. Appropriately labels the data set with descriptive variable names

This task was performed in the first exercise.

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

to perform this task I wrote this function:

```{r}
Mean_tbl_field <- function(X, field1, field2){
  Mean_tbl_field <- aggregate(select_if(X,is.numeric), list(X[,field1], X[,field2]), FUN=mean)
}

```

X parameter is the data frame from exercise 3 (result3), and field1 and field2 are the grouping field. In our case, as I named those fields as "activity_label", "volunteers", I will pass those string chains as parameters.

```{r}
result4  <-  Mean_tbl_field(result3, "activity_label", "volunteers" )

```

```{r}
dim (result4)
[1] 180  83
```
