# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function(){
  print("hello world")
}

#finding dependencies of package?
melda.findDependency <- function(x){
  tools::package_dependencies(x)
}



#' Load a melda.io json
#'
#This function takes melda.io json file. It gives R code blocks of melda.io json file as output
#'
#' @param meldaJson is filepath.
#' @return returns R code blocks of melda.io json file as a data frame
#' @export
melda.read_object <- function(meldaJson){

  json <- jsonlite::fromJSON(meldaJson,simplifyDataFrame = FALSE)

  temp <- vector(mode = "list",  length = length(json$project$stages))

  print("Json file is read")
  tryCatch({

    for(i in 1:length(json$project$stages)){ #looping for stages

      for(j in 1:length(json$project$stages[[i]][[8]])){ # for each R code cell in ith stage

        if(json$project$stages[[i]][[8]][[j]]$language == "R"){


          temp[[i]] <- paste(temp[[i]], json$project$stages[[i]][[8]][[j]]$code, sep = "\n")
          print( paste( "Stage number is:", i, "cell number is ", j))

        }

      }
    }

  },error = function(e) {
    print(paste("Error in stage number:", i,"and cell number:" , j ))
  })

  temp

}



#' Load a Matrix
#'
#This function takes function names as input.
#'
#' @param input function name as a string
#' @return loads the functions' library if it is not loaded.
#' @export

melda.findLibrary <- function(input,load = FALSE){

  if(is.character(input) ){

    df <- help.search(input)

    df <- df$matches

    x  <- strsplit(rem_dup.one(paste(df[df$Topic == input,5],collapse = " ")) , " ")[[1]]
    if(length(x) > 1){

      if(load == TRUE){
        cat(  paste("1. is",x[[1]],"\n","2. is",x[[2]],"\n"), sep = "")

        userInput <-as.numeric( readline(prompt =("Type 1 or 2:  ")))

        print( paste( x[[userInput]],"is choosed","\n",x[[userInput]], "is attaching/loading"), sep= "")

        userLibrary <- as.character(paste(x[[userInput]]) , sep = "")


      }

      print(x[[1]])

    }else if(length(x) == 1){

      if(load  == TRUE){
        print( paste(x[[1]], "is loading"), sep= "")

        userLibrary <- x[[1]]

      }
      print(x[[1]])


    }



  }else{
    print("This is not character")
  }

}




#' Load a Character Vector
#'
#This function takes function names as input.
#'
#' @param x takes  character vector  as an input
#' @return returns function names as list
#' @export
melda.findFunctionName <- function(x){
  x <- all.names(parse(text = x ), unique = TRUE) # converts character vector as R expression
  x
}



#' Remove duplicate words
#'
#This function takes character vector and removes
#'the duplicate words.
#' @param x takes  character vector  as an input
#' @return returns non-duplicated words
#' @export

rem_dup.one <- function(x){
  paste(unique(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T)))),collapse = " ")
}




