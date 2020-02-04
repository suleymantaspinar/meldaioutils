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
  print("Hello Melda")
}

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
    for(i in 1:length(json$project$stages)){ #looping for stages // i is the stage number      # print( paste( "Stage number is:", i))
      z <- 1
      for(j in 1:length(json$project$stages[[i]][[8]])){ # for each R code cell in ith stage // j is the cell number # print( paste( "Stage number is:", i, "cell number is ", j))
        if(json$project$stages[[i]][[8]][[j]]$language == "R"){
          temp[[i]][[z]] <-  json$project$stages[[i]][[8]][[j]]$code
          temp[[i]][[z]] <- as.list(temp[[i]][[z]]) # print( paste( "Stage number is:", i, "cell number is ", j))
          z <- z + 1
        }
      }
    }
  },error = function(e) {
    # print(paste("Error in stage number:", i,"and cell number:" , j,e ))
  })
  return(temp)
}


rem_dup.one <- function(x){

  paste(unique(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T)))),collapse = " ")

}

#' Load a Matrix
#'
#This function takes expression as input.
#'
#' @param expr is R expression   in chr
#' @return loads the functions' library if it is not loaded.
#' @export
melda.searchLibrary <- function(expr){

  if(!is.null(expr) && length(expr) != 0){ #debugging
    chr <- strsplit(expr, "\n" )[[1]]
    temp <- ""
    print(length(chr))
    if( (length(chr) == 1 && grepl("library\\(",chr)) || grepl("require\\(",chr)|| grepl("devtools",chr) || grepl("install.packages\\(",chr)){ #checking for
      print("Library founded")
      y <- regexpr( "\\(.*?\\)" , chr , perl = TRUE)
      div <- regmatches( chr,y )
      div <- rem_dup.one(div)
      div <- gsub("'","",div)
      return(div)

    }else if( length(chr) > 1){

      for(a in 1:length(chr)){

        if( grepl("library\\(",chr[[a]]) || grepl("require\\(",chr[[a]]) || grepl("devtools",chr[[a]]) ||  grepl("install.packages\\(",chr[[a]])){
          print("Library founded")
          y <- regexpr( "\\(.*?\\)" , chr[[a]] , perl = TRUE)
          div <- regmatches( chr[[a]],y )
          temp <- paste(temp,div,sep = " \n ")
          temp <- sub("\n"," ",temp)
        }
      }

      temp <- rem_dup.one(temp)
      temp <- gsub('"',"",temp)
      temp <- strsplit(temp," ")[[1]]
      return( temp[-1] )

    }else{

      print("Library not found")
      return(NULL)
    }
  }else{

    print("Library not found")
    return(NULL)
  }

}

#' Load a Character Vector
#'
#This function takes function names as input.
#'
#' @param x takes  character vector  as an input
#' @return returns function names as list
#' @export
melda.findFunctionName <- function(chr){
  tryCatch({
  if(is.character(chr)){
      chr <- getInputs(parse(text = chr))
      chr <- chr@functions
      chr <- names(chr)
      return(chr)
  }else{
    "It's not an expression"
  }
  },error = function(e){
    print(e)
  })
}
#' Load a Matrix
#'
#This function takes function names as input.
#'
#' @param input function name as a string
#' @return loads the functions' library if it is not loaded.
#' @export
melda.findLibrary <- function(input,load = FALSE, dblcolon = FALSE){
  tryCatch(
    {
      # input <-gsub("\\[","\\\\[",input)
      df <- help.search(input)
      df <- df$matches
      x  <- strsplit(rem_dup.one(paste(df[df$Topic == input,5],collapse = " ")) , " ")[[1]]
    },error = function(e){
      return(NULL)
    }
  )
  if(length(x) > 1){
    if(load == TRUE){
      cat(  paste("1. is",x[[1]],"\n","2. is",x[[2]],"\n"), sep = "")
      userInput <- as.numeric( readline(prompt =("Type 1 or 2:  ")))
      print( paste( x[[userInput]],"is choosed","\n",x[[userInput]], "is attaching/loading"), sep= "")
      userLibrary <- as.character(paste(x[[userInput]]) , sep = "")
    }
    if(dblcolon == T ){

      for(i in 1:length(x)){
        x[[i]] <- paste(x[[i]],"::",input,sep ="")
      }
      return(x)
    }
    return(x)
  }else if(length(x) == 1){

    if(load  == TRUE){
      print( paste(x[[1]], "is loading"), sep= "")
      userLibrary <- x[[1]]
    }

    if(dblcolon == T ){
      print(paste(x,"::",input,sep =""))
    }
    return(x)
  }else{
    print("Function not found.")
    return(NULL)
  }
}




#' Load a Matrix
#'
#This function takes function names as input.
#'
#' @param funcName function name as a string
#' @return loads the functions' library if it is not loaded.
#' @export
melda.findLibraryInDefPkgs <- function(funcName){
  defaultLibs <- sessionInfo()
  defaultLibs <- c(defaultLibs$basePkgs,names(defaultLibs$otherPkgs))
  libName <- melda.findLibrary( funcName )
  if( !is.null(libName) && length(libName) == 1){
    libName <- libName
    funcName <- paste(defaultLibs[[ind]], "::", funcName,sep = "")
  }else if( !is.null(libName) && length(libName) > 1){
    for( m in 1:length(libName)){
      tryCatch({
        if( grep( libName[[m]],defaultLibs) != 0){
          ind <- grep(libName[[m]],defaultLibs)
          libName <- defaultLibs[[ind]]
          funcName <- paste(defaultLibs[[ind]], "::",funcName,sep = "")
          break()
        }else{
          libname <- NULL
        }
      },error = function(e){print(paste("Function is not found in default libraries"))})
    }
  }else{
    libName <- "-"
    funcName <- funcName
  }
  return(data.frame(libName = as.character(libName),
                    funcName =as.character(funcName),stringsAsFactors = F))
}


