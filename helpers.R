options( stringsAsFactors = FALSE)

## Before using this library, define LOGDIR constant
## which should be the name of the directory, where the log fils are to be saved.
## LOGDIR <- "logs"

processError <- function(e, info) {
    errorText <- paste(info, "--", "R output error:", e)
    print(errorText)
    log(errorText)
}


log <- function(logText, logFile = getLogFile()){
    append2File(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), logText), logFile)
}

logAndPrint <- function(logText, logFile = getLogFile()){
    print(logText)
    log(logText, logFile)
}

getLogFile <- function(dir = LOGDIR) {
    return(file.path(dir, format(Sys.time(), "log-%Y-%m-%d.txt")))
}

append2File <- function(text, fileIn) {
    ## Check if file exists
    if (fileExists(fileIn)) {
        tryCatch(cat(as.character(text), sep="\n", file=fileIn, append=TRUE),
                 error = function(e){processError(e,
                     paste("Error while appending into file", fileIn))})
    } else {
        tryCatch(cat(as.character(text), sep="\n", file=fileIn),
                 error=function(e){processError(e,
                     paste("Error while writing into file", fileIn))})
    }
}

fileExists <-function(fileIn) {
    return(tryCatch(file.exists(fileIn),
                    error=function(e) {processError(e,
                        paste("Error while checking file existence",
                              as.character(fileIn)))}))
}

