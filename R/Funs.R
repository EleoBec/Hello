#' Hello World
#'
#' `hello` says _'Hello, world!'_
#'
#' @usage hello(who="world", lang="en",langdata)
#'
#' @param who specifies the name of the person to whom the “hello” message is addressed. This must be a character vector of length 1.
#'
#' @param lang a character vector of length 1 that specifies the user preferred language. This can be "EN" (the default value) for English, "FR" for French, “IT” for Italian, “ES” for Spanish, or “DE” for German.
#' Case should be ignored the code should consider ‘a’ and ‘A’ to be the same characters. If an undefined language is chosen, the function must show a message about that (see below).
#'
#' @param langdata an optional data.frame with two columns. The first column gives the language codes and the second column gives the corresponding “hello” word. The default value should be set to the data.frame language defined above.
#'
#' @return a character vector with a personalized "hello" message
#'
#' @examples hello("James")
#' hello("Amelia")
#'
#' @export

hello <- function(who="world", lang="en", langdata=language[1,1]) {
  if(langdata==language[1,1]) {
    if(any(language$code==tolower(lang))){
      hello=language$hello[language$code==tolower(lang)]
      phrase<-stringr::str_c(c(hello, who, "!"), collapse = ", ")
      cat(phrase)
      #cat(hello,", ", who, "!")
      }
    else{
      phrase<-stringr::str_c(c("Sorry,", who, "your language (", lang, ") is not available!"), collapse = ", ")
      cat(phrase)
      #cat("Sorry,", who, "your language (", lang, ") is not available!")
      }
  }
  else{
    hello=langdata[1,2]
    phrase<-stringr::str_c(c(hello, who, "!"), collapse = ", ")
    cat(phrase)}
}
