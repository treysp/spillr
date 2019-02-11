# function to add function namespace to every package function call
#   for example, `str_match` would be replaced with `stringr::str_match`
colonify_functions <- function(code = NULL, file_path = NULL) {
if (!require(stringr)) stop("Please install the `stringr` package.")
  
if (!is.null(code) && !is.null(file_path)) {
  stop("Only one of `code` and `file_path` can be specified.")  
}
  
if (!is.null(file_path)) {  
  my_file <- readLines(file_path)
} else {
  my_file <- code
}

libs <- str_match_all(my_file, "library\\((.*?)[,\\)]|require\\((.*?)[,\\)]")
libs <- do.call("rbind", libs)
libs <- ifelse(is.na(libs[, 2]), libs[, 3], libs[, 2])
libs <- libs[!(is.na(libs) | libs == "")]

funs <- lapply(libs, function(x) {
  pack_funs <- getNamespaceExports(x)
  
  data.frame(
    fun = pack_funs,
    fun_colons = paste0(x, "::", pack_funs),
    stringsAsFactors = FALSE
    )
  })
funs <- do.call("rbind", funs)

if (any(duplicated(funs$fun))) {
  warning("Packages contain functions with the same name. Assuming last `library`",
          "or `require` call masked functions in earlier calls.")
  
  masked_funs <- rev(duplicated(rev(funs$fun)))
  funs <- funs[!masked_funs,]
}

locate_fun <- function(fun) {
  paste0(funs[funs$fun_regex == fun, "fun_colons"], "(")
}

funs$fun_regex <- str_replace_all(funs$fun, 
                                  "\\.|\\-|\\[|\\]", 
                                  function(x) paste0("\\", x))

fun_regex <- paste0(funs$fun_regex, "\\(", collapse = "|")

funs$fun_regex <- paste0(funs$fun_regex, "(")

str_replace_all(my_file, fun_regex, locate_fun)
}

library(stringr)

my_file <- readLines(system.file("doc/stringr.R", package = "stringr"))
(my_file <- my_file[1:15])
(colonized <- colonify_functions(code = my_file))


