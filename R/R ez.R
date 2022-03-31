#' Fix path
#'
#' This function fixes the path of the windows files to be used in R.
#' @examples
#' repath()
#' @export
repath <- function() {
  cat('Pega el directorio y da ENTER dos veces')
  x <- scan(what = "")
  xa <- gsub('\\\\', '/', x)
  writeClipboard(paste(xa, collapse = " "))
  cat('Tu directorio corregido. (Queda en el clipboard)\n', xa, '\n')
}

#' Quickly copy a df
#'
#'This function quickly copies an object of type df.
#
#' @examples
#' ctrl_c(iris) # Use this function to copy the clipboard
#' @export
ctrl_c <- function(df) {
  write.table(
    df,
    "clipboard-16384",
    sep = "\t",
    row.names = FALSE,
    na = ""
  )
}

#' Quickly import a df to R
#'
#'This function quickly imports a df that is contained in the clipboard to R.
#
#' @examples
#' ctrl_v() # Use this function to import the df
#' @export
ctrl_v <- function(nom = TRUE)
{
  if (nom != T & nom != F)
  {
    cat("The parameter must be TRUE or FALSE, to indicate if the data contains headers")
    x = ""
  } else
  {
    x <- read.table("clipboard-16384", sep = "\t", header = nom,na = NA)
  }
  return(x)
}

#' Get the total of numeric variables
#'
#' @param df Object of type dataframe
#'
#'This function calculates the total of all numerical variables, there must be at least one variable of any type other than numerical.
#
#' @examples
#' get_total(iris)
#' @export
get_total = function(df) {

  df = df %>% select(where(~ !is.numeric(.x)), where(~ is.numeric(.x)))
  Total = df %>%
    mutate(across(.cols = where(~ is.numeric(.x)), .fns = ~ sum(., na.rm = T))) %>%
    head(1)
  df = df %>% rbind(Total) %>%
    mutate(across(.cols = 1, as.character))
  df[(dim(df)[1]), 1] = "Total"

  return(df)

}

#' Most recent file
#'
#'Returns the name of the most recent file in the directory
#
#' @examples
#' getwd() # Get of the directory
#' recent_files()
#' @export
recent_files <- function(wd = NULL)
{

  if (!is.null(wd))
  {
    x = list.files(wd)[which.max(file.info(list.files(wd, full.names = T))$mtime)]
  } else
  {
    x = list.files()[which.max(file.info(list.files(full.names = T))$mtime)]
  }
  return(x)

}

#' Oldest file
#'
#'Returns the name of the oldest file in the directory.
#
#' @examples
#' getwd() # Get of the directory
#' old_files()
#' @export
old_files <- function(wd = NULL)
{

  if (!is.null(wd))
  {
    x = list.files(wd)[which.min(file.info(list.files(wd, full.names = T))$mtime)]
  } else
  {
    x = list.files()[which.min(file.info(list.files(full.names = T))$mtime)]
  }
  return(x)

}

#' Date sequence
#'
#'This function returns a sequence of dates.
#
#' @param date1 Vector of type character or date
#' @param date2 Vector of type character or date
#' @examples
#' seq_dates("2021-01-01","2021-01-31")
#'
#' seq_dates("20210101","20210131")
#' @export
seq_dates = function(date1, date2) {

  if ((grepl("-|/", date1) & grepl("-|/", date2)) == T)
  {
    return(seq(as.Date(date1), as.Date(date2), by = "days"))
  }
  else
  {
    return(seq(ymd(date1), ymd(date2), by = "days"))
  }
}

#' Count duplicates
#'
# 'This function returns the sum of the duplicate values for a previously grouped database
#
#' @param x Vector of numeric type or character
#' @examples
#' data_example = data.frame(x = c(1,2,2,2,3,3))
#' @export
duplicates_count <- function(x) {
  return(sum(as.numeric(duplicated(x)), na.rm = T))
}

#' melt data frame by date
#'
#'This function returns a data frame in long format, where the dates are completed from start to end by some id.
#
#' @param data Object of type dataframe
#' @param start date type value
#' @param end date type value
#' @param id Character value that will be used as id to melt the data frame from start to end by dates
#' @examples
#' data_example = data.frame(id = c(1, 2),
#' start = as.Date(c("2021-01-01", "2021-01-10")),
#' end = as.Date(c("2021-01-10", "2021-01-15")))
#' melt_date(data = data_example, start = start, end = end, id = id)
#' @export
melt_date <- function(data, start, end, id) {

  df = data %>%
    rowwise() %>%
    do(data.frame(id = .$id,
                  fecha = seq(.$start, .$end, by = "1 day")))

  return(df)
}

#' Expand data frame
#'
#'This function expands a dataframe to a desired dimension.
#
#' @param df Object of type dataframe
#' @param n Integer value
#' @examples
#' df_example = data.frame(var_1 = letters[1:3],var_2 = seq(1:3))
#'
#' expand_df(df_example,5)
#' @export
expand_df <- function(df, n) {

  df = do.call(rbind, lapply(seq_len(n), function(x)return(df)))
  return(df)

}

#' Fast Alpha
#'
#'Quick function to remove non-alphanumeric character
#
#' @param x Vector of type character
#' @examples
#' x = c("!"#$%&/String")
#' fast_alpha(x)
#' @export
fast_alpha <- function(x){gsub("[^[:alnum:]]","",x)}

#' Convert dates from Excel
#'
#'This function returns a vector of dates with excel origin, which is "1899-12-30"
#
#' @param x numerical vector
#' @examples
#' x = c(44197,44198,44198)
#' date_excel(x)
#' @export
date_excel <- function(x){as.Date(as.numeric(x),origin = "1899-12-30")}

#' Convert dates from R
#'
#'This function returns a vector of dates with R origin, which is "1970-01-01"
#
#' @param x numerical vector
#' @examples
#' x = c(18628,18629,18630)
#' date_R(x)
#' @export
date_R <- function(x){as.Date(as.numeric(x),origin = "1970-01-01")}

#' Convert dates from Stata
#'
#'This function returns a vector of dates with Stata origin, which is "1960-01-01"
#
#' @param x numerical vector
#' @examples
#' x = c(22281,22282,22283)
#' date_stata(x)
#' @export
date_stata <- function(x){as.Date(as.numeric(x),origin = "1960-01-01")}

#' Style elegant xlsx
#'
#' This function expands a dataframe to a desired dimension.
#
#' @param sheet Sheet name
#' @param name file name
#' @param data Database name
#' @examples
#' write.xlsx_format("example","iris.xlsx",iris)
#' @export
write.xlsx_format = function(sheet, name, data) {
  wb = createWorkbook()
  addWorksheet(wb, sheet)
  bold.style <- createStyle(textDecoration = "Bold")
  setColWidths(wb, sheet, cols = 1:ncol(data), widths = 21)

  addStyle(
    wb,
    sheet,
    createStyle(
      fgFill = "#ff0000",
      fontColour = "#FFFFFF",
      wrapText = T,
      valign = "center",
      halign = "center",
      borderStyle = "thin",
      border = "TopBottomLeftRight",
      borderColour = "black",
      textDecoration = "bold",
      fontSize = 11
    ),
    cols = 1:ncol(data),
    rows = 1,
    gridExpand = T
  )

  addStyle(
    wb,
    sheet,
    cols = 1:(ncol(data) + 1) ,
    rows = 2:(nrow(data) + 1),
    gridExpand = TRUE,
    style = createStyle(halign = 'center')
  )
  writeData(
    wb,
    sheet,
    data,
    headerStyle = bold.style
  )
  saveWorkbook(wb, name , overwrite = T)
}

#' Create training and test df
#'
#'This function returns a list with two df, which are separated with probability p.
#
#' @param df Object of type dataframe
#' @param p Value between 0 and 1 that contains the probability of separating the df
#' @examples
#'
#' train_test(iris,0.8)
#' @export
train_test <- function(df , p) {

  set.seed(1234)

  dim_data   <- dim(df)[1]
  sample_1   <- sample(x = dim_data ,size = p * dim_data , replace = FALSE)
  data_train <- df[ c(sample_1), ]
  data_test  <- df[-c(sample_1), ]

  lista_datas <- list(data_train = data_train, data_test = data_test)

  return(lista_datas)

}


#' Convert to number
#'
#'This function returns a numeric value or vector, which can be integer or decimal, containing non-numeric characters that prevent transformation.
#
#' @param x Vector of numeric type or character
#' @examples
#' x = c("1","2","3.1","4,2","5,0")
#' to_num(x)
#' @export
to_num <- function(x) {
  x = as.numeric(gsub("[^0-9.]", "", gsub("\\,", "\\.",as.character(x))))
  return(x)
}

#' Fix Rut
#'
#' This function fixes the rut
#' @param x Vector of numeric type or character
#' @examples
#' x = c("012345678-9","0123456789 "," 0.12345678 9")
#' fix_rut(x)
#' @export
fix_rut <- function(x) {

  print("Estandarizando RUTS")

  x = gsub("\\([^()]*\\)", "", x)
  x = gsub("(?<=^| )0+", "", x, perl = TRUE)
  x = gsub("[^[:alnum:]]", "", x)
  x = stri_trans_general(toupper(x),"Latin-ASCII")

  return(x)
}

#' Clean character
#'
#' This function clears a text string, removing accents and non-alphanumeric characters.
#
#' @param x Vector of type character
#' @examples
#' x = c("Test Message (?)")
#' clean_chr(x)
#' @export
clean_chr <- function(x) {
  x = stri_trans_general(toupper(x),"Latin-ASCII")
  x = gsub("^\\s+|\\s+$", "", gsub("\\([^()]*\\)", "", x))

  return(x)
}


















