
print_text_table <- function(df, title = NULL, header_style = "bold", border_style = "double-single", ...) {
  if (length(title)) cli::cli_h1(title)
  ct <- cli_table(df, header_style = header_style, border_style = border_style, ...)
  cat(ct, sep = "\n")
}