print_text_table_with_huxtable <- function(df, title = NULL, footnote = NULL, 
  heatmap_columns = NULL, hilite_rows = NULL,
  styler = huxtable_text_table_default_styler, ...) 
{
  ### build the huxtable
  tt <- huxtable::as_hux(df)

  if (length(footnote)) tt <- huxtable::add_footnote(tt, footnote)
  if (length(title)) tt <- huxtable::set_caption(tt, title)
  

  if (length(styler)) tt <- styler(tt, heatmap_columns = heatmap_columns, hilite_rows = hilite_rows)

  # now print
  lines <- huxtable::to_screen(tt, max_width = Inf, colnames = FALSE)
  cat(lines, sep = '\n')

  invisible(tt)
}

huxtable_text_table_default_styler <- function(tt,  
  heatmap_columns = NULL, heatmap_colorspace = c('green', 'red'),
  hilite_rows = NULL,  hilite_bg = 'red'
, fg = 'black', bg = '#f7f7f7') 
{
  ### set the title position: top
  huxtable::caption_pos(tt) <- 'top'
  ### make the header line BOLD
  tt <- huxtable::set_bold(tt, 1, huxtable::everywhere, TRUE)

  ### colors
  tt <- huxtable::set_background_color(tt, 1, huxtable::everywhere, bg)
  tt <- huxtable::set_text_color(tt, 1, huxtable::everywhere, fg)

  ### hilite
  tt <- huxtable::set_background_color(tt, hilite_rows + 1, huxtable::everywhere, hilite_bg)

  ### heatmap
  if (length(heatmap_columns))
    for (col in heatmap_columns)
      tt <- huxtable::map_background_color(tt, huxtable::everywhere, col, huxtable::by_colorspace(heatmap_colorspace))


  ### borders
  tt <- huxtable::set_bottom_border(tt, 1, huxtable::everywhere, 1)
  tt <- huxtable::set_left_border(tt, 1)
  tt <- huxtable::set_outer_borders(tt, 1)
  tt <- huxtable::set_bottom_border(tt, huxtable::final(1), huxtable::everywhere, 1)

  ### alignment
  tt <- huxtable::set_align(tt, value = 'left')

  tt
}

print_text_table_with_base <- function(df, ...) {
  print(df)
  invisible()
}

print_text_table <- function(df, ...) {
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    print_text_table_with_base(df, ...)
  } else {
    print_text_table_with_huxtable(df, ...)
  }
  invisible()
}
