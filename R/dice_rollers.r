roll_scatter = function()
{
  row = sample(NROW(gmh_directions), 1)
  df = gmh_directions[row, ]
  
  g = ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::theme_void() +
    ggplot2::geom_segment(ggplot2::aes(xend=xend, yend=yend), arrow=ggplot2::arrow(), size=2.5) +
    ggplot2::scale_x_continuous(limits = c(-1, 1)) +
    ggplot2::scale_y_continuous(limits = c(-1, 1))
  
  g
}
