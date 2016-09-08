#documentation start
#=============================================================================
# File data
# creator: downloaded from http://www.colbyimaging.com/wiki/statistics/color-bars
# acknowledgements: 
# primary authority: 
# other authorities: 
#=============================================================================
# File contents
# creates color bar, useful when using color coded variables
# input: 
# lut - colorRampPalette (which colors are to be used, same as in colorCode)
# min and max of coded variable
# nticks - number of ticks to be displayed on color bar
# output:
# plotted color bar
# dependencies:
# best used together with colorCode.R
#=============================================================================
#documentation end


color.bar <- function(lut, min, 
                      max = -min, nticks = 11, ticks = round(seq(min, max, len = nticks), 2), title='') {
  scale <- (length(lut)-1)/(max-min)
  plot(
    c(0, 10), 
    c(min, max), 
    type = 'n', 
    bty = 'n', 
    xaxt = 'n', 
    xlab = '', 
    yaxt = 'n', 
    ylab = '', 
    main = title, 
    cex.main = 1.5
  )
  axis(
    2, 
    ticks, 
    las = 1, 
    cex.axis = 1
  )
  for (i in 1:(length(lut) - 1)) {
    y <- (i-1)/scale + min
    rect(
      0,
      y,
      10,
      y + 1/scale, 
      col = lut[i], 
      border=NA
    )
  }
}
