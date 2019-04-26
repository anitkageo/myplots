#' @title PLot at least one Y vs single X
#'
#' @description The function makes plots (line and/or points)
#' of at least one Y value vs a single X value. There are a single x and single y axes.
#' X and Y are columns from a signle data set. Type/shape, color and size of the
#' lines/points are controled by the function's input parameters.
#' Legend names for colors and shape/type are controled by the
#' function's input parameters. Legend position can't be controlled
#' by the function's input parameters. The plot can be either save
#' as pdf and/or png file or plotted in the Plots window of RStudio.
#'
#' @param Input.dataset A data frame with one column as X and other(s) for Y
#' @param X.data A column of the Input.dataset to be used as x
#' @param Y.line.data One o many columns of the Input.dataset to be used as y for line plots
#' @param Y.line.data.scale A vector of scaling factors (if needed) for Y.line.data
#' @param Y.point.data One o many columns of the Input.dataset to be used as y for point plots
#' @param Y.point.data.scale A vector of scaling factors (if needed) for Y.point.data
#' @param X.Time If TRUE then X is considered as date-time with corresponding x axis
#' @param X.limits Set limits for the x axis (if needed)
#' @param Y.limits Set limits for the y axis (if needed)
#' @param X.breaks Set of breaks for the x axis (if needed)
#' @param Y.breaks Set of breaks for the y axis (if needed)
#' @param X.labels Labels for the X axis (if needed)
#' @param X.minor.breaks Set of minor breaks for the x axis (if needed)
#' @param Plot.Title The title of the plot
#' @param X.axis.title The title for the x axis
#' @param X.label.angle The angle for the x axis' labels
#' @param Y.axis.title The title for the y axis
#' @param Y.label.angle The angle for the y axis' labels
#' @param Y.pointColor.leg Descriptions for the color legend for the point plots
#' @param Y.pointColor List of colors for points
#' @param Y.pointShape.leg Descriptions for the shape legend for the point plots
#' @param Y.pointShape List of shapes for points
#' @param Y.pointSize List of sizes for points
#' @param Y.lineColor.leg Descriptions for the color legend for the line plots.
#' NB: if the lists in Y.lineColor.leg and Y.pointColor.leg are the same, only one color legend
#' is generated
#' @param Y.lineColor List of colors for lines
#' @param Y.lineType.leg Descriptions for the line type legend for the line plots
#' @param Y.lineType List of line types
#' @param Y.lineSize List of sizes for lines
#' @param AddLegend if TRUE the legend is added
#' @param Save.pdf if TRUE the plot is saved to pdf file
#' @param Save.png if TRUE the plot is saved to png file
#' @param Out.file.Name Output file name
#'
#' @return Either a plot in 'Plots' window or a pdf/png file with plot
#'
#' @author Anna Morozova
#'
line.point_plot <- function(Input.dataset,
                       X.data = 1,
                       Y.line.data = NULL,#c(2,3),
                       Y.line.data.scale = NULL,
                       Y.point.data = NULL,#c(2,3),c(3,4),
                       Y.point.data.scale = NULL,

                       X.Time = F,
                       X.limits = NULL,
                       Y.limits = NULL,
                       X.breaks = waiver(),
                       Y.breaks = waiver(),
                       X.labels = waiver(),
                       X.minor.breaks = waiver(),

                       Plot.Title = "plot Title",
                       X.axis.title = "X axis",
                       X.label.angle = 0,
                       Y.axis.title = "Y axis",
                       Y.label.angle = 0,

                       Y.pointColor.leg = "color",
                       Y.pointColor = NULL,#"black",
                       Y.pointShape.leg = "shape",
                       Y.pointShape = 20,
                       Y.pointSize = 1,

                       Y.lineColor.leg = "color",
                       Y.lineColor = NULL,#"black",
                       Y.lineType.leg = "type",
                       Y.lineType = "solid",
                       Y.lineSize = 1,

                       AddLegend = F,
                       Save.pdf = F,
                       Save.png = F,
                       Out.file.Name = "plot") {

  #suppressPackageStartupMessages(library(ggplot2))

## font/size definition

  windowsFonts("Times" = windowsFont("Times"))

  #Plot.Title = paste(Plot.Title, ", Survey data, 14.11.18-02.01.19", sep = " ")
  Text.size <- 16

## X & Y definition
## only one X but could be many Y
  Line.X <- Input.dataset[,X.data]

  if (!is.null(Y.line.data)) {
    Num.L.Y <- length(Y.line.data)
    Line.Y <- data.frame(NA,
                         ncol = Num.L.Y,
                         nrow = length(Input.dataset[,X.data]))
    Line.Y <-  Input.dataset[,Y.line.data, drop = F]
    if ((length(Y.lineColor) != Num.L.Y) & (!is.null(Y.lineColor))) {
      print(paste(length(Y.lineColor), Num.L.Y))
      a <- vector(length = Num.L.Y, mode = "character")
      b <- vector(length = Num.L.Y, mode = "character")
      for (i in 1:Num.L.Y) {
        a[i] <- Y.lineColor.leg
        b[i] <- Y.lineColor
      }
      Y.lineColor.leg <- a
      Y.lineColor <- b
      rm(a)
      rm(b)

    }

  }

  if (!is.null(Y.point.data)) {
    Num.P.Y <- length(Y.point.data)
    Point.Y <- data.frame(NA,
                          ncol = Num.P.Y,
                          nrow = length(Input.dataset[,X.data]))
    Point.Y <-  Input.dataset[,Y.point.data, drop = F]
    if ((length(Y.pointColor) != Num.P.Y) & (!is.null(Y.pointColor))) {
      a <- vector(length = Num.P.Y, mode = "character")
      b <- vector(length = Num.P.Y, mode = "character")
      for (i in 1:Num.P.Y) {
        a[i] <- Y.pointColor.leg
        b[i] <- Y.pointColor
      }
      Y.pointColor.leg <- a
      Y.pointColor <- b
      rm(a)
      rm(b)
    }
   }


## theme & scales
  myplot <- ggplot()

  myplot <- myplot + labs(title = Plot.Title,
                          x = X.axis.title,
                          y = Y.axis.title)

  myplot <- myplot + theme(plot.title = element_text(hjust=0,
                                                     family = "Times",
                                                     size = Text.size,
                                                     colour = "black"),
                           axis.title.x = element_text(family = "Times",
                                                       angle = 0,
                                                       size = Text.size,
                                                       vjust = 0.5,
                                                       colour = "black"),#element_blank()
                           axis.title.y = element_text(family = "Times",
                                                       angle = 90,
                                                       size = Text.size,
                                                       vjust = 0.5,
                                                       colour = "black"),
                           axis.text.x = element_text(family = "Times",
                                                      angle = X.label.angle,
                                                      hjust = 0.5,
                                                      vjust = 0.5,
                                                      size = Text.size,
                                                      colour = "black"),
                           axis.text.y = element_text(family = "Times",
                                                      angle = Y.label.angle,
                                                      hjust = 0.5,
                                                      vjust = 0.5,
                                                      size = Text.size,
                                                      colour = "black")
  )

  myplot <- myplot + theme(panel.background = element_blank(),
                           axis.line = element_line(colour = "black",
                                                    size = 0.25),
                           panel.grid.major.y = element_line(colour = "light grey",
                                                             linetype = 2,
                                                             size = 0.1),
                           panel.grid.major.x = element_line(colour = "dark grey",
                                                             linetype = 3,
                                                             size = 0.15),
                           panel.grid.minor.x = element_line(colour = "dark grey",
                                                             linetype = 3,
                                                             size = 0.15)

  )


  myplot <- myplot + scale_y_continuous(limits = Y.limits,
                                        breaks = Y.breaks)

  if (X.Time) {
    myplot <- myplot + scale_x_datetime(date_breaks = X.breaks,
                                        date_labels = X.labels,
                                        date_minor_breaks = X.minor.breaks)
  } else {
    myplot <- myplot + scale_x_continuous(limits = X.limits,
                                          breaks = X.breaks,
                                          labels = X.labels,
                                          minor_breaks = X.minor.breaks)
  }

  ############## data plot ##########################################
  if (!is.null(Y.point.data)) {
    for (n in 1:Num.P.Y) {
      #str(Y.pointColor[n])
      if (is.null(Y.point.data.scale)) {
        tmp.scale <- 1
      } else {
        tmp.scale <- Y.point.data.scale[n]
      }
      tmp.data <- data.frame(tmp.x = Line.X,
                             tmp.y = Point.Y[,n]*tmp.scale,
                             tmp.col = Y.pointColor.leg[n],
                             tmp.shape = Y.pointShape.leg[n],
                             tmp.size = Y.pointSize[n])
      myplot <- myplot + geom_point(data = tmp.data,
                                    aes(x= tmp.x,
                                        y=tmp.y,
                                        color = tmp.col,
                                        shape = tmp.shape), # + alpha, fill, size
                                    show.legend = AddLegend,
                                    na.rm = T)
    }
  }


  if (!is.null(Y.line.data)) {
    for (n in 1:Num.L.Y) {
      if (is.null(Y.line.data.scale)) {
        tmp.scale <- 1
      } else {
        tmp.scale <- Y.line.data.scale[n]
      }

      tmp.data <- data.frame(tmp.x = Line.X,
                             tmp.y = Line.Y[,n]*tmp.scale,
                             tmp.col = Y.lineColor.leg[n],
                             tmp.type = Y.lineType.leg[n],
                             tmp.size = Y.lineSize[n] )

      myplot <- myplot + geom_line(data = tmp.data,
                                   aes(x= tmp.x,
                                       y=tmp.y,
                                       color = tmp.col,
                                       linetype = tmp.type),
                                   size = tmp.data$tmp.size,# # + alpha, linetype, size
                                   show.legend = AddLegend,
                                   na.rm = T)  }
  }

## legend (no control from outside)
  myplot <- myplot + theme(legend.background = element_blank(),#element_rect(fill = "white"),
                           legend.key = element_rect(fill = "white"),#element_blank(),
                           legend.justification = "center",#c(1,0.5),#
                           legend.position = "right",#c(0.9,0.5),#
                           legend.box = "vertical",
                           legend.text = element_text(family = "Times",
                                                      size = Text.size-2,
                                                      colour = "black")
  )

  myplot <- myplot + scale_colour_manual("",
                                         breaks = c(Y.pointColor.leg,
                                                    Y.lineColor.leg),
                                         values = c(Y.pointColor,
                                                    Y.lineColor))
  # if (!is.null(Y.lineColor.leg)) {
  #   myplot <- myplot + scale_colour_manual("",
  #                                          breaks = Y.lineColor.leg,
  #                                          values = Y.lineColor)
  # }

  if (!is.null(Y.lineType.leg)) {
    myplot <- myplot + scale_linetype_manual("",
                                             breaks = Y.lineType.leg,
                                             values = Y.lineType)
  }

  # if (!is.null(Y.pointColor.leg)) {
  #   myplot <- myplot + scale_colour_manual("",
  #                                          breaks = Y.pointColor.leg,
  #                                          values = Y.pointColor)
  # }

  # if (!is.null(Y.pointShape.leg)) {
  #   myplot <- myplot + scale_shape_manual("",
  #                                          breaks = Y.pointShape.leg,
  #                                          values = Y.pointShape)
  # }

  if (!is.null(Y.pointShape.leg)) {
    myplot <- myplot + scale_shape_manual("",
                                          breaks = Y.pointShape.leg,
                                          values = Y.pointShape)
  }


  if (Save.pdf) {
    Out.file.Name.pdf <- paste(Out.file.Name, ".pdf", sep = "")
    pdf(Out.file.Name.pdf,
        width = 11.69,
        height = 8.27,
        paper = "a4r")
    print(myplot)
    dev.off()
  }


  if (Save.png) {
    Out.file.Name.png <- paste(Out.file.Name, ".png", sep = "")
    png(Out.file.Name.png,
        width = 751,
        height = 450)
    print(myplot)
    dev.off()
  }


  if ((!Save.png) & (!Save.pdf)) {print(myplot)}

}
