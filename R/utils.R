#  Helper functions for HydRochemistRy package
#

# returns valid parameters
valid_parameters <- function() {
  return(c("EC", "Cl", "HCO3", "SO4", "NO3", "PO4", "Na", "Ca", "Mg", "K", "Fe", "NH4"))
}

# test if mandatory columns are present
check_columns <- function(d) {

  col_numeric <- c("year", "value", "sd", "detection_limit", "xcoord", "ycoord")
  col <- c("putcode", "samplecode", "parameter", "limit_symbol", "units", "method", "notes",
           "watercode", "sampletype", "subtype", col_numeric)

  if(length(dplyr::setdiff(col, names(d)) > 0)) {
    stop("Columns are missing or are not recognized: Check column names.")
  }

  for(i in col_numeric) {
    if(!is.numeric(d[[i]])) {
      stop(paste("Column", i, "is not numeric"))
    }
  }
}

#### Helper functions PiperPlot function ####
# takes a dataframe with cation and anion concentrations in meql and converts them to percentages
# Step 1 - takes as input a dataframe with cation and anion concentrations in meql and converts them to percentages
toPercent <- function(d, NO3 = FALSE) {

  # If NO3 should not be considered
  if(NO3 == FALSE) {

    totalCations <- d$Ca + d$Mg + d$Na + d$K
    d$Ca <- 100 * (d$Ca/totalCations)
    d$Mg <- 100 * (d$Mg/totalCations)
    d$Na <- 100 * (d$Na/totalCations)
    d$K <- 100 * (d$K/totalCations)
    totalAnions <- d$Cl + d$SO4 + d$HCO3
    d$Cl <- 100 * (d$Cl/totalAnions)
    d$SO4 <- 100 * (d$SO4/totalAnions)
    #d$CO3 <- 100 * (d$CO3/totalAnions)
    d$HCO3 <- 100 * (d$HCO3/totalAnions)

    d <- d %>%
      dplyr::select(samplecode, EC, Cl, SO4, HCO3, Na, Ca, Mg, K)
  }
  # if NO3 should be considered
  if(NO3 == TRUE) {

    totalCations <- d$Ca + d$Mg + d$Na + d$K
    d$Ca <- 100 * (d$Ca/totalCations)
    d$Mg <- 100 * (d$Mg/totalCations)
    d$Na <- 100 * (d$Na/totalCations)
    d$K <- 100 * (d$K/totalCations)
    totalAnions <- d$Cl + d$SO4 + d$HCO3 + d$NO3
    d$Cl <- 100 * (d$Cl/totalAnions)
    d$SO4 <- 100 * (d$SO4/totalAnions)
    #d$CO3 <- 100 * (d$CO3/totalAnions)
    d$HCO3 <- 100 * (d$HCO3/totalAnions)
    d$NO3 <- 100 * (d$NO3/totalAnions)

    d <- d %>%
      dplyr::select(samplecode, EC, Cl, SO4, HCO3, NO3, Na, Ca, Mg, K)
  }
  return(d)
}

# Step 2 - takes percentage data and converts them to xy coordinates for plotting on Piper diagram
# transform_piper_data <- function(Mg, Ca, Cl, SO4, name = samplecode){
transform_piper_data <- function(d){
  Mg <- d$Mg
  Ca <- d$Ca
  Cl <- d$Cl
  SO4 <- d$SO4
  name <- d$samplecode

  if(is.null(name)){
    name = rep(1:length(Mg),3)
  } else {
    name = rep(name,3)
  }
  y1 <- Mg * 0.86603
  x1 <- 100 * (1 - (Ca / 100) - (Mg / 200))
  y2 <- SO4 * 0.86603
  x2 <- 120 + (100 * Cl / 100 + 0.5 * 100 * SO4 / 100)
  new_point <- function(x1, x2, y1, y2, grad = 1.73206){
    b1 <- y1 - (grad * x1)
    b2 <- y2 - (-grad * x2)
    M <- matrix(c(grad, -grad, -1, -1), ncol = 2)
    intercepts <- as.matrix(c(b1, b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x = t_mat[1, 1], y = t_mat[2, 1])
  }
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind",np_list)
  data.frame(observation = name, x = c(x1, x2, npoints$x), y = c(y = y1, y2, npoints$y))
}

# Step 3 - create piper diagram grid
## function to create the piper plot
# ggplot_piper <- function(piper.data, output = c("ggplot","plotly"), scale = sampletype) {
ggplot_piper <- function(piper.data, output = ggplot) {
  grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1+120
  grid2p1$x2 <- grid2p1$x2+120
  grid2p2 <<- grid1p2
  grid2p2$x1 <- grid2p2$x1+120
  grid2p2$x2 <- grid2p2$x2+120
  grid2p3 <<- grid1p3
  grid2p3$x1 <- grid2p3$x1+120
  grid2p3$x2 <- grid2p3$x2+120
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))
  grid3p3 <<- data.frame(x1=c(85, 85, 85, 85),y1=c(60.6221, 147.2251, 147.2251, 60.6221), x2=c(135,135, 135, 135), y2=c(147.2251, 60.6221, 147.2251, 60.6221))

  p <- ggplot2::ggplot() +
    ## left hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=100, yend=0), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=50, yend=86.603), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=50,y=86.603, xend=100, yend=0), size = 0.75) +
    ## right hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=220, yend=0), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=170, yend=86.603), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=170,y=86.603, xend=220, yend=0), size = 0.75) +
    ## Upper diamond
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=60, yend=103.9236), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=160, yend=103.9236), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=160, yend=103.9236), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=60, yend=103.9236), size = 0.75) +
    ## Add grid lines to the plots
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    # Add bold grid lines for water types
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p3, size = 0.25, colour = "grey20") +

    ### Labels and grid values
    ggplot2::geom_text(ggplot2::aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::coord_equal(ratio=1) +
    ggplot2::geom_text(ggplot2::aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())

  # Place numbers for watertypes inside the piper diagram plot
  p <- p + ggplot2::annotate(
    geom = "text",
    label = c("1", "2", "3", "4", "5", "6"),
    x = c(70, 110, 150, 110, 110, 105),
    y = c(103.92, 175, 103.92, 32, 75, 138.5648)
  ) +
    # Add legend with water types
    ggplot2::geom_text(ggplot2::aes(
      #geom = "text",
      x = -10,
      y = c(170, 160, 150, 140, 130, 120, 110),
      #label = "Alkalinity~as~HCO[3]^'-'"
      #label = "1 Ca-HCO[3]\n 2 Ca-Cl\n 3 Na-Cl\n 4 Na-HCO[3]\n 5 Mixed type"
      label = c("Water~type", "1~Ca-`HCO`[3]", "2~Ca-`Cl`", "3~Na-`Cl`",
                "4~Na-`HCO`[3]", "5~Mixed~Na-Ca-`HCO`[3]", "6~Mixed~Ca-Mg-`Cl`")
    ), parse=TRUE,
    hjust = 0) +
    theme(legend.position = c(0.9, 0.72))

  # ### Plot data
  # if(scale == "sampletype"){
  #   p <- p + ggplot2::geom_point(data=piper.data,
  #                                ggplot2::aes(x,y, colour=factor(sampletype),
  #                                             shape = sampletype,
  #                                             text = paste(observation,
  #                                                          '</br></br>Date: ',
  #                                                          date))) +
  #     scale_colour_manual(name = "sample type",
  #                         values = c("#F8766D", "#00BFC4", "#00BA38",
  #                                    "#B79F00", "#619CFF", "#C77CFF", "grey")) +
  #     scale_shape_manual(name = "sample type",
  #                        values = c(19, 17, 15, 3, 7, 8, 9))
  # }

  # if(scale == "geology"){
  #   p <- p + ggplot2::geom_point(data=piper.data,
  #                                ggplot2::aes(x,y, colour=factor(geology),
  #                                             text = paste(observation,
  #                                                          '</br></br>Date: ',
  #                                                          date))) +
  #     scale_colour_manual(name = "geology",
  #                         values = c("yellowgreen", "seagreen4", "lightskyblue2",
  #                                                 "purple4", "coral1", "yellow2", "grey50"))
  # }

  # if(scale == "EC") {
  #   p <- p + ggplot2::geom_point(data=piper.data,
  #                                #ggplot2::aes(x,y, size = EC, colour = cut(EC, breaks = c(0, 1000, 5000, 10000, 20000, 50000)),
  #                                ggplot2::aes(x,y, size = EC, colour = EC,
  #                                             text = paste(observation,
  #                                                          '</br></br>Date: ',
  #                                                          date)),
  #                                alpha = 0.7) +
  #     # scale_colour_continuous(breaks = c(1000, 5000, 10000, 20000, 50000),
  #     #                         type = "viridis") +
  #     scale_colour_distiller(name = "EC (uS/cm)",
  #                            breaks = c(1000, 5000, 10000, 20000, 50000),
  #                            palette = "Spectral",
  #                            direction = -1) +
  #     scale_size_continuous(name = "EC (uS/cm)",
  #                           breaks = c(1000, 5000, 10000, 20000, 50000)) +
  #     guides(color = guide_legend(), size = guide_legend())
  #   # scale_size(name = "EC (uS/cm)",
  #   #            breaks = c(1000, 5000, 10000, 20000, 50000))
  # }

  if (output == "ggplot"){
    p <- p +
      ggplot2::geom_text(ggplot2::aes(17,50, label="Mg^'2+'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(77.5,50, label="Na^'+'~+~K^'+'"), angle=-60, size=4,parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(50,-10, label="Ca^'2+'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(170,-10, label="Cl^'-'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(205,50, label="SO[4]^'2-'"), angle=-60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(142,50, label="Alkalinity~as~HCO[3]^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(72.5,150, label="SO[4]^'2-'~+~Cl^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(147.5,150, label="Ca^'2+'~+~Mg^'2+'"), angle=-60, size=4, parse=TRUE)
  }

  if (output == "plotly"){
    #this fixes an issue that plotly can't render geom_text() with the  angle option set properly
    p <- plotly::ggplotly(p,
                          tooltip = c("text")
    )
    p <- p  %>% plotly::layout(
      annotations=list(text=c("Mg<sup>2+</sup>",
                              "Na<sup>+</sup> + K<sup>+</sup>",
                              "Ca<sup>2+</sup>",
                              "Cl<sup>-</sup>",
                              "SO<sub>4</sub><sup>2-</sup>",
                              "Alkalinity as HCO<sub>3</sub><sup>-</sup>",
                              "SO<sub>4</sub><sup>2-</sup> + Cl<sup>-</sup>",
                              "Ca<sup>2+</sup> + Mg<sup>2+</sup>"),
                       x = c(17,77.5,50,170,205,142.5,72.5,147.5),
                       y = c(50,50,-10,-10,50,50,150,150),
                       textangle = c(-60,60,0,0,60,-60,-60,60),
                       "showarrow"=F, font=list(size = 12, color = "black")
      ))

  }

  return(p)
}


#### new part ####
