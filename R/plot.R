#' #' @title scatter plot
#' #' @param fb datos
#' #' @param xcol x column
#' #' @param ycol y column
#' #' @description plot the scatter plot
#' #' 
#' #' @export
#' #' 
#' #' 
#' plot_corr <- function(fb, xcol, ycol){
#' 
#' gg <- ggplot(fb, aes_string(x=xcol, y=ycol)) + 
#'   geom_point() + 
#'   # xlim(c(0, 0.1)) + 
#'   # ylim(c(0, 500000)) + 
#'   labs(subtitle="Test for scatter plot", 
#'        y="Y", 
#'        x="X", 
#'        title="Scatterplot", 
#'        caption = "Source: CIP")
#' 
#'   print(gg)
#' 
#' }
#' 
#' #' @title histogram
#' #' @param fb datos
#' #' @param xcol x column
#' #' @description plot the histogram
#' #' @export
#' #' 
#' #' 
#' plot_hist <- function(fb, xcol){
#'   
#'   
#'   gg <-  ggplot(data = fb, aes_string(x = xcol)) +
#'         #geom_histogram(binwidth= 0.5, color="black", fill="grey", size=1)+
#'         geom_histogram( aes(y =..density..) , bins = 50, color="black", fill="grey", size=1)+
#'         # geom_vline(aes_(xintercept= mean(xcol)),
#'         #            color="red", linetype="dashed", size=1)+
#'         labs(x = "TRAIT", y = "Trait Name")+
#'         scale_color_manual(values = c("#00AFBB", "#E7B800"))+
#'         scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
#'         #geom_density(col=2)+
#'         theme_classic()+
#'         theme(
#'         axis.text.x = element_text(size = 12, colour = "black",face = "bold"),
#'         axis.text.y = element_text(size = 12, colour = "black",face = "bold"),
#'         axis.line.x = element_line(colour = "black", size = 1),
#'         axis.line.y = element_line(colour = "black", size = 1),
#'         legend.position = "bottom"
#'       )
#'     print(gg)
#' 
#'   
#' }
#' 
#' 
#' #' @title boxplot
#' #' @param fb datos
#' #' @param xcol x column
#' #' @param ycol y column
#' #' @description plot the boxplot
#' #' @export
#' #'
#' #'
#' plot_bplot <- function(fb, xcol, ycol){
#' 
#'     p <- ggplot(fb, aes_string(x = xcol, y = ycol))
#'     p + geom_boxplot()
#'     print(p)
#' 
#' }