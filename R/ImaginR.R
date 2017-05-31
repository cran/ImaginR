#' Give RGB color code from HEX color code
#'
#' Give a RGB color code matrix from HEX color code vector.
#'
#' @param color.vector HEX code in a vector
#' @return RGB code color
#' @examples
#' # RGB color code of "Alice Blue" color:
#' hex2rgb("#f0f8ff")
#' # RGB color code of "Red" color:
#' hex2rgb("#ff0000")
#' # RGB color code of "Green" color:
#' hex2rgb("#008000")
#'@export
hex2rgb <- function(color.vector) {
  resultat <- matrix( NA, ncol=3, nrow=length(color.vector), dimnames=list( NULL, c( "R", "G", "B")))
  resultat[,"R"] <- strtoi( substr(color.vector, 2, 3), 16L)
  resultat[,"G"] <- strtoi( substr(color.vector, 4, 5), 16L)
  resultat[,"B"] <- strtoi( substr(color.vector, 6, 7), 16L)
  resultat
}

#' Usefull for others functions
#'
#' Function to print the results (usefull for the others functions)
#'
#' @param x the results
#' @param ... if necessary
#' @return the header of the results
#' @export
print.myclass <- function(x, ...){
  cat(x$header,"\n\n")
  cat("Hexadecimal code: ", sprintf("%s: %s", names(x$const), x$const), "\n\n")
  cat("HSV code:\n")
  print(x$data, ...)
}


#' Give HEX and HSV color code
#'
#' To get the mean of the HEX color code and the HSV color code of the picture
#' @param picture The picture uploaded by load.image()
#' @return results
#' the HEX and HSV color code
#' @details
#' \itemize{
#'  \item{h : the hue of the hsv color code}
#'  \item{s : the saturation of the hsv color code}
#'  \item{v : the value of the hsv color code}
#'  \item{hex : the hexadecimal color code}
#' }
#' @examples
#' fpath <- system.file('extdata/image.jpg',package='ImaginR')
#' picture <- load.image(fpath)
#' PictureResults(picture)
#' @export
PictureResults<-function(picture){
  # picture <- load.image("p.jpg")
  #picture.blurry <- isoblur(picture,5) # blur picture to averaging the neighboring pixels
  #save.image(picture.blurry, "blur_picture.jpeg") # save the new picture
 # picture <- readJPEG("blur_picture.jpeg") # read it
  pictureDm <- dim(picture) # dimension it
  # create a new data.frame with these datas
  pictureRGB <- data.frame(
    R = as.vector(picture[,,1]),
    G = as.vector(picture[,,2]),
    B = as.vector(picture[,,3])
  )
  # "rgb it" the get the HEX color code
  pictureHEX <- rgb(pictureRGB[,"R"], pictureRGB[,"G"], pictureRGB[,"B"], maxColorValue=1)
  # remove <- read.table("remove.txt", sep=",") # this table contain all white HEX color code
  remove <- system.file("extdata", "remove.txt", package="ImaginR")
  remove <-trimws(as.vector(t(remove))) #trimws to fix the bug with spaces and comas into the txt file
  pictureHEX <- pictureHEX [! pictureHEX %in% remove] # remove the white color of the background of the picture


  # hex2rgb : to give RGB color code from HEX color code
  hex2rgb <- function(color.vector) {
    resultat <- matrix( NA, ncol=3, nrow=length(color.vector), dimnames=list( NULL, c( "R", "G", "B")))
    resultat[,"R"] <- strtoi( substr(color.vector, 2, 3), 16L)
    resultat[,"G"] <- strtoi( substr(color.vector, 4, 5), 16L)
    resultat[,"B"] <- strtoi( substr(color.vector, 6, 7), 16L)
    resultat
  }

  RGB0 <- matrix(hex2rgb(pictureHEX), ncol=3, byrow=F) # creat a matrix with the new datas
  colnames(RGB0) <- c("R", "G", "B") # give columns a name
  mean.color <- as.data.frame(RGB0) # create new data.frame to averaging the colors
  rm <- mean(mean.color$R) # averaging the red channel color
  rmr <- round(rm, 0)
  gm <- mean( mean.color$G) # averaging the green channel color
  gmr <- round(gm, 0)
  bm <- mean(mean.color$B) # averaging the blue channel color
  bmr <- round(bm, 0)
  color <- rgb(rmr, gmr, bmr, maxColorValue=255) # convert code 1
  hsv <-rgb2hsv(col2rgb(color)) # convert code 2

  # give a marvelous made
  header <- "The average color of the picture"
  results <- list(header = header,
                  const = list(color=color),
                  data = list(hsv = hsv))
  class(results) <- "myclass"
  return(results)
}


#' Give the color phenotype of the pearl oyster's
#'
#' Returns the color phenotype of the pearl oyster's inner shell (\emph{Pinctada margaritifera})
#' @param picture The picture uploaded by load.image()
#' @return The color phenotype of the pearl oyster's inner shell
#' @examples
#' fpath <- system.file('extdata/image.jpg',package='ImaginR')
#' picture <- load.image(fpath)
#' ColorPhenotype(picture)
#' @export
ColorPhenotype <- function(picture){
  #picture <- load.image("p.jpg")
  #picture.blurry <- isoblur(picture,5)
  #save.image(picture.blurry, "blur_picture.jpeg")
  #picture <- readJPEG("blur_picture.jpeg")
  pictureDm <- dim(picture)
  pictureRGB <- data.frame(
    R = as.vector(picture[,,1]),
    G = as.vector(picture[,,2]),
    B = as.vector(picture[,,3])
  )
  pictureHEX <- rgb(pictureRGB[,"R"], pictureRGB[,"G"], pictureRGB[,"B"], maxColorValue=1)
  remove <- system.file("extdata", "remove.txt", package="ImaginR")
  remove <-trimws(as.vector(t(remove)))
  pictureHEX <- pictureHEX [! pictureHEX %in% remove]

  hex2rgb <- function(color.vector) {
    resultat <- matrix( NA, ncol=3, nrow=length(color.vector), dimnames=list( NULL, c( "R", "G", "B")))
    resultat[,"R"] <- strtoi( substr(color.vector, 2, 3), 16L)
    resultat[,"G"] <- strtoi( substr(color.vector, 4, 5), 16L)
    resultat[,"B"] <- strtoi( substr(color.vector, 6, 7), 16L)
    resultat
  }

  RGB0 <- matrix(hex2rgb(pictureHEX), ncol=3, byrow=F)
  colnames(RGB0) <- c("R", "G", "B")
  mean.color <- as.data.frame(RGB0)
  rm <- mean(mean.color$R)
  rmr <- round(rm, 0)
  gm <- mean( mean.color$G)
  gmr <- round(gm, 0)
  bm <- mean(mean.color$B)
  bmr <- round(bm, 0)
  color <- rgb(rmr, gmr, bmr, maxColorValue=255)
  hsv <-rgb2hsv(col2rgb(color))

  # Import the hsv data of 30 valves of 5 Pinctada margaritifera wich are come from Rikitea
  # (Gambier's archipelago) and from a same reproduction.
  # The brood were choseen for they coloration
  # There are 10 valves by color phenotypes (red, yellow and green)
  # To see some individuals, go to https://plstenger.github.io/
  # res <- read.csv("res.csv", header=T, sep=';')
  # res

  # re-format the datas
  # h <- res$h
  # s <- res$s
  # v <- res$v
  # phe <- res$phenotype
  # dat <- data.frame(phe, h, s, v)
  # dat

  # Subset the datas by phenotypes and extract the range by maximum + standard error and minimum + standard error:

  # For red phenotype
  # datR <- subset(dat, phe == "R")
  # minR <- min(datR$h)
  # maxR <- max(datR$h)
  # sdR <- sd(datR$h)
  # R1 <- maxR + (sdR/2) - 0.002499 # here I substract 0.002499 to avoid conflict with the largeless yellow phenotype's range
  # R2 <- minR + (sdR/2)

  # For yellow phenotype ("J" for Jaune in French)
  # datJ <- subset(dat, phe == "J")
  # minJ <- min(datJ$h)
  # maxJ <- max(datJ$h)
  # sdJ <- sd(datJ$h)
  # J1 <- maxJ + (sdJ)
  # J2 <- minJ - (sdJ/2)

  # For green phenotype ("V" for Vert in French)
  # datV <- subset(dat, phe == "V")
  # minV <- min(datV$h)
  # maxV <- max(datV$h)
  # sdV <- sd(datV$h)
  # V1 <- maxV + (sdV)
  # V2 <- minV - (sdV)

  R1 <- 0.1625770
  R2 <- 0.0000000    # 0.02340927
  J1 <- 0.2790814
  J2 <- 0.1625774
  V1 <- 0.5637775
  V2 <- 0.3215928

  # What's color phenotype is it ?
  phenotype <- if ((hsv[1,] >= 0) & (hsv[1,] <= R1)){
    "Red phenotype"
  } else if ((hsv[1,] >= 1-R1) & (hsv[1,] <= 1)){
    "Red phenotype"
  } else if ((hsv[1,] >= J2) & (hsv[1,] <= J1)){
    "Yellow phenotype"
  } else if ((hsv[1,] >= V2) & (hsv[1,] <= V1)){
    "Green phenotype"
  }else {
    "other phenotype"
  }
  return(phenotype)
}





#' Get phenotype, HEX and HSV color code for one picture
#'
#' Get results in one row
#' @param picture The picture uploaded by load.image()
#' @return The HEX and HSV color code and the color phenotype of the pearl oyster's inner shell for one image in one row
#' @details
#' In header:
#' \itemize{
#'  \item{id : the name of your pictures}
#'  \item{h : the hue of the hsv color code}
#'  \item{s : the saturation of the hsv color code}
#'  \item{v : the value of the hsv color code}
#'  \item{hex : the hexadecimal color code}
#'  \item{phenotype : returns the color phenotype of the pearl oyster's inner shell (\emph{Pinctada margaritifera})}
#' }
#' @examples
#' fpath <- system.file('extdata/image.jpg',package='ImaginR')
#' picture <- load.image(fpath)
#' OneRow(picture)
#' @export
OneRow <- function(picture){
  pictureDm <- dim(picture) # dimension it
  # create a new data.frame with these datas
  pictureRGB <- data.frame(
    R = as.vector(picture[,,1]),
    G = as.vector(picture[,,2]),
    B = as.vector(picture[,,3])
  )
  # "rgb it" the get the HEX color code
  pictureHEX <- rgb(pictureRGB[,"R"], pictureRGB[,"G"], pictureRGB[,"B"], maxColorValue=1)
  # remove <- read.table("remove.txt", sep=",") # this table contain all white HEX color code
  remove <- system.file("extdata", "remove.txt", package="ImaginR")
  remove <-trimws(as.vector(t(remove))) #trimws to fix the bug with spaces and comas into the txt file
  pictureHEX <- pictureHEX [! pictureHEX %in% remove] # remove the white color of the background of the picture


  # hex2rgb : to give RGB color code from HEX color code
  hex2rgb <- function(color.vector) {
    resultat <- matrix( NA, ncol=3, nrow=length(color.vector), dimnames=list( NULL, c( "R", "G", "B")))
    resultat[,"R"] <- strtoi( substr(color.vector, 2, 3), 16L)
    resultat[,"G"] <- strtoi( substr(color.vector, 4, 5), 16L)
    resultat[,"B"] <- strtoi( substr(color.vector, 6, 7), 16L)
    resultat
  }

  RGB0 <- matrix(hex2rgb(pictureHEX), ncol=3, byrow=F) # creat a matrix with the new datas
  colnames(RGB0) <- c("R", "G", "B") # give columns a name
  mean.color <- as.data.frame(RGB0) # create new data.frame to averaging the colors
  rm <- mean(mean.color$R) # averaging the red channel color
  rmr <- round(rm, 0)
  gm <- mean( mean.color$G) # averaging the green channel color
  gmr <- round(gm, 0)
  bm <- mean(mean.color$B) # averaging the blue channel color
  bmr <- round(bm, 0)
  color <- rgb(rmr, gmr, bmr, maxColorValue=255) # convert code 1
  hsv <-rgb2hsv(col2rgb(color)) # convert code 2

  R1 <- 0.162577
  R2 <- 0.0000000  # 0.02340927
  J1 <- 0.2790814
  J2 <- 0.1625774
  V1 <- 0.5637775
  V2 <- 0.3215928

  # What's color phenotype is it ?
  phenotype <- if ((hsv[1,] >= 0) & (hsv[1,] <= R1)){
    "Red phenotype"
  } else if ((hsv[1,] >= 1-R1) & (hsv[1,] <= 1)){
    "Red phenotype"
  } else if ((hsv[1,] >= J2) & (hsv[1,] <= J1)){
    "Yellow phenotype"
  } else if ((hsv[1,] >= V2) & (hsv[1,] <= V1)){
    "Green phenotype"
  }else {
    "other phenotype"
  }

  # get results in one row
  mhsv <- matrix(hsv[,1], dimnames = NULL)
  OneRow <- c(c(mhsv), color, phenotype)
  OneRow <- c(OneRow)

  return(OneRow)
}



#' Get phenotype, HEX and HSV color code for all pictures
#'
#' Get results in a .txt file, .csv file and in R data.frame
#' This function does what all the others functions do in a very simple way. Just put your images in your working directory (don't forget to getwd() !), do library this package and paste this only code: "OutPutResult()". You will get the results into your consol and in a results.csv file in your working directory.
#' @param id The name of the pictures in your working directory
#' @return The HEX and HSV color code and the color phenotype of the pearl oyster's inner shell for all images in a results.csv file
#' @details
#' In results.csv:
#' \itemize{
#'  \item{id : the name of your pictures}
#'  \item{h : the hue of the hsv color code}
#'  \item{s : the saturation of the hsv color code}
#'  \item{v : the value of the hsv color code}
#'  \item{hex : the hexadecimal color code}
#'  \item{phenotype : returns the color phenotype of the pearl oyster's inner shell (\emph{Pinctada margaritifera})}
#' }
#' @export
OutPutResult <- function(id){
  id <- list.files(pattern = ".jpg")
  for(i in id){
    sink("OutPutAnalysis.txt", append=TRUE)
    picture <- load.image(i)
    a <- OneRow(picture)
    print(a)
    sink()
    sink()
    sink()
  }
  id <- list.files(pattern = ".jpg")
  res <- read.table("OutPutAnalysis.txt")
  dat <- data.frame(id, res$V2, res$V3, res$V4, res$V5, res$V6)
  colnames(dat) <- c("id", "h", "s", "v", "hex","phenotype")
  write.table(dat, file = "results.csv", sep=";")
  return(dat)
}




