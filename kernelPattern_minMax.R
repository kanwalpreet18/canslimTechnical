rm(list = ls())

library("np")
library("quantmod")
library("TTR")
library("parallel")
#library("plotly")
#library("candlesticks")
#library("googleVis")

current.date <- Sys.Date()

generateContenders <- function(getNewData = FALSE, daysBack = 300, numDaysPast = 270){
    ## daysBack takes of no. of days back in history to start from
    ## noDaysPast maximum denotes no. of days prior to current date when pattern was finised

    start.date <- current.date - daysBack
    
                                        # getting price volume information
    symbols.list <- read.csv("DATA/bseSymbols.csv")[ ,2]
    symbols.list <- paste0(symbols.list, ".BO")
    
    if(getNewData){
        system.time(price.data <- mclapply(symbols.list, function(x){
                                               tryCatch(
                                                   getSymbols(x, env = NULL,
                                                              from = start.date),
                                                   error = function(e) NULL)
                                           }, mc.cores = 3))
        
        names(price.data) <- symbols.list
        price.data <- price.data[!sapply(price.data, is.null)]
        save(price.data, file = "prices.Rda")
    }
    else 
        load("prices.Rda")

    computeHeikenashiPrices <- function(data){
                                        # compute heikenashi prices
        data <- data[paste0(start.date, "/", current.date)]
        
        data$date.no <- 1:nrow(data)
        data$xopen <- data$open
        data$xhigh <- data$high
        data$xlow <- data$low
        data$xclose <- data$close
        
        for (i in 2:nrow(data)){
            data$xopen[i] <- (data$xopen[i-1] + data$close[i-1])/2
            data$xclose[i] <- (data$open[i] + data$high[i] + data$low[i] + data$close[i])/4
            data$xhigh[i] <- max(data$high[i], data$xopen[i], data$xclose[i])
            data$xlow[i] <- min(data$low[i], data$xopen[i], data$xclose[i])
        }
        
        return(data)
}

                                        # searching for patterns in each stock
    searchPattern <- function(price){
        tryCatch({
            data <- price
            company.name <- strsplit(colnames(data)[1], "\\.")[[1]][1]
            print(company.name)
            colnames(data) <- c("open", "high", "low", "close",
                                "volume", "adjusted")

            data <- computeHeikenashiPrices(data)
            
            model.np <- npreg(close ~ date.no, regtype = "ll", bws = 2.5,
                              gradients = TRUE, data = as.data.frame(data))
            
            data$fitted <- fitted(model.np)
            data$rpv <- NA
            data$rpv[-1] <- diff(fitted(model.np)) * data$volume[-1]
            
            data$ma.volume <- SMA(data$volume, 50)
            data$ma.rpv <- SMA(data$rpv, 50)
            
                                        # setting the min and max locations
            data$max.loc <- NA
            data$min.loc <- NA
            data$max.loc[which(diff(sign(diff(data$fitted))) == -2) - 1] <- 1
            data$min.loc[which(diff(sign(diff(data$fitted))) == 2) - 1] <- 1
            
            max.loc <- which(data$max.loc == 1) 
            min.loc <- which(data$min.loc == 1)

            max.loc <- max.loc[max.loc > 50]
            min.loc <- min.loc[min.loc > 50]

            good.points <- list()
            i <- 1

            for(a in max.loc){
                
                k <- 20
                while(k > 5){
                    dat <- data[paste0("/", index(data)[a-k])]

                    d.k <- index(data)[a-k]
                    p.k <- as.vector(data[d.k, "fitted"])

                    d.a <- index(data)[a]
                    p.a <- as.vector(data[d.a, "fitted"])
                    m.price <- max(data[index(data) < d.a, "fitted"])
                    

                    uprv1 <- abs(mean(dat[index(dat) %in% d.k:d.a & dat$rpv > 0,
                                          "rpv"],
                                      na.rm = TRUE))
                    dprv1 <- abs(mean(dat[index(dat) %in% d.k:d.a & dat$rpv <= 0,
                                          "rpv"],
                                      na.rm = TRUE))

                    if(is.na(dprv1) | dprv1 == 0)
                        dprv1 <- abs(mean(data[index(dat) %in% index(data)[(a - 50):a], "rpv"],
                                          na.rm = TRUE))

                    alpha1 <- uprv1/dprv1
                    delta <- p.a/p.k

                    if(is.na(uprv1) | uprv1 == 0)
                        alpha1 <- 0
                    
                    if(delta >= 1.13 & alpha1 > 1 & p.a > 30 &
                       p.a > m.price){
                        min.loc1 <- min.loc[which(min.loc > a)]
                        
                        for(b in min.loc1){
                            d.b <- index(data)[b]
                            p.b <- as.vector(data[d.b, "fitted"])
                            
                            dat <- data[paste0(d.a, "/", d.b)]
                            avg.vol <- mean(dat$volume, na.rm = TRUE)
                            avg.ma.vol <- mean(dat$ma.volume, na.rm = TRUE)

                            dur.cond1 <- ((d.b - d.a) < 60) & ((d.b - d.a) > 20)
                            
                            if(p.b < 0.85*p.a & p.b >= 0.65*p.a &
                               avg.vol < avg.ma.vol &
                               dur.cond1){

                                max.loc1 <- max.loc[which(max.loc > b)]
                                
                                for(c in max.loc1){
                                    d.c <- index(data)[c]
                                    p.c <- as.vector(data[d.c, "fitted"])
                                    
                                    dat <- data[paste0(d.b, "/", d.c)]
                                    uprv2 <- abs(mean(dat[dat$rpv > 0, "rpv"], na.rm = TRUE))
                                    dprv2 <- abs(mean(dat[dat$rpv <= 0, "rpv"], na.rm = TRUE))
                                    
                                    if(is.na(dprv2) | dprv2 == 0)
                                        dprv2 <- abs(mean(data[index(data) %in% index(data)[(c - 50):c],
                                                               "rpv"], na.rm = TRUE))
                                    
                                    alpha2 <- uprv2/dprv2
                                    dur.cond2 <- ((d.c - d.b) > 3) & ((d.c - d.b) < 30) 
                                    
                                    if(p.c > 0.6*p.a + 0.4*p.b &
                                       alpha2 > 1 &
                                       dur.cond2 & p.c <= p.a){

                                        min.loc2 <- min.loc[which(min.loc > c)]
                                        for(d in min.loc2){
                                            d.d <- index(data)[d]
                                            p.d <- as.vector(data[d.d, "fitted"])

                                            dat <- data[paste0(d.c, "/", d.d)]
                                            uprv3 <- abs(mean(dat[dat$rpv > 0, "rpv"], na.rm = TRUE))
                                            dprv3 <- abs(mean(dat[dat$rpv <= 0, "rpv"], na.rm = TRUE))

                                            if(is.na(dprv3) | dprv3 == 0)
                                                dprv3 <- abs(mean(data[index(data) %in%
                                                                          index(data)[(d - 50):d], "rpv"],
                                                                  na.rm = TRUE))

                                            beta <- uprv2/dprv3
                                            dur.cond3 <- ((d.d - d.c) < 20) &
                                                ((d.d - d.c) > 3)

                                            price.cond <- p.d <= p.c 

                                            if(price.cond & beta > 1 & dur.cond3 & (current.date - d.d < numDaysPast)){
                                                gamma <- log(alpha2) + log(beta) + delta
                                                
                                                good.points[[i]] <- data.frame(d.k, d.a,
                                                                               d.b, d.c, d.d,
                                                                               gamma)
                                                i <- i + 1
                                            }
                                            else
                                                next
                                        }
                                    }
                                    else
                                        next
                                }
                            }
                            else
                                next
                        }
                    }
                    k <- k - 1
                }
            }
 
            good.points <- do.call(rbind, good.points)
            
            if(!is.null(good.points)){
                                        # find the best points with highest gamma

                best.point <- c(good.points[which(good.points$gamma == max(good.points$gamma)), ])
                if(best.point$gamma > 5){
                    best.point <- sapply(best.point, as.character)
                    
                    
                    plotData <- data[ , c("xopen", "xhigh", "xlow", "xclose", "volume")]

                    chartSeries(plotData,
                                theme = chartTheme("white"), log.scale = TRUE,
                                main = company.name, legend = "")
                    plot(addTA(data$fitted, on = 1, col = "blue", lwd = 3, legend = ""))
                    plot(addLines(v = which(index(data) %in% as.Date(best.point[1:5])),
                                  col = "brown"))
                    dev.copy(pdf, paste0("PLOTS/", company.name, ".pdf"))
                    dev.off()                

                }
                return(good.points)
            }
            else
                return(NULL)
        }, error = function(e) NULL)
    }
    
    contenders <- lapply(price.data, searchPattern)
    save(contenders, file = "contenders.Rda")
}



backtestContenders <- function()
    {
load("contenders_all.Rda")
load("prices.Rda")

contenders <- contenders[!sapply(contenders, is.null)]

}

