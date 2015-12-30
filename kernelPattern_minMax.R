rm(list = ls())

library("np")
library("quantmod")
library("TTR")
library("parallel")

current.date <- Sys.Date()
start.date <- current.date - 300

                                        # getting price volume information
symbols.list <- read.csv("DATA/bseSymbols.csv")[ ,2]
symbols.list <- paste0(symbols.list, ".BO")

system.time(price.data <- mclapply(symbols.list, function(x){
                           tryCatch(
                               getSymbols(x, env = NULL,
                                          from = start.date),
                               error = function(e) NULL)
                       }, mc.cores = 3))

names(price.data) <- symbols.list
price.data <- price.data[!sapply(price.data, is.null)]
save(price.data, file = "prices.Rda")

                                        # searching for patterns in each stock
contenders <- mclapply(price.data, function(price){
                           tryCatch({
                               data <- price
                               company.name <- strsplit(colnames(data)[1], "\\.")[[1]][1]
                               print(company.name)
                               colnames(data) <- c("open", "high", "low", "close",
                                                   "volume", "adjusted")
                               
                               data <- data[paste0(start.date, "/", current.date)]
                               
                               data <- as.data.frame(data)
                               data$date.no <- order(rownames(data))
                               data$date <- as.Date(rownames(data))
                               
                               model.np <- npreg(close ~ date.no, regtype = "ll", bws = 3.5,
                                                 gradients = TRUE, data = data)
                               
                               ## summary(model.np)
                               ## npsigtest(model.np)
                               
                               data$fitted <- fitted(model.np)
                               data$rpv <- NA
                               data$rpv[-1] <- diff(fitted(model.np)) * data$volume[-1]
                               
                               data$ma.volume <- SMA(data$volume, 50)
                               data$ma.rpv <- SMA(data$rpv, 50)
                               
                                        # setting the min and max locations
                               data$max.loc <- NA
                               data$min.loc <- NA
                               data$max.loc[which(diff(sign(diff(data$fitted))) == -2) + 1] <- 1
                               data$min.loc[which(diff(sign(diff(data$fitted))) == 2) + 1] <- 1
                               
                               max.loc <- which(data$max.loc == 1) 
                               min.loc <- which(data$min.loc == 1)

                               max.loc <- max.loc[max.loc > 50]
                               min.loc <- min.loc[min.loc > 50]

                               good.points <- list()
                               i <- 1

                               for(a in max.loc){
                                   
                                   k <- 10
                                   while(k > 5){
                                       dat <- data[data$date >= data$date[a-k], ]

                                       d.k <- data$date[a-k]
                                       p.k <- dat[dat$date == d.k, "fitted"]

                                       d.a <- data$date[a]
                                       p.a <- dat[dat$date == d.a, "fitted"]

                                       uprv1 <- abs(mean(dat[dat$date %in% d.k:d.a & dat$rpv > 0,
                                                             "rpv"],
                                                         na.rm = TRUE))
                                       dprv1 <- abs(mean(dat[dat$date %in% d.k:d.a & dat$rpv <= 0,
                                                             "rpv"],
                                                         na.rm = TRUE))

                                       if(is.na(dprv1) | dprv1 == 0)
                                           dprv1 <- abs(mean(data[data$date %in% data$date[a - 50]:d.a, "rpv"],
                                                             na.rm = TRUE))

                                       alpha1 <- uprv1/dprv1
                                       delta <- p.a/p.k
                                       
                                       if(delta > 1 & alpha1 > 1 & p.a > 30){
                                           min.loc1 <- min.loc[which(min.loc > a)]
                                           for(b in min.loc1){
                                               d.b <- data$date[b]
                                               p.b <- data[data$date == d.b, "fitted"]
                                               
                                               dat <- data[data$date %in% d.a:d.b, ]
                                               avg.vol <- mean(dat$volume, na.rm = TRUE)
                                               avg.ma.vol <- mean(dat$ma.volume, na.rm = TRUE)

                                               dur.cond1 <- ((d.b - d.a) < 60) & ((d.b - d.a) > 20)
                                               
                                               if(p.b < p.a & avg.vol < avg.ma.vol &
                                                  dur.cond1){

                                                   max.loc1 <- max.loc[which(max.loc > b)]
                                                   
                                                   for(c in max.loc1){
                                                       d.c <- data$date[c]
                                                       p.c <- data[data$date == d.c, "fitted"]
                                                       
                                                       dat <- data[data$date %in% d.b:d.c, ]
                                                       uprv2 <- abs(mean(dat[dat$rpv > 0, "rpv"], na.rm = TRUE))
                                                       dprv2 <- abs(mean(dat[dat$rpv <= 0, "rpv"], na.rm = TRUE))
                                                       
                                                       if(is.na(dprv2) | dprv2 == 0)
                                                           dprv2 <- abs(mean(data[data$date %in% data$date[c - 50]:d.c,
                                                                                  "rpv"], na.rm = TRUE))
                                                       
                                                       alpha2 <- uprv2/dprv2
                                                       dur.cond2 <- ((d.c - d.b) > 3) & ((d.c - d.b) < 60) &
                                                           ((d.c - d.b) <= 2.5*(d.b - d.a))
                                                       
                                                       if(p.c > p.b & alpha2 > 1 & dur.cond2
                                                          & p.c > 0.5*p.a){

                                                           min.loc2 <- min.loc[which(min.loc > c)]
                                                           for(d in min.loc2){
                                                               d.d <- data$date[d]
                                                               p.d <- data[data$date == d.d, "fitted"]

                                                               dat <- data[data$date %in% d.c:d.d, ]
                                                               uprv3 <- abs(mean(dat[dat$rpv > 0, "rpv"], na.rm = TRUE))
                                                               dprv3 <- abs(mean(dat[dat$rpv <= 0, "rpv"], na.rm = TRUE))

                                                               if(is.na(dprv3) | dprv3 == 0)
                                                                   dprv3 <- abs(mean(data[data$date %in%
                                                                                              data$date[d - 50]:d.d, "rpv"],
                                                                                     na.rm = TRUE))

                                                               beta <- uprv2/dprv3
                                                               dur.cond3 <- ((d.d - d.c) < 20) &
                                                                   ((d.d - d.c) > 3) &
                                                                       ((d.d - d.c) <= 1.5*(d.c - d.b))

                                                               price.cond <- p.d <= p.c &
                                                               (p.d > 0.7*p.c + 0.3*p.b) &
                                                               p.c <= p.a & p.d > p.b &
                                                               p.c >= 0.8*p.a
                                                               # & p.d < 0.9*p.c
                                                               
                                                               
                                                               if(price.cond & beta > 0.6 & dur.cond3){
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
                                   if(best.point$gamma > 6){
                                       pdf(paste0("PLOTS/", company.name,
                                                  ".pdf"))
                                       par(mfrow = c(2, 1))
                                       plot(data$date, data$close, cex = .1,
                                            xlab = "date", ylab = "price",
                                            type = "p", xaxt = "n",
                                            main = company.name)
                                       axis.Date(side = 1, data$date,
                                                 format = "%d/%m/%Y")
                                       lines(data$date, fitted(model.np),
                                             type = "l", col = "blue")
                                       abline(v = best.point[1:5])
                                        #abline(v = data$date[max.loc], col = "green")
                                        #abline(v = data$date[min.loc], col = "pink")
                                       barplot(data$volume, ylab = "volume",
                                               xlab = "date")
                                       dev.off()
                                       return(good.points)
                                   }
                                   else
                                       return(NULL)
                               }
                               else
                                   return(NULL)
                           }, error = function(e) NULL)
                       }, mc.cores = 3)

            

