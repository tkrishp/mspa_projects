# import libraries
library(dplyr)
library(tidyr)
library(e1071)
library(ggplot2)
library(lattice)
library(GGally)
library(RColorBrewer)

draw_plots = function(allstate_data_tbl) {
  allstate_data_purchase = filter(allstate_data_tbl, record_type == 1)
  
  p = qplot(age_oldest,age_youngest,colour=factor(A),data=allstate_data_purchase)
  p + scale_colour_manual(values=c("red","green","black"))
  
  # scatter plots using lattice
  xyplot(car_age ~ (age_oldest - age_youngest), data = allstate_data_purchase)
  xyplot(car_age ~ cost, data = allstate_data_purchase)
  xyplot(A ~ B, data = allstate_data_tbl)
  
  # histograms
  histogram(~ car_age | factor(A), data = allstate_data_purchase, 
            ref = TRUE,
            main = "Youngest age in the gruop",
            xlab = "age")
  
  histogram(~ factor(A) | factor(B), data = allstate_data_purchase, 
            ref = TRUE,
            main = "Youngest age in the gruop",
            xlab = "age")
  
  # matrix plots
  pairs(allstate_data_purchase[,c(10,13,14,17)],
        upper.panel=panel.cor,
        diag.panel=panel.hist)
  
  # histograms
  histogram(~ product_selection | product, 
            data = as.data.frame(allstate_data_purchase %>% 
                                   select(A, B, C, D, E, F, G) %>% 
                                   gather(product, product_selection)
            ),
            ref = TRUE,
            main = "Histogram of product selections",
            xlab = "Product Selection",
            par.settings = my.settings,
            par.strip.text=list(col="white", font=2),
  )
  
  histogram(~ hour | factor(A), data = allstate_data_purchase,
            ref = TRUE,
            main = "Histogram of hour of purchase",
            xlab = "Hour"
  )
  
  histogram(~ car_age, data = allstate_data_purchase,
            ref = TRUE,
            main = "Histogram of cost",
            xlab = "cost"
  )
  
  histogram(~ car_value, data = allstate_data_purchase,
            ref = TRUE,
            main = "Histogram of cost",
            xlab = "cost"
  )
  
  histogram(~ cost, data = allstate_data_tbl,
            ref = TRUE,
            main = "Histogram of cost",
            xlab = "cost"
  )
  
  # bar plots
  # interesting plots
  barchart(day ~ count | product, 
           groups = product_selection,
           origin=0,
           data = as.data.frame(allstate_data_purchase %>% 
                                  select(day, A, B, C, D, E, F, G) %>% 
                                  gather(product, product_selection, -day) %>%
                                  group_by(day, product, product_selection) %>%
                                  summarise(count=n())
           ),
           main="Purchases per Day per Product Category", 
           xlab="Total Purchases", 
           ylab="Day of the Week",
           scales=list(alternating=1),                  
           auto.key=list(space="top", columns=4, 
                         points=FALSE, rectangles=TRUE,
                         title="Product Preferences", cex.title=1
           ),
           #        par.settings = my.settings,
           par.strip.text=list(col="white", font=2),
           panel=function(x,y,...){
             panel.grid(h=-1, v=0); 
             panel.barchart(x,y,...)
           }
  )
  
  barchart(hod ~ count | product, 
           groups = product_selection,
           origin=0,
           data = as.data.frame(allstate_data_purchase %>% 
                                  select(hod, A, B, C, D, E, F, G) %>% 
                                  gather(product, product_selection, -hod) %>%
                                  group_by(hod, product, product_selection) %>%
                                  summarise(count=n())
           ),
           main="Purchases per hour per Product Category", 
           xlab="Total Purchases", 
           ylab="Hour of the Day",
           scales=list(alternating=1),                  
           auto.key=list(space="top", columns=4, 
                         points=FALSE, rectangles=TRUE,
                         title="Product Preferences", cex.title=1
           ),
           #        par.settings = my.settings,
           par.strip.text=list(col="white", font=2),
           panel=function(x,y,...){
             panel.grid(h=-1, v=0); 
             panel.barchart(x,y,...)
           }
  )
  
  barchart(day ~ n, 
           data = as.data.frame(allstate_data_tbl %>%
                                  select(day, A) %>%
                                  group_by(day, A) %>%
                                  summarise(n=n())), 
           groups = A, stack = FALSE, 
           main = list(label="Distribution of purchases per day",cex=1.6),
           xlab = "Total purchases",
           ylab = "Day of the week",
           auto.key = list(space='right', cex=1.2, title = "A")
  )
  
  barchart(day ~ n, 
           data = as.data.frame(allstate_data_tbl %>%
                                  select(day, D) %>%
                                  group_by(day, D) %>%
                                  summarise(n=n())), 
           groups = D, stack = TRUE, 
           main = list(label="Distribution of purchases per day",cex=1.6),
           xlab = "Total purchases",
           ylab = "Day of the week",
           auto.key = list(space='right', cex=1.2, title = "D")
  )
  
  # box-plot
  bwplot(~ cost, data = allstate_data_purchase,
         ref = TRUE,
         main = "Boxplot of cost",
         xlab = "cost"
  )
}