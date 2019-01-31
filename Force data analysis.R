Url = "https://github.com/zzzz88/Force-data-analysis/blob/master/Force%20data.xlsx"
download.file(Url, destfile = "force data.xlsx",method = "curl")
library(readxl)
forcedata <- read_xlsx("force data.xlsx")

colnames(forcedata) <- c( "force_measurement","pig1_piece1","pig1_piece2",
                        "pig2_piece1","pig2_piece2",
                        "pig3_piece1","pig3_piece2")
forcedata$force_measurement <- 1:nrow(forcedata)
df <- as.data.frame(sapply(forcedata, as.numeric))
library(reshape2)
cmbdata1 <-melt(df,id.vars = "force_measurement",
                measure.vars = c("pig1_piece1","pig1_piece2"), 
                variable.name = "pig1", na.rm = TRUE)
cmbdata2 <-melt(df,id.vars = "force_measurement",
                measure.vars = c("pig2_piece1","pig2_piece2"), 
                variable.name = "pig2", na.rm = TRUE)
cmbdata3 <-melt(df,id.vars = "force_measurement",
                measure.vars = c("pig3_piece1","pig3_piece2"), 
                variable.name = "pig1", na.rm = TRUE)
P1 <-t.test(df$pig1_piece1,df$pig1_piece2,paired = FALSE,var.equal = FALSE,na.rm
            =TRUE)
P2 <-t.test(df$pig2_piece1,df$pig2_piece2,paired = FALSE,var.equal = FALSE,na.rm 
            =TRUE)
P3 <-t.test(df$pig2_piece1,df$pig3_piece2,paired = FALSE,var.equal = FALSE,na.rm 
            =TRUE)
P4 <-t.test(cmbdata1$value,cmbdata2$value,paired = FALSE,var.equal = FALSE)
P5 <-t.test(cmbdata1$value,cmbdata3$value,paired = FALSE,var.equal = FALSE)
P6 <-t.test(cmbdata3$value,cmbdata2$value,paired = FALSE,var.equal = FALSE)
P.value <- c(P1$p.value,P2$p.value,P3$p.value,P4$p.value,P5$p.value,P6$p.value)

P.value
