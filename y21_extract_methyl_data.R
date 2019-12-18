### Author: Joshua Danoff
### Date: 11.3.18
### Goal: Extract methylation data from .html files

# Input: List of lists of files; Each list should be one set of samples,
# in the order of rep 1, rep 2, and rep 3.
# Output: Data frame of samples and .txt file

# Load packages

library(stringi)
library(tidyverse)

# Specify file input

files <- list(c("CpG Analysis Results Report Y21_AG_P1_analyzed.html",
              "CpG Analysis Results Report Y21_AG_P1_analyzed.html",
              "CpG Analysis Results Report Y21_AG_P1_analyzed.html"),
              c("CpG Analysis Results Report Y21_AG_P2_analyzed.html",
                "CpG Analysis Results Report Y21_AG_P2_analyzed.html",
                "CpG Analysis Results Report Y21_AG_P2_analyzed.html"),
              c("CpG Analysis Results Report Y21_AG_P3_analyzed.html",
                "CpG Analysis Results Report Y21_AG_P3_analyzed.html",
                "CpG Analysis Results Report Y21_AG_P3_analyzed.html"),
              c("CpG Analysis Results Report Y21_AG_P4_analyzed.html",
                "CpG Analysis Results Report Y21_AG_P4_analyzed.html",
                "CpG Analysis Results Report Y21_AG_P4_analyzed.html"))

# Run Joshua Danoff's function for error estimation

jessSE <- function(x1,x2,x3) {
  if (anyNA(c(x1,x2,x3))){
    return(NA)
  } else {
    return(mean(c(abs(x1-mean(c(x1,x2,x3)))),abs(x2-mean(c(x1,x2,x3))),abs(x3-mean(c(x1,x2,x3)))))
  }
}

# Run Joshua Danoff's function for data extraction

getMethylData <- function(files) {
  for (i in 1:length(files)) {
    
    connellyString1 <- readLines(files[[i]][[1]],skipNul=TRUE)
    connellyString2 <- readLines(files[[i]][[2]],skipNul=TRUE)
    connellyString3 <- readLines(files[[i]][[3]],skipNul=TRUE)
    
    
    connellyString1 <- stri_split_fixed(connellyString1,"</tr><tr>")[[1]]
    connellyString2 <- stri_split_fixed(connellyString2,"</tr><tr>")[[1]]
    connellyString3 <- stri_split_fixed(connellyString3,"</tr><tr>")[[1]]
    
    
    connellyString1 <- stri_split_fixed(connellyString1,"</td><td>")
    connellyString2 <- stri_split_fixed(connellyString2,"</td><td>")
    connellyString3 <- stri_split_fixed(connellyString3,"</td><td>")
    
    
    connellyString1 <- lapply(connellyString1,function(x){stri_replace_all_regex(x,"</td>|<td>","")})
    connellyString2 <- lapply(connellyString2,function(x){stri_replace_all_regex(x,"</td>|<td>","")})
    connellyString3 <- lapply(connellyString3,function(x){stri_replace_all_regex(x,"</td>|<td>","")})
    
    
    connellyString1[[1]][1] <- stri_replace_all_fixed(connellyString1[[1]][1],"\377\376<H1 align=\"center\" style='text-align:center'><span style='font-size:12.0pt;font-family:Verdana'>","")
    connellyString1[[1]][1] <- stri_replace_all_fixed(connellyString1[[1]][1],"<U+FFFD><U+FFFD><H1 align=\"center\" style='text-align:center'><span style='font-size:12.0pt;font-family:Verdana'>","")
    connellyString1[[1]][1] <- stri_replace_all_fixed(connellyString1[[1]][1],"</H1><BR/><table style='text-align:left;font-size: 8pt; font-family: Verdana;'><tr> ","")
    connellyString1[[length(connellyString1)]][20] <- stri_replace_all_fixed(connellyString1[[length(connellyString1)]][20],"</tr></table><span style='font-size:8.0pt;font-family:Verdana'>","")
    
    connellyString2[[1]][1] <- stri_replace_all_fixed(connellyString2[[1]][1],"\377\376<H1 align=\"center\" style='text-align:center'><span style='font-size:12.0pt;font-family:Verdana'>","")
    connellyString2[[1]][1] <- stri_replace_all_fixed(connellyString2[[1]][1],"<U+FFFD><U+FFFD><H1 align=\"center\" style='text-align:center'><span style='font-size:12.0pt;font-family:Verdana'>","")
    connellyString2[[1]][1] <- stri_replace_all_fixed(connellyString2[[1]][1],"</H1><BR/><table style='text-align:left;font-size: 8pt; font-family: Verdana;'><tr> ","")
    connellyString2[[length(connellyString2)]][20] <- stri_replace_all_fixed(connellyString2[[length(connellyString2)]][20],"</tr></table><span style='font-size:8.0pt;font-family:Verdana'>","")
    
    connellyString3[[1]][1] <- stri_replace_all_fixed(connellyString3[[1]][1],"\377\376<H1 align=\"center\" style='text-align:center'><span style='font-size:12.0pt;font-family:Verdana'>","")
    connellyString3[[1]][1] <- stri_replace_all_fixed(connellyString3[[1]][1],"<U+FFFD><U+FFFD><H1 align=\"center\" style='text-align:center'><span style='font-size:12.0pt;font-family:Verdana'>","")
    connellyString3[[1]][1] <- stri_replace_all_fixed(connellyString3[[1]][1],"</H1><BR/><table style='text-align:left;font-size: 8pt; font-family: Verdana;'><tr> ","")
    connellyString3[[length(connellyString3)]][20] <- stri_replace_all_fixed(connellyString3[[length(connellyString3)]][20],"</tr></table><span style='font-size:8.0pt;font-family:Verdana'>","")
    
    
    connellyString1[[2]][5:13] <- paste0(connellyString1[[2]][5:13],rep(1:3,each=3))
    connellyString2[[2]][5:13] <- paste0(connellyString2[[2]][5:13],rep(1:3,each=3))
    connellyString3[[2]][5:13] <- paste0(connellyString3[[2]][5:13],rep(1:3,each=3))
    
    actualData1 <- as.data.frame(do.call(rbind,connellyString1[-1:-2]))
    colnames(actualData1) <- connellyString1[[2]]
    
    actualData2 <- as.data.frame(do.call(rbind,connellyString2[-1:-2]))
    colnames(actualData2) <- connellyString2[[2]]
    
    actualData3 <- as.data.frame(do.call(rbind,connellyString3[-1:-2]))
    colnames(actualData3) <- connellyString3[[2]]
    if (i == 1) {
      dataOutput <- data.frame('Well' = actualData1$Well, 'Assay' = actualData1$Assay, 'Sample' = actualData1$`Sample ID`,
                               'Pos1Rep1' = actualData1$`Meth. (%)1`, 'Pos1Rep2' = actualData2$`Meth. (%)1`,
                               'Pos1Rep3' = actualData3$`Meth. (%)1`, 'Pos2Rep1' = actualData1$`Meth. (%)2`,
                               'Pos2Rep2' = actualData2$`Meth. (%)2`, 'Pos2Rep3' = actualData3$`Meth. (%)2`)
    } else {
      dataAppend <- data.frame('Well' = actualData1$Well, 'Assay' = actualData1$Assay, 'Sample' = actualData1$`Sample ID`,
                               'Pos1Rep1' = actualData1$`Meth. (%)1`, 'Pos1Rep2' = actualData2$`Meth. (%)1`,
                               'Pos1Rep3' = actualData3$`Meth. (%)1`, 'Pos2Rep1' = actualData1$`Meth. (%)2`,
                               'Pos2Rep2' = actualData2$`Meth. (%)2`, 'Pos2Rep3' = actualData3$`Meth. (%)2`)
      dataOutput <- rbind(dataOutput, dataAppend)
    }
    
    errors1 <- !actualData1$Quality1=='Passed' | !actualData1$Quality2=='Passed' | !actualData3$Quality1=='Passed'
    if (length(dataOutput$Sample[errors1])) {
      warning(paste0('Check the following samples: ',
                     paste0(dataOutput$Sample[errors1],collapse = ', ')))
    }
    
  }
  dataOutput2 <- dataOutput
  for (i in 4:9){
    dataOutput2[,i] <- as.numeric(as.character(dataOutput2[,i]))
  }
  dataOutput2$Pos1Mean <- rowMeans(dataOutput2[,c('Pos1Rep1', 'Pos1Rep2', 'Pos1Rep3')])
  dataOutput2$Pos2Mean <- rowMeans(dataOutput2[,c('Pos2Rep1', 'Pos2Rep2', 'Pos2Rep3')])
  dataOutput2$Pos1SE <- mapply(FUN = jessSE
                               ,dataOutput2$Pos1Rep1,
                               dataOutput2$Pos1Rep2,
                               dataOutput2$Pos1Rep3)
  dataOutput2$Pos2SE <- mapply(FUN = jessSE
                               ,dataOutput2$Pos2Rep1,
                               dataOutput2$Pos2Rep2,
                               dataOutput2$Pos2Rep3)
  return(dataOutput2)
}

# Assign output of function 

methyl_data <- getMethylData(files) %>%
  rename(subj = Sample, site_924_unscaled = Pos1Mean, 
    site_934_unscaled = Pos2Mean) %>%
  select(subj, site_924_unscaled, site_934_unscaled)

# Remove control sample rows

control_start <- which(methyl_data$subj == "AM")
control_end <- nrow(methyl_data)
methyl_data <- methyl_data[-(control_start:control_end),]

# Scale methylation values for modeling
### Check here!

methyl_data <- methyl_data %>%
  mutate(subj = as.numeric(as.character(subj)),
         site_924 = scale(site_924_unscaled), 
         site_934 = scale(site_934_unscaled))

# Write data to file

write.table(methyl_data,"methyl_data.txt")
