options(warn=-1)
library(readr)
library(reshape2)
library(ggplot2)
library(tidyr)
library("RColorBrewer")
library(scales)
library(dplyr)
library(ggpubr)
library(magrittr)
library(hexbin)
library(grid)
library(gridExtra)
library(GGally)
library(plyr)
setwd("~/")

run <- function(){
  cat("* Extracting classes that run (out of 346 classes)\n")
  extract_classes()
  cat("* Merging the coverage resulting from running the GA 30 times\n")
  merge_coverage_runs()
  cat("* Calculating the autocorrelation\n")
  calculate_AC()
  cat("* Merging the data of the two properties of neutrality walk\n")
  merge_neutrality_properties()
  cat("* Calculating the information analysis measures\n")
  calculate_IA()
  cat("* Calculating the Success Rate (SR) with the six measures\n")
  get_SR_measures()
  cat("* Producing Figure 1 (Results of the six fitness landscape measures applied on the branches)\n")
  generate_figure1()
  cat("* Producing Figure 2 (The Spearman correlation of SR with each of the six measures for all the branches)\n")
  generate_figure2()
  cat("* Calculating the coverage by test cases of RW and GA\n")
  get_branch_coverage_by_RW_and_GA() 
  cat("* Producing Figure 3 (Four groups of the branches based on their coverage by MOSA and random walk)\n")
  generate_figure3()
  cat("* Classifying the branches into four groups\n")
  classify_branches()
  cat("* Calculating the frequency of fitness values during RW\n")
  calculate_RW_fitness_frequency()
  cat("* Producing Figure 7 (Number of discrete fitness values obtained by the random walk for each branch in the four groups)\n")
  generate_figure7()
  cat("* Calculating the number of method executions during RW & method types\n")
  get_method_execution_RW()
  cat("* Producing Figure 4 (Number of method executions during the random walk for each branch in the four groups)\n")
  generate_figure4()
  cat("* Producing Figure 5 (Types of methods containing each branch in the four groups)\n")
  generate_figure5()
  cat("* Classifying branch types based on four groups\n")
  classify_branch_types()
  cat("* Producing Figure 8 (Classifications of the branch types in the four groups)\n")
  generate_figure8()
  cat("* Calculating the number of thrown exceptions by methods during RW\n")
  get_thrown_exceptions_RW()
  cat("* Producing Figure 6 (Number of exceptions thrown by methods containing each branch in the four groups)\n")
  generate_figure6()
}

# Extract classes names and total branches
extract_classes <- function(){
  df <- read.csv("Data/pre_process/statistics.csv")
  list <- df %>% select(TARGET_CLASS, Total_Goals)
  names(list)[names(list) == "TARGET_CLASS"] <- "Class"
  names(list)[names(list) == "Total_Goals"] <- "Branches"
  write.csv(list, "Data/Classes.csv",row.names = FALSE)
}

# merge the coverage of 30 runs 
merge_coverage_runs <- function(){
  classes <- read.csv("Data/Classes.csv")
  ifelse(!dir.exists(file.path("Data/post_process/", "GA_coverage")), dir.create(file.path("Data/post_process/", "GA_coverage")), FALSE)
  for(class in classes$Class){
    dir_path <- paste("Data/pre_process/GA_coverage/",class,sep = "")
    data_per_class <- list.files(path=dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
    merged_data <- do.call("rbind",lapply(data_per_class,FUN=function(files1){read.csv(files1)}))
    output_path <- paste("Data/post_process/GA_coverage/",class,".csv",sep = "")
    write.csv(merged_data, output_path,row.names = FALSE)
  }
}

# calculate autocorrelation (AC)
calculate_AC <- function(){
  classes <- read.csv("Data/Classes.csv")
  ifelse(!dir.exists(file.path("Data/post_process/", "AC")), dir.create(file.path("Data/post_process/", "AC")), FALSE)
  for(class in classes$Class){
    transpose_data(class)
    call_AC_calculator(class)
  }
}

# Transapose data -- as part of calculating AC & IA 
transpose_data <- function(class){
  ifelse(!dir.exists(file.path("Data/process/", "Transposed_data")), dir.create(file.path("Data/process/", "Transposed_data")), FALSE)
  unlink("Data/process/Transposed_data/*")
  dir_path <- paste("Data/pre_process/RW/",class,sep = "")
  data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
  for(i in 1:length(data_per_class)){
    df <- read.csv(data_per_class[i])
    df <- df[-c(1:2), -c(1:5)]
    transpoed_data <- t(df)
    output_path <- paste("Data/process/Transposed_data/transposedData",i,".csv",sep="")
    write.table(transpoed_data, output_path, sep=",", col.names=FALSE, row.names = FALSE)
  }
}

# Call AC calculator class -- as part of calculating AC
call_AC_calculator <- function(class){
  library(rJava)
  .jinit()
  .jaddClassPath("Supporting/")
  obj <- .jnew("CalculateAC")
  dd <- .jcall(obj,"V","main",.jarray(class, "java/lang/String"))
  dd
}

# Merge the two properties of neutrality walk 
merge_neutrality_properties <- function(){
  ifelse(!dir.exists(file.path("Data/post_process/", "ND")), dir.create(file.path("Data/post_process/", "ND")), FALSE)
  ifelse(!dir.exists(file.path("Data/post_process/", "NV")), dir.create(file.path("Data/post_process/", "NV")), FALSE)
  classes <- read.csv("Data/Classes.csv")
  for(class in classes$Class){
    file_name <- paste(class,"_NT.csv",sep = "")
    data_per_class <- list.files("Data/pre_process/NT/", file_name, recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
    nv_data <- data.frame()
    nd_data <- data.frame()
    for(i in 1:length(data_per_class)){
      df <- read_csv(data_per_class[i])
      df <- df[!(df$Class=="Class"),]
      df <- subset(df, select=-c(X10))
      goals <- unique(df$Total_goals)
      if(nrow(df) > goals){
        print(nrow(df))
        st <- goals+1
        df <- df[-(st:nrow(df)),]
      }
      for(j in 1:goals){
        nv_data[j,i] <- df$NeutralityVolume[df$Branch == j]
        nd_data[j,i] <- df$NeutralityDistance[df$Branch == j]
      }
    }
    NV_output_path <- paste("Data/post_process/NV/",class,".csv",sep = "")
    write.table(nv_data, NV_output_path, sep=",", col.names=FALSE, row.names = FALSE)
    ND_output_path <- paste("Data/post_process/ND/",class,".csv",sep = "")
    write.table(nd_data, ND_output_path, sep=",", col.names=FALSE, row.names = FALSE)
  }
}

# calculate information analysis measures (IA)
calculate_IA <- function(){
  classes <- read.csv("Data/Classes.csv")
  ifelse(!dir.exists(file.path("Data/post_process/", "IA")), dir.create(file.path("Data/post_process/", "IA")), FALSE)
  for(class in classes$Class){
    transpose_data(class)
    call_IA_calculator(class)
  }
}

# Call IA calculator class -- as part of calculating IA
call_IA_calculator <- function(class){
  library(rJava)
  .jinit()
  .jaddClassPath("Supporting/")
  obj <- .jnew("CalculateIA")
  dd <- .jcall(obj,"V","main",.jarray(class, "java/lang/String"))
  dd
}

# Get Success Rate (SR) with the six measures
get_SR_measures <- function(){
  classes <- read.csv("Data/Classes.csv")
  ifelse(!dir.exists(file.path("Data/post_process/", "RQ1_and_RQ2_data")), dir.create(file.path("Data/post_process/", "RQ1_and_RQ2_data")), FALSE)
  SR_AC_data <- data.frame(TARGET_CLASS = NA, SR = NA, AC = NA)
  SR_NV_data <- data.frame(TARGET_CLASS = NA, SR = NA, NV = NA)
  SR_ND_data <- data.frame(TARGET_CLASS = NA, SR = NA, ND = NA)
  SR_IA_data <- data.frame(TARGET_CLASS = NA, SR = NA, IC = NA, PIC = NA, DBI = NA)
  x <- 1
  for(class in classes$Class){
    SR_data <- read.csv(paste("Data/post_process/GA_coverage/",class,".csv",sep = ""))
    AC_data <- read.csv(paste("Data/post_process/AC/",class,".csv",sep = ""), header = FALSE)
    NV_data <- read.csv(paste("Data/post_process/NV/",class,".csv",sep = ""), header = FALSE)
    ND_data <- read.csv(paste("Data/post_process/ND/",class,".csv",sep = ""), header = FALSE)
    IA_data <- read.csv(paste("Data/post_process/IA/",class,".csv",sep = ""), header = FALSE)
    SR_data <- SR_data[,-1:-3]
    AC_data <- AC_data[,-1]
    AC_data <- AC_data[,colSums(is.na(AC_data))<nrow(AC_data)]
    NV_data <- NV_data[,colSums(is.na(NV_data))<nrow(NV_data)]
    ND_data <- ND_data[,colSums(is.na(ND_data))<nrow(ND_data)]
    IA_data <- aggregate(IA_data[,4:6], list(IA_data$V1, IA_data$V2), max)
    IA_data <- aggregate(IA_data[,3:5] , list(Group.2=IA_data$Group.2), mean)
    for(i in 1:nrow(SR_data)){
      total <- 0
      execution_count <- as.vector(unlist(SR_data[i,]))
      for(j in 1:ncol(SR_data)){
        if(!is.na(execution_count[j]) & execution_count[j] != 0){
          total <- total + 1
        }
      }
      SR_value  <- total / nrow(AC_data)
      AC_value  <- mean(AC_data[,i], na.rm = TRUE)
      NV_value  <- (mean(NV_data[,i], na.rm = TRUE))/1000
      ND_value  <- (mean(ND_data[,i], na.rm = TRUE))/1000
      IA_vector <- as.vector(unlist(IA_data[i,]))
      IC_value  <- IA_data[2]
      PIC_value <- IA_data[3]
      DBI_value <- IA_data[4]
      SR_AC_data[x,] <- c(class, SR_value, AC_value)
      SR_NV_data[x,] <- c(class, SR_value, NV_value)
      SR_ND_data[x,] <- c(class, SR_value, ND_value)
      SR_IA_data[x,] <- c(class, SR_value, IC_value, PIC_value, DBI_value)
      x <- x + 1
      }
  }
  write.csv(SR_AC_data,"Data/post_process/RQ1_and_RQ2_data/SR_AC.csv",row.names=FALSE)
  write.csv(SR_NV_data,"Data/post_process/RQ1_and_RQ2_data/SR_NV.csv",row.names=FALSE)
  write.csv(SR_ND_data,"Data/post_process/RQ1_and_RQ2_data/SR_ND.csv",row.names=FALSE)
  write.csv(SR_IA_data,"Data/post_process/RQ1_and_RQ2_data/SR_IA.csv",row.names=FALSE)
  SR_Average  <- setNames(aggregate(as.double(SR_AC_data$SR), list(TARGET_CLASS=SR_AC_data$TARGET_CLASS), mean), c("Class","SR"))
  AC_Average  <- setNames(aggregate(as.double(SR_AC_data$AC) , list(TARGET_CLASS=SR_AC_data$TARGET_CLASS), mean), c("Class","AC"))
  ND_Average  <- setNames(aggregate(as.double(SR_ND_data$ND) , list(TARGET_CLASS=SR_ND_data$TARGET_CLASS), mean), c("Class","ND"))
  NV_Average  <- setNames(aggregate(as.double(SR_NV_data$NV) , list(TARGET_CLASS=SR_NV_data$TARGET_CLASS), mean), c("Class","NV"))
  IC_Average  <- setNames(aggregate(SR_IA_data$IC , list(TARGET_CLASS=SR_IA_data$TARGET_CLASS), mean), c("Class","IC"))
  PIC_Average <- setNames(aggregate(SR_IA_data$PIC , list(TARGET_CLASS=SR_IA_data$TARGET_CLASS), mean), c("Class","PIC"))
  DBI_Average <- setNames(aggregate(SR_IA_data$DBI , list(TARGET_CLASS=SR_IA_data$TARGET_CLASS), mean), c("Class","DBI"))
  combined <- cbind(SR=SR_Average$SR, AC=AC_Average$AC, ND=ND_Average$ND, NV=NV_Average$NV, IC=IC_Average$IC, PIC=PIC_Average$PIC, DBI=DBI_Average$DBI)
  write.csv(combined,"Data/post_process/RQ1_and_RQ2_data/SR_All.csv",row.names=FALSE)
}

# Generate plot shown in Figure 1
# Results of the six fitness landscape measures applied on the branches of the 331 classes
generate_figure1 <- function(){
  ifelse(!dir.exists(file.path("Data/post_process/", "Plots")), dir.create(file.path("Data/post_process/", "Plots")), FALSE)
  df <- read.csv("Data/post_process/RQ1_and_RQ2_data/SR_All.csv")
  df <- subset(df, select=-c(SR))
  df_long <- melt(df)
  my_breaks <- function(x){seq(min(x), min(1,max(x)), length.out = 5)}
  my_labels <- function(x){round(x, digits = 2)}
  thePlot <- ggplot(df_long, aes(x=variable,y=value))+
    geom_violin() +facet_wrap(~variable, scales = 'free', ncol = 3) + 
    theme(text = element_text(face = "bold", size = 20), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    scale_y_continuous(breaks = my_breaks, labels = my_labels, expand = c(0, 0)) + ylab("Values\n") + geom_boxplot(width=0.1, outlier.shape = NA) + theme(panel.spacing = unit(1, "lines"))
  
  thePlot
  ggsave("Data/post_process/Plots/Figure1.pdf")
}

# Generate plot shown in Figure 2
# The Spearman correlation of SR with each of the six measures for all the branches
generate_figure2 <- function(){
  df <- read.csv("Data/post_process/RQ1_and_RQ2_data/SR_All.csv")
  dt_long <- gather(df, key, value, -SR)
  min_max_trans = trans_new("min_max",function(i)i/(max(i)-min(i)),inverse=identity)
  thePlot <- ggplot(dt_long, aes(SR, value)) + geom_hex(bins = 13) + 
    facet_wrap(~key, scales = 'free_y', ncol = 3) +
    scale_fill_gradientn(colors = brewer.pal(3,"Greys"), trans=min_max_trans,labels=percent(0.25*0:4)) +
    theme_bw() + theme(text = element_text(face = "bold", size = 26), axis.text.x=element_text(angle=40, hjust=1), legend.position="bottom", panel.spacing = unit(1.5, "lines"), legend.key.size = unit(1.5, "cm"), legend.title = element_blank()) + xlab("\nSR") + ylab("Measure values\n") + #labs(fill = "Count") +
    geom_smooth(method="lm")
  thePlot
  ggsave("Data/post_process/Plots/Figure2.pdf")
}

# Is a branch covered by test cases of RW and GA?
get_branch_coverage_by_RW_and_GA <- function(){
  coverage_RW <- get_branch_coverage("RW")
  coverage_GA <- get_branch_coverage("GA")
  merge_coverage_data(coverage_RW, coverage_GA)
}

# How many times a brach covered by test cases of an algorithm
get_branch_coverage <- function(algorithm){
  classes <- read.csv("Data/Classes.csv")
  ifelse(!dir.exists(file.path("Data/post_process/", "RQ3_data")), dir.create(file.path("Data/post_process/", "RQ3_data")), FALSE)
  coverage_data <- data.frame(Class = NA, Branch = NA, Count = NA)
  x <- 1
  for(class in classes$Class){
    if(algorithm == "RW"){
      dir_path <- paste("Data/pre_process/RW/",class,sep = "")
    }else{
      dir_path <- paste("Data/pre_process/Raw_fitness_GA/",class,sep = "")
    }
    data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
    branches <- classes$Branches[classes$Class == class]
    for(i in 1:branches){
      count <- 0
      for(j in 1:length(data_per_class)){
        df <- read.csv(data_per_class[j])
        if(algorithm == "RW"){
          df <- df[-c(1:2), -c(1:5)]
          if("0.0" %in% df[,i]){
            count <- count + 1
          }
        }else{
          df <- df[, -c(1:3)]
          if("0" %in% df[,i]){
            count <- count + 1
          }
        }
      }
      coverage_data[x,] <- c(class, as.character(i), as.character(count))
      x <- x + 1
    }
  }
  return(coverage_data)
}

# Merge the branch coverage data of RW and GA 
merge_coverage_data <- function(RW_data, GA_data){
  merged_data <- cbind(Class=as.character(RW_data$Class), Branch=RW_data$Branch, Count_RW=RW_data$Count, Count_GA=GA_data$Count)
  merged_data <- as.data.frame(merged_data)
  merged_data$Count_RW <- as.numeric(as.character(merged_data$Count_RW))
  merged_data$Count_GA <- as.numeric(as.character(merged_data$Count_GA))
  write.csv(merged_data,"Data/post_process/RQ3_data/Branch_coverage.csv",row.names=FALSE)
}

# Generate plot shown in Figure 3 
# Four groups of the branches based on their coverage by MOSA and random walk
generate_figure3 <- function(){
  df <- read.csv("Data/post_process/RQ3_data/Branch_coverage.csv")
  df <- df [!duplicated(df[c(1,2)]),]
  counts <- ddply(df, .(df$Count_RW, df$Count_GA), nrow) 
  names(counts) <- c("RW", "GA", "Freq")
  counts$percFreq <- counts$Freq
  lbreaks<-sort(unique(counts$percFreq))
  fr <- unique(counts$Freq)
  bp <- ggplot(counts, aes(x=RW, y=GA, size=Freq, color=as.factor(percFreq))) + geom_point(alpha=0.8) +
    scale_size(range = c(1, 16), name="Freq %", limits=c(1,2012), breaks=lbreaks) + 
    scale_colour_grey(name="Count", breaks=lbreaks, labels=percent(lbreaks, accuracy = 0.03)) + 
    theme_bw() +
    theme(text = element_text(face = "bold", size = 16), axis.title=element_text(size=15)) + xlab("Covered by RW") + ylab("Covered by search (MOSA)") + theme(legend.position = "none") +
    geom_hline(yintercept=15, linetype="dashed") + geom_vline(xintercept=15, linetype="dashed") +
    annotate(geom="text", x=18.5, y=18, label=expression(atop(bold("Group 1"), bold("Easy branches"))), size=3) +
    annotate(geom="text", x=11.2, y=18, label=expression(atop(bold("Group 2"), bold("Covered by search"))), size=3) +
    annotate(geom="text", x=12, y=12, label=expression(atop(bold("Group 3"), bold("Hard branches"))), size=3) +
    annotate(geom="text", x=18.5, y=12, label=expression(atop(bold("Group 4"), bold("Covered by RW"))), size=3) +
    scale_x_continuous(labels=c("0" = "0", "10" = "0.25", "20" = "0.75", "30" = "1")) +
    scale_y_continuous(labels=c("0" = "0", "10" = "0.25", "20" = "0.75", "30" = "1")) +
    scale_fill_gradientn(colors = brewer.pal(3,"Dark2"))
  bp
  ggsave("Data/post_process/Plots/Figure3.pdf")
}

# Classify the branches into four groups
classify_branches <- function(){
  df <- read.csv("Data/post_process/RQ3_data/Branch_coverage.csv")
  df_updated <- data.frame()
  for(i in 1:nrow(df)){
    row <- df[i,]
    g <- 0
    if(row$Count_GA >= 15 & row$Count_RW >= 15){
      g <- 1
    }else if(row$Count_GA >= 15 & row$Count_RW < 15){  
      g <- 2
    }else if(row$Count_GA < 15 & row$Count_RW < 15){ 
      g <- 3
    }else if(row$Count_GA < 15 & row$Count_RW >= 15){
      g <- 4
    }
    row[,"Group"] <- g
    df_updated <- rbind(df_updated, row)
  }
  write.csv(df_updated, "Data/post_process/RQ3_data/Branch_groups.csv", row.names = FALSE)
}

# Calculate the frequency of fitness values during RW
calculate_RW_fitness_frequency <- function(){
  data <- read.csv("Data/post_process/RQ3_data/Branch_coverage.csv")
  ifelse(!dir.exists(file.path("Data/post_process/RQ3_data/", "RW_fitness_frequency")), dir.create(file.path("Data/post_process/RQ3_data/", "RW_fitness_frequency")), FALSE)
  for(i in 1:nrow(data)){
    row <- data[i,]
    dir_path <- paste("Data/pre_process/RW/",row$Class,sep = "")
    data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
    fitness_frequency <- data.frame()
    branch <- as.integer(row$Branch)
    for(j in 1:length(data_per_class)){
      dataset <- read.csv(data_per_class[j])
      dataset <- droplevels(dataset[-c(1:2), -c(1:5)])
      df <- as.data.frame(table(dataset[,branch]))
      df[,"Run"] <- j
      fitness_frequency <- rbind.fill(fitness_frequency, df)
    }
    write.csv(fitness_frequency, paste("Data/post_process/RQ3_data/RW_fitness_frequency/",row$Class,"_",branch,".csv",sep = ""), row.names = FALSE)
  }
}

# Generate plot shown in Figure 7 
# Number of discrete fitness values obtained by the random walk for each branch in the four groups
generate_figure7 <- function(){
  dataset <- read.csv("Data/post_process/RQ3_data/Branch_groups.csv")
  dist_fit_1 <- c()
  dist_fit_2 <- c()
  dist_fit_3 <- c()
  dist_fit_4 <- c()
  for(x in unique(dataset$Group)){
    subdata <- subset(dataset, Group == x)
    for(i in 1:nrow(subdata)){
      row <- subdata[i,]
      df <- read.csv(paste("Data/post_process/RQ3_data/RW_fitness_frequency/",row$Class,"_",row$Branch,".csv",sep = ""))
      discrete_fitness <- length(unique(df$Var1))
      if(x == 1){
        dist_fit_1 <- append(dist_fit_1, discrete_fitness)
      }else if(x == 2){
        dist_fit_2 <- append(dist_fit_2, discrete_fitness)
      }else if(x == 3){
        dist_fit_3 <- append(dist_fit_3, discrete_fitness)
      }else if(x == 4){
        dist_fit_4 <- append(dist_fit_4, discrete_fitness)
      }
    }
  }
  all <- list(dist_fit_1,dist_fit_2,dist_fit_3,dist_fit_4)
  df_di <- data.frame(Count = unlist(all), Group = rep(letters[1:length(all)],times = sapply(all,length)))
  p <- ggplot(df_di,aes(x = Group, y = Count)) + geom_boxplot(outlier.shape = NA) + 
    theme_bw() +
    theme(text = element_text(face = "bold", size = 20), axis.title.x=element_blank()) + ylab("# fitness values") +
    scale_x_discrete(labels=c("a" = "Easy", "b" = "Search", "c" = "Hard", "d" = "RW")) 
  p
  ggsave("Data/post_process/Plots/Figure7.pdf")
}

# Get the number of method executions during RW & method types 
get_method_execution_RW <- function(){
  dataset <- read.csv("Data/post_process/RQ3_data/Branch_groups.csv")
  ifelse(!dir.exists(file.path("Data/post_process/RQ3_data/", "RW_method_execution")), dir.create(file.path("Data/post_process/RQ3_data/", "RW_method_execution")), FALSE)
  methods_exec_data <- data.frame(Group = NA, Count = NA)
  a <- 1
  methods_type_data <- data.frame(Group = NA, Type = NA)
  b <- 1
  for(x in unique(dataset$Group)){
    subdata <- subset(dataset, dataset$Group == x)
    for(i in 1:nrow(subdata)){
      row <- subdata[i,]
      branch <- as.integer(row$Branch)
      dir_path <- paste("Data/pre_process/Methods/",row$Class,sep = "")
      data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
      MethodData <- read.csv(data_per_class)
      Method_name <- MethodData[,2+branch]
      dir_path <- paste("Data/pre_process/Method_execution_RW/",row$Class,sep = "")
      data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
      count <- 0
      Method_type <- NULL
      for(j in 1:length(data_per_class)){
        df <- read.csv(data_per_class[j])
        col <- df[[as.character(Method_name)]]
        df <- tail(df, -2)
        num_executions <- sum(as.numeric(as.character(df[[as.character(Method_name)]])))
        count <- count + num_executions
        Method_type <- gsub("([A-Za-z]+).*", "\\1", as.character(col[2]))
      }
      if(x == 1){
        methods_exec_data[a,] <- c("1", count)
        methods_type_data[b,] <- c("1", Method_type)
        a <- a + 1
        b <- b + 1
      }else if(x == 2){
        methods_exec_data[a,] <- c("2", count)
        methods_type_data[b,] <- c("2", Method_type)
        a <- a + 1
        b <- b + 1
      }else if(x == 3){
        methods_exec_data[a,] <- c("3", count)
        methods_type_data[b,] <- c("3", Method_type)
        a <- a + 1
        b <- b + 1
      }else if(x == 4){
        methods_exec_data[a,] <- c("4", count)
        methods_type_data[b,] <- c("4", Method_type)
        a <- a + 1
        b <- b + 1
      }
    }
  }
  write.csv(methods_exec_data, "Data/post_process/RQ3_data/RW_method_execution/Methods_executions.csv", row.names = FALSE)
  write.csv(methods_type_data, "Data/post_process/RQ3_data/RW_method_execution/Methods_types.csv", row.names = FALSE)
}

# Generate plot shown in Figure 4
# Number of method executions during the random walk for each branch in the four groups
generate_figure4 <- function(){
  df <- read.csv("Data/post_process/RQ3_data/RW_method_execution/Methods_executions.csv")
  p <- ggplot(df, aes(x = as.factor(Group), y = Count)) + geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(text = element_text(face = "bold", size = 20), axis.title.x=element_blank()) + ylab("# method executions") 
    scale_x_discrete(labels=c("1" = "Easy", "2" = "Search", "3" = "Hard", "4" = "RW"))
  p
  ggsave("Data/post_process/Plots/Figure4.pdf")
}

# Generate plot shown in Figure 5
# Types of methods containing each branch in the four groups
generate_figure5 <- function(){
  df <- read.csv("Data/post_process/RQ3_data/RW_method_execution/Methods_types.csv")
  types <- c("public","protected","private","Constructor")
  df <- df[(df$Type %in% types),]
  counts <- ddply(df, .(df$Type, df$Group), nrow)
  names(counts) <- c("Count", "Group", "Freq")
  df_trans <- transform(counts, z = ave(Freq, Group, FUN = prop.table))
  p <- ggplot(df_trans, aes(x = Group, y = Freq, group = Count)) +
    geom_col(aes(fill = Count), position = 'fill',color = "black") +
    geom_text(aes(label = percent(z, accuracy = 1)), position = position_fill(vjust = .5)) +
    theme_bw() +
    theme(text = element_text(face = "bold", size = 20), legend.position = "bottom", legend.title = element_blank(), axis.title.x=element_blank()) + ylab("Frequency ") + labs(fill='Method type') +
    scale_x_continuous(breaks = 1:4, labels = c("Easy", "Search", "Hard", "RW")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_fill_brewer(type = "seq", palette = "Greys")
  p
  ggsave("Data/post_process/Plots/Figure5.pdf")
}

# Classify branch types based on four groups
classify_branch_types <- function(){
  dataset <- read.csv("Data/post_process/RQ3_data/Branch_groups.csv")
  c1 <- c("IF_ICMPEQ","IF_ICMPNE","IF_ICMPLT","IF_ICMPGE","IF_ICMPGT","IF_ICMPLE")
  c2 <- c("IFEQ","IFNE","IFLT","IFGE","IFGT","IFLE")
  c3 <- c("IF_ACMPEQ","IF_ACMPNE")
  c4 <- c("IFNULL","IFNONNULL")
  branch_type_data <- data.frame(Group = NA, Class = NA)
  a <- 1
  for(x in unique(dataset$Group)){
    subdata <- subset(dataset, Group == x)
    for(i in 1:nrow(subdata)){
      row <- subdata[i,]
      class <- as.character(row$Class)
      branch <- as.numeric(row$Branch)
      dir_path <- paste("Data/pre_process/RW/",class,sep = "")
      data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
      BranchData <- read.csv(sample(data_per_class,1))
      Branch_type <- BranchData[1,5+branch]
      bt_class <- "root"
      if(Branch_type %in% c1){
        bt_class <- "Integer_Integer"
      }else if(Branch_type %in% c2){
        bt_class <- "Integer_Zero"
      }else if(Branch_type %in% c3){
        bt_class <- "Reference_Reference"
      }else if(Branch_type %in% c4){
        bt_class <- "Reference_Null"
      }
      if(x == 1){
        branch_type_data[a,] <- c("1", bt_class)
        a <- a + 1
      }else if(x == 2){
        branch_type_data[a,] <- c("2", bt_class)
        a <- a + 1
      }else if(x == 3){
        branch_type_data[a,] <- c("3", bt_class)
        a <- a + 1
      }else if(x == 4){
        branch_type_data[a,] <- c("4", bt_class)
        a <- a + 1
      }
    }
  }
  write.csv(branch_type_data, "Data/post_process/RQ3_data/RW_method_execution/Branch_types.csv", row.names = FALSE)
}

# Generate plot shown in Figure 8
# Classifications of the branch types in the four groups
generate_figure8 <- function(){
  df <- read.csv("Data/post_process/RQ3_data/RW_method_execution/Branch_types.csv")
  away <- c("root")
  df <- df[!(df$Class %in% away),]
  counts <- ddply(df, .(df$Class, df$Group), nrow)
  names(counts) <- c("Count", "Group", "Freq")
  xx <- transform(counts, z = ave(Freq, Group, FUN = prop.table))
  p <- ggplot(xx, aes(x = Group, y = Freq, group = Count)) +
    geom_col(aes(fill = Count), position = 'fill',color = "black") +
    geom_text(aes(label = percent(z, accuracy = 1)), position = position_fill(vjust = .5)) +
    theme_bw() +
    theme(text = element_text(face = "bold", size = 20), legend.position = "bottom", legend.title = element_blank(), axis.title.x=element_blank()) + ylab("Frequency ") + labs(fill='Method type') +
    scale_x_continuous(breaks = 1:4, labels = c("Easy", "Search", "Hard", "RW")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    scale_fill_brewer(type = "seq", palette = "Greys")
  p
  ggsave("Data/post_process/Plots/Figure8.pdf")
}

# Get the number of thrown exceptions by methods during RW
get_thrown_exceptions_RW <- function(){
  dataset <- read.csv("Data/post_process/RQ3_data/Branch_groups.csv")
  thrown_exceptions_data <- data.frame(Group = NA, Count = NA)
  a <- 1
  for(x in unique(dataset$Group)){
    subdata <- subset(dataset, dataset$Group == x)
    for(i in 1:nrow(subdata)){
      row <- subdata[i,]
      branch <- as.integer(row$Branch)
      dir_path <- paste("Data/pre_process/Methods/",row$Class,sep = "")
      data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
      MethodData <- read.csv(data_per_class)
      Method_name <- MethodData[,2+branch]
      dir_path <- paste("Data/pre_process/Method_execution_RW/",row$Class,sep = "")
      data_per_class <- list.files(dir_path, pattern="*.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
      count <- 0
      for(j in 1:length(data_per_class)){
        df <- read.csv(data_per_class[j])
        col <- df[[as.character(Method_name)]]
        excep <- as.character(col[1])
        if(! (length(excep) == 0)){
          if(excep == "Exception"){
            count <- count + 1
          }
        }
      }
      if(x == 1){
        thrown_exceptions_data[a,] <- c("1", count)
        a <- a + 1
      }else if(x == 2){
        thrown_exceptions_data[a,] <- c("2", count)
        a <- a + 1
      }else if(x == 3){
        thrown_exceptions_data[a,] <- c("3", count)
        a <- a + 1
      }else if(x == 4){
        thrown_exceptions_data[a,] <- c("4", count)
        a <- a + 1
      }
    }
  }
  write.csv(thrown_exceptions_data, "Data/post_process/RQ3_data/RW_method_execution/Thrown_exceptions.csv", row.names = FALSE)
}

# Generate plot shown in Figure 6
# Number of exceptions thrown by methods containing each branch in the four groups
generate_figure6 <- function(){
  df <- read.csv("Data/post_process/RQ3_data/RW_method_execution/Thrown_exceptions.csv")
  p <- ggplot(df, aes(x = as.factor(Group), y = Count)) + geom_boxplot() + 
    theme_bw() +
    theme(text = element_text(face = "bold", size = 20), axis.title.x=element_blank()) + ylab("# thrown exceptions") +
    scale_x_discrete(labels=c("1" = "Easy", "2" = "Search", "3" = "Hard", "4" = "RW"))
  p
  ggsave("Data/post_process/Plots/Figure6.pdf")
}

