
# 0 is a input that is treated as NA for now because later 0 will be converted back to NA.
c1.dataset <- as.integer(strsplit(readline(prompt="Enter Sample 1: (space-separated list) \n"), " ")[[1]])
c2.dataset <- as.integer(strsplit(readline(prompt="Enter Sample 2: (space-separated list) \n"), " ")[[1]])


my.function <- function(inputA, inputB) {
  box1 <- c()
  box2 <- c()
  box3 <- c()
  box4 <- c()
  
  for (i in 1: length(inputA)) {
    pair <- c(inputA[i], inputB[i])
    if (pair[1] == 0) {
      if (pair[2] == 0) {
        box4 <- append(box4, pair)
      } else {
        box3 <- append(box3, pair)
      }
    } else {
      if (pair[2] == 0) {
        box2 <- append(box2, pair)
      } else {
        box1 <- append(box1, pair)
      }
    }
  }
  #return(list(box1, box2, box3, box4))
  c1 <- c()
  for(i in 1:length(box1)) {
    c1 <- append(c1, box1[i][1])
  }
  for(i in 1:length(box2)) {
    c1 <- append(c1, box2[i][1])
  }
  for(i in 1:length(box3)) {
    c1 <- append(c1, box3[i][1])
  }
  #for(i in 1:length(box4)) {
  #  c1 <- append(c1, box4[i][1])
  #}
  # df for the boxes
  box1.df <- data.frame(matrix(box1, ncol = 2, byrow= TRUE))
  box1.df[box1.df == 0] <- NA
  box2.df <- data.frame(matrix(box2, ncol = 2, byrow= T))
  box2.df[box2.df == 0] <- NA
  box3.df <- data.frame(matrix(box3, ncol = 2, byrow= T))
  box3.df[box3.df == 0] <- NA
  
  # Create a data frame
  new <- data.frame(matrix(c1, ncol = 2,  byrow = TRUE))
  colnames(new) <- c("Sample1", "Sample2")
  # convert all 0 into NA
  new[new == 0] <- NA
  #print(new.c1)
  
  # Data in two numeric vectors
  col.1 <- new$Sample1
  col.2 <- new$Sample2

  
  # number of pairs
  n1 <- length(box1)/2
  n2 <- length(box2)/2
  n3 <- length(box3)/2
  n4 <- length(box4)/2
  
  # kim et 
  # mean.diff.box1
  d.bar <- mean(box1.df$X2, na.rm = TRUE) - mean(box1.df$X1, na.rm = TRUE)
  # sd.diff.box1
  s.D <- sd(box1.df$X2, na.rm = T) - sd(box1.df$X1, na.rm = T)
  # mean.box2
  t.bar <- mean(box2.df$X1, na.rm = TRUE)
  # sd.box2
  s.T <- sd(box2.df$X1, na.rm = TRUE)
  # mean.box3
  n.bar <- box3.df$X2
  # sd.box3
  s.N. <- box3.df$X2
  # harmonic mean
  n.H <- 2/(1/n2 + 1/n3)
  t3 <- (n1*d.bar + n.H *(t.bar - n.bar)) / (sqrt( n1*(s.D)^2 + n.H^2 * (s.T^2/n2 + s.N.^2/n3)))
  print(list(t3))
  # Kim p-value
  print(t3[1])
  # Kim Confidence Interval
  print(t3[2])
  
  # Looney and Jones's Corrected Z-Test
  box1.and.box2.df <- rbind(box1.df, box2.df)
  box2.and.box3.df <- rbind(box2.df, box3.df)
  t.bar.star <- mean(box1.and.box2.df$X1, na.rm = TRUE)
  s.T.star <- sd(box1.and.box2.df$X1, na.rm = TRUE)
  n.bar.star <-mean(box2.and.box3.df$X2, na.rm = TRUE)
  s.N.star <- sd(box2.and.box3.df$X2, na.rm = TRUE)
  s.TN1 <- cov(box1.df$X1, box1.df$X2, use = "pairwise.complete.obs")
  #zcorr
  zcorr <- (t.bar.star - n.bar.star) / sqrt(s.T.star^2 / (n1 + n2) + s.N.star^2 / (n1 + n3) - 2*n1*s.TN1 / ((n1+n2)*(n1+n3)) )
  print(zcorr)
  
  
  
  # case 1
  if (n1 > 0 && n2 > 0 && n3 > 0) {
    # check normality
    results <- shapiro.test(x = c(col.1, col.2))
    print(results)
    if (results$p.value > 0.05) {
      if (t3[1] > 0.05) {
        print("Kim et.'s Modified t-Statistic; ")
        print(t3)
        if (zcorr > 0.05) {
          print("Looney and Jones's Corrected Z-Test; ")
          print(zcorr)
        }
        
      }else{
        # two sample paired t test
        print(t.test(col.1, col.2, paired = TRUE))
        # one sample independent t test
        print(t.test(col.1, col.2, var.equal = TRUE))
      }
    } else {
      # not normal paired t test
      print(wilcox.test(col.1, col.2, paired = TRUE))
    }
  }
  # case 2
  if (n1 > 0 && n2 > 0 && n3 == 0) {
    # check normality
    results <- shapiro.test(x = c(col.1, col.2))
    print(results)
    if (results$p.value > 0.05) {
      # two sample paired t test
      print(t.test(col.1, col.2, paired = TRUE))
      # one sample independent t test
      print(t.test(col.1, col.2, var.equal = TRUE))
    } else {
      # not normal paired t test
      print(wilcox.test(col.1, col.2, paired = TRUE))
    }
  }
  # case 3
  if (n1 > 0 && n2 == 0 && n3 > 0) {
    # check normality
    results <- shapiro.test(x = c(col.1, col.2))
    print(results)
    if (results$p.value > 0.05) {
      # two sample paired t test
      print(t.test(col.1, col.2, paired = TRUE))
      # one sample independent t test
      print(t.test(col.1, col.2))
    } else {
      # not normal paired t test
      print(wilcox.test(col.1, col.2, paired = TRUE))
    }
  }
  # case 4
  if (n1 > 0 && n2 == 0 && n3 == 0) {
    # check normality
    results <- shapiro.test(x = c(col.1, col.2))
    print(results)
    if (results$p.value > 0.05) {
      # two sample paired t test
      print(t.test(col.1, col.2, paired = TRUE))
      # two sample independent t test
      print(t.test(col.1, col.2, var.equal = TRUE))
    } else {
      # not normal t test
      print(wilcox.test(col.1, col.2, paired = TRUE))
    }
  }
  # case 5
  if (n1 == 0 && n2 > 0 && n3 > 0) {
    # check normality
    results <- shapiro.test(x = c(col.1, col.2))
    print(results)
    if (results$p.value > 0.05) {
      # two sample paired t test
      print(t.test(col.1, col.2, paired = FALSE))
      # two sample independent t test
      print(t.test(col.1, col.2, var.equal = TRUE))
    } else {
      # not normal t test
      print(wilcox.test(col.1, col.2, var.equal = TRUE))
    }
  }
  # case 6
  if (n1 == 0 && n2 > 0 && n3 == 0) {
    # check normality
    results <- shapiro.test(x = c(col.1, col.2))
    print(results)
    if (results$p.value > 0.05) {
      # one sample t test
      print(t.test(col.1, col.2))
    } else {
      # not normal t test
      print(wilcox.test(col.1, col.2))
    }
  }
  # case 7
  if (n1 == 0 && n2 == 0 && n3 > 0) {
    # check normality
    results <- shapiro.test(x = c(col.1, col.2))
    print(results)
    if (results$p.value > 0.05) {
      # one sample independent t test
      print(t.test(col.1, col.2))
    } else {
      # not normal t test
      print(wilcox.test(new.c1))
    }
  }
  # case 8
  if (n1 == 0 && n2 == 0 && n3 == 0 && n4 == 0) {
    print("this is a empty set")
  }
  # case 9
  if (n1 == 0 && n2 == 0 && n3 == 0 && n4 > 0) {
    print("input is missing all variables")
  }

  
}
my.function(c1.dataset, c2.dataset)
