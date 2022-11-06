#Data reading/writing
birth = read.csv("./Data/birthstatistics.csv")
blog = read.csv("./Data/blogData_test.csv", header = FALSE)
tecator = readxl::read_excel("./Data/tecator.xls")
write.csv(tecator, file = "./Data/tecator.csv",row.names = FALSE)
#Basic data manipulation
str(tecator)
tecator1 = as.data.frame(tecator)
str(tecator1)
rownames(tecator1) <- tecator1$Sample+10
rownames(tecator1)
colnames(tecator1)[1] <- "ID"
tecator1[(tecator1$Channel1 > 3 & tecator1$Channel2 > 3), 5:8]
tecator1$ID <- NULL
#Divide by means
col_indx <- stringr::str_which(colnames(tecator1), "Channel")
means <- colMeans(tecator1[,col_indx])
tecator1[,col_indx] <- tecator1[,col_indx]/matrix(means,
                                                  nrow = nrow(tecator1),
                                                  ncol = length(means),
                                                  byrow = TRUE)
#Sum of squares
tecator2 <- matrix(apply(tecator1[1:5,], 1, FUN = function(x) return(sum(x^2))),
                   ncol = 1)

#(XTX)−1XTy
X <- as.matrix(tecator1[,-(101:103)])
Y <- as.matrix(tecator1[,"Fat", drop=FALSE])
dim(Y)
result1 <- solve(t(X)%*%X)%*%(t(X)%*%Y)

#New factor column
tecator1$ChannelX <- as.factor(ifelse(tecator1$Channel1>1, 
                                      "high", 
                                      "low"))
tecator1[,c("Channel1", "ChannelX")]

#Calc intercept
intercept_v <- numeric(100)
for (i in 1:100) {
  reg <- lm(formula = paste("Fat ~ Channel", i, sep = ""), data = tecator1)
  intercept_v[i] <- coef(reg)[1]
}
print(intercept_v)

#Plotting y vs x
x <- c(1:3)
y <- 5*x + 1
plot(x, y, type = 'l', color = 'blue')

#Data manipulation: dplyr, tidyr
birth1 <- dplyr::tibble(birth)

birth2 <- select(birth1, X2002:X2020)

birth1 <- dplyr::mutate(birth1, Status = ifelse(foreign.Swedish.background == 
                                        "born in Sweden with two parents born in Sweden",
                                      "Yes", "No"))

dplyr::count(birth1, sex, region)#Group by sex and region

#Group by Region and Status and get Sum
birth3 <- dplyr::select(birth1, -sex, -foreign.Swedish.background) %>%
  dplyr::group_by(region, Status) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup()

#Get Percentage by Region
birth4 <- birth3 %>%
  group_by(region) %>%
  mutate(Percentage = (X2002/sum(X2002))*100) %>%
  filter(Status == "Yes") %>%
  select(region, Percentage) %>%
  arrange(Percentage)
View(birth4)

#Pivot columns to rows
birth5 <- birth1 %>%
  group_by(region, sex, foreign.Swedish.background, Status) %>% 
  tidyr::pivot_longer(X2002:X2020, names_to = "Year", values_to = "Born") %>%
  dplyr::mutate(Year = as.numeric(stringr::str_remove(Year, "X")))
View(birth5)

#Pivot rows to columns
birth6 <- birth5 %>%
  tidyr::pivot_wider(names_from = "Year", values_from = "Born", names_prefix = 'Y_')
View(birth6)

#Remove entries with all 0s
blogS <- dplyr::select_if(blog, function(x) !all(x==0))
View(blogS)
