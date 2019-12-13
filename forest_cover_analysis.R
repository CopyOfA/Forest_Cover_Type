#Meant to be run in Jupyter notebook; triple line skips imply a new code block

library(plyr)
library(dplyr)
library(lubridate)



# Read in csv file.
forest_cover = read.csv("/forest_cover.csv")
head(forest_cover)



# Examine structure of the dataframe.
str(forest_cover)



forest_cover$Cover_Type <- mapvalues(forest_cover$Cover_Type, from = c(1,2,3,4,5,6,7), 
to = c("1.SpruceFir","2.LodgepolePine","3.PonderosaPine","4.Cottonwood-Willow","5.Aspen","6.Douglas-fir","7.Krummholz"))



head(forest_cover)



coverType_count = forest_cover %>%  
  group_by(Cover_Type) %>%    # data is grouped according to labels (1,2,3,4,5,6,7) of cover type
  dplyr::summarize(count = n()) %>%  # count the number of observations in each group
  mutate(cover_type_ratio=count/sum(count)) # divide the counts obtained in above step to get the ratio.
                                           
# # Display the contents of the variable coverType_count
coverType_count



# plot the distribution of cover type using the dataframe "coverType_count" created above. 
plot<-ggplot(coverType_count, aes(x=Cover_Type, y=count)) +   
  geom_bar(stat="identity") + # plot a bar graph 
  scale_y_continuous() + 
  geom_text(data=coverType_count, aes(label=count,y=count+100),size=4) + # Display the count for each category 
                    # at y position "count(2160)+100". This will display the value right above the bars.
  geom_text(data=coverType_count, aes(label=paste0(round(cover_type_ratio*100,1),'%'),y=count+200),size=4)+
                    # Display the count percentage rounded to one decimal place at y position "count(2160)+200". This  
                    # will display the ratios right above the counts printed in previous steps.          

                    # You should convert the ratio 0.1428571 to 14.3% for printing in above step.

  theme(axis.text.x=element_text(angle=30,hjust=1,size=10))+ # code to adjust the indices on x-axis 
                                                            # tilted. Play with the code to see what each parameter is doing.
  ggtitle('Cover Type Distribution')

plot



oldnames<-c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology",
                  "Horizontal_Distance_To_Roadways","Horizontal_Distance_To_Fire_Points","Hillshade_9am",
                  "Hillshade_Noon","Hillshade_3pm")

newnames<-c("Elevation","Aspect","Slope","HD.Hydro","VD.Hydro","HD.Road","HD.Fire","HS.9am","HS.noon","HS.3pm")

library(data.table)

setnames(forest_cover, oldnames, newnames)



p=list()
for(i in 1:length(newnames)){
  p[[i]] <- ggplot(forest_cover, aes_string(x='Cover_Type', y=newnames[i])) + 
              geom_boxplot() + 
              theme(axis.text.x=element_blank(),axis.title.x=element_blank()) # This line will keep the
               # x-axis label and title of each plot empty.
  }

# Arrange the plots in a grid using grid.arrange() in grid.Extra package. Arrange 2 plots in a row.
library(gridExtra)
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],
    ncol=2) # ncol will arrange two plots in each row.
    
    
    
library(GGally)
corr_plot<-ggcorr(forest_cover[,2:10], label=T, label_round = 2) 
corr_plot



corrFeature1<-c("HS.3pm", "Elevation", "Aspect", "Aspect", "Slope", "HD.Hydro", "HS.3pm") # So you will include HS.noon in this list
corrFeature2<-c("HS.noon", "HD.Road", "HS.9am", "HS.3pm", "HS.noon", "VD.Hydro", "HS.9am") # Corresponding feature HS.3pm is included in the list 
                                                                        # in the same position as one to one mapping.

p=list()
for(i in 1:length(corrFeature1)){
  p[[i]] <- ggplot(forest_cover, aes_string(x=corrFeature1[i], y=corrFeature2[i])) +
              geom_point(alpha=1/10)
  }

# Arrange the plots in a grid using grid.arrange() in grid.Extra package. Arrange 2 plots in a row.
library(gridExtra)
grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]],
    ncol=2) # ncol=2 will arrange two plots in each row.
    
    
    
# Existing column names. These 4 columns have to be fused into one column called wildernessArea
oldCols <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")

# New labels that are to be assigned to each row in wildernessArea corresponding to the category 
# of wilderness_Area it belongs
newLabels <- c("Rawah","Neota","ComanchePeak","CachePoudre")

# forest_cover is a dataframe. Convert it into a data.table to assign values using := operator.
forest_cover=data.table(forest_cover)
is.data.table(forest_cover) # make sure forest_cover is a data.table. 

for(i in 1:length(newLabels)) { # Loop for 4 times. length(newLabels) is 4. 
   refColumn<-oldCols[i] # Take the name in vector oldCols according to the loop count. oldCols[1] 
                         # will give Wilderness_Area1.
   refValue<-newLabels[i]  # Take the name in vector newLabels according to the loop count. 
                         # newLabels[1] will give Rawah
    
   forest_cover<-forest_cover[get(refColumn)==1,wildernessArea:=refValue]
}



#Recode the Lables for the "Soil_Type" to one column called soilType. 
newLabels<-c('Cathedral','Vanet','Haploborolis','Ratake','Vanet1','Vanet2','Gothic','Supervisor',
             'Troutville','Bullwark1','Bullwark2','Legault','Catamount1','Pachic','unspecified',
             'Cryaquolis','Gateview','Rogert','Typic1','Typic2','Typic3','Leighcan1','Leighcan2',
             'Leighcan3','Leighcan4','Granile','Leighcan5','Leighcan6','Como1','Como2','Leighcan7',
             'Catamount2','Leighcan8','Cryorthents','Cryumbrepts','Bross','Rock','Leighcan9','Moran1',
             'Moran2')

oldCols <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8",
"Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16",
"Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
"Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
"Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")

# The following code is same as previous cell.
for(i in 1:length(newLabels)) {
   refColumn<-oldCols[i]
   refValue<-newLabels[i]
   forest_cover<-forest_cover[get(refColumn)==1,soilType:=refValue]
}

# Remove the binary columns Wilderness_Area1, Wilderness_Area2, Wilderness_Area3, Wilderness_Area4 and 
# other 40 columns of soil type. These binary column span from 12 through 55 columns. 
# Assign the these columns to NULL to delete them.
forest_cover <- forest_cover[ , colnames(forest_cover[,12:55,with=FALSE]):=NULL]



temp <- forest_cover[ , colnames(forest_cover[,1:5,with=TRUE])]
dim(temp)

temp <- forest_cover[ , colnames(forest_cover[,1:5,with=FALSE])]
(temp)



str(forest_cover)



# Use setcolorder() function to reorder the columns in a dataset.
colOrder<-c("Id","Elevation","Aspect","Slope","HD.Hydro","VD.Hydro","HD.Road","HD.Fire",
            "HS.9am","HS.noon","HS.3pm","Cover_Type","wildernessArea","soilType")

# Rearrange the columns of forest_cover according to above order.
setcolorder(forest_cover, colOrder)

# Shorten the names of columns for readability.
setnames(forest_cover,colOrder)

# Remove the Id column from forest_cover.
forest_cover$Id=NULL

str(forest_cover)



forest_cover$wildernessArea = factor(forest_cover$wildernessArea)
forest_cover$soilType = factor(forest_cover$soilType)



library(randomForest)

forest_cover$Cover_Class <- factor(forest_cover$Cover_Type,
levels = c("1.SpruceFir","2.LodgepolePine","3.PonderosaPine","4.Cottonwood-Willow","5.Aspen","6.Douglas-fir","7.Krummholz"),
labels = c(1,2,3,4,5,6,7))


RandomForest_fit <- randomForest(Cover_Class~.-Cover_Type,
                                 method="class",
                                 data=forest_cover, importance=TRUE) 

plot(RandomForest_fit)
legend("topright", colnames(RandomForest_fit$err.rate),col=1:4,cex=0.8,fill=1:4)
forest_cover$Cover_Class <- NULL



importance(RandomForest_fit)



varImpPlot(RandomForest_fit)



library(caTools)
set.seed(100) # set.seed() will help us to reproduce the results.
split = sample.split(forest_cover$Cover_Type, SplitRatio=0.7)

Forestcover_train  = subset(forest_cover, split==T)

# Test data will have the rest 30% of data
Forestcover_test  = subset(forest_cover, split==F)



Forestcover_train$Cover_Type <- factor(Forestcover_train$Cover_Type,
levels = c("1.SpruceFir","2.LodgepolePine","3.PonderosaPine","4.Cottonwood-Willow","5.Aspen","6.Douglas-fir","7.Krummholz"),
labels = c(1,2,3,4,5,6,7))

Forestcover_test$Cover_Type <- factor(Forestcover_test$Cover_Type,
levels = c("1.SpruceFir","2.LodgepolePine","3.PonderosaPine","4.Cottonwood-Willow","5.Aspen","6.Douglas-fir","7.Krummholz"),
labels = c(1,2,3,4,5,6,7))


Forestcover_train$soilType <- factor(Forestcover_train$soilType,
levels = c('Cathedral','Vanet','Haploborolis','Ratake','Vanet1','Vanet2','Gothic','Supervisor',
             'Troutville','Bullwark1','Bullwark2','Legault','Catamount1','Pachic','unspecified',
             'Cryaquolis','Gateview','Rogert','Typic1','Typic2','Typic3','Leighcan1','Leighcan2',
             'Leighcan3','Leighcan4','Granile','Leighcan5','Leighcan6','Como1','Como2','Leighcan7',
             'Catamount2','Leighcan8','Cryorthents','Cryumbrepts','Bross','Rock','Leighcan9','Moran1',
             'Moran2'),
labels = c(1:40))

Forestcover_test$soilType <- factor(Forestcover_test$soilType,
levels = c('Cathedral','Vanet','Haploborolis','Ratake','Vanet1','Vanet2','Gothic','Supervisor',
             'Troutville','Bullwark1','Bullwark2','Legault','Catamount1','Pachic','unspecified',
             'Cryaquolis','Gateview','Rogert','Typic1','Typic2','Typic3','Leighcan1','Leighcan2',
             'Leighcan3','Leighcan4','Granile','Leighcan5','Leighcan6','Como1','Como2','Leighcan7',
             'Catamount2','Leighcan8','Cryorthents','Cryumbrepts','Bross','Rock','Leighcan9','Moran1',
             'Moran2'),
labels = c(1:40))



forest_cover$wildernessArea <- factor(forest_cover$wildernessArea,
                                      levels=c("Rawah","Neota","ComanchePeak","CachePoudre"), 
                                      labels=c(1:4))
                                      
                                      
                                      
anovaFire <- aov(Elevation ~ wildernessArea, data = forest_cover)
anovaFire
summary(anovaFire)



anovaElevation <- aov(HS.noon~wildernessArea, data=forest_cover)
anovaElevation
summary(anovaElevation)



anovaElevation <- aov(HD.Road~wildernessArea, data=forest_cover)
anovaElevation
summary(anovaElevation)



anovaElevation <- aov(HD.Fire~wildernessArea, data=forest_cover)
anovaElevation
summary(anovaElevation)



anovaElevation <- aov(HD.Hydro~wildernessArea, data=forest_cover)
anovaElevation
summary(anovaElevation)



manovaResults <- manova(cbind(Elevation, HS.noon, HD.Road, HD.Fire, HD.Hydro) ~ wildernessArea, data=forest_cover)
manovaResults
summary(manovaResults)



library(MASS)
Forestcover_lda=lda(Cover_Type ~.-soilType, data=Forestcover_train)
summary(Forestcover_lda)



Forestcover_pred=predict(Forestcover_lda, Forestcover_test)
table(Forestcover_pred$class, Forestcover_test$Cover_Type) #confusion matrix
mean(Forestcover_pred$class==Forestcover_test$Cover_Type) #accuracy



Forestcover_lda1=lda(Cover_Type ~ Elevation + HD.Hydro + HD.Road + HD.Fire, data=Forestcover_train)
Forestcover_lda1



Forestcover_pred1=predict(Forestcover_lda1, Forestcover_test)
table(Forestcover_pred1$class, Forestcover_test$Cover_Type) #confusion matrix
mean(Forestcover_pred1$class==Forestcover_test$Cover_Type) #accuracy



library("e1071")
library(caret)
#Try different values of gamma and cost to determine good
forestcover_svmfit <- svm(Cover_Type ~., data=Forestcover_train, kernel='radial', gamma=0.1, cost=10, scale=T)



#The radial svm models with scaled data performed perfectly, and they trained quickly, so that is the final model

# The cost function makes the decision boundary tighter around the training data, which can cause overfitting
# For this model, I iterated between building the model and evaluating it on training and testing data until a 
# balance was achieved

forestcover_svmfit <- svm(Cover_Type ~., data=Forestcover_train, kernel='radial', gamma=0.5, cost=10, scale=T)
confusionMatrix(data=predict(forestcover_svmfit, Forestcover_train), Forestcover_train$Cover_Type)



#Prediction
svm.pred <- predict(forestcover_svmfit, Forestcover_test)
confusionMatrix(data=svm.pred, Forestcover_test$Cover_Type)



Forestcover_svmfit1=svm(Cover_Type ~ Elevation + HD.Hydro + HD.Road + HD.Fire + soilType,
                        data=Forestcover_train, kernel='radial', cost=10, gamma=0.5, scale=T)
confusionMatrix(data=predict(Forestcover_svmfit1, Forestcover_train), Forestcover_train$Cover_Type)
Forestcover_svmfit1



svm.pred1 <- predict(Forestcover_svmfit1, Forestcover_test)
confusionMatrix(data=svm.pred1, Forestcover_test$Cover_Type)



