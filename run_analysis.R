download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              "dataset.zip")
  data = read.table('C:/Users/Mariela/Desktop/Jade tareas/Curso Cienciade Datos/R/datasciencecoursera/UCI HAR Dataset/features.txt')
  feature_vector<-data[,"V2"]
  x_train<-read.table('C:/Users/Mariela/Desktop/Jade tareas/Curso Cienciade Datos/R/datasciencecoursera/UCI HAR Dataset/train/X_train.txt',
                      col.names=feature_vector,check.names=FALSE)
  y_train<-read.table('C:/Users/Mariela/Desktop/Jade tareas/Curso Cienciade Datos/R/datasciencecoursera/UCI HAR Dataset/train/y_train.txt',
                      col.names="Activity")
  subject_train<-read.table('C:/Users/Mariela/Desktop/Jade tareas/Curso Cienciade Datos/R/datasciencecoursera/UCI HAR Dataset/train/subject_train.txt',
                      col.names="Subject")
  
  x_test<-read.table('C:/Users/Mariela/Desktop/Jade tareas/Curso Cienciade Datos/R/datasciencecoursera/UCI HAR Dataset/test/X_test.txt',
                     col.names=feature_vector,check.names=FALSE)
  y_test<-read.table('C:/Users/Mariela/Desktop/Jade tareas/Curso Cienciade Datos/R/datasciencecoursera/UCI HAR Dataset/test/y_test.txt',
                      col.names="Activity")
  subject_test<-read.table('C:/Users/Mariela/Desktop/Jade tareas/Curso Cienciade Datos/R/datasciencecoursera/UCI HAR Dataset/test/subject_test.txt',
                            col.names="Subject")
  
  df<-rbind.data.frame(x_train,x_test)
  activity<-rbind.data.frame(y_train,y_test)
  subject<-rbind.data.frame(subject_train,subject_test)
  
  df1<-df[,grep("mean\\(\\)",names(df),perl=TRUE)]
  df2<-df[,grep("std\\(\\)",names(df),perl=TRUE)]
  
  final_df<-cbind(df1,df2)
  final_df<-final_df[,order(names(final_df))]
  
  final_df<-cbind(activity,subject,final_df)
  
  names(final_df)<-gsub("BodyBody","Body",names(final_df),fixed=TRUE)
  
  final_df$Activity[which(final_df$Activity==1)]="WALKING"
  final_df$Activity[which(final_df$Activity==2)]="WALKING_UPSTAIRS"
  final_df$Activity[which(final_df$Activity==3)]="WALKING_DOWNSTAIRS"
  final_df$Activity[which(final_df$Activity==4)]="SITTING"
  final_df$Activity[which(final_df$Activity==5)]="STANDING"
  final_df$Activity[which(final_df$Activity==6)]="LAYING"
  
  library(plyr)
  library(dplyr)
  
  final_df<-final_df[order(final_df$Subject,final_df$Activity),]
  
  means<-suppressWarnings(aggregate(final_df,by=list(final_df$Activity,final_df$Subject),
                                    function(x) mean(as.numeric(as.character(x)))))
  
  means<-subset(means,select=-c(Activity,Subject))
  names(means)[names(means)=="Group.1"]<-"Activity"
  names(means)[names(means)=="Group.2"]<-"Subject"
  
  means<-means%>% select(Subject,everything())
  
  write.table(means,file="tidy_data.txt",row.names = FALSE)