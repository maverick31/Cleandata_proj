run_analysis <- function()
{ 
  Ftr=read.table("features.txt",sep="")   
  Ftr<-Ftr[,2]
  feat <- grep("*-mean|-std*", Ftr)
  
  X_train = read.table("X_train.txt", sep="",col.names=c(Ftr))
  y_train<-read.table("y_train.txt")
  subject_train<-read.table("subject_train.txt") 
  Xtrain1<-X_train[,feat]
  
  Train<-cbind(subject_train,y_train,Xtrain1)
  Train1<-Train
  
  colnames(Train1)<-gsub("Acc.","Acceleration",colnames(Train1))
  colnames(Train1)<-gsub("Gyro","Gyroscope",colnames(Train1))
  colnames(Train1)<-gsub("Mag","Magnitude",colnames(Train1))
  colnames(Train1)<-gsub("^f","Frequency",colnames(Train1))
  colnames(Train1)<-gsub("^t","Time",colnames(Train1))
  colnames(Train1)[1]<-"Subject"
  colnames(Train1)[2]<-"Activity"
  
  t<-ddply(Train1, .(Subject,Activity), numcolwise(mean))
  
  
  
  X_test = read.table("X_test.txt", sep="",col.names=c(Ftr))
  y_test<-read.table("y_test.txt")
  subject_test<-read.table("subject_test.txt") 
  Xtest1<-X_test[,feat]
  
  Test<-cbind(subject_test,y_test,Xtest1)
  Test1<-Test
  
  colnames(Test1)<-gsub("Acc.","Acceleration",colnames(Test1))
  colnames(Test1)<-gsub("Gyro","Gyroscope",colnames(Test1))
  colnames(Test1)<-gsub("Mag","Magnitude",colnames(Test1))
  colnames(Test1)<-gsub("^f","Frequency",colnames(Test1))
  colnames(Test1)<-gsub("^t","Time",colnames(Test1))
  colnames(Test1)[1]<-"Subject"
  colnames(Test1)[2]<-"Activity"
  
  m<-ddply(Test1, .(Subject,Activity), numcolwise(mean))
  
  
  d<-merge(m,t,by = intersect(names(m), names(t)),all=TRUE)
  colnames(d)<-names(Test1)
  d
}