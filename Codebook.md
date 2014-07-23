Codebook(DATA variable)
========================================================


Subject- denotes data obseravation from 30 different persons, each index corresponding to observed value from each person.

Activity- index represents activity as shown below.

1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

Variable index and Variable name-

"1" "Subject"
"2" "Activity"
"3" "TimeBodyAcceleration.mean...X"
"4" "TimeBodyAcceleration.mean...Y"
"5" "TimeBodyAcceleration.mean...Z"
"6" "TimeBodyAcceleration.std...X"
"7" "TimeBodyAcceleration.std...Y"
"8" "TimeBodyAcceleration.std...Z"
"9" "TimeGravityAcceleration.mean...X"
"10" "TimeGravityAcceleration.mean...Y"
"11" "TimeGravityAcceleration.mean...Z"
"12" "TimeGravityAcceleration.std...X"
"13" "TimeGravityAcceleration.std...Y"
"14" "TimeGravityAcceleration.std...Z"
"15" "TimeBodyAccelerationJerk.mean...X"
"16" "TimeBodyAccelerationJerk.mean...Y"
"17" "TimeBodyAccelerationJerk.mean...Z"
"18" "TimeBodyAccelerationJerk.std...X"
"19" "TimeBodyAccelerationJerk.std...Y"
"20" "TimeBodyAccelerationJerk.std...Z"
"21" "TimeBodyGyroscope.mean...X"
"22" "TimeBodyGyroscope.mean...Y"
"23" "TimeBodyGyroscope.mean...Z"
"24" "TimeBodyGyroscope.std...X"
"25" "TimeBodyGyroscope.std...Y"
"26" "TimeBodyGyroscope.std...Z"
"27" "TimeBodyGyroscopeJerk.mean...X"
"28" "TimeBodyGyroscopeJerk.mean...Y"
"29" "TimeBodyGyroscopeJerk.mean...Z"
"30" "TimeBodyGyroscopeJerk.std...X"
"31" "TimeBodyGyroscopeJerk.std...Y"
"32" "TimeBodyGyroscopeJerk.std...Z"
"33" "TimeBodyAccelerationMagnitude.mean.."
"34" "TimeBodyAccelerationMagnitude.std.."
"35" "TimeGravityAccelerationMagnitude.mean.."
"36" "TimeGravityAccelerationMagnitude.std.."
"37" "TimeBodyAccelerationJerkMagnitude.mean.."
"38" "TimeBodyAccelerationJerkMagnitude.std.."
"39" "TimeBodyGyroscopeMagnitude.mean.."
"40" "TimeBodyGyroscopeMagnitude.std.."
"41" "TimeBodyGyroscopeJerkMagnitude.mean.."
"42" "TimeBodyGyroscopeJerkMagnitude.std.."
"43" "FrequencyBodyAcceleration.mean...X"
"44" "FrequencyBodyAcceleration.mean...Y"
"45" "FrequencyBodyAcceleration.mean...Z"
"46" "FrequencyBodyAcceleration.std...X"
"47" "FrequencyBodyAcceleration.std...Y"
"48" "FrequencyBodyAcceleration.std...Z"
"49" "FrequencyBodyAcceleration.meanFreq...X"
"50" "FrequencyBodyAcceleration.meanFreq...Y"
"51" "FrequencyBodyAcceleration.meanFreq...Z"
"52" "FrequencyBodyAccelerationJerk.mean...X"
"53" "FrequencyBodyAccelerationJerk.mean...Y"
"54" "FrequencyBodyAccelerationJerk.mean...Z"
"55" "FrequencyBodyAccelerationJerk.std...X"
"56" "FrequencyBodyAccelerationJerk.std...Y"
"57" "FrequencyBodyAccelerationJerk.std...Z"
"58" "FrequencyBodyAccelerationJerk.meanFreq...X"
"59" "FrequencyBodyAccelerationJerk.meanFreq...Y"
"60" "FrequencyBodyAccelerationJerk.meanFreq...Z"
"61" "FrequencyBodyGyroscope.mean...X"
"62" "FrequencyBodyGyroscope.mean...Y"
"63" "FrequencyBodyGyroscope.mean...Z"
"64" "FrequencyBodyGyroscope.std...X"
"65" "FrequencyBodyGyroscope.std...Y"
"66" "FrequencyBodyGyroscope.std...Z"
"67" "FrequencyBodyGyroscope.meanFreq...X"
"68" "FrequencyBodyGyroscope.meanFreq...Y"
"69" "FrequencyBodyGyroscope.meanFreq...Z"
"70" "FrequencyBodyAccelerationMagnitude.mean.."
"71" "FrequencyBodyAccelerationMagnitude.std.."
"72" "FrequencyBodyAccelerationMagnitude.meanFreq.."
"73" "FrequencyBodyBodyAccelerationJerkMagnitude.mean.."
"74" "FrequencyBodyBodyAccelerationJerkMagnitude.std.."
"75" "FrequencyBodyBodyAccelerationJerkMagnitude.meanFreq.."
"76" "FrequencyBodyBodyGyroscopeMagnitude.mean.."
"77" "FrequencyBodyBodyGyroscopeMagnitude.std.."
"78" "FrequencyBodyBodyGyroscopeMagnitude.meanFreq.."
"79" "FrequencyBodyBodyGyroscopeJerkMagnitude.mean.."
"80" "FrequencyBodyBodyGyroscopeJerkMagnitude.std.."
"81" "FrequencyBodyBodyGyroscopeJerkMagnitude.meanFreq.."


```r
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
```
