#read the data file
x=data.frame(read.csv(file='trip3.csv',header=TRUE))

x$date=as.Date(x$dayOfVisit,format='%d-%b-%y')
x$year=as.numeric(substr(x$date,1,4))
x$month=as.numeric(substr(x$date,6,7))
x$yearmonth=as.numeric(paste0(substr(x$date,1,4),substr(x$date,6,7)))

#Table 1 & 2: Number of unique and total patients seen
#unique patients
unique.patients=data.frame(unique.patients=length(unique(x$patientId)))
write.csv(unique.patients,file='Table 1.csv')

#total patients
patients=data.frame(patients=length(x$patientId))
write.csv(patients,file='Table 2.csv')

#Table 3 & 4: Number of unique and total patients broken down by year
#unique patients
lu=function(a){length(unique(a))}
unique.patients.by.year=tapply(x$patientId,x$year,lu)
unique.patients.by.year.df=data.frame(year=names(unique.patients.by.year),unique.patients=unique.patients.by.year)
write.csv(unique.patients.by.year.df,file='Table 3.csv')

#total patients
lu=function(a){length(a)}
patients.by.year=tapply(x$patientId,x$year,lu)
patients.by.year.df=data.frame(year=names(unique.patients.by.year), patients = patients.by.year)
write.csv(patients.by.year.df,file='Table 4.csv')

#Table 5: Total number of unique patients who have been repeat patients (seen in different months)
pym=tapply(x$yearmonth,x$patientId,lu)
unique.repeat.patients=data.frame(unique.repeat.patients=sum(pym>1))
write.csv(unique.repeat.patients,file='Table 5.csv')

#Table 6: Total number of unique patients who have been repeat patients broken down by number of times seen
unique.repeat.patient.by.times.seen=summary(as.factor(pym))
unique.repeat.patient.by.times.seen.df=data.frame(times.seen=names(unique.repeat.patient.by.times.seen),unique.repeat.patients=unique.repeat.patient.by.times.seen)
write.csv(unique.repeat.patient.by.times.seen.df,file='Table 6.csv')

#generate recoding of problem field

x$problem1=paste0(tolower(x$problem),' ')

y=data.frame(read.csv(file='problem codes.csv',header=TRUE))

#adist computes the approximate distance between words in y$Value and x$problem1, producing a matrix
#with one row for each value of y$Value and one column for each x$problem1
#(partial=TRUE means that it matches the entries in y$Value to substrings in x$problem1)
a=adist(tolower(y$Value),x$problem1,partial=TRUE)
#we search for GERD, HTN, STD, STI and UTI separately because they are case sensitive and only 3 or 4 letters
if('GERD'%in%y$Value){
  a1=adist('GERD',x$problem,partial=TRUE)
  #since we don't want a match if the distance is 1, recode all distances 1 or greater to 4
  a1[a1>0]=4
  #look up row corresponding to GERD and update this row of a[,] to a1
  a[which(y$Value=='GERD'),]=a1
}
#HTN handled similarly to GERD
if('HTN'%in%y$Value){
  a1=adist('HTN',x$problem,partial=TRUE)
  a1[a1>0]=3
  a[which(y$Value=='HTN'),]=a1
}
#STD handled similarly to GERD and HTN
if('STD'%in%y$Value){
  a1=adist('STD',x$problem,partial=TRUE)
  a1[a1>0]=3
  a[which(y$Value=='STD'),]=a1
}
#STI handled similarly to GERD, HTN and STD
if('STI'%in%y$Value){
  a1=adist('STI',x$problem,partial=TRUE)
  a1[a1>0]=3
  a[which(y$Value=='STI'),]=a1
}
#UTI handled similarly to GERD, HTN, STD and STI
if('UTI'%in%y$Value){
  a1=adist('UTI',x$problem,partial=TRUE)
  a1[a1>0]=3
  a[which(y$Value=='UTI'),]=a1
}
#we now need to select in each column of a[,] the entry to put into the recode field for that column
b=matrix(rep(0,ncol(a)*nrow(a)),ncol=ncol(a))
for(n in 1:ncol(a)){cat(n,'\n');b[,n]=c(1:nrow(y))*(a[,n]<=y$Exceptions)}
b[b==0]=nrow(y)+1
for(n in 1:nrow(x)){cat(n,'\n');x$pcode[n]=min(b[,n])}
plist1=c(as.character(y$Problem),'')
x$recode=plist1[x$pcode]
for(n in 1:nrow(x))if(x$recode[n]=='')x$recode[n]=as.character(x$problem[n])

#Table 7: for each diagnosis, break down how many patients were diagnosed with it by year and total (both patient count and unique patient count)
diagnosis=c('Hypertension','Fever','Headache','Diabetes','UTI','Malaria','Anemia','Pregnancy','Tinea', 'Vaginal infection','Child Wellness','Scabies','Dehydration')
years=as.character(levels(factor(x$year)))
o=data.frame()
for(d in diagnosis){
  xx=x[x$recode==d,]
  t=tapply(xx$patientId,xx$year,length)
  o1=data.frame(diagnosis=d,patient='Count')
  total=0
  for(nn in years){if(nn%in%names(t)) c=t[nn] else c=0;o1[[paste0('y',nn)]]=c;total=total+c}
  o1$total=total
  t=tapply(xx$patientId,xx$year,lu)
  o2=data.frame(diagnosis=d,patient='Unique')
  total=0
  for(nn in years){if(nn%in%names(t)) c=t[nn] else c=0;o2[[paste0('y',nn)]]=c;total=total+c}
  o2$total=total
  o=rbind(o,o1,o2)  
}
write.csv(o,file='Table 7.csv')

#write data file with recoded problem field
x.recoded=subset(x,select=c(1:70,77))
write.csv(x.recoded,file='recoded data file.csv')

#write data file containing diagnoses that are not in the set diagnosis=c('Hypertension','Fever','Headache','Diabetes','UTI','Malaria','Anemia')
x.other=x[!x$recode%in%diagnosis,]
x.other=subset(x.other,select=c(1:70,77))
write.csv(x.other,file='recoded data file other diagnoses.csv')

#write data file containing repeat patients only
repeat.patients=names(which(pym>1))
x.repeat=x[x$patientId%in%repeat.patients,]
x.repeat=subset(x.repeat,select=c(1:70,77))
write.csv(x.repeat,file='recoded data file repeat patients only.csv')
