i=1
j=5

train.years=1996:2000+i-1
test.years=2000+i

print(i)

train.labels=head(which((years>=train.years[1] & months >8)),1):tail(which(years<=train.years[5]+1 & months <6),1)
test.labels=which((years==test.years & months>8) | (years==test.years+1 & months < 6))


train.rows=which(date.ind%in%train.labels)
test.rows=which(date.ind%in%test.labels)

train.nn[i]=length(train.rows)
test.nn[i]=length(test.rows)

train.rows.mon=train.rows[(all.months[train.rows]==j)] #colin what is a prettier way to do this? 
test.rows.mon=test.rows[(all.months[train.rows]==j)]


create.wt<-function(train.rows.mon){
  rain.rows=which(ptype[train.rows.mon]=="RA")
  snow.rows=which(ptype[train.rows.mon]=="SN")
  pellet.rows=which(ptype[train.rows.mon]=="IP")
  ice.rows=which(ptype[train.rows.mon]=="FZRA")
  
  r.l<-length(rain.rows)
  s.l<-length(snow.rows)
  p.l<-length(pellet.rows)
  i.l<-length(ice.rows)
  p.lengths<-c(i.l,p.l,r.l,s.l)
  p.class<-p.lengths!=0
  
  ref<-which(p.lengths==min(p.lengths[p.lengths!=0]))
  class.wts<-p.lengths[ref]/p.lengths[p.class]
  
  return(class.wts)
}

#Do any months not have a certain type of observation for their training set? 
class.type=c('FZRA', 'IP', 'RA','SN')
for(i in 1:12){
  train.years=1996:2000+i-1
  test.years=2000+i
  
  print(i)
  
  train.labels=head(which((years>=train.years[1] & months >8)),1):tail(which(years<=train.years[5]+1 & months <6),1)
  test.labels=which((years==test.years & months>8) | (years==test.years+1 & months < 6))
  
  
  train.rows=which(date.ind%in%train.labels)
  test.rows=which(date.ind%in%test.labels)
  
  train.nn[i]=length(train.rows)
  test.nn[i]=length(test.rows)
  for( j in unique(months)){
    train.rows.mon=train.rows[(all.months[train.rows]==j)] #colin what is a prettier way to do this? 
    test.rows.mon=test.rows[(all.months[train.rows]==j)]
    
    rain.rows=which(ptype[train.rows.mon]=="RA")
    snow.rows=which(ptype[train.rows.mon]=="SN")
    pellet.rows=which(ptype[train.rows.mon]=="IP")
    ice.rows=which(ptype[train.rows.mon]=="FZRA")
    
    r.l<-length(rain.rows)
    s.l<-length(snow.rows)
    p.l<-length(pellet.rows)
    i.l<-length(ice.rows)
    p.lengths.train<-c(i.l,p.l,r.l,s.l)
    issues.train<-which(p.lengths.train==0)
    if(length(issues.train)>0){print(paste('training: ', i, ' month: ', j, ' ptype: ', class.type[issues.train]))}
    
    rain.rows=which(ptype[test.rows.mon]=="RA")
    snow.rows=which(ptype[test.rows.mon]=="SN")
    pellet.rows=which(ptype[test.rows.mon]=="IP")
    ice.rows=which(ptype[test.rows.mon]=="FZRA")
    
    r.l<-length(rain.rows)
    s.l<-length(snow.rows)
    p.l<-length(pellet.rows)
    i.l<-length(ice.rows)
    p.lengths.test<-c(i.l,p.l,r.l,s.l)
    issues.test<-which(p.lengths.test==0)
    if(length(issues.test)>0){print(paste('testing: ', i, ' month: ', j, ' ptype: ', class.type[issues.test]))}
    
  }
}