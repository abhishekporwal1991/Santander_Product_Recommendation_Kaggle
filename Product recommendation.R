setwd("V:/Abhishek_R/Project2_Santander_Product Recommandation")
getwd()

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

# Theme selection
my_theme = theme_bw()+
theme(axis.title=element_text(size=24),
      plot.title=element_text(size=36),
      axis.text =element_text(size=16))

my_theme_dark = theme_dark()+
theme(axis.title=element_text(size=24),
      plot.title=element_text(size=36),
      axis.text =element_text(size=16))

# Loading the data
set.seed(1)
df = fread("train_ver2.csv", nrows=-1)
str(df)

# Unique customer index
unique.id = unique(df$ncodpers)
limit.people = 3.5e5
unique.id = unique.id[sample(length(unique.id),limit.people)]
df = df[df$ncodpers %in% unique.id,]
str(df)

# Conversion of dates
df$fecha_dato = as.POSIXct(strptime(df$fecha_dato, format="%Y-%m-%d"))
df$fecha_alta = as.POSIXct(strptime(df$fecha_alta, format="%Y-%m-%d"))
unique(df$fecha_dato)

# adding 'month' variable
df$month = month(df$fecha_dato)
str(df)	

# missing value columns
sapply(df, function(x)any(is.na(x)))

# ***Data Cleaning***

# Age distribution
ggplot(data=df, aes(x=age)) +
geom_bar(alpha=0.75, fill="tomato", color="black") +
ggtitle("Age distribution") +
my_theme

#seperating the ages
df$age[df$age < 18]   = mean(df$age[(df$age >= 18) & (df$age <= 30) ], na.rm=TRUE)
df$age[df$age > 100]  = mean(df$age[(df$age >= 30) & (df$age <= 100)], na.rm=TRUE)
df$age[is.na(df$age)] = median(df$age, na.rm=TRUE)
df$age                = round(df$age)

#age.change  = df[month.id>6,.(age,month,month.id,age.diff=c(0,diff(age))),by="ncodpers"]
#age.change  = age.change[age.diff==1]
#age.change  = age.change[!duplicated(age.change$ncodpers)]
#setkey(df,ncodpers)
#df = merge(df,age.change[,.(ncodpers,birthday.month=month)],by=c("ncodpers"),all.x=TRUE,sort=FALSE)
#df$birthday.month[is.na(df$birthday.month)] = 7 # July is the only month we don't get to check for increment so if there is no update then use it
#df$age[df$birthday.month <= 7 & df$month.id<df$birthday.month] = df$age[df$birthday.month <= 7 & df$month.id<df$birthday.month]  - 1 # correct ages in the first 6 months

df$age[is.na(df$age)] <- -1


ggplot(data=df, aes(x=age)) +
geom_bar(alpha=0.75, fill="tomato", color="black") +
xlim(c(18,100)) +
ggtitle("Age distribution") +
my_theme


sum(is.na(df$ind_nuevo))

# month histroy for the customer
months.active = df[is.na(df$ind_nuevo),] %>%
	group_by(ncodpers) %>%
	summarise(months.active=n()) %>%
	select(months.active)

max(months.active)

# as all the customers added only 6 month earlier
df$ind_nuevo[is.na(df$ind_nuevo)] = 1

sum(is.na(df$antiguedad))

# same customer that have missing value, also have missing seniority...Check
summary(df[is.na(df$antiguedad),]%>% select(ind_nuevo))

# Same customer, so provide the minimun seniority
df$antiguedad[is.na(df$antiguedad)] = min(df$antiguedad, na.rm=TRUE)
summary(df$antiguedad)

df$antiguedad[df$antiguedad < 0] = 0
summary(df$antiguedad)

# Filling the joining date (fecha_alta)
df$fecha_alta[is.na(df$fecha_alta)] = median(df$fecha_alta, na.rm=TRUE)
sapply(df, function(x)any(is.na(x)))

# indrel
table(df$indrel)

# filling the missing value with more common data
df$indrel[is.na(df$indrel)] = 1
sapply(df, function(x)any(is.na(x)))

# Addr type (tipodom) & Province code not required (cod_prov)
df = df %>% select(-tipodom, -cod_prov)
str(df)

# ind_actividad_cliente
sum(is.na(df$ind_actividad_cliente))

df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] = median(df$ind_actividad_cliente, na.rm=TRUE)
sapply(df, function(x)any(is.na(x)))

unique(df$nomprov)

df$nomprov[df$nomprov == ""] = "UNKNOWN"

# renta
sum(is.na(df$renta))

# seperate the gross income as per province
df %>%
	filter(!is.na(renta)) %>%
	group_by(nomprov)     %>%
	summarise(med.income = median(renta)) %>%
	arrange(med.income)   %>%
	mutate(city=factor(nomprov, levels=nomprov)) %>%
	ggplot(aes(x=city, y=med.income)) +
	geom_point(color="Red")         +
	guides(color=FALSE)               +
	xlab("City")                      +
	ylab("Median Income")             +
	my_theme                          +
	theme(axis.text.x=element_blank(), axis.ticks=element_blank()) +
	geom_text(aes(x=city, y=med.income, label=city), angle=90, hjust = -0.25) +
	theme(panel.background=element_rect(fill="Yellow")) +
	ylim(c(50000,200000))             +
	ggtitle("Income distribution by city")

new.incomes = df %>%
	select(nomprov) %>%
	merge(df %>%
	group_by(nomprov) %>%
	summarise(med.income=median(renta, na.rm=TRUE)), by="nomprov") %>%
	select(nomprov, med.income) %>%
	arrange(nomprov)

df = arrange(df,nomprov)
df$renta[is.na(df$renta)] = new.incomes$med.income[is.na(df$renta)]
rm(new.incomes)

df$renta[is.na(df$renta)] = median(df$renta, na.rm=TRUE)
df = arrange(df, fecha_dato)
sapply(df, function(x)any(is.na(x)))

sum(is.na(df$ind_nomina_ult1))
sum(is.na(df$ind_nom_pens_ult1))

# Filling with 0 as it is small in numbersss
df[is.na(df)] = 0
sapply(df, function(x)any(is.na(x)))

# Finding empty strings in char vectors
char.col = names(df)[sapply(df, is.character)]
char.col

for(name in char.col){
	print(sprintf("Unique values for %s:", name))
	print(unique(df[[name]]))
	cat('\n')
	}

# Fill missing value with most common observation
# indfall
table(df$indfall)
df$indfall[df$indfall == ""] = "N"
table(df$indfall)

# tiprel_1mes
table(df$tiprel_1mes)
df$tiprel_1mes[df$tiprel_1mes == ""] = "A"
table(df$tiprel_1mes)

# indrel_1mes
table(df$indrel_1mes)
df$indrel_1mes[df$indrel_1mes == ""] = "1"
table(df$indrel_1mes)

df$indrel_1mes[df$indrel_1mes == "P"] = "5"    # converting to the number, removing letter
table(df$indrel_1mes)

df$indrel_1mes = as.factor(as.integer(df$indrel_1mes))
table(df$indrel_1mes)

# rest with "UNKNOWN"
# ind_empleado
table(df$ind_empleado)
df$ind_empleado[df$ind_empleado == ""] = "UNKNOWN"
table(df$ind_empleado)

# pais_residencia
table(df$pais_residencia)
df$pais_residencia[df$pais_residencia == ""] = "UNKNOWN"
table(df$pais_residencia)

# sexo
table(df$sexo)
df$sexo[df$sexo == ""] = "UNKNOWN"
table(df$sexo)

# ult_fec_cli_1t
table(df$ult_fec_cli_1t)
df$ult_fec_cli_1t[df$ult_fec_cli_1t == ""] = "UNKNOWN"
table(df$ult_fec_cli_1t)

# indext
table(df$indext)
df$indext[df$indext == ""] = "UNKNOWN"
table(df$indext)

# indresi
table(df$indresi)
df$indresi[df$indresi == ""] = "UNKNOWN"
table(df$indresi)

# conyuemp
table(df$conyuemp)
df$conyuemp[df$conyuemp== ""] = "UNKNOWN"
table(df$conyuemp)

# segmento
table(df$segmento)
df$segmento[df$segmento == ""] = "UNKNOWN"
table(df$segmento)

# converting features into muneric vectors
features = grepl("ind_+.*ult.*", names(df))
df[,features] = lapply(df[,features], function(x)as.integer(round(x)))
str(df)
df$total.services = rowSums(df[,features], na.rm=TRUE)
str(df)

# Assigning numeric id to each time stamp
df = df %>% arrange(fecha_dato)
df$month.id = as.numeric(factor(df$fecha_dato))
str(df)
df$month.next.id = df$month.id + 1
df$month.previous.id = df$month.id - 1
str(df)

# Lag feature
create.lag.feature <- function(dt, # should be a data.table!
                               feature.name, # name of the feature to lag
                               months.to.lag=1,# vector of integers indicating how many months to lag
                               by=c("ncodpers","month.id"), # keys to join data.tables by
                               na.fill = NA)  
  {
  # get the feature and change the name to avoid .x and .y being appending to names
  dt.sub <- dt[,mget(c(by,feature.name))]
  names(dt.sub)[names(dt.sub) == feature.name] <- "original.feature"
  original.month.id <- dt.sub$month.id
  added.names <- c()
  for (month.ago in months.to.lag){
    print(paste("Collecting information on",feature.name,month.ago,"month(s) ago"))
    colname <- paste("lagged.",feature.name,".",month.ago,"months.ago",sep="")
    added.names <- c(colname,added.names)
    # This is a self join except the month is shifted
    dt.sub <- merge(dt.sub,
                    dt.sub[,.(ncodpers,
                              month.id=month.ago+original.month.id,
                              lagged.feature=original.feature)],
                    by=by,
                    all.x=TRUE,
                    sort=FALSE)
    names(dt.sub)[names(dt.sub)=="lagged.feature"] <- colname
    # dt.sub[[colname]][is.na(dt.sub[[colname]])] <- dt.sub[["original.feature"]][is.na(dt.sub[[colname]])]
  }
  df <- merge(dt,
              dt.sub[,c(by,added.names),with=FALSE],
              by=by,
              all.x=TRUE,
              sort=FALSE)
  df[is.na(df)] <- na.fill
  return(df)
}

df = as.data.table(df)
df = create.lag.feature(df,'ind_actividad_cliente',1:11, na.fill=0)


# discountinuation of juniour accounts at the age of 20...check
df[,last.age:=lag(age),by="ncodpers"]
df$turned.adult = ifelse(df$age==20 & df$last.age==19,1,0)
df = as.data.frame(df)

features = names(df)[grepl("ind_+.*ult.*", names(df))]

test = df %>%
	filter(month.id==max(df$month.id))

df = df %>%
	filter(month.id==max(df$month.id))

write.csv(df,"cleaned_train.csv", row.names=FALSE)
write.csv(test,"cleaned_test.csv", row.names=FALSE)


# Function to understand the customer behaviour
status.change = function(x){
	if(length(x) == 1){                       # if only one entry then new customer; label=Added
	label = ifelse(x==1,"Added", "Maintained")
	} else {
	  diffs = diff(x)                         # diff month by month
	  diffs = c(0,diffs)                      # First occurrance is considered as "Maintained"
	  label = rep("Maintained", length(x))
	  label = ifelse(diffs==1, "Added",
				ifelse(diffs == -1,"Dropped", "Maintained"))
	}
	label
}

# Apply the function to features
df[,features] = lapply(df[,features], function(x) return(ave(x, df$ncodpers, FUN = status.change)))

interesting = rowSums(df[,features] != "Maintained")
df = df[interesting>0,]
df = df %>%
		gather(key=feature,
		value=status,
		ind_ahor_fin_ult1:ind_recibo_ult1)
df = filter(df, status != "Maintained")
head(df)


# *** Data Visualization ***
totals.by.feature = df %>%
	group_by(month,feature) %>%
	summarise(counts=n())

df %>%
	group_by(month,feature,status) %>%
	summarise(counts=n()) %>%
	ungroup() %>%
	inner_join(totals.by.feature, by=c("month","feature")) %>%
	mutate(counts=counts.x/counts.y) %>%
	ggplot(aes(y=counts, x=factor(month.abb[month], levels = month.abb[seq(12,1,-1)]))) +
	geom_bar(aes(fill=status), stat="identity") +
	facet_wrap(facets=~feature, ncol=6) +
	coord_flip()  +
	my_theme_dark +
	ylab("Count") +
	xlab("") +
	ylim(limits=c(0,1)) +
	ggtitle("Relative Service \nChanges by Month") +
	theme(axis.text   = element_text(size=7),
		legend.text = element_text(size=10),
		legend.title= element_blank()      ,
		strip.text  = element_text(face="bold")) 
	scale_fill_manual(values=c("cyan", "magenta"))

month.counts = table(unique(df$month.id)%%12)
cur.names = names(month.counts)
cur.names[cur.names == "0"] = "12"
names(month.counts) = cur.names
month.counts = data.frame(month.counts) %>%
	rename(month=Var1, month.count=Freq) %>% mutate(month=as.numeric(month))

# Average service changes by month
df %>%
	group_by(month,feature,status) %>%
	summarise(counts=n()) %>%
	ungroup() %>%
	inner_join(month.counts, by="month") %>%
	
	mutate(counts=counts/month.count) %>%
	ggplot(aes(y=counts, x=factor(month.abb[month], levels = month.abb[seq(12,1,-1)]))) +
	geom_bar(aes(fill=status), stat="identity") +
	facet_wrap(facets=~feature, ncol=6) +
	coord_flip()  +
	my_theme_dark +
	ylab("Count") +
	xlab("") +
	ylim(limits=c(0,1)) +
	ggtitle("Average Service \nChanges by Month") +
	theme(axis.text   = element_text(size=7),
		legend.text = element_text(size=10),
		legend.title= element_blank()      ,
		strip.text  = element_text(face="bold")) 
	scale_fill_manual(values=c("cyan", "magenta"))

# Service changes by Gender
df %>%
	filter(sexo!= "UNKNOWN") %>%
	ggplot(aes(x=sexo)) +
	geom_bar(aes(fill=status)) +
	facet_wrap(facets=~feature,ncol=6) +
	my_theme_dark +
	ylab("Count") +
	xlab("") +
	ggtitle("Service change by Gender")+
	theme(axis.text    = element_text(size=7) ,
		legend.text  = element_text(size=14),
		legend.title = element_blank()      ,
		strip.text   = element_text(face="bold")) +
	scale_fill_manual(values=c("cyan", "magenta"))

# Normalized service change by Gender
tot.H = sum(df$sexo == "H")
tot.V = sum(df$sexo == "V")

tmp.df = df %>%
	group_by(sexo,status) %>%
	summarise(counts = n())

tmp.df$counts[tmp.df$sexo=="H"] = tmp.df$counts[tmp.df$sexo=="H"] / tot.H
tmp.df$counts[tmp.df$sexo=="V"] = tmp.df$counts[tmp.df$sexo=="V"] / tot.V

tmp.df %>%
	filter(sexo!= "UNKNOWN") %>%
	ggplot(aes(x=factor(feature),y=counts)) +
	geom_bar(aes(fill=status,sexo), stat='identity') +
	coord_flip() +
	my_theme_dark +
	ylab("Ratio") +
	xlab("") +
	ggtitle(" Normalized Service \n Changes by Gender")+
	theme(axis.text    = element_text(size=20) ,
		legend.text  = element_text(size=14),
		legend.title = element_blank()      ,
		strip.text   = element_text(face="bold")) +
	scale_fill_manual(values=c("cyan", "magenta"))

rm(tmp.df)

# Normalized service change by New Status
tot.new     <- sum(df$ind_nuevo==1)
tot.not.new <- sum(df$ind_nuevo!=1)

tmp.df      <- df %>%
  group_by(ind_nuevo,status) %>%
  summarise(counts=n())

tmp.df$counts[tmp.df$ind_nuevo==1] = tmp.df$counts[tmp.df$ind_nuevo==1] / tot.new
tmp.df$counts[tmp.df$ind_nuevo!=1] = tmp.df$counts[tmp.df$ind_nuevo!=1] / tot.not.new

tmp.df %>%
  ggplot(aes(x=factor(feature),y=counts)) +
  geom_bar(aes(fill=status,factor(ind_nuevo)),stat='identity') +
  coord_flip() +
  my_theme_dark + 
  ylab("Count") +
  xlab("") +
  ggtitle("Normalized Service \n Changes by New Status") +
  theme(axis.text    = element_text(size=10),
        legend.text  = element_text(size=14),
        legend.title = element_blank()      ,
        strip.text   = element_text(face="bold")) +
  scale_fill_manual(values=c("cyan","magenta"))

rm(tmp.df)

# Service changes by City
df %>%
	group_by(nomprov,status) %>%
	summarise(y=mean(total.services)) %>%
	ggplot(aes(x=factor(nomprov, levels=sort(unique(nomprov),decreasing=TRUE)),y=y)) +
	geom_bar(stat="identity", aes(fill=status)) +
	geom_text(aes(label=nomprov),y=0.2,hjust=0,angle=0,size=3,color="Pink") +
	coord_flip() +
	my_theme_dark +
	xlab("City") +
	ylab("Total # Changes") +
	ggtitle("Service Changes \n by City") +
	theme(axis.text    = element_blank(),
		legend.text  = element_text(size=14),
		legend.title = element_text(size=18)) +
	scale_fill_manual(values=c("cyan","magenta"))

# Service change by Seniority
df %>%
  group_by(antiguedad,status) %>%
  summarise(counts=n()) %>%
  ggplot(aes(x=factor(antiguedad),y=log(counts))) +
  geom_point(alpha=0.6,aes(color=status)) +
  my_theme_dark +
  xlab("Seniority (Months)") +
  ylab("Total # Changes") + 
  ggtitle("Service Changes \n by Seniority") +
  theme(axis.text    = element_blank(),
        legend.text  = element_text(size=14),
        legend.title = element_text(size=18)) +
  scale_color_manual(values=c("cyan","magenta"))

# Income vs Age
#df %>%
 # ggplot(aes(x=age,y=log(renta))) +
  #geom_point(alpha=0.5,aes(color=status)) +
  #my_theme_dark +
  #xlab("Age") +
  #ylab("Income (log scale)") + 
  #ggtitle("Income vs. Age") +
  #theme(
  #      legend.text  = element_text(size=14),
  #      legend.title = element_text(size=18)) +
  #scale_color_manual(values=c("cyan","magenta"))

# Seniority vs. Age
df %>%
  group_by(ncodpers) %>%
  summarise(age=max(age),seniority=max(antiguedad)) %>%
  select(age,seniority) %>%
  ggplot(aes(x=age,y=seniority)) +
  geom_point(alpha=0.4) +
  ggtitle("Seniority vs. Age") + 
  my_theme


# *** Feature Engineering***

# list of purchased products
library(data.table)
df     = fread("cleaned_train.csv")
labels = names(df)[grepl("ind_+.*_+ult", names(df))]
cols   = c("ncodpers", "month.id", "month.previous.id", labels)
df     = df[,names(df) %in% cols, with=FALSE]
str(df)

# connect each month to the previous one
df = merge(df,df, by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers", "month.id"),all.x=TRUE)

df[is.na(df)] = 0

products = rep("",nrow(df))
for(label in labels){
	colx = paste0(label,".x")
	coly = paste0(label,".y")
	diffs = df[,(get(colx)-get(coly))]
	products[diffs>0] = paste0(products[diffs>0], label, sep="")
}

# write results
df = df[,.(ncodpers,month.id,products)]
write.csv(df,"purchased-products.csv", row.names=FALSE)


## feature-purchase-frequency.R
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",labels)
df     <- df[,names(df) %in% cols,with=FALSE]
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)

df[is.na(df)] <- 0
products <- rep("",nrow(df))
num.transactions = rep(0,nrow(df))
purchase.frequencies = data.frame(ncodpers=df$ncodpers, month.id=(df$month.previous.id + 2))

for(label in labels){
	colx = paste0(label,".x")
	coly = paste0(label,".y")
	diffs = df[,.(ncodpers,month.id,change=get(colx)-get(coly))]
	
	num.transactions = num.transactions + as.integer(diffs$change!=0)
	diffs[diffs<0] = 0

	setkey(diffs,ncodpers)
	d = diffs[,.(frequency = cumsum(change)), by=ncodpers]
	purchase.frequencies[[paste(label,"_purchase.count",sep="")]] = d$frequency	
}

purchase.frequencies$num.transactions = num.transactions
purchase.frequencies = purchase.frequencies %>%
	dplyr::group_by(ncodpers) %>%
	dplyr::mutate(num.transactions = cumsum(num.transactions))

write.csv(purchase.frequencies,"purchase.frequencies.csv", row.names = FALSE)


# Function:how many months have passed since the product was last owned
months.since.owned = function(dt,products,months.to.search,default.value=999){
	for(product in products){
		print(paste("Finding months since owning", product))
		colname = paste(product,".last.owned", sep="")
		dt[[colname]] = default.value
	 for(month.ago in seq(months.to.search,1,-1)){
		cur.colname = paste(product,"_",month.ago,"month_ago", sep="")
		dt[[colname]][dt[[colname]]==1] = month.ago
	 }
	 }
return(dt)
}

# engineer-features.R

library(tidyr)
library(xgboost)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(caret)
library(pROC)
library(lubridate)
library(fasttime)

set.seed(1)
# Train on month 5 and 11 and validate on 17 for CV data then
# train on month 6 and 12 and predict on test. The second months are separated
# into a separate variable
val.train.month = 5
val.test.month = 17
train.month     = 6
extra.train.months.val = c(11)
extra.train.months.test = c(12)

months.to.keep = c(val.train.month,val.test.month,train.month,extra.train.months.val,extra.train.months.test)

df   = fread("cleaned_train.csv")
test = fread("cleaned_test.csv")

# add activity index previous month
recent.activity.index = merge(rbind(df[,.(ncodpers, month.id, ind_actividad_cliente,segmento)],
					   test[,.(ncodpers, month.id, ind_actividad_cliente, segmento)]),
				   df[,.(ncodpers,month.id=month.id+1,
					   old.ind_actividad_cliente=ind_actividad_cliente,
					   old.segmento=segmento)],
				   by=c("ncodpers", "month.id"),
				   sort = FALSE)

recent.activity.index[,activity.index.change:= ind_actividad_cliente - old.ind_actividad_cliente]
recent.activity.index[,segmento.change := as.integer(segmento!= old.segmento)]

df = merge(df, recent.activity.index[, .(ncodpers, month.id, old.ind_actividad_cliente, activity.index.change,
						 old.segmento, segmento.change)],
	    by=c("ncodpers", "month.id"),all.x=TRUE)

test = merge(test, recent.activity.index[, .(ncodpers, month.id, old.ind_actividad_cliente, activity.index.change,
						 old.segmento, segmento.change)],
	    by=c("ncodpers", "month.id"),all.x=TRUE)

df$old.segmento[is.na(df$old.segmento)] = df$segmento[is.na(df$old.segmento)]
df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] = df$old.ind_actividad_cliente[is.na(df$ind_actividad_cliente)]

df[is.na(df)] = 0

products = names(df)[grepl("ind_+.*_+ult", names(df))]

# create a data frame with just the product ownership variables
products.owned = df %>%
	select(ncodpers, month.id, one_of(products)) %>%
	as.data.table()

df = as.data.table(df)
test = as.data.table(df)
original.month.id = products.owned$month.id
df = df[month.id %in% months.to.keep,]

test = test[, !names(test) %in% products, with=FALSE]

# create features indicating whether or not a product was owned in each of the past
for(month.ago in 1:11){
	print(paste("Collecting data on product ownership", month.ago, "months ago..."))
	products.owned[, month.id:=original.month.id + month.ago]
	df = merge(df, products.owned, by=c("ncodpers", "month.id"), all.x=TRUE)
	change.names = names(df)[grepl("\\.y", names(df))]
	new.names = gsub("\\.y", paste("_",month.ago, "month_ago", sep=""), change.names)
	names(df)[grepl("\\.y", names(df))] =new.names

	change.names = names(df)[grepl("\\.x", names(df))]
	new.names = gsub("\\.x", "", change.names)
	names(df)[grepl("\\.x", names(df))] =new.names

test = merge(test, products.owned, by=c("ncodpers", "month.id"), all.x=TRUE)
	change.names = names(test)[grepl("\\.y", names(test))]
	new.names = gsub("\\.y", paste("_",month.ago, "month_ago", sep=""), change.names)
	names(test)[grepl("\\.y", names(test))] =new.names

	change.names = names(test)[grepl("\\.x", names(test))]
	new.names = gsub("\\.x", "", change.names)
	names(test)[grepl("\\.x", names(test))] =new.names
}

names(test)[names(test) %in% products] = paste(names(test)[names(test) %in% products], "_1month_ago",sep="")
df[is.na(df)] = 0
test[is.na(test)]= 0

# get the number of months since each product was owned
df = months.since.owned(df,products,12)
test = months.since.owned(test,products,12)
df <- as.data.frame(df)
test <- as.data.frame(test)

# compute total number of products owned previous month
df$total_products = rowSums(df[,names(df) %in% names(df)[grepl("ind.*1month\\_ago", names(df))]], na.rm=TRUE)
test$total_products = rowSums(test[,names(test) %in% names(test)[grepl("ind.*1month\\_ago", names(test))]], na.rm=TRUE)

# save the month id
products.owned$month.id = original.month.id

# windows of product ownership.
for(product in products){
   for(window.size in 2:6){
	print(paste("Getting ownship for", product, "within last", window.size, "months"))
	colname = paste(product, ".owned.within", window.size, "months", sep="")
	df[[colname]] = 0
	test[[colname]] = 0

	for(month.ago in 1:window.size){
	 current.col = paste(product,"_",month.ago,"month_ago", sep="")
	 df[[colname]] = df[[colname]] + df[[current.col]]
	 test[[colname]] = test[[colname]] + test[[current.col]]
	}
	df[[colname]] = as.integer(df[[colname]] > 0)
	test[[colname]] = as.integer(test[[colname]] > 0)
   }
}

# add in purchase frequency feature for each product
purchase.frequencies <- fread("purchase.frequencies.csv")

df = merge(df, purchase.frequencies, by=c("month.id", "ncodpers"), all.x=TRUE)
test = merge(test, purchase.frequencies, by=c("month.id", "ncodpers"), all.x=TRUE)
df[is.na(df)] = 0
test[is.na(test)] = 0

# fix some rare value that was causing an error
df$sexo[df$sexo=="UNKNOWN"] = "V"
test$sexo[test$sexo=="UNKNOWN"] = "V"

# append "_target" to identify target variable
new.names = names(df)
new.names[new.names %in% products] = paste(new.names[new.names %in% products], "_target", sep="")
names(df) = new.names

labels = names(df)[grepl(".*_target", names(df))]
purchase.w = names(df)[grepl(".*.count", names(df))]

ownership.names = names(df)[grepl("month\\_ago", names(df))]

# causing errors with factors
test$ind_empleado[test$ind_empleado=="S"] = "N" 
char.cols = names(test)[sapply(test,is.character)]
test[,char.cols] = lapply(test[,char.cols], as.factor)

df$ind_empleado[df$ind_empleado=="S"] = "N"
char.cols = names(df)[sapply(df,is.character)]
df[,char.cols] = lapply(df[,char.cols], as.factor)

# force the factor levels to be the same 
factor.cols = names(test)[sapply(test,is.factor)]
for (col in factor.cols){
  df[[col]] = factor(df[[col]],levels=levels(test[[col]]))
}
df$ult_fec_cli_1t[is.na(df$ult_fec_cli_1t)] <- "UNKNOWN"

# only keep entries where customers purchased products and the month matches one of our sets
purchased <- as.data.frame(fread("purchased-products.csv"))
ids.val.train   <- purchased$ncodpers[purchased$month.id %in% val.train.month & (purchased$products!="")]
ids.val.test    <- purchased$ncodpers[purchased$month.id %in% val.test.month & (purchased$products!="")]
ids.train       <- purchased$ncodpers[purchased$month.id %in% train.month & (purchased$products!="")]

extra.train.ids.val <- purchased$ncodpers[purchased$month.id %in% extra.train.months.val & (purchased$products!="")]
extra.train.ids.test <- purchased$ncodpers[purchased$month.id %in% extra.train.months.test & (purchased$products!="")]

# convert the birthday month feature to a named factor
#df$birthday.month   <- factor(month.abb[df$birthday.month],levels=month.abb)
#test$birthday.month <- factor(month.abb[test$birthday.month],levels=month.abb)

df$month   = factor(month.abb[df$month],levels=month.abb)
test$month = factor(month.abb[test$month],levels=month.abb)

# discard some columns that are no longer useful
df = select(df,-fecha_alta,-fecha_dato,-month.previous.id)

# separate the data into the various parts
extra.train.val = df %>% 
  filter(ncodpers %in% extra.train.ids.val & month.id %in% extra.train.months.val)

extra.train.test = df %>% 
  filter(ncodpers %in% extra.train.ids.test & month.id %in% extra.train.months.test)

val.train = df %>% 
  filter(ncodpers %in% ids.val.train & month.id %in% val.train.month)

val.test = df %>% 
  filter(ncodpers %in% ids.val.test & month.id %in% val.test.month) 

df = df %>% 
  filter(ncodpers %in% ids.train & month.id %in% train.month) 

test = test %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-month.previous.id) 

# save as binary for faster loading
save(df,test,val.train,val.test,extra.train.val,extra.train.test,file="data_prepped.RData")


# model_xgboost_singleclass_ajp_best.R
library(tidyr)
library(xgboost)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(caret)
library(pROC)
library(lubridate)

set.seed(1)
use.resampling.weights <- FALSE
use.many.seeds         <- TRUE
if (use.many.seeds){
  rand.seeds <- 1:10
} else{
  rand.seeds <- 1
}

load("data_prepped.RData")
use.extra.train.FLAG = TRUE
if (use.extra.train.FLAG){
  val.train <- rbind(val.train,extra.train.val)
  df       <- rbind(df,extra.train.test)
}

#purchase.count <- fread("purchase-count.csv")
#df   <- merge(df,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
##test <- merge(test,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
#val.train   <- merge(val.train,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
#val.test <- merge(val.test,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
#rm(purchase.count)

factor.cols <- names(test)[sapply(test,is.factor)]
for (col in factor.cols){
  df[[col]] <- factor(df[[col]],levels=union(levels(df[[col]]),levels(test[[col]])))
  val.train[[col]] <- factor(val.train[[col]],levels=union(levels(val.train[[col]]),levels(val.test[[col]])))
}

# there's a bunch of features related to the products, and thus they have similar
# names. Separate them out to keep things straight
labels               <- names(df)[grepl(".*_target",names(df)) & !grepl("ahor|aval",names(df))] # target values
purchase.w           <- names(df)[grepl(".*.count",names(df))] # number of times a product has been bought in the past 5 months
ownership.names      <- names(df)[grepl("month\\_ago",names(df)) & !grepl("month\\.previous",names(df))] # various features indicating whether or not a product was owned X months ago
drop.names           <- names(df)[grepl("dropped",names(df))] # various features indicating whether or not a product was owned X months ago
add.names            <- names(df)[grepl("added",names(df))] # various features indicating whether or not a product was owned X months ago
num.added.names      <- names(df)[grepl("num\\.added",names(df))]  # total number of products added X months ago
num.purchases.names  <- names(df)[grepl("num\\.purchases",names(df))]  # total number of products added X months ago
total.products.names <- names(df)[grepl("total\\.products",names(df))]  # total number of products owned X months ago
owned.within.names   <- names(df)[grepl("owned\\.within",names(df))]  # whether or not each product was owned with X months

# numeric features to use
numeric.cols <- c("age",
                  "renta",
                  "antiguedad",
                  purchase.w,
                  "total_products",
                  "num.transactions",
                  num.purchases.names)

categorical.cols <- c("sexo",
                      "ind_nuevo",
                      "ind_empleado",
                      "segmento",
                      "nomprov",
                      "indext",
                      "indresi",
                      "indrel",
                      "tiprel_1mes",
                      ownership.names,
                      owned.within.names,
                      "segmento.change",
                      "activity.index.change",
                      "ind_actividad_cliente",
                      "month"),
                      "birthday.month")

#one-hot encode the categorical features
ohe <- dummyVars(~.,data = df[,names(df) %in% categorical.cols])
ohe <- as(data.matrix(predict(ohe,df[,names(df) %in% categorical.cols])), "dgCMatrix")
ohe.test <- dummyVars(~.,data = test[,names(test) %in% categorical.cols])
ohe.test <- as(data.matrix(predict(ohe.test,test[,names(test) %in% categorical.cols])), "dgCMatrix")
ohe.val.train <- dummyVars(~.,data = val.train[,names(val.train) %in% categorical.cols])
ohe.val.train <- as(data.matrix(predict(ohe.val.train,val.train[,names(val.train) %in% categorical.cols])), "dgCMatrix")
ohe.val.test  <- dummyVars(~.,data = val.test[,names(val.test) %in% categorical.cols])
ohe.val.test  <- as(data.matrix(predict(ohe.val.test,val.test[,names(val.test) %in% categorical.cols])), "dgCMatrix")

train.labels        <- list()
train.labels.val        <- list()
# convert labels into XGBoost's sparse matrix representation
for (label in labels){
  train.labels[[label]]     <- as(data.matrix(df[[label]]),'dgCMatrix')
  train.labels.val[[label]] <- as(data.matrix(val.train[[label]]),'dgCMatrix')
}

# remember the id's for people and months for later since all that actually goes
# into xgboost is the raw feature data
save.id       <- df$ncodpers
save.month.id <- df$month.id
save.month    <- df$month
save.id.test       <- test$ncodpers
save.month.id.test <- test$month.id
df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric.cols]))
test       <- cbind(ohe.test,data.matrix(test[,names(test) %in% numeric.cols]))

save.id.val       <- val.train$ncodpers
save.month.id.val <- val.train$month.id
save.id.test.val       <- val.test$ncodpers
save.month.id.test.val <- val.test$month.id
save.month.val    <- val.train$month
val.train         <- cbind(ohe.val.train,data.matrix(val.train[,names(val.train) %in% numeric.cols]))
val.test          <- cbind(ohe.val.test,data.matrix(val.test[,names(val.test) %in% numeric.cols]))
set.seed(1)

 use a 75/25 train/test split. The test set
# is predicted using a model trained on all of the training data
train.ind  <- createDataPartition(1:nrow(df),p=0.75)[[1]]

test.save <- test
best.map <- 0
depth <- 7
eta <- 0.05
test <- test.save
predictions         <- list()
predictions_val     <- list()
predictions_val_future     <- list()

# this function takes in training/testing data and returns predicted probabilities
build.predictions.xgboost <- function(df, test, label, label.name,depth,eta,weights){
	dtrain <- xgb.DMatrix(data = df, label=label,weight=weights)
	
	model <- xgboost(data = dtrain,
                     max.depth = depth,
                     eta = eta, nthread = 4,
                     nround = 80, 
                     subsample=0.75,
                     # colsample_bytree=0.5,
                     objective = "binary:logistic", 
                     verbose =1 ,
                     print.every.n = 10)

preds <- predict(model,test)

imp <- xgb.importance(feature_names = colnames(df),model=model)
  save(imp,file=paste("IMPORTANCE_",gsub("\\_target","",label.name),".RData",sep=""))
  print(imp)
  predictions        <- list(preds)
  names(predictions) <- paste(gsub("_target","",label.name),"_pred",sep="")
  return(predictions)
}
