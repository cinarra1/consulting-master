#connect to server 
library(PKI)
library(RJDBC)

driverClass <- "com.amazon.redshift.jdbc41.Driver"
classPath <- "/etc/jdbc/RedshiftJDBC41-1.1.7.1007.jar"

require(PKI)
key <- PKI.load.key(format="PEM", file="key.rda")
load("encryptedPW.rda")
decryptedPW <- PKI.decrypt(e, key)

user <- "nmahtani"
password <- rawToChar(decryptedPW)
database <- "deci"
port <- "5439"
url <-  "jdbc:redshift://deci.cq0tiumbbtoo.us-east-1.redshift.amazonaws.com:5439/deci"
getdatajdbc <- function(conn, query) {
  
  data <-  dbGetQuery(conn, query)
  return(data)
  dbDisconnect(conn)
}
popDataQuery <- paste0("SELECT serialno, hisp, rac1p, puma, st, agep, lanp, relp, pwgtp from public_data.acs_2014_1yr_pus")

driver <- JDBC(driverClass = driverClass,  classPath = classPath)
if(exists("conn")) {rm(conn)}
conn <- dbConnect(driver, user=user, password=password, database=database, port=port, 
                  url= url)
pop <- getdatajdbc(conn, query = popDataQuery)
dbDisconnect(conn)

pop<-as.data.table(pop)

#create puma_num variable with unique state code 
pop[,puma_num:= st*1e5+puma]

#create variables for population being mapped, in this case Hispanics 21+
pop$Hisp1 <- ifelse(pop$hisp==1,0,1) 
pop[,HispLDA:=ifelse (((Hisp1==1)&(agep>20)),1,0)]
pop[HispLDA==1,sum(pwgtp)]

# create definition for mapping
int_data<-subset(pop, select=c("puma_num", "serialno", "Hisp1", "pwgtp"))
int_data[,hispanic:=ifelse(Hisp1==1,pwgtp,0)]
int_data[Hisp1==1,sum(hispanic)]

#aggregate data by puma_num
puma_agg<-aggregate(cbind(hispanic) ~puma_num ,data=int_data,sum)
sum(puma_agg$hispanic) 

#retrieve puma to MSA raw data from http://mcdc.missouri.edu/websas/geocorr12.html or similar website
#clean data, ensure puma_num definition is equal to puma_num definition above (puma_num:= st*1e5+puma) and that allocation data is on excel

#export puma to MSA data as .csv and upload to R 
puma_to_msa<-read.csv("raw_hisp.csv", header=TRUE)

#merge aggregated puma data and .csv 
puma_agg<-as.data.table(puma_agg)
puma_agg<-puma_agg[order(puma_num)]
msa_agg<-merge(puma_agg, puma_to_msa, by=("puma_num"),all = FALSE, sort = TRUE)
msa_agg[,hispanic_msa:= hispanic*allocation]
sum(msa_agg$hispanic_msa)
msa_out2<-aggregate(cbind(hispanic_msa) ~Msa_name ,data=msa_agg,sum)

#export as .csv 
write.csv(msa_out2, "msa_output2.csv")
