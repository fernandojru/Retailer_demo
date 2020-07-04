library(stringi)
library(randomNames)
library(sqldf)

Store<-data.frame(
  Store_ID=c(1:15),
  City=c("Chicago","Mexico City","Los Angeles","New York","Sao Paulo","Bogota",
         "Guadalajara","Monterrey","Miami","San Francisco","Montreal","Seattle","Dallas",
         "Houston","Santiago")
)

Department<-data.frame(
  Depa_ID=c(1:15),
  Depa_name=c("Electronics","Frozen","Beverages","Fruits","Seasonal","Meat","Milk","Car",
              "Cereal & pastas","Candy","Cleaning","Clothing","Shoes","Pets","Luggage")
)

Item<-data.frame(
  Code=c(1:150),
  Name=stri_rand_strings(150,length = 5),
  Depa_ID=sample(1:15, 150,replace = TRUE),
  Price_USD=runif(150, 20, 1000)
)


Invoice_header<-data.frame(
  Invoice_ID=c(1:500),
  Store_ID=sample(1:15, 500,replace = TRUE),
  Client_name=randomNames(500, sample.with.replacement=FALSE),
  Date=as.Date(sample(as.Date("2020-02-01"): as.Date("2020-06-30"), 500, 
                      replace = T),origin='1970-01-01'),
  Local_hour=sample(9:22, 500,replace = TRUE)
)

Items_invoice<-data.frame(
  Invoice_ID=sample(1:500, 1200,replace = TRUE),
  Code = sample(1:150, 1200,replace = TRUE)
)

#write.csv(Store, "Store.csv",row.names = FALSE)
#write.csv(Department, "Department.csv",row.names = FALSE)
#write.csv(Item, "Item.csv",row.names = FALSE)
#write.csv(Invoice_header, "Invoice_header.csv",row.names = FALSE)
#write.csv(Items_invoice, "Items_invoice.csv",row.names = FALSE)

Store<-read.csv("Store.csv")
Department<-read.csv("Department.csv")
Item<-read.csv("Item.csv")
Invoice_header<-read.csv("Invoice_header.csv")
Items_invoice<-read.csv("Items_invoice.csv")

#Top 10 selling stores on the timeframe selected

QueryC<-sqldf("SELECT A.Store_ID, B.City, COUNT(A.INVOICE_ID) as Number_sales 
      From Invoice_header A 
      INNER JOIN Store B ON A.Store_ID =B.Store_ID
      group by A.Store_ID
      ORDER BY Number_sales DESC")

#Hourly sales per selected store on a timeframe.
QueryD<-sqldf("SELECT Local_hour, COUNT(INVOICE_ID) as Number_sales
      From Invoice_header
      group by Local_hour")

#Top 20 best-selling items overall and per store.

#Overall
QueryE_1<-sqldf("SELECT A.Code, B.Name, COUNT(A.Invoice_ID) as Number_sales
      FROM Items_invoice A
      INNER JOIN Item B on A.Code=B.Code
      GROUP BY A.Code
      ORDER BY Number_sales DESC
      LIMIT 20")

QueryE_2<-sqldf("SELECT B.Store_ID, C.City,A.Code, COUNT(A.Invoice_ID) as Number_sales
      FROM Items_invoice A
      INNER JOIN Invoice_header B ON A.Invoice_ID=B.Invoice_ID
      INNER JOIN Store C ON B.Store_ID=C.Store_ID
      GROUP BY B.Store_ID, A.Code
      ORDER BY Number_sales DESC
      LIMIT 20")

#Top 10 best-selling item-departments overall and per store.
QueryF_1<-sqldf("SELECT B.Depa_ID, COUNT(A.Invoice_ID) as Number_sales
      FROM Items_invoice A
      INNER JOIN Item B ON A.Code=B.Code
      GROUP BY B.Depa_ID
      ORDER BY Number_sales DESC")

QueryF_2<-sqldf("SELECT C.Store_ID, D.Depa_ID, COUNT(A.Invoice_ID) as Number_sales
      FROM Items_invoice A
      INNER JOIN Invoice_header C on A.Invoice_ID=C.Invoice_ID
      INNER JOIN Item D on A.Code=D.Code
      GROUP BY C.Store_ID, D.Depa_ID
      ORDER BY Store_ID, Number_sales DESC")

#Trend of sales on a time frame overall and per store.
QueryG_1<-sqldf("SELECT Date, COUNT(Invoice_ID) as Number_sales
      FROM Invoice_header 
      GROUP BY Date")

QueryG_2<-sqldf("SELECT Store_ID, Date, COUNT(Invoice_ID) as Number_sales
      FROM Invoice_header 
      GROUP BY Store_ID, Date")








