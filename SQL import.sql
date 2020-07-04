create database WAL_TEST;
use WAL_TEST;
set global sql_mode="ONLY_FULL_GROUP_BY";

CREATE TABLE Store (
  Store_ID integer PRIMARY KEY,
  City varchar(80)
);

CREATE TABLE Department (
  Depa_ID integer PRIMARY KEY,
  Depa_name varchar(80)
);

CREATE TABLE Item (
  Code integer PRIMARY KEY,
  Name varchar(80),
  Depa_ID integer references Department(Depa_ID),
  Price_USD integer DEFAULT 0
);

CREATE TABLE Invoice_header (
  Invoice_ID integer PRIMARY KEY,
  Store_ID integer references Store(Store_ID),
  Client_name varchar(80),
  Date date NOT NULL,
  Local_hour integer DEFAULT 9
);

CREATE TABLE Items_invoice (
  Invoice_ID integer references Invoice_header(Invoice_ID),
  Code integer references Item(Code)
);

#SET GLOBAL local_infile = 1;

#SHOW VARIABLES LIKE 'local_infile';

LOAD DATA LOCAL INFILE '/Users/fernandorodriguez/Downloads/Test/Store.csv'
INTO TABLE Store
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE '/Users/fernandorodriguez/Downloads/Test/Department.csv'
INTO TABLE Department
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE '/Users/fernandorodriguez/Downloads/Test/Item.csv'
INTO TABLE Item
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE '/Users/fernandorodriguez/Downloads/Test/Invoice_header.csv'
INTO TABLE Invoice_header
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE '/Users/fernandorodriguez/Downloads/Test/Items_invoice.csv'
INTO TABLE Items_invoice
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;
