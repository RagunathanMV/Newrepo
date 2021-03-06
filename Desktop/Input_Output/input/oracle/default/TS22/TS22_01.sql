CREATE TABLE TS22_01.SUPPLIERS
(
SUPPLIER_ID NUMBER(10),
SUPPLIER_NAME VARCHAR2(20),
SUPPLIER_ADDRESS VARCHAR2(20)
);

CREATE TABLE TS22_01.ORDERS
(
ORDER_NUMBER NUMBER(10),
SUPPLIER_ID NUMBER(10),
CITY VARCHAR2(20)
);

CREATE OR REPLACE PROCEDURE TS22_01.Create_Tab_SUP_ORD 
IS
BEGIN

CREATE TABLE TS22_01.SUP_ORD AS SELECT ORDERS.ORDER_NUMBER, ORDERS.CITY, SUPPLIERS.SUPPLIER_NAME  
FROM TS22_01.SUPPLIERS  
RIGHT OUTER JOIN TS22_01.ORDERS  
ON SUPPLIERS.SUPPLIER_ID = ORDERS.SUPPLIER_ID;

END;
/