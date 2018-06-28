CREATE TABLE TS26_01.SUPPLIERS
(
SUPPLIER_ID NUMBER(10),
SUPPLIER_NAME VARCHAR2(20),
SUPPLIER_ADDRESS VARCHAR2(20)
);

CREATE TABLE TS26_01.ORDERS
(
ORDER_NUMBER NUMBER(10),
SUPPLIER_ID NUMBER(10),
CITY VARCHAR2(20)
);

CREATE OR REPLACE PROCEDURE TS26_01.CREATE_TAB_SUP_ORD2 
IS
BEGIN

CREATE TABLE TS26_01.SUP_ORD AS SELECT 
S1.SUPPLIER_ID, 
S1.SUPPLIER_NAME,
S1.SUPPLIER_ADDRESS,
O1.ORDER_NUMBER,
O1.SUPPLIER_ID SUP_ID,
O1.CITY 
FROM TS26_01.SUPPLIERS S1,TS26_01.ORDERS O1;

END;
/