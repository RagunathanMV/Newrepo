CREATE TABLE TS30_01.SUPPLIERS (
    SUPPLIER_ID        NUMBER(10),
    SUPPLIER_NAME      VARCHAR2(20),
    SUPPLIER_ADDRESS   VARCHAR2(20)
);

CREATE TABLE TS30_01.SUPPLIERS_META (
    SUPPLIER_NUM       NUMBER(10),
    SUPPLIER_NAME      VARCHAR2(20),
    SUPPLIER_ADDRESS   VARCHAR2(20)
);

CREATE OR REPLACE PROCEDURE TS30_01.CREATE_TAB_SUPP_META 
IS
BEGIN

-- CREATING SEQUENCE

CREATE SEQUENCE TS30_01.SUPPLIER_SEQ MINVALUE 1 START WITH 1 INCREMENT BY 1 CACHE 20;

-- INSERT DATA INTO TABLE USING SEQUENCE

INSERT INTO TS30_01.SUPPLIERS_META (
    SUPPLIER_NUM,
    SUPPLIER_NAME,
    SUPPLIER_ADDRESS
)
    SELECT
        SUPPLIER_SEQ.NEXTVAL,
        S1.SUPPLIER_NAME,
        S1.SUPPLIER_ADDRESS
    FROM
        TS30_01.SUPPLIERS S1;

END;
/