CREATE TABLE TS01_03.BOOKS (
    BOOK_ID       NUMBER PRIMARY KEY,
    BOOK_NAME     VARCHAR2(30),
    AUTHOR_NAME   VARCHAR2(40),
    BOOK_ISBN     VARCHAR2(20)
)
TABLESPACE SYSTEM;