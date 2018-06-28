CREATE TABLE TS12_01.STUDENTS1
(
STUDENT_ID NUMBER(5) NOT NULL,
STUDENT_NAME VARCHAR2(20),
GROUP_ID FLOAT(5),
GROUP_ID1 BINARY_FLOAT,
GROUP_ID2 BINARY_DOUBLE
);

CREATE TABLE TS12_01.STUDENTS2
(
STUDENT_ID NUMBER(5) NOT NULL,
STUDENT_NAME VARCHAR2(20),
GROUP_ID FLOAT(5),
GROUP_ID1 BINARY_FLOAT,
GROUP_ID2 BINARY_DOUBLE
);

CREATE FORCE VIEW TS12_01.F_V_STUD AS SELECT S1.STUDENT_ID,S1.STUDENT_NAME,S2.GROUP_ID FROM TS12_01.STUDENTS1 S1,TS12_01.STUDENTS2 S2;
