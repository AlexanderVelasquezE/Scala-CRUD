CREATE TABLE USERS (
  ID BIGSERIAL PRIMARY KEY,
  LEGAL_ID VARCHAR NOT NULL,
  FIRST_NAME VARCHAR NOT NULL,
  LAST_NAME VARCHAR NOT NULL,
  EMAIL VARCHAR NOT NULL,
  PHONE VARCHAR NOT NULL
);
