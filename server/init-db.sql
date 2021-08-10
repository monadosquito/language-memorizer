CREATE TABLE lang_memorizer
    ( id SERIAL PRIMARY KEY
    , email VARCHAR(255) UNIQUE
    , name VARCHAR(255)
    , password VARCHAR(255)
    ); 
