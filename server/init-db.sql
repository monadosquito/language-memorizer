CREATE TABLE lang_memorizer
    ( id SERIAL PRIMARY KEY
    , email VARCHAR(255) UNIQUE
    , name VARCHAR(255)
    , password VARCHAR(255)
    ); 

CREATE TABLE set
    ( id SERIAL PRIMARY KEY
    , owner_id INT REFERENCES lang_memorizer(id) ON DELETE NO ACTION
    , name VARCHAR(255)
    );

CREATE TABLE translate
    ( id SERIAL PRIMARY KEY
    , unit_id INT REFERENCES unit(id) ON DELETE CASCADE
    , text VARCHAR(255)
    );

CREATE TABLE unit
    ( id SERIAL PRIMARY KEY
    , set_id INT REFERENCES set(id) ON DELETE CASCADE
    , text VARCHAR(255)
    );
