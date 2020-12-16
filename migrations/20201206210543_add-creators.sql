CREATE TABLE IF NOT EXISTS creators
(
    id          BIGSERIAL PRIMARY KEY,
    bio         TEXT,
    birth       INTEGER,
    death       INTEGER,
    firstname   TEXT NOT NULL,
    lastname    TEXT,
    source      TEXT
);
