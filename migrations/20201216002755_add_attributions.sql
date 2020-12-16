CREATE TABLE IF NOT EXISTS attributions
(
    id                 BIGSERIAL PRIMARY KEY,
    title              TEXT NOT NULL,
    link               TEXT,
    accessed           TIMESTAMP WITH TIME ZONE,
    year_created       INTEGER,
    place_published    TEXT,
    author             TEXT,
    publisher          TEXT
);
