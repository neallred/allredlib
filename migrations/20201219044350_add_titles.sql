CREATE TABLE IF NOT EXISTS titles
(
    id                   BIGSERIAL PRIMARY KEY,
    title                TEXT NOT NULL,
    year                 INTEGER,
    synopsis             TEXT,
    series_part          TEXT,
    series_id            BIGINT REFERENCES series(id) ON UPDATE CASCADE ON DELETE CASCADE,
    subseries_part       TEXT,
    subseries_id         BIGINT REFERENCES subseries(id) ON UPDATE CASCADE ON DELETE CASCADE
);

