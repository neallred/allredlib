CREATE TABLE IF NOT EXISTS subseries
(
    id                   BIGSERIAL PRIMARY KEY,
    synopsis             TEXT,
    title                TEXT NOT NULL,
    total_book_members   INTEGER NOT NULL,
    series_id            BIGINT NOT NULL REFERENCES series(id) ON UPDATE CASCADE ON DELETE CASCADE
);
