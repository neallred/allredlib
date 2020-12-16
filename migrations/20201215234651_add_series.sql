CREATE TABLE IF NOT EXISTS series
(
    id                   BIGSERIAL PRIMARY KEY,
    synopsis             TEXT,
    title                TEXT NOT NULL,
    total_book_members   INTEGER NOT NULL,
    total_subseries      INTEGER NOT NULL
);
