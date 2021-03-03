CREATE TYPE role AS ENUM ('anonymous', 'unadmitted', 'user', 'admin', 'super_admin');

CREATE TABLE IF NOT EXISTS users
(
    id          BIGSERIAL PRIMARY KEY,
    role        role NOT NULL,
    username    TEXT NOT NULL,
    email       TEXT,
    pass        TEXT NOT NULL,
    UNIQUE(username)
);
