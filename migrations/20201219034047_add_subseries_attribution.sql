CREATE TABLE IF NOT EXISTS subseries_attributions
(
    id                   BIGSERIAL PRIMARY KEY,
    subseries_id         BIGINT NOT NULL REFERENCES subseries(id) ON UPDATE CASCADE ON DELETE CASCADE,
    attribution_id       BIGINT NOT NULL REFERENCES attributions(id) ON UPDATE CASCADE ON DELETE CASCADE
);
