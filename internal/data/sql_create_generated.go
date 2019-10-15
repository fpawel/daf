package data

const SQLCreate = `
PRAGMA foreign_keys = ON;
PRAGMA encoding = 'UTF-8';

CREATE TABLE IF NOT EXISTS party
(
    party_id         INTEGER PRIMARY KEY NOT NULL,
    created_at       TIMESTAMP           NOT NULL                                DEFAULT (datetime('now')) UNIQUE,
    product_type     SMALLINT            NOT NULL                                DEFAULT 1 CHECK ( product_type > 0 ),
    component        SMALLINT            NOT NULL                                DEFAULT 1 CHECK ( component > 0 ),
    c1               REAL                NOT NULL CHECK ( c1 >= 0 )              DEFAULT 0,
    c2               REAL                NOT NULL CHECK ( c2 >= 0 )              DEFAULT 200,
    c3               REAL                NOT NULL CHECK ( c3 >= 0 )              DEFAULT 1000,
    c4               REAL                NOT NULL CHECK ( c4 >= 0 )              DEFAULT 2000,
    abs_error_limit1 REAL                NOT NULL CHECK ( abs_error_limit1 > 0 ) DEFAULT 100,
    abs_error_limit2 REAL                NOT NULL CHECK ( abs_error_limit2 > 0 ) DEFAULT 100,
    abs_error_limit3 REAL                NOT NULL CHECK ( abs_error_limit3 > 0 ) DEFAULT 100,
    abs_error_limit4 REAL                NOT NULL CHECK ( abs_error_limit4 > 0 ) DEFAULT 100,
    variation_limit3 REAL                NOT NULL CHECK ( variation_limit3 > 0 ) DEFAULT 100
);


DROP VIEW IF EXISTS last_party;
CREATE VIEW IF NOT EXISTS last_party AS
SELECT *
FROM party
ORDER BY created_at DESC
LIMIT 1;

CREATE TABLE IF NOT EXISTS product
(
    product_id  INTEGER PRIMARY KEY NOT NULL,
    party_id    INTEGER             NOT NULL,
    serial      INTEGER             NOT NULL CHECK (serial > 0 ),
    scale_begin REAL                NOT NULL DEFAULT 0,
    scale_end   REAL                NOT NULL DEFAULT 1000,
    UNIQUE (party_id, serial),
    FOREIGN KEY (party_id) REFERENCES party (party_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS product_test
(
    product_id     INTEGER NOT NULL,
    test_number    INTEGER NOT NULL,
    concentration  REAL    NOT NULL,
    output_current REAL    NOT NULL,
    thr1           BOOLEAN NOT NULL,
    thr2           BOOLEAN NOT NULL,
    PRIMARY KEY (product_id, test_number),
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS product_entry
(
    product_entry_id  INTEGER PRIMARY KEY NOT NULL,
    product_id INTEGER   NOT NULL,
    stored_at  TIMESTAMP NOT NULL DEFAULT (DATETIME('now')),
    test       TEXT      NOT NULL,
    ok         BOOlEAN   NOT NULL,
    message    TEXT      NOT NULL,
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE
);


CREATE INDEX IF NOT EXISTS index_product_product_entry_test ON product_entry (test);


CREATE VIEW IF NOT EXISTS last_party_products AS
SELECT *
FROM product
WHERE party_id = (SELECT party_id FROM last_party);

CREATE VIEW IF NOT EXISTS last_party_products AS
SELECT *
FROM product
WHERE party_id = (
    SELECT party_id
    FROM party
    ORDER BY created_at DESC
    LIMIT 1);
`
