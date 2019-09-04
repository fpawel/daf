PRAGMA foreign_keys = ON;
PRAGMA encoding = 'UTF-8';

CREATE TABLE IF NOT EXISTS party
(
    party_id     INTEGER PRIMARY KEY NOT NULL,
    created_at   TIMESTAMP           NOT NULL                   DEFAULT (datetime('now')) UNIQUE,
    product_type SMALLINT            NOT NULL                   DEFAULT 1 CHECK ( product_type > 0 ),
    component    SMALLINT            NOT NULL                   DEFAULT 1 CHECK ( component > 0 ),
    scale        REAL                NOT NULL                   DEFAULT 1000,
    abs_err_rng  REAL                NOT NULL                   DEFAULT 200,
    abs_err_lim  REAL                NOT NULL                   DEFAULT 50,
    rel_err_lim  REAL                NOT NULL                   DEFAULT 20,
    thr1_prod    REAL                NOT NULL                   DEFAULT 200,
    thr2_prod    REAL                NOT NULL                   DEFAULT 1000,
    thr1_test    REAL                NOT NULL                   DEFAULT 200,
    thr2_test    REAL                NOT NULL                   DEFAULT 1000,
    c1           REAL                NOT NULL CHECK ( c1 >= 0 ) DEFAULT 0,
    c2           REAL                NOT NULL CHECK ( c2 >= 0 ) DEFAULT 200,
    c3           REAL                NOT NULL CHECK ( c3 >= 0 ) DEFAULT 1000,
    c4           REAL                NOT NULL CHECK ( c4 >= 0 ) DEFAULT 2000
);


DROP VIEW IF EXISTS last_party;
CREATE VIEW IF NOT EXISTS last_party AS
SELECT *
FROM party
ORDER BY created_at DESC
LIMIT 1;

CREATE TABLE IF NOT EXISTS product
(
    product_id INTEGER PRIMARY KEY NOT NULL,
    party_id   INTEGER             NOT NULL,
    serial     INTEGER             NOT NULL CHECK (serial > 0 ),
    UNIQUE (party_id, serial),
    FOREIGN KEY (party_id) REFERENCES party (party_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS product_test
(
    product_id    INTEGER NOT NULL,
    test          INTEGER NOT NULL CHECK (test BETWEEN 1 AND 7),
    concentration REAL    NOT NULL,
    current       REAL    NOT NULL,
    thr1          BOOLEAN NOT NULL,
    thr2          BOOLEAN NOT NULL,
    PRIMARY KEY (product_id, test),
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS product_work
(
    product_id INTEGER   NOT NULL,
    stored_at  TIMESTAMP NOT NULL,
    work       TEXT      NOT NULL,
    ok         BOOlEAN   NOT NULL,
    message    TEXT      NOT NULL,
    PRIMARY KEY (product_id, work),
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE
);



CREATE VIEW IF NOT EXISTS last_party_products AS
SELECT *
FROM product
WHERE party_id = (SELECT party_id FROM last_party);