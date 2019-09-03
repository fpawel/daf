PRAGMA foreign_keys = ON;
PRAGMA encoding = 'UTF-8';

CREATE TABLE IF NOT EXISTS party
(
    party_id    INTEGER PRIMARY KEY NOT NULL,
    created_at  TIMESTAMP           NOT NULL                   DEFAULT (datetime('now')) UNIQUE,
    type        INTEGER             NOT NULL                   DEFAULT 1 CHECK ( type > 0 ),
    component   TEXT                NOT NULL                   DEFAULT 'гексан C₆H₁₄',
    scale       REAL                NOT NULL                   DEFAULT 1000,
    abs_err_rng REAL                NOT NULL                   DEFAULT 200,
    abs_err_lim REAL                NOT NULL                   DEFAULT 50,
    rel_err_lim REAL                NOT NULL                   DEFAULT 20,
    thr1_prod   REAL                NOT NULL                   DEFAULT 200,
    thr2_prod   REAL                NOT NULL                   DEFAULT 1000,
    thr1_test   REAL                NOT NULL                   DEFAULT 200,
    thr2_test   REAL                NOT NULL                   DEFAULT 1000,
    c1          REAL                NOT NULL CHECK ( c1 >= 0 ) DEFAULT 0,
    c2          REAL                NOT NULL CHECK ( c2 >= 0 ) DEFAULT 200,
    c3          REAL                NOT NULL CHECK ( c3 >= 0 ) DEFAULT 1000,
    c4          REAL                NOT NULL CHECK ( c4 >= 0 ) DEFAULT 2000
);

CREATE TABLE IF NOT EXISTS product
(
    product_id INTEGER PRIMARY KEY NOT NULL,
    party_id   INTEGER             NOT NULL,
    created_at TIMESTAMP           NOT NULL DEFAULT (datetime('now')) UNIQUE,
    serial     INTEGER             NOT NULL CHECK (serial > 0 ),
    addr       SMALLINT            NOT NULL CHECK (addr > 0),
    UNIQUE (party_id, addr),
    UNIQUE (party_id, serial),
    FOREIGN KEY (party_id) REFERENCES party (party_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS work
(
    work TEXT NOT NULL PRIMARY KEY
);

INSERT OR IGNORE INTO work
VALUES ('work_lin'),
       ('work_temp'),
       ('work_checkup'),
       ('work_tex1'),
       ('work_tex2');

CREATE TABLE IF NOT EXISTS temp
(
    temp TEXT NOT NULL PRIMARY KEY
);

INSERT OR IGNORE INTO temp
VALUES ('temp_minus'),
       ('temp_20'),
       ('temp_plus'),
       ('temp_90');

CREATE TABLE IF NOT EXISTS gas
(
    gas INTEGER NOT NULL PRIMARY KEY
);

INSERT OR IGNORE INTO gas
VALUES (1),
       (2),
       (3),
       (4);

CREATE TABLE IF NOT EXISTS var
(
    var  INTEGER NOT NULL PRIMARY KEY,
    name TEXT    NOT NULL
);
INSERT OR IGNORE INTO var
VALUES (0, 'концентрация'),
       (2, 'температура'),
       (4, 'ток излучателя'),
       (8, 'var8'),
       (10, 'var10'),
       (12, 'рабочий'),
       (14, 'опорный'),
       (16, 'var16');


CREATE TABLE IF NOT EXISTS product_test_concentration
(
    product_id    INTEGER NOT NULL,
    test          INTEGER NOT NULL CHECK (test BETWEEN 1 AND 7),
    concentration REAL    NOT NULL,
    current       REAL    NOT NULL,
    thr1          BOOLEAN NOT NULL,
    thr2          BOOLEAN NOT NULL,
    UNIQUE (product_id, test),
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS product_entry
(
    product_entry_id INTEGER PRIMARY KEY NOT NULL,
    product_id       INTEGER             NOT NULL,
    created_at       TIMESTAMP           NOT NULL DEFAULT (datetime('now')),
    work             TEXT                NOT NULL,
    ok               BOOlEAN             NOT NULL,
    message          TEXT                NOT NULL,
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE
);

CREATE VIEW IF NOT EXISTS last_party AS
SELECT *
FROM party
ORDER BY created_at DESC
LIMIT 1;


CREATE VIEW IF NOT EXISTS last_party_products AS
SELECT *
FROM product
WHERE party_id = (SELECT party_id FROM last_party);