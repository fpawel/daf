package data

const SQLCreate = `
PRAGMA foreign_keys = ON;
PRAGMA encoding = 'UTF-8';

CREATE TABLE IF NOT EXISTS party
(
    party_id     INTEGER PRIMARY KEY NOT NULL,
    created_at   TIMESTAMP           NOT NULL DEFAULT (DATETIME('now', '+3 hours')),
    product_type TEXT                NOT NULL DEFAULT '00.01',
    c1           REAL                NOT NULL DEFAULT 0 CHECK (c1 >= 0),
    c2           REAL                NOT NULL DEFAULT 25 CHECK (c2 >= 0),
    c3           REAL                NOT NULL DEFAULT 50 CHECK (c3 >= 0),
    c4           REAL                NOT NULL DEFAULT 100 CHECK (c4 >= 0)
);

CREATE TABLE IF NOT EXISTS product
(
    product_id INTEGER PRIMARY KEY NOT NULL,
    party_id   INTEGER             NOT NULL,
    serial     SMALLINT            NOT NULL CHECK (serial > 0 ),
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


CREATE TABLE IF NOT EXISTS product_value
(
    product_id INTEGER NOT NULL,
    var        INTEGER NOT NULL,
    work       TEXT    NOT NULL,
    gas        INTEGER NOT NULL,
    temp       TEXT    NOT NULL,
    value      REAL    NOT NULL,
    PRIMARY KEY (product_id, var, work, gas, temp),
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE,
    FOREIGN KEY (var) REFERENCES var (var),
    FOREIGN KEY (work) REFERENCES work (work),
    FOREIGN KEY (gas) REFERENCES gas (gas),
    FOREIGN KEY (temp) REFERENCES temp (temp)
);

CREATE TABLE IF NOT EXISTS product_coefficient
(
    product_id  INTEGER NOT NULL,
    coefficient INTEGER NOT NULL CHECK ( coefficient >= 0 ),
    value       REAL    NOT NULL,
    PRIMARY KEY (product_id, coefficient),
    FOREIGN KEY (product_id) REFERENCES product (product_id) ON DELETE CASCADE
);

CREATE VIEW IF NOT EXISTS last_party AS
SELECT *
FROM party
ORDER BY created_at DESC
LIMIT 1;


CREATE VIEW IF NOT EXISTS last_party_products1 AS
SELECT *
FROM product
WHERE product_id = (SELECT party_id FROM last_party);

CREATE VIEW IF NOT EXISTS last_party_products AS
SELECT count(*) - 1 AS place, cur.*
FROM (SELECT * FROM last_party_products1) AS cur
         LEFT JOIN (SELECT * FROM last_party_products1) AS oth
WHERE cur.product_id >= oth.product_id
GROUP BY cur.product_id;`
