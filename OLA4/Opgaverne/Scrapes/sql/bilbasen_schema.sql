-- OLA4 opgave 2.1: skema for bilbasen
--
-- Designet (det samme beskriver vi i ER-diagrammet):
--   Invariante ting = ting der ikke ændrer sig: bilen selv og forhandleren.
--   Varierende ting = prisen (og km), som kan ændre sig fra scrape til scrape.
--   Derfor en tidsserie-tabel "pris" med én række pr. bil pr. scrapedate,
--   og den seneste pris er den nyeste række pr. carid.
--
--   forhandler (1) ---< bil (1) ---< pris
--
--   Solgte biler slettes ikke, de markeres med sold = TRUE.

CREATE DATABASE IF NOT EXISTS bilbasen;
USE bilbasen;

CREATE TABLE forhandler (
  forhandler_id INT PRIMARY KEY AUTO_INCREMENT,
  logo VARCHAR(500) UNIQUE   -- logo-linket er det eneste faste kendetegn vi har på forhandleren
);

CREATE TABLE bil (
  carid BIGINT PRIMARY KEY,          -- bilens eget id fra Bilbasens html, vores primære nøgle
  link VARCHAR(500),
  model VARCHAR(100),
  variant VARCHAR(200),
  måned INT,
  år INT,
  beskrivelse TEXT,                  -- TEXT fordi beskrivelser kan være lange
  sted VARCHAR(100),
  forhandler_id INT,
  sold BOOLEAN NOT NULL DEFAULT FALSE,
  FOREIGN KEY (forhandler_id) REFERENCES forhandler(forhandler_id)
);

CREATE TABLE pris (
  pris_id INT PRIMARY KEY AUTO_INCREMENT,
  carid BIGINT NOT NULL,
  pris INT,
  km INT,                            -- km ligger her, for den ændrer sig også over tid
  scrapedate DATE NOT NULL,
  FOREIGN KEY (carid) REFERENCES bil(carid)
);
