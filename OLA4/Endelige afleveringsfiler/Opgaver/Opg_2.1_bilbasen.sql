-- OLA4 opgave 2.1: skema for bilbasen
-- forhandler og biler ændrer sig ikke. pris er tidsserien, én række pr. bil pr. scrapedate
CREATE DATABASE IF NOT EXISTS bilbasen;
USE bilbasen;
-- bilbasen.forhandler definition

CREATE TABLE `forhandler` (
  `forhandler_id` int NOT NULL,
  `logo` varchar(255) DEFAULT NULL,
  `sted` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`forhandler_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;


-- bilbasen.biler definition

CREATE TABLE `biler` (
  `carid` int NOT NULL,
  `forhandler_id` int DEFAULT NULL,
  `link` varchar(255) DEFAULT NULL,
  `model` varchar(100) DEFAULT NULL,
  `variant` varchar(255) DEFAULT NULL,
  `år` int DEFAULT NULL,
  `km` int DEFAULT NULL,
  `beskrivelse` text,
  `email` varchar(255) DEFAULT NULL,
  `telefon` varchar(20) DEFAULT NULL,
  `sold` tinyint(1) DEFAULT '0',
  PRIMARY KEY (`carid`),
  KEY `biler_forhandler_FK` (`forhandler_id`),
  CONSTRAINT `biler_forhandler_FK` FOREIGN KEY (`forhandler_id`) REFERENCES `forhandler` (`forhandler_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;


-- bilbasen.pris definition

CREATE TABLE `pris` (
  `prisid` int NOT NULL AUTO_INCREMENT,
  `carid` int NOT NULL,
  `pris` int DEFAULT NULL,
  `scrapedate` date DEFAULT NULL,
  PRIMARY KEY (`prisid`),
  KEY `pris_biler_FK` (`carid`),
  CONSTRAINT `pris_biler_FK` FOREIGN KEY (`carid`) REFERENCES `biler` (`carid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE VIEW seneste_pris AS
SELECT p.carid, p.pris, p.scrapedate
FROM pris p
WHERE p.scrapedate = (SELECT MAX(p2.scrapedate) FROM pris p2 WHERE p2.carid = p.carid);
