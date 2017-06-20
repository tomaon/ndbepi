ALTER TABLE `City`            DROP FOREIGN KEY `city_ibfk_1`;
ALTER TABLE `CountryLanguage` DROP FOREIGN KEY `countryLanguage_ibfk_1`;

ALTER TABLE `City`            ENGINE NDBCLUSTER;
ALTER TABLE `Country`         ENGINE NDBCLUSTER;
ALTER TABLE `CountryLanguage` ENGINE NDBCLUSTER;
