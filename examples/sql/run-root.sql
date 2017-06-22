DROP DATABASE IF EXISTS `test`
;

CREATE DATABASE `test`
;

DROP USER /*!50708 IF EXISTS */
  'test'@'localhost'
;

CREATE USER 'test'@'localhost' IDENTIFIED BY 'test';
GRANT ALL ON test.* TO 'test'@'localhost';

FLUSH PRIVILEGES;
