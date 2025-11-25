CREATE USER 'coolView'@'localhost' IDENTIFIED BY 'Ayzek123321';

GRANT SELECT ON student_adm.E_Stats1 TO 'coolView'@'localhost';
GRANT SELECT, INSERT, UPDATE, DELETE ON student_adm.Enrollee TO 'coolView'@'localhost';
GRANT SELECT, INSERT, UPDATE, DELETE ON student_adm.Achievment TO 'coolView'@'localhost';
GRANT SELECT, INSERT, UPDATE, DELETE ON student_adm.Choice TO 'coolView'@'localhost';
GRANT SELECT, INSERT, UPDATE, DELETE ON student_adm.Specialty TO 'coolView'@'localhost';
FLUSH PRIVILEGES;
