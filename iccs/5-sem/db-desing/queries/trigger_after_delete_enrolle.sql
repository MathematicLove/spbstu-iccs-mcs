DROP TRIGGER IF EXISTS UECAD2;

DELIMITER //
CREATE TRIGGER UECAD2
AFTER DELETE ON Choice
FOR EACH ROW
BEGIN
    DECLARE old_department_id INT;
    SELECT s.department_id INTO old_department_id
    FROM Specialty s
    WHERE s.specialty_id = OLD.specialty_id
    LIMIT 1;

    IF (SELECT submitted_docs_id FROM Enrollee WHERE enrollee_id = OLD.enrollee_id) = 1 THEN
        UPDATE de_count_tr
        SET enrollee_count = enrollee_count - 1
        WHERE department_id = old_department_id;
    END IF;
END //
DELIMITER ;
