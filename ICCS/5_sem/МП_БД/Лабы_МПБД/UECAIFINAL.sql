DELIMITER //

CREATE TRIGGER UECAF2
AFTER INSERT ON Choice
FOR EACH ROW
BEGIN
    DECLARE new_department_id INT;
    SELECT s.department_id INTO new_department_id
    FROM Specialty s
    WHERE s.specialty_id = NEW.specialty_id
    LIMIT 1;

    IF (SELECT enrollment_status_id FROM Enrollee WHERE enrollee_id = NEW.enrollee_id) = 6 THEN
        UPDATE de_count_tr
        SET enrollee_count = enrollee_count + 1
        WHERE department_id = new_department_id;
    END IF;
END //

DELIMITER ;
