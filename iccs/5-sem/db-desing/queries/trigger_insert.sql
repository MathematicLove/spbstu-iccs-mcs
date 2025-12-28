DELIMITER //

CREATE TRIGGER update_enrollee_count_after_insert
AFTER INSERT ON Enrollee
FOR EACH ROW
BEGIN
    IF NEW.enrollment_status_id = 6 THEN
        UPDATE de_count_tr
        JOIN Specialty ON Specialty.department_id = de_count_tr.department_id
        JOIN Choice ON Specialty.specialty_id = Choice.specialty_id
        SET de_count_tr.enrollee_count = de_count_tr.enrollee_count + 1
        WHERE Choice.enrollee_id = NEW.enrollee_id;
    END IF;
END //

DELIMITER ;
