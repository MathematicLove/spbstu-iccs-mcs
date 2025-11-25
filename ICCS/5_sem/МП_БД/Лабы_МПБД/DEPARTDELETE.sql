DELIMITER //

DROP TRIGGER IF EXISTS delete_department;

CREATE TRIGGER delete_department
BEFORE DELETE ON Department
FOR EACH ROW
BEGIN
    DELETE FROM de_count_tr
    WHERE department_id = OLD.department_id;
END //

DELIMITER ;
