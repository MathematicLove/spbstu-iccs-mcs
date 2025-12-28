DELIMITER //

CREATE TRIGGER add_new_department
AFTER INSERT ON Department
FOR EACH ROW
BEGIN
    -- При добавлении нового института добавляем запись в de_count_tr с нулевым количеством абитуриентов
    INSERT INTO de_count_tr (department_id, department_name, enrollee_count)
    VALUES (NEW.department_id, NEW.name, 0);
END //

DELIMITER ;
