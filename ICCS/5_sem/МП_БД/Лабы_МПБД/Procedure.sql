DELIMITER //
DROP PROCEDURE IF exists AddIS;
CREATE PROCEDURE AddIS(
    IN p_department_name VARCHAR(80),
    IN p_phone_number VARCHAR(20),
    IN p_address TEXT,
    IN p_headmaster_name VARCHAR(250),
    IN p_specialty_code VARCHAR(50),
    IN p_specialty_name VARCHAR(50)
)
BEGIN
    DECLARE v_department_id INT;
    DECLARE v_specialty_id INT;
    SELECT department_id INTO v_department_id
    FROM Department
    WHERE name = p_department_name
    LIMIT 1;

    IF v_department_id IS NULL THEN
        INSERT INTO Department (name, phone_number, address, headmaster_name)
        VALUES (p_department_name, p_phone_number, p_address, p_headmaster_name);
        SET v_department_id = LAST_INSERT_ID();
    END IF;
    SELECT specialty_id INTO v_specialty_id
    FROM Specialty
    WHERE code = p_specialty_code AND name = p_specialty_name
    LIMIT 1;
    IF v_specialty_id IS NULL THEN
        INSERT INTO Specialty (department_id, code, name)
        VALUES (v_department_id, p_specialty_code, p_specialty_name);
    ELSE
        UPDATE Specialty
        SET department_id = v_department_id
        WHERE specialty_id = v_specialty_id;
    END IF;

END //

DELIMITER ;
