DROP TABLE IF EXISTS `Subject_Requirement`;
DROP TABLE IF EXISTS `Choice`;
DROP TABLE IF EXISTS `Achievment`;
DROP TABLE IF EXISTS `Document`;
DROP TABLE IF EXISTS `Specialty`;
DROP TABLE IF EXISTS `Enrollee`;
DROP TABLE IF EXISTS `Department`;
DROP TABLE IF EXISTS `Document_Type`;
DROP TABLE IF EXISTS `Enrollment_Status`;
DROP TABLE IF EXISTS `Subject`;

CREATE TABLE `Subject`
( `subject_id` INT NOT NULL AUTO_INCREMENT
, `name` VARCHAR(50) NOT NULL
, PRIMARY KEY (`subject_id`)
) ENGINE = InnoDB; 

CREATE TABLE `Enrollment_Status`
( `enrollment_status_id` INT NOT NULL AUTO_INCREMENT
, `name` VARCHAR(20) NOT NULL
, PRIMARY KEY (`enrollment_status_id`)
) ENGINE = InnoDB; 

CREATE TABLE `Document_Type`
( `document_type_id` INT NOT NULL AUTO_INCREMENT
, `name` VARCHAR(80) NOT NULL
, PRIMARY KEY (`document_type_id`)
) ENGINE = InnoDB;

CREATE TABLE `Department`
( `department_id` INT NOT NULL AUTO_INCREMENT
, `name` VARCHAR(80) NOT NULL	
, `phone_number` VARCHAR(20) NOT NULL
, `address` TEXT NOT NULL
, `headmaster_name` VARCHAR(250) NOT NULL
, PRIMARY KEY (`department_id`)
) ENGINE = InnoDB; 


CREATE TABLE `Enrollee`
( `enrollee_id` INT NOT NULL AUTO_INCREMENT 
, `first_name` VARCHAR(80) NOT NULL 
, `last_name` VARCHAR(80) NOT NULL 
, `middle_name` VARCHAR(80) NOT NULL 
, `birth_date` DATE NOT NULL
, `enrollment_status_id` INT NOT NULL DEFAULT 1
, PRIMARY KEY (`enrollee_id`)
, FOREIGN KEY (`enrollment_status_id`)  REFERENCES `Enrollment_Status` (`enrollment_status_id`)
    ON UPDATE RESTRICT ON DELETE RESTRICT
) ENGINE = InnoDB;

CREATE TABLE `Specialty`
( `specialty_id` INT NOT NULL AUTO_INCREMENT
, `department_id` INT NOT NULL
, `code` VARCHAR(50) NOT NULL
, `name` VARCHAR(50) NOT NULL
, PRIMARY KEY (`specialty_id`)
, FOREIGN KEY (`department_id`) REFERENCES `Department` (`department_id`)
    ON UPDATE RESTRICT ON DELETE RESTRICT
) ENGINE = InnoDB;

CREATE TABLE `Document`
( `document_id` INT NOT NULL AUTO_INCREMENT
, `enrollee_id` INT NOT NULL
, `document_type_id` INT NOT NULL
, PRIMARY KEY (`document_id`)
, FOREIGN KEY (`enrollee_id`) REFERENCES `Enrollee` (`enrollee_id`)
    ON UPDATE RESTRICT ON DELETE CASCADE
, FOREIGN KEY (`document_type_id`) REFERENCES `Document_Type` (`document_type_id`)
    ON UPDATE RESTRICT ON DELETE RESTRICT
) ENGINE = InnoDB;

CREATE TABLE `Achievment`
( `achievment_id` INT NOT NULL AUTO_INCREMENT
, `enrollee_id` INT NOT NULL
, `subject_id` INT NOT NULL
, `ege_result` INT UNSIGNED
, `inner_result` INT UNSIGNED
, PRIMARY KEY (`achievment_id`)
, FOREIGN KEY (`enrollee_id`) REFERENCES `Enrollee` (`enrollee_id`)
    ON UPDATE RESTRICT ON DELETE CASCADE
, FOREIGN KEY (`subject_id`) REFERENCES `Subject` (`subject_id`)
    ON UPDATE RESTRICT ON DELETE RESTRICT
) ENGINE = InnoDB;

CREATE TABLE `Choice`
( `choice_id` INT NOT NULL AUTO_INCREMENT
, `enrollee_id` INT NOT NULL
, `specialty_id` INT NOT NULL
, `priority_index` INT UNSIGNED NOT NULL
, PRIMARY KEY (`choice_id`)
, FOREIGN KEY (`enrollee_id`) REFERENCES `Enrollee` (`enrollee_id`)
    ON UPDATE RESTRICT ON DELETE CASCADE
, FOREIGN KEY (`specialty_id`) REFERENCES `Specialty` (`specialty_id`)
    ON UPDATE RESTRICT ON DELETE RESTRICT
) ENGINE = InnoDB;

CREATE TABLE `Subject_Requirement`
( `subject_requirement_id` INT NOT NULL AUTO_INCREMENT
, `subject_id` INT NOT NULL
, `specialty_id` INT NOT NULL
, `ege_minimal_result` INT UNSIGNED NOT NULL
, `inner_minimal_result` INT UNSIGNED NOT NULL
, PRIMARY KEY (`subject_requirement_id`)
, FOREIGN KEY (`subject_id`) REFERENCES `Subject` (`subject_id`)
    ON UPDATE RESTRICT ON DELETE RESTRICT
, FOREIGN KEY (`specialty_id`) REFERENCES `Specialty` (`specialty_id`)
    ON UPDATE RESTRICT ON DELETE RESTRICT
) ENGINE = InnoDB;