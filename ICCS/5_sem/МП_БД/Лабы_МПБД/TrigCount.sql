CREATE TABLE de_count_tr (
    department_id INT NOT NULL,
    department_name VARCHAR(80) NOT NULL,
    enrollee_count INT UNSIGNED DEFAULT 0,
    PRIMARY KEY (department_id),
    FOREIGN KEY (department_id) REFERENCES Department(department_id)
        ON UPDATE RESTRICT ON DELETE RESTRICT
);
