import random
from datetime import datetime, timedelta
import pymysql

def generate_random_date(start_year, end_year):
    start_date = datetime(year=start_year, month=1, day=1)
    end_date = datetime(year=end_year, month=12, day=31)
    delta = end_date - start_date
    random_date = start_date + timedelta(days=random.randint(0, delta.days))
    return random_date.strftime('%Y-%m-%d')

def generate_random_name(names):
    return random.choice(names)

def generate_random_phone_number():
    return random.randint(1000000000, 9999999999)

def generate_random_result():
    return random.randint(1, 100)

def get_existing_enrollee_ids(cursor):
    cursor.execute("SELECT enrollee_id FROM Enrollee")
    return [row['enrollee_id'] for row in cursor.fetchall()]

def get_existing_department_ids(cursor):
    cursor.execute("SELECT department_id FROM Department")
    return [row['department_id'] for row in cursor.fetchall()]

ENROLLEE_COUNT = 200_000

try:
    connection = pymysql.connect(
        host='127.0.0.1',
        user='root',
        password='Ayzek123321',
        database='student_adm',
        charset='utf8mb4',
        cursorclass=pymysql.cursors.DictCursor
    )

    with connection.cursor() as cursor:
        first_names = ['Ayzek', 'Petya', 'Viktor', 'Mikhail', 'Lanita', 'Igor',
                       'Svetlana', 'Irina', 'Olga', 'Vladimir']
        last_names = ['Salimli', 'Grigoryev', 'Kuznetsov', 'Kapustin', 'Silvanovich',
                      'Malikov', 'Bakinets', 'Dolgov', 'Nepomnyashchikh', 'Istov']
        middle_names = ['Petrovich', 'Vladimirovich', 'Alexandrovich', 'Gennadyevich',
                        'Mikhailovich', 'Pavlovich', 'Alexeyevich', 'Viktorovich', 'Ivanovich', 'Rustamovich']

        adrespoly = ['Polytechnicheskaya 29', 'Gidrotekhnikov 2', 'Polytechnicheskaya 21', 'Obruchevykh 1', 
                     'Gzhatskaya 4', 'Polytechnicheskaya 34', 'Novorossiyskaya 50', 'Vernosti 5', 
                     'Grazhdansky pr. 7', 'Vernosti 4', 'Nepokorennykh 6']

        document_types = ['Passport', 'SNILS', 'Attestat']
        department_names = ['IKNK', 'GI', 'PhysMech', 'IBSiB', 'IMPP', 'IMMiT', 'IE',
                            'ISI', 'YA', 'IFKiS', 'IPMEiT']
        subject_names = ['Mathematics', 'Physics', 'Chemistry', 'Biology', 'English',
                         'History', 'Geography', 'Informatics', 'Economics', 'Literature', 'Music']
        specialty_names = ['MKN', 'PI', 'PMiF', 'BF', 'BI', 'RiOS',
                           'ZR', 'MS', 'YA', 'Eco']
        specialty_ids = range(1, len(specialty_names) + 1)

        # Вставка данных в таблицу Subject
        query = "INSERT INTO Subject (name) VALUES (%s)"
        for subject_name in subject_names:
            cursor.execute(query, (subject_name,))

        query = "INSERT INTO Enrollment_Status (name) VALUES (%s)"
        for status in ["added", "cancelled"]:
            cursor.execute(query, (status,))
        
        cursor.execute("SELECT enrollment_status_id FROM Enrollment_Status")
        existing_status_ids = [row['enrollment_status_id'] for row in cursor.fetchall()]
        
        cursor.execute("SELECT subject_id FROM Subject")
        existing_subject_ids = [row['subject_id'] for row in cursor.fetchall()]

        # Вставка данных в таблицу Enrollee
        query = ("INSERT INTO Enrollee (first_name, last_name, middle_name, birth_date, enrollment_status_id)"
                 "VALUES (%s, %s, %s, %s, %s)")
        cursor.executemany(query, tuple(
            (generate_random_name(first_names),
             generate_random_name(last_names),
             generate_random_name(middle_names),
             generate_random_date(1980, 2005),
             random.choice(existing_status_ids)) for _ in range(ENROLLEE_COUNT)
        ))

        # Вставка данных в таблицу Department
        query = "INSERT INTO Department (name, phone_number, address, headmaster_name) VALUES (%s, %s, %s, %s)"
        for dept_name in department_names:
            phone_number = generate_random_phone_number()
            address = random.choice(adrespoly)
            headmaster_name = generate_random_name(last_names)
            cursor.execute(query, (dept_name, phone_number, address, headmaster_name))

        # Вставка данных в таблицу Document_Type
        query = "INSERT INTO Document_Type (name) VALUES (%s)"
        for document_type_name in document_types:
            cursor.execute(query, (document_type_name,))

        # Вставка данных в таблицу Specialty
        existing_department_ids = get_existing_department_ids(cursor)
        query = "INSERT INTO Specialty (department_id, code, name) VALUES (%s, %s, %s)"
        for i, specialty_name in enumerate(specialty_names):
            department_id = random.choice(existing_department_ids)
            specialty_code = f"Code_{i + 1}"
            cursor.execute(query, (department_id, specialty_code, specialty_name))

        # Получение существующих enrollee_ids
        existing_enrollee_ids = get_existing_enrollee_ids(cursor)
        if not existing_enrollee_ids:
            raise ValueError("Не найдено ни одного enrollee_id в таблице Enrollee")
        
        cursor.execute("SELECT document_type_id FROM Document_Type")
        existing_document_type_ids = [row['document_type_id'] for row in cursor.fetchall()]

        # Вставка данных в таблицу Document
        query = "INSERT INTO Document (enrollee_id, document_type_id) VALUES (%s, %s)"
        def docs():
            for enrollee_id in existing_enrollee_ids:
                document_count = random.randint(1, 3)
                for document_type_id in random.sample(existing_document_type_ids, document_count):
                    yield enrollee_id, document_type_id

        cursor.executemany(query, tuple(docs()))

        # Вставка данных в таблицу Achievment
        query = ("INSERT INTO Achievment (enrollee_id, subject_id, ege_result, inner_result)"
                 "VALUES (%s, %s, %s, %s)")
        def achievments():
            for enrollee_id in existing_enrollee_ids:
                for subject_id in random.sample(existing_subject_ids, random.randint(3, 5)):
                    ege_result = generate_random_result()
                    inner_result = generate_random_result()
                    yield enrollee_id, subject_id, ege_result, inner_result

        cursor.executemany(query, tuple(achievments()))

        # Вставка данных в таблицу Choice
        query = "INSERT INTO Choice (enrollee_id, specialty_id, priority_index) VALUES (%s, %s, %s)"
        def choices():
            for enrollee_id in existing_enrollee_ids:
                for j, specialty_id in enumerate(random.sample(specialty_ids, random.randint(1, 5))):
                    priority_index = j + 1
                    yield enrollee_id, specialty_id, priority_index

        cursor.executemany(query, tuple(choices()))

        # Вставка данных в таблицу Subject_Requirement
        query = ("INSERT INTO Subject_Requirement (subject_id, specialty_id, ege_minimal_result, inner_minimal_result)"
                 "VALUES (%s, %s, %s, %s)")
        for specialty_id in specialty_ids:
            for subject_id in random.sample(existing_subject_ids, random.randint(3, 4)):
                ege_minimal_result = generate_random_result()
                inner_minimal_result = generate_random_result()
                cursor.execute(query, (subject_id, specialty_id, ege_minimal_result, inner_minimal_result))

        # Заполнение таблицы de_count_tr
        query = """
            INSERT INTO de_count_tr (department_id, department_name, enrollee_count)
            SELECT d.department_id, d.name, COUNT(e.enrollee_id)
            FROM Department d
            LEFT JOIN Specialty s ON d.department_id = s.department_id
            LEFT JOIN Choice c ON s.specialty_id = c.specialty_id
            LEFT JOIN Enrollee e ON c.enrollee_id = e.enrollee_id
            WHERE e.enrollment_status_id = 1  -- Статус 'added'
            GROUP BY d.department_id, d.name
        """
        cursor.execute(query)

        connection.commit()
        print("Данные успешно заполнены и таблица de_count_tr обновлена.")

finally:
    if 'connection' in locals() and connection.open:
        connection.close()
        print("Соединение с базой данных закрыто.")
