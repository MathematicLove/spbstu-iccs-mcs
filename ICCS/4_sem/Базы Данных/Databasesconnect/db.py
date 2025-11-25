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
    """Получает существующие enrollee_id из таблицы Enrollee."""
    cursor.execute("SELECT enrollee_id FROM Enrollee")
    return [row[0] for row in cursor.fetchall()]


def get_existing_department_ids(cursor):
    """Получает существующие department_id из таблицы Department."""
    cursor.execute("SELECT department_id FROM Department")
    return [row[0] for row in cursor.fetchall()]


ENROLLEE_COUNT = 200_000
    
connection = pymysql.connect(
    host='127.0.0.1',
    user='root',
    password='Ayzek123321',
    database='student_adm',
    port=3306,
    autocommit=False
)
try:
    if not connection.open:
        print("Not connected. Bye")
        exit(1)
    cursor = connection.cursor()

    first_names = ['Айзек', 'Петя', 'Виктор', 'Михаил', 'Ланита', 'Игорь', 'Светлана', 'Ирина', 'Ольга', 'Владимир']
    last_names = ['Салимли', 'Григорьев', 'Кузнецов', 'Капустин', 'Сильванович', 'Маликовн', 'Бакинец', 'Долгов', 'Непомнящих', 'Истов']
    middle_names = ['Петрович', 'Владимирович', 'Александрович', 'Геннадьевич', 'Михайлович', 'Павлович', 'Алексеевич', 'Викторович', 'Иванович', 'Рустамович']

    adrespoly = ['Политехническая 29', 'Гидротехников 2', 'Политехническая 21', 'Обручевых 1', 'Гжатская 4','Политехническая 34','Новороссийская 50','Верности 5','Гражданский пр. 7','Верности 4','Непокоренных 6']
    document_types = ['Паспорт', 'СНИЛС', 'Аттестат']
    department_names = ['ИКНК', 'ГИ', 'ФизМех', 'ИБСиБ', 'ИМПП', 'ИММиТ', 'ИЭ', 'ИСИ', 'ЯИ', 'ИФКиС', 'ИПМЭиТ']
    subject_names = ['Математика', 'Физика', 'Химия', 'Биология', 'English', 'История', 'География',
                     'Информатика', 'Экономика', 'Литература', 'Музыка']
    specialty_names = ['МКН', 'ПИ', 'ПМиФ', 'БФ', 'БИ', 'РиОС',
                       'ЗР', 'МС', 'ЯЭ', 'Эко']
    specialty_ids = range(1, len(specialty_names) + 1)

    # Вставка данных в таблицу Subject
    query = "INSERT INTO Subject (name) VALUES (%s)"
    for subject_name in subject_names:
        cursor.execute(query, (subject_name,))

    query = "INSERT INTO Enrollment_Status (name) VALUES (%s)"
    for status in ["added", "cancelled"]:
        cursor.execute(query, (status,))

    cursor.execute("SELECT enrollment_status_id FROM Enrollment_Status")
    existing_status_ids = [row[0] for row in cursor.fetchall()]
    cursor.execute("SELECT subject_id FROM Subject")
    existing_subject_ids = [row[0] for row in cursor.fetchall()]

    # Вставка данных в таблицу Enrollee
    query = ("INSERT INTO Enrollee (first_name, last_name, middle_name, birth_date, enrollment_status_id)"
             "VALUES (%s, %s, %s, %s, %s)")
    cursor.executemany(query, tuple(
        (first_name := generate_random_name(first_names),
         last_name := generate_random_name(last_names),
         middle_name := generate_random_name(middle_names),
         birth_date := generate_random_date(1980, 2005),
         status := random.choice(existing_status_ids)) for i in range(ENROLLEE_COUNT)
    ))

    # Вставка данных в таблицу Department
    query = "INSERT INTO Department (name, phone_number, address, headmaster_name) VALUES (%s, %s, %s, %s)"
    for i, dept_name in enumerate(department_names):
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
    for i in range(len(specialty_names)):  # Используем len(specialty_names), чтобы цикл не выходил за пределы списка
        # Выбор случайного существующего department_id
        department_id = random.choice(existing_department_ids)
        specialty_code = f"Code_{i + 1}"
        specialty_name = specialty_names[i]

        cursor.execute(query, (department_id, specialty_code, specialty_name))

    # Получение существующих enrollee_id из таблицы Enrollee
    existing_enrollee_ids = get_existing_enrollee_ids(cursor)
    if not existing_enrollee_ids:
        raise ValueError("No existing enrollee_id found in the Enrollee table")

    cursor.execute("SELECT document_type_id FROM Document_Type")
    existing_document_type_ids = [row[0] for row in cursor.fetchall()]

    # Вставка данных в таблицу Document
    query = "INSERT INTO Document (enrollee_id, document_type_id) VALUES (%s, %s)"
    def docs():
        for enrollee_id in existing_enrollee_ids:
            # Вставка для каждого абитуриента по 1-3 документа
            document_count = random.randint(1, 3)
            for document_type_id in random.sample(existing_document_type_ids, document_count):
                yield enrollee_id, document_type_id
    cursor.executemany(query, tuple(docs()))

    # Вставка данных в таблицу Achievment
    query = ("INSERT INTO Achievment (enrollee_id, subject_id, ege_result, inner_result)"
             "VALUES (%s, %s, %s, %s)")
    def achievments():
        for enrollee_id in existing_enrollee_ids:
            # Вставка для каждого абитуриента по 3-5 результатов испытаний
            for subject_id in random.sample(existing_subject_ids, random.randint(3, 5)):
                ege_result = generate_random_result()
                inner_result = generate_random_result()
                yield enrollee_id, subject_id, ege_result, inner_result
    cursor.executemany(query, tuple(achievments()))

    # Вставка данных в таблицу Choice
    query = "INSERT INTO Choice (enrollee_id, specialty_id, priority_index) VALUES (%s, %s, %s)"
    def choices():
        for enrollee_id in existing_enrollee_ids:
            # Вставка для каждого абитуриента по 1-5 выборов
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

    connection.commit()
    print("Данные успешно заполнены.")
finally:
    connection.close()
    print("Соединение с базой данных закрыто.")
