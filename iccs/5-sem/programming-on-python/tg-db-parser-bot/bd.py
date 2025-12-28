import sqlite3
import random
from datetime import date, timedelta

# Функция для инициализации базы данных и создания таблиц
def initialize_database():
    connection = sqlite3.connect("pricebot.db")
    cursor = connection.cursor()

    # Создаем таблицу пользователей
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        telegram_handle TEXT NOT NULL
    );
    """)

    # Создаем таблицу товаров
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL
    );
    """)

    # Создаем таблицу цен товаров
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS item_prices (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        id_item INTEGER NOT NULL,
        price REAL NOT NULL,
        date DATE NOT NULL,
        FOREIGN KEY (id_item) REFERENCES items (id)
    );
    """)

    # Создаем таблицу подписок
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS subscriptions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        id_user INTEGER NOT NULL,
        id_item INTEGER NOT NULL,
        FOREIGN KEY (id_user) REFERENCES users (id),
        FOREIGN KEY (id_item) REFERENCES items (id)
    );
    """)

    connection.commit()
    connection.close()
    print("База данных и таблицы успешно инициализированы.")

# Список реальных предметов
items_list = [
    "iPhone 14 Pro", "Samsung Galaxy S23", "Sony PlayStation 5", "MacBook Air M2",
    "Smart TV LG 55'", "Wireless Headphones Sony WH-1000XM5", "Electric Scooter Xiaomi",
    "Dyson Vacuum Cleaner", "Canon EOS R8 Camera", "Apple Watch Series 9",
    "Air Fryer Philips", "Nespresso Coffee Machine", "Nike Air Max 270",
    "Adidas Ultraboost", "North Face Jacket", "Levi's 501 Jeans", "Calvin Klein T-Shirt",
    "Lego Star Wars Set", "Monopoly Board Game", "Kindle Paperwhite",
    "Herman Miller Chair", "Ikea Desk", "Google Nest Thermostat", "Amazon Echo Dot",
    "KitchenAid Stand Mixer", "Tefal Pan Set", "Samsung SSD 1TB", "WD External Drive 2TB",
    "Logitech MX Master Mouse", "JBL Portable Speaker"
]

# Генерация целесообразных цен для категорий товаров
def generate_price(item_name):
    if "iPhone" in item_name or "Samsung Galaxy" in item_name:
        return random.randint(800, 1200)  # Смартфоны
    elif "PlayStation" in item_name or "MacBook" in item_name:
        return random.randint(1000, 2500)  # Электроника премиум класса
    elif "TV" in item_name:
        return random.randint(400, 1000)  # Телевизоры
    elif "Headphones" in item_name or "Speaker" in item_name:
        return random.randint(100, 500)  # Аудиотехника
    elif "Vacuum" in item_name or "Scooter" in item_name:
        return random.randint(200, 800)  # Бытовая техника / транспорт
    elif "Jeans" in item_name or "Jacket" in item_name or "Shoes" in item_name:
        return random.randint(50, 200)  # Одежда и обувь
    elif "Board Game" in item_name or "Lego" in item_name:
        return random.randint(20, 100)  # Игрушки и настольные игры
    elif "Chair" in item_name or "Desk" in item_name:
        return random.randint(150, 700)  # Мебель
    elif "Kitchen" in item_name or "Coffee Machine" in item_name or "Mixer" in item_name:
        return random.randint(50, 400)  # Бытовая техника
    else:
        return random.randint(20, 500)  # Прочее

# Создание предметов
def create_items():
    connection = sqlite3.connect("pricebot.db")
    cursor = connection.cursor()
    
    # Создаем предметы
    for item_name in items_list:
        cursor.execute("INSERT INTO items (name) VALUES (?)", (item_name,))
    
    connection.commit()
    connection.close()
    print("Реальные предметы добавлены в базу данных.")

# Создание цен для каждого предмета
def create_prices():
    connection = sqlite3.connect("pricebot.db")
    cursor = connection.cursor()
    
    # Получаем все ID предметов
    cursor.execute("SELECT id, name FROM items")
    items = cursor.fetchall()
    
    # Генерируем 10 цен за последние 14 дней для каждого предмета
    for item_id, item_name in items:
        for _ in range(10):
            # Случайная дата за последние 14 дней
            random_date = date.today() - timedelta(days=random.randint(0, 13))
            # Генерация целесообразной цены для предмета
            random_price = generate_price(item_name)
            cursor.execute(
                "INSERT INTO item_prices (id_item, price, date) VALUES (?, ?, ?)",
                (item_id, random_price, random_date.strftime('%Y-%m-%d'))
            )
    
    connection.commit()
    connection.close()
    print("Цены для каждого предмета добавлены в базу данных.")

# Основная функция для инициализации базы и добавления данных
def main():
    initialize_database()
    create_items()
    create_prices()

# Запуск программы
if __name__ == "__main__":
    main()
