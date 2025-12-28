from dataclasses import dataclass
from datetime import date, timedelta
from decimal import Decimal
import sqlite3

@dataclass
class Price:
    item_id: int
    date: date
    price: Decimal

def database_init():
    connection = sqlite3.connect("pricebot.db")
    return connection

def database_get_user_id(telegram_handle):
    connection = database_init()
    cursor = connection.cursor()
    cursor.execute("SELECT id FROM users WHERE telegram_handle = ?", (telegram_handle,))
    result = cursor.fetchone()
    connection.close()
    return result[0] if result else None

def database_get_item_id(item_name):
    connection = database_init()
    cursor = connection.cursor()
    cursor.execute("SELECT id FROM items WHERE name = ?", (item_name,))
    result = cursor.fetchone()
    connection.close()
    return result[0] if result else None

def database_get_subscribed_item_ids(user_id):
    connection = database_init()
    cursor = connection.cursor()
    cursor.execute("SELECT id_item FROM subscriptions WHERE id_user = ?", (user_id,))
    result = cursor.fetchall()
    connection.close()
    return [row[0] for row in result]

def database_get_unsubscribed_item_ids(user_id):
    connection = database_init()
    cursor = connection.cursor()
    cursor.execute("SELECT id FROM items WHERE id NOT IN (SELECT id_item FROM subscriptions WHERE id_user = ?)", (user_id,))
    result = cursor.fetchall()
    connection.close()
    return [row[0] for row in result]

def database_get_item_name(item_id):
    connection = database_init()
    cursor = connection.cursor()
    cursor.execute("SELECT name FROM items WHERE id = ?", (item_id,))
    result = cursor.fetchone()
    connection.close()
    return result[0] if result else None

def database_add_subscription(user_id, item_name):
    connection = database_init()
    cursor = connection.cursor()
    item_id = database_get_item_id(item_name)
    if item_id:
        cursor.execute("INSERT INTO subscriptions (id_user, id_item) VALUES (?, ?)", (user_id, item_id))
        connection.commit()
    connection.close()

def database_remove_subscription(user_id, item_name):
    connection = database_init()
    cursor = connection.cursor()
    item_id = database_get_item_id(item_name)
    if item_id:
        cursor.execute("DELETE FROM subscriptions WHERE id_user = ? AND id_item = ?", (user_id, item_id))
        connection.commit()
    connection.close()

def database_get_prices_of_item_for_n_days(item_id, n_days):
    connection = database_init()
    cursor = connection.cursor()
    start_date = (date.today() - timedelta(days=n_days)).strftime('%Y-%m-%d')
    cursor.execute("SELECT date, price FROM item_prices WHERE id_item = ? AND date >= ?", (item_id, start_date))
    result = cursor.fetchall()
    connection.close()
    return [Price(item_id=item_id, date=row[0], price=Decimal(row[1])) for row in result]

def database_add_user(telegram_handle):
    """
    Добавляет нового пользователя в таблицу users.
    Если пользователь уже существует, возвращает его ID.

    :param telegram_handle: Telegram-имя пользователя (например, '@user').
    :return: ID пользователя.
    """
    connection = database_init()
    cursor = connection.cursor()
    
    # Проверяем, существует ли пользователь
    user_id = database_get_user_id(telegram_handle)
    if user_id:
        connection.close()
        return user_id

    # Добавляем нового пользователя
    cursor.execute("INSERT INTO users (telegram_handle) VALUES (?)", (telegram_handle,))
    connection.commit()

    # Получаем ID нового пользователя
    user_id = cursor.lastrowid
    connection.close()

    return user_id
