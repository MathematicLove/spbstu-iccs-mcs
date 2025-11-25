from dataclasses import dataclass
import database;

@dataclass
class PriceChange:
    date: date
    price: Decimal
    price_change: Decimal

def get_subscribed(telegram_handle):
    user_id = database_get_user_id(telegram_handle)
    item_ids = database_get_subscribed_item_ids(user_id)
    return [database_get_item_name(item_id) for item_id in item_ids]

def get_unsubscribed(telegram_handle):
    user_id = database_get_user_id(telegram_handle)
    item_ids = database_get_unsubscribed_item_ids(user_id)
    return [database_get_item_name(item_id) for item_id in item_ids]

def subscribe(telegram_handle, item_name):
    user_id = database_get_user_id(telegram_handle)
    item_id = database_get_item_id(item_name)
    database_add_subscription(user_id, item_name)

def unsubscribe(telegram_handle, item_name):
    user_id = database_get_user_id(telegram_handle)
    item_id = database_get_item_id(item_name)
    database_remove_subscription(user_id, item_name)

def get_report(user, n_days):
    user_id = database_get_user_id(telegram_handle)
    item_ids = database_get_subscribed_item_ids(user_id)
    price_changes = []
    for item_id in item_ids:
        item_name = database_get_item_name(item_id)
        price_changes[item_name] = []
        prices = database_get_prices_of_item_for_n_days(item_id, n_days)
        last_price = 0
        for price in prices:
            price_changes[item_name].append(
                PriceChange(price.date,
                            price.price,
                            price.price - last_price))
