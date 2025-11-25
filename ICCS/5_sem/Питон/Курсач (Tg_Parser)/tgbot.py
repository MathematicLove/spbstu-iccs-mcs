from datetime import date, timedelta
from decimal import Decimal
from aiogram import Bot, Dispatcher, Router, types
from aiogram.types import (
    ReplyKeyboardMarkup,
    KeyboardButton,
    CallbackQuery,
    ReplyKeyboardRemove
)
from aiogram.fsm.storage.memory import MemoryStorage
from aiogram.fsm.state import State, StatesGroup
from aiogram.fsm.context import FSMContext
from aiogram.filters import Command
from aiogram.utils.keyboard import InlineKeyboardBuilder
from database import (
    database_get_user_id,
    database_get_item_id,
    database_get_subscribed_item_ids,
    database_get_unsubscribed_item_ids,
    database_get_item_name,
    database_add_subscription,
    database_remove_subscription,
    database_get_prices_of_item_for_n_days,
    database_add_user
)
API_TOKEN = "7900491190:AAEBcRlxhdrZCELhnzvWOvsN8HVbNTKBK2A"

bot = Bot(token=API_TOKEN)
storage = MemoryStorage()
dp = Dispatcher(storage=storage)
router = Router()
dp.include_router(router)

class Form(StatesGroup):
    waiting_for_days = State()

# Сегодня в меню:
main_menu = ReplyKeyboardMarkup(
    keyboard=[
        [KeyboardButton(text="Подписаться")],
        [KeyboardButton(text="Отписаться")],
        [KeyboardButton(text="Получить изменения за n дней")],
        [KeyboardButton(text="Мои подписки")]
    ],
    resize_keyboard=True
)

def create_inline_buttons_for_subscribe(item_names, back_text):
    builder = InlineKeyboardBuilder()
    for name in item_names:
        builder.button(text=name, callback_data=f"subscribe:{name}")
    builder.button(text=back_text, callback_data="back_to_main")
    return builder.as_markup()

def create_inline_buttons_for_unsubscribe(item_names, back_text):

    builder = InlineKeyboardBuilder()
    for name in item_names:
        builder.button(text=name, callback_data=f"unsubscribe:{name}")
    builder.button(text=back_text, callback_data="back_to_main")
    return builder.as_markup()

def create_inline_buttons_for_report(item_names, back_text):
    builder = InlineKeyboardBuilder()
    for name in item_names:
        builder.button(text=name, callback_data=f"report:{name}")
    builder.button(text=back_text, callback_data="back_to_main")
    return builder.as_markup()

async def get_user_db_id(message: types.Message) -> int:
    telegram_handle = str(message.from_user.id)
    user_id = database_add_user(telegram_handle)   
    return user_id

@router.message(Command("start"))
async def start_command(message: types.Message):
    user_db_id = await get_user_db_id(message)
    await message.answer(
        "Добро пожаловать в бот отслеживания цен! Вы успешно зарегистрированы.",
        reply_markup=main_menu
    )

@router.message(lambda message: message.text in [
    "Подписаться",
    "Отписаться",
    "Получить изменения за n дней",
    "Мои подписки"
])
async def main_menu_handler(message: types.Message, state: FSMContext):
    """
    Здесь используется уже зарегистрированный пользователь!!!!
    (регистрация происходит при /start !!!!!!!!!).
    """
    telegram_handle = str(message.from_user.id)
    user_db_id = database_get_user_id(telegram_handle)
    if not user_db_id:
        await message.answer("Сначала введите /start для регистрации!!!", reply_markup=main_menu)
        return

    if message.text == "Подписаться":
        # Получаем ID товаров на которые НЕ подписан!!!!!!
        unsub_item_ids = database_get_unsubscribed_item_ids(user_db_id)
        if not unsub_item_ids:
            await message.answer("Вы уже подписаны на все товары или нет доступных товаров.", reply_markup=main_menu)
            return

        item_names = []
        for item_id in unsub_item_ids:
            name = database_get_item_name(item_id)
            if name:
                item_names.append(name)

        if item_names:
            await message.answer(
                "Выберите товар для подписки:",
                reply_markup=create_inline_buttons_for_subscribe(item_names, "Вернуться в главное меню")
            )
        else:
            await message.answer("Нет доступных для подписки товаров.", reply_markup=main_menu)

    elif message.text == "Отписаться":
        sub_item_ids = database_get_subscribed_item_ids(user_db_id)
        if not sub_item_ids:
            await message.answer("Вы не подписаны ни на один товар.", reply_markup=main_menu)
            return
        
        item_names = []
        for item_id in sub_item_ids:
            name = database_get_item_name(item_id)
            if name:
                item_names.append(name)

        if item_names:
            await message.answer(
                "Выберите товар для отписки:",
                reply_markup=create_inline_buttons_for_unsubscribe(item_names, "Вернуться в главное меню")
            )
        else:
            await message.answer("Вы не подписаны ни на один товар.", reply_markup=main_menu)

    elif message.text == "Получить изменения за n дней":
        sub_item_ids = database_get_subscribed_item_ids(user_db_id)
        if not sub_item_ids:
            await message.answer("Вы не подписаны ни на один товар.", reply_markup=main_menu)
            return
        
        item_names = []
        for item_id in sub_item_ids:
            name = database_get_item_name(item_id)
            if name:
                item_names.append(name)

        if item_names:
            await message.answer(
                "Выберите товар для отчёта:",
                reply_markup=create_inline_buttons_for_report(item_names, "Вернуться в главное меню")
            )
        else:
            await message.answer("Вы не подписаны ни на один товар.", reply_markup=main_menu)

    elif message.text == "Мои подписки":
        sub_item_ids = database_get_subscribed_item_ids(user_db_id)
        if not sub_item_ids:
            await message.answer("Вы не подписаны ни на один товар.", reply_markup=main_menu)
            return
        
        item_names = []
        for item_id in sub_item_ids:
            name = database_get_item_name(item_id)
            if name:
                item_names.append(name)

        subscribed_items = "\n".join(item_names) if item_names else "Нет подписок."
        await message.answer(f"Вы подписаны на следующие товары:\n{subscribed_items}", reply_markup=main_menu)

@router.callback_query()
async def inline_button_handler(callback_query: CallbackQuery, state: FSMContext):
    telegram_handle = str(callback_query.from_user.id)
    user_db_id = database_get_user_id(telegram_handle)
    if not user_db_id:
        await callback_query.message.answer("Сначала введите /start для регистрации.", reply_markup=main_menu)
        return

    data = callback_query.data

    if data == "back_to_main":
        await callback_query.message.edit_text("Возвращение в главное меню.", reply_markup=None)
        await callback_query.message.answer("Главное меню:", reply_markup=main_menu)
        return

    if data.startswith("subscribe:"):
        item_name = data.split("subscribe:")[1]
        database_add_subscription(user_db_id, item_name)
        await callback_query.message.edit_text(f"Вы успешно подписались на {item_name}!", reply_markup=None)
        await callback_query.message.answer("Главное меню:", reply_markup=main_menu)
        return

    if data.startswith("unsubscribe:"):
        item_name = data.split("unsubscribe:")[1]
        database_remove_subscription(user_db_id, item_name)
        await callback_query.message.edit_text(f"Вы успешно отписались от {item_name}!", reply_markup=None)
        await callback_query.message.answer("Главное меню:", reply_markup=main_menu)
        return

    if data.startswith("report:"):
        item_name = data.split("report:")[1]
        await state.update_data(selected_item=item_name)
        await callback_query.message.edit_text(f"Введите количество дней для товара: {item_name}", reply_markup=None)

        await callback_query.message.answer(
            "Введите количество дней:",
            reply_markup=types.ReplyKeyboardMarkup(
                keyboard=[[KeyboardButton(text="Вернуться в главное меню")]],
                resize_keyboard=True
            )
        )
        await state.set_state(Form.waiting_for_days)

@router.message(Form.waiting_for_days)
async def handle_days_input(message: types.Message, state: FSMContext):
    telegram_handle = str(message.from_user.id)
    user_db_id = database_get_user_id(telegram_handle)
    if not user_db_id:
        await message.answer("Сначала введите /start для регистрации.", reply_markup=main_menu)
        await state.clear()
        return

    data = await state.get_data()
    selected_item = data.get("selected_item", "")

    if message.text == "Вернуться в главное меню":
        await state.clear()
        await message.answer("Главное меню:", reply_markup=main_menu)
        return

    if message.text.isdigit():
        n = int(message.text)
        item_id = database_get_item_id(selected_item)
        if item_id is None:
            await message.answer(f"Товар {selected_item} не найден в базе.", reply_markup=main_menu)
            await state.clear()
            return
        prices = database_get_prices_of_item_for_n_days(item_id, n)
        if not prices:
            report = f"За последние {n} дней нет данных о цене для {selected_item}."
        else:
            lines = [f"Дата: {p.date}, цена: {p.price}" for p in prices]
            report = (
                f"Изменения цен за последние {n} дней для {selected_item}:\n"
                + "\n".join(lines)
            )
        await message.answer(report, reply_markup=main_menu)
        await state.clear()
    else:
        await message.answer(
            "Введите корректное число дней или выберите 'Вернуться в главное меню'.",
            reply_markup=types.ReplyKeyboardMarkup(
                keyboard=[[KeyboardButton(text="Вернуться в главное меню")]],
                resize_keyboard=True
            )
        )

async def main():
    print("Бот запускается...")
    await dp.start_polling(bot)
if __name__ == "__main__":
    import asyncio
    asyncio.run(main())
