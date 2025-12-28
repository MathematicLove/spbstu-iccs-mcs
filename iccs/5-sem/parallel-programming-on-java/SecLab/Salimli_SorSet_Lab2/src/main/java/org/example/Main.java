package org.example;
import java.util.InputMismatchException;
import java.util.Scanner;
import java.util.NoSuchElementException;

public class Main {
    public static void main(String[] args) {
        BestSSet<Integer> mySet = new BestSSet<>();
        Scanner scanner = new Scanner(System.in);
        System.out.println("💥Super Sorted Set💥");
        while (true) {
            System.out.println("\nВведите цифру из меню:");
            System.out.println("1| Добавить элемент ➕");
            System.out.println("2| Удалить элемент ➖");
            System.out.println("3| Есть ли элемент в мн.ве? ❓");
            System.out.println("4| 1-ый элемент 1️⃣");
            System.out.println("5| Последний элемент 🔚");
            System.out.println("6| Мощность множества #️⃣");
            System.out.println("7| Очистить все 🧹");
            System.out.println("8| Вывести все множества 🙀");
            System.out.println("9| Выйти❌");

            System.out.print("Введите номер действия: ");
            int choice;
            try {
                choice = scanner.nextInt();
            } catch (InputMismatchException e) {
                System.out.println("Ошибка: Введите число из меню!");
                scanner.nextLine();
                continue;
            }

            switch (choice) {
                case 1 -> {
                    System.out.print("Введите что добавить (Int): ");
                    try {
                        int element = scanner.nextInt();
                        if (mySet.add(element)) {
                            System.out.println("Элемент " + element + " добавлен");
                        } else {
                            System.out.println("Элемент " + element + " уже есть!");
                        }
                    } catch (InputMismatchException e) {
                        System.out.println("Ошибка: Только Int!!!");
                        scanner.nextLine();
                    }
                }
                case 2 -> {
                    System.out.print("Введите что удалить: ");
                    try {
                        int element = scanner.nextInt();
                        if (mySet.remove(element)) {
                            System.out.println("Элемент " + element + " удален");
                        } else {
                            System.out.println("Элемента " + element + "нету в мн.ве (");
                        }
                    } catch (InputMismatchException e) {
                        System.out.println("Ошибка: Только Int!!!.");
                        scanner.nextLine();
                    }
                }
                case 3 -> {
                    System.out.print("Введите элемент для проверки: ");
                    try {
                        int element = scanner.nextInt();
                        if (mySet.contains(element)) {
                            System.out.println("Элемент " + element + " есть!");
                        } else {
                            System.out.println("Элемента " + element + " нету!");
                        }
                    } catch (InputMismatchException e) {
                        System.out.println("Ошибка: Только Int!!!");
                        scanner.nextLine();
                    }
                }
                case 4 -> {
                    try {
                        System.out.println("Первый элемент множества: " + mySet.first());
                    } catch (NoSuchElementException e) {
                        System.out.println("Упс, тут пусто!");
                    }
                }
                case 5 -> {
                    try {
                        System.out.println("Последний элемент множества: " + mySet.last());
                    } catch (NoSuchElementException e) {
                        System.out.println("Упс, тут пусто!");
                    }
                }
                case 6 -> System.out.println("Мощность мн-ва: " + mySet.size());
                case 7 -> {
                    mySet.clear();
                    System.out.println("Почищено!");
                }
                case 8 -> {
                    System.out.println("Элементы множества:");
                    for (Integer value : mySet) {
                        System.out.println(value);
                    }
                }
                case 9 -> {
                    System.out.println("До свидания!");
                    scanner.close();
                    return;
                }
                default -> System.out.println("Ошибка: Введите числа из меню!");
            }
        }
    }
}
