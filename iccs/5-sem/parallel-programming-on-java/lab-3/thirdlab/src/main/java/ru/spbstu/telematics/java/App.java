package ru.spbstu.telematics.java;

import java.util.Scanner;

public class App {
    public static void main(String[] args) {
        Skany skany = new Skany();
        Thread radioThread = new Thread(new RadioPotok(skany), "Сканы. поток(2)");
        Thread lockThread = new Thread(new PotokBlokirov(skany), "Блок. поток(3)");
        radioThread.start();
        lockThread.start();
        Scanner scanner = new Scanner(System.in);

        printMenu();
        while (true) {
            System.out.print("Введите: ");
            String choice = scanner.nextLine().trim();
            switch (choice) {
                case "1":
                    System.out.println("[Основ. поток(1)] Нажата кнопка on/off");
                    skany.pressOnOff();
                    printSkanyState(skany);
                    waitForScan(skany);
                    break;
                case "2":
                    System.out.println("[Основ. поток(1)] Нажата кнопка scan");
                    skany.pressScan();
                    printSkanyState(skany);
                    waitForScan(skany);
                    break;
                case "3":
                    System.out.println("[Основ. поток(1)] Нажата кнопка reset");
                    skany.pressReset();
                    printSkanyState(skany);
                    waitForScan(skany);
                    break;
                case "4":
                    System.out.println("[Основ. поток(1)] Нажата кнопка lock/unlock");
                    if (skany.isLocked()) {
                        skany.requestUnlock();
                    } else {
                        skany.requestLock();
                    }
                    printSkanyState(skany);
                    waitForScan(skany);
                    break;
                case "5":
                    System.out.println("[Основ. поток(1)] Нажата кнопка end");
                    skany.end();
                    scanner.close();
                    try {
                        radioThread.join();
                        lockThread.join();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                    System.out.println("[Основ. поток(1)] До скорой встречи!");
                    System.exit(0);
                    break;
                default:
                    System.out.println("[Основ. поток(1)] Выберите из меню!");
                    waitForScan(skany);
                    break;
            }
            printMenu();
        }
    }

    private static void printMenu() {
        System.out.println("\n !-------------- РАДИО -------------!");
        System.out.println("1. on/off");
        System.out.println("2. scan (вниз)");
        System.out.println("3. reset (вверх)");
        System.out.println("4. lock/unlock");
        System.out.println("5. end");
    }

    private static void waitForScan(Skany skany) {
        if (skany.isScanning()) {
            skany.waitForScanEnd();
        }
    }

    private static void printSkanyState(Skany skany) {
        boolean on = skany.isOn();
        double freq = skany.getFrequency();
        boolean scanning = skany.isScanning();
        boolean scanningUp = skany.isScanningUp();
        boolean locked = skany.isLocked();

        StringBuilder sb = new StringBuilder();
        sb.append("Сканы - ").append(on ? "вкл." : "выкл.");
        sb.append(" | Частота: ").append(String.format("%.1f", freq)).append(" мгц");
        if (scanning) {
            sb.append(" | Сканирование");
            sb.append(" | Направление: ").append(scanningUp ? "вверх" : "вниз");
        }
        if (on) {
            if (locked) {
                sb.append(" | Разблок.");
            } else {
                sb.append(" | Заблок.");
            }
        }
        System.out.println("[Основ. поток(1)] Состояние: " + sb.toString());
    }
}
