package ru.spbstu.telematics.java;

import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

public class AppTest {
    @Test
    @DisplayName("Проверка запуска, взаимодействия и завершения без deadlock")
    void testNoDeadlockAndNoRaceConditions() throws InterruptedException {
        Skany skany = new Skany();
        Thread radioThread = new Thread(new RadioPotok(skany), "Сканы. поток(2)");
        Thread lockThread = new Thread(new PotokBlokirov(skany), "Блок. поток(3)");

        radioThread.start();
        lockThread.start();
        Thread.sleep(200);

        skany.pressOnOff();
        assertTrue(skany.isOn(), "Радио должно быть включено");

        skany.pressScan();
        Thread.sleep(300);
        assertTrue(skany.isScanning(), "Должно идти сканирование");

        skany.requestLock();
        Thread.sleep(300);
        assertTrue(skany.isLocked(), "Сканы должны быть заблокированы");

        skany.pressReset();
        Thread.sleep(300);
        assertTrue(skany.isLocked(), "Сканы всё ещё заблокированы");
        assertTrue(skany.isOn(), "Радио всё ещё включено");

        skany.requestUnlock();
        Thread.sleep(300);
        assertFalse(skany.isLocked(), "Сканы должны быть разблокированы");

        skany.pressReset();
        Thread.sleep(300);
        assertTrue(skany.isScanning(), "Снова идёт сканирование");

        skany.end();
        radioThread.join(2000);
        lockThread.join(2000);

        assertFalse(radioThread.isAlive(), "Поток сканирования должен завершиться");
        assertFalse(lockThread.isAlive(), "Поток блокировки должен завершиться");
    }
}
