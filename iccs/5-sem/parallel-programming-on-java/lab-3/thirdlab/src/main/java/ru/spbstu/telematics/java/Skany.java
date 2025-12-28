package ru.spbstu.telematics.java;

import java.util.TreeSet;

class Skany {
    // сделал volatile что б если одно значение меняется в одном потоке,
    // другой поток, который читает это значение, не увидит старого результата
    private volatile boolean isOn = false;
    private volatile double frequency = 100.0;
    private volatile boolean scanning = false;
    private volatile boolean end = false;
    private volatile boolean scanningUp = false;
    private volatile boolean locked = false;
    private volatile boolean lockRequested = false;
    private volatile boolean unlockRequested = false;
    private volatile boolean actionBlocked = false;
    private final Object monitor = new Object();
    private final double UPPER_BOUND = 108.0;
    private final double LOWER_BOUND = 80.0;
    private final double STEP = 0.5;
    private final TreeSet<Double> discoveredStations = new TreeSet<>();

    public void pressOnOff() {
        synchronized (monitor) {
            if (!isOn) {
                isOn = true;
                frequency = 100.0;
                scanning = false;
                scanningUp = false;
                System.out.println("[Сканы] Включено!");
            } else {
                isOn = false;
                scanning = false;
                scanningUp = false;
                System.out.println("[Сканы] Выключено");
            }
            monitor.notifyAll();
        }
    }

    public void pressScan() {
        synchronized (monitor) {
            if (isOn) {
                if (locked) {
                    actionBlocked = true;
                } else {
                    scanningUp = false;
                    scanning = true;
                    System.out.println("[Сканы] Сканирование вниз от " + frequency + " МГц!");
                }
            } else {
                System.out.println("[Сканы] Упс! Кажись радио выкл.!");
            }
            monitor.notifyAll();
        }
    }

    public void pressReset() {
        synchronized (monitor) {
            if (isOn) {
                if (locked) {
                    actionBlocked = true;
                } else {
                    scanningUp = true;
                    scanning = true;
                    System.out.println("[Сканы] Сканирование вверх от " + frequency + " Мгц!");
                }
            } else {
                System.out.println("[Сканы] Упс! Кажись радио выкл.!");
            }
            monitor.notifyAll();
        }
    }

    public void requestLock() {
        synchronized (monitor) {
            lockRequested = true;
            monitor.notifyAll();
        }
    }

    public void requestUnlock() {
        synchronized (monitor) {
            unlockRequested = true;
            monitor.notifyAll();
        }
    }

    public void end() {
        synchronized (monitor) {
            end = true;
            System.out.println("[Сканы] Завершение работы");
            monitor.notifyAll();
        }
    }

    public void logDlyaSkanirov() {
        while (!isEnd()) {
            synchronized (monitor) {
                while (!end && (!isOn || !scanning || locked)) {
                    try {
                        monitor.wait();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
                if (end) break;
            }
            try {
                Thread.sleep((long) (Math.random() * 400 + 200));
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            synchronized (monitor) {
                if (end || locked || !isOn) {
                    scanning = false;
                    monitor.notifyAll();
                    continue;
                }
                Double nextStation = scanningUp
                        ? discoveredStations.higher(frequency)
                        : discoveredStations.lower(frequency);
                if (nextStation != null) {
                    frequency = nextStation;
                    scanning = false;
                    foundStation(frequency, "(ранняя находка)");
                } else {
                    double newFreq = scanningUp ? frequency + STEP : frequency - STEP;
                    if ((scanningUp && newFreq > UPPER_BOUND) || (!scanningUp && newFreq < LOWER_BOUND)) {
                        frequency = scanningUp ? UPPER_BOUND : LOWER_BOUND;
                        scanning = false;
                        foundStation(frequency, scanningUp ? "(верхняя граница)" : "(нижняя граница)");
                    } else {
                        frequency = newFreq;
                        if (Math.random() > 0.9) {
                            scanning = false;
                            foundStation(frequency, " - новая станция");
                        }
                    }
                }
                if (!scanning) {
                    monitor.notifyAll();
                }
            }
        }
        System.out.println("[Сканы. поток(2)] Завершен(");
    }

    private void foundStation(double freq, String reason) {
        System.out.println("[Сканы] Станция нашлась " + freq + " Мгц " + reason);
        discoveredStations.add(freq);
    }

    public void waitForScanEnd() {
        synchronized (monitor) {
            while (!end && scanning) {
                try {
                    monitor.wait();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

    public void logDlyaBlokirov() {
        while (!isEnd()) {
            synchronized (monitor) {
                while (!end && !lockRequested && !unlockRequested && !actionBlocked) {
                    try {
                        monitor.wait();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
                if (end) break;
                if (lockRequested) {
                    locked = true;
                    lockRequested = false;
                    System.out.println("[Блокировка] Сканы заблокировано");
                }
                if (unlockRequested) {
                    locked = false;
                    unlockRequested = false;
                    System.out.println("[Блокировка] Разблокировано");
                    monitor.notifyAll();
                }
                if (actionBlocked) {
                    System.out.println("[Блокировка] Упс! Кажись радио заблокировано!");
                    actionBlocked = false;
                }
            }
        }
        System.out.println("[Блок. поток(3)] Завершен(");
    }

    public double getFrequency() {
        return frequency;
    }

    public boolean isOn() {
        return isOn;
    }

    public boolean isEnd() {
        return end;
    }

    public boolean isScanning() {
        return scanning;
    }

    public boolean isScanningUp() {
        return scanningUp;
    }

    public boolean isLocked() {
        return locked;
    }
}
