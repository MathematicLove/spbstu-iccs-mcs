package ru.spbstu.telematics.java;

class RadioPotok implements Runnable {
    private final Skany skany;

    public RadioPotok(Skany skany) {
        this.skany = skany;
    }

    @Override
    public void run() {
        System.out.println("[Сканы. поток(2)] Действует!");
        skany.logDlyaSkanirov();
        System.out.println("[Сканы. поток(2)] Завершен(");
    }
}
