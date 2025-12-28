package ru.spbstu.telematics.java;

class PotokBlokirov implements Runnable {
    private final Skany skany;

    public PotokBlokirov(Skany skany) {
        this.skany = skany;
    }

    @Override
    public void run() {
        System.out.println("[Блок. поток(3)] Действует!");
        skany.logDlyaBlokirov();
        System.out.println("[Блок. поток(3)] Завершен(");
    }
}
