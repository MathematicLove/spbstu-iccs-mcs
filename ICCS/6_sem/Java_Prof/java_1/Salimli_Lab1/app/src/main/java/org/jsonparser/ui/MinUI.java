package org.jsonparser.ui;

public class MinUI {
    private static final int LENGTH = 40;

    public void start(String title) {
        System.out.println();
        System.out.println();
        for (int i = 0; i < LENGTH; i++) {
            System.out.print("\\");
        }
        System.out.print(" " + title + " ");
        for (int i = 0; i < LENGTH; i++) {
            System.out.print("/");
        }
        System.out.println();
        System.out.println();
    }

    public void end(String title) {
        System.out.println();
        int total = LENGTH * 2 + title.length() + 2;
        for (int i = 0; i < total; i++) {
            System.out.print("=");
        }
        System.out.println();
        System.out.println();
    }
}
