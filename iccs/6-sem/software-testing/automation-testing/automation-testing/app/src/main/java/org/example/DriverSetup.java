package org.example;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.safari.SafariDriver;

public class DriverSetup {
    protected static WebDriver driver;

    public static void initDriver() {
        driver = new SafariDriver();
    }

    public static void quitDriver() {
        if (driver != null) {
            driver.quit();
        }
    }
}
