package org.example;

import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import static org.testng.Assert.*;

public class GoogleTest {
    @BeforeClass
    public void setUp() {
        DriverSetup.initDriver();
    }

    @AfterClass
    public void tearDown() {
        DriverSetup.quitDriver();
    }

    @Test
    public void testGoogleTitle() {
        DriverSetup.driver.get("https://www.google.com");
        assertEquals(DriverSetup.driver.getTitle(), "Google");
    }
}
