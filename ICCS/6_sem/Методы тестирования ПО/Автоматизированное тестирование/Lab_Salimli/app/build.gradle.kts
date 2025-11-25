plugins {
    // Подключаем плагины Kotlin‑DSL‑способом
    id("java")
    id("application")
}

repositories {
    mavenCentral()
}

dependencies {
    // JUnit 5 для тестирования
    testImplementation("org.testng:testng:7.8.0")
    testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")
    // Selenium WebDriver
    // Launcher для JUnit Platform
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.9.2")
    // Зависимость для основного кода (пример: Guava)
    implementation("com.google.guava:guava:32.1.2-jre")
    implementation("org.seleniumhq.selenium:selenium-java:4.31.0")
    implementation("io.github.bonigarcia:webdrivermanager:6.0.1")
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

// Настройка таска test в Kotlin DSL
tasks.test {
    useTestNG {
        suites("src/test/resources/suite-for-hw2.xml")
    }
}

tasks.register<Test>("webDriverTests") {
    group = "verification"
    description = "Запуск WebDriver‑тестов из suite-for-hw2.xml"

    useTestNG {
        suites("src/test/resources/suite-for-hw2.xml")
    }
    testClassesDirs = sourceSets.test.get().output.classesDirs
    classpath = sourceSets.test.get().runtimeClasspath

    testLogging {
        events("PASSED", "FAILED", "SKIPPED")
    }
}



application {
    // Устанавливаем главный класс приложения
    mainClass.set("org.example.App")
}
