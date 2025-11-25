plugins {
    id("java")
    id("application")
}
repositories {
    mavenCentral()
}
dependencies {
    implementation("org.seleniumhq.selenium:selenium-java:4.31.0")
    implementation("io.github.bonigarcia:webdrivermanager:6.0.1")
    // ваши прочие зависимости…
    testImplementation("org.testng:testng:7.8.0")
    testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.9.2")
}
application {
    mainClass.set("org.example.Main")
}
