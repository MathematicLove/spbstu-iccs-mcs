package com.algorithm.fxlab;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;
import javafx.stage.Stage;
import java.io.IOException;
public class LiveTheGame extends Application {
    @Override
    public void start(Stage stage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(LiveTheGame.class.getResource("hello-view.fxml"));
        Scene scene = new Scene(fxmlLoader.load());
        stage.setTitle(" 🖋Клеточный Автомат️✒️");
        stage.setScene(scene);
        stage.show();
        stage.setOnCloseRequest(event -> {
            event.consume();
            exit(stage);
        });
    }

    public void exit(Stage stage) {
        Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
        alert.setTitle("Стойте✋🏻");
        alert.setHeaderText("⚠️Вы нажали на выйти!⚠️");
        alert.setContentText("Вы уверены что хотите выйти?🤨");

        if (alert.showAndWait().get() == ButtonType.OK) {
            System.out.println("Удачи!");
            stage.close();
        }
    }

    public static void main(String[] args) {
        launch();
    }
}
