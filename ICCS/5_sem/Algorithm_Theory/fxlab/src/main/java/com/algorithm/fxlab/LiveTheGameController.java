package com.algorithm.fxlab;

import javafx.animation.PauseTransition;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Label;
import javafx.scene.layout.AnchorPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.stage.Stage;
import javafx.scene.control.TextField;
import javafx.event.ActionEvent;
import javafx.util.Duration;


import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class LiveTheGameController {
    @FXML
    private boolean isRunning = false;
    @FXML
    private TextField tGridW;
    @FXML
    private TextField Titer;
    @FXML
    private Label checking;
    @FXML
    private AnchorPane gridPane;

    private Rectangle[][] cells;
    private int gridSize;
    private int iterations;
    private boolean isGridInitialized = false;
    private Map<String, Boolean> truthTable = new HashMap<>();
    private Random random = new Random();

    @FXML
    public void initialize() {
        System.out.println("gridPane initialized: " + (gridPane != null));
        loadTruthTable();
    }

    private void loadTruthTable() {
        truthTable.put("00000", false);
        truthTable.put("00001", false);
        truthTable.put("00010", false);
        truthTable.put("00011", false);
        truthTable.put("00100", false);
        truthTable.put("00101", false);
        truthTable.put("00110", false);
        truthTable.put("00111", false);
        truthTable.put("01000", true);
        truthTable.put("01001", false);
        truthTable.put("01010", true);
        truthTable.put("01011", true);
        truthTable.put("01100", false);
        truthTable.put("01101", true);
        truthTable.put("01110", true);
        truthTable.put("01111", false);
        truthTable.put("10000", true);
        truthTable.put("10001", true);
        truthTable.put("10010", true);
        truthTable.put("10011", false);
        truthTable.put("10100", false);
        truthTable.put("10101", true);
        truthTable.put("10110", false);
        truthTable.put("10111", false);
        truthTable.put("11000", false);
        truthTable.put("11001", false);
        truthTable.put("11010", true);
        truthTable.put("11011", false);
        truthTable.put("11100", false);
        truthTable.put("11101", false);
        truthTable.put("11110", false);
        truthTable.put("11111", false);
    }

    private void createGrid(boolean randomFill) {
        gridPane.getChildren().clear();
        cells = new Rectangle[gridSize][gridSize];
        isGridInitialized = true;
        double cellSize = Math.min(gridPane.getWidth() / gridSize, gridPane.getHeight() / gridSize);

        for (int i = 0; i < gridSize; i++) {
            for (int j = 0; j < gridSize; j++) {
                Rectangle cell = new Rectangle(cellSize, cellSize);
                cell.setFill(randomFill && random.nextBoolean() ? Color.DARKBLUE : Color.LIGHTGRAY);
                cell.setStroke(Color.WHITE);
                cell.setOnMouseClicked(event -> toggleCellState(cell));

                cells[i][j] = cell;
                gridPane.getChildren().add(cell);
                cell.setLayoutX(i * cellSize);
                cell.setLayoutY(j * cellSize);
            }
        }
        gridPane.widthProperty().addListener((obs, oldVal, newVal) -> resizeCells());
        gridPane.heightProperty().addListener((obs, oldVal, newVal) -> resizeCells());
    }
    private void resizeCells() {
        if (cells == null || gridSize == 0) return;

        double cellSize = Math.min(gridPane.getWidth() / gridSize, gridPane.getHeight() / gridSize);
        for (int i = 0; i < gridSize; i++) {
            for (int j = 0; j < gridSize; j++) {
                Rectangle cell = cells[i][j];
                cell.setWidth(cellSize);
                cell.setHeight(cellSize);
                cell.setLayoutX(i * cellSize);
                cell.setLayoutY(j * cellSize);
            }
        }
    }

    private void toggleCellState(Rectangle cell) {
        cell.setFill(cell.getFill() == Color.LIGHTGRAY ? Color.DARKBLUE : Color.LIGHTGRAY);
    }

    @FXML
    private boolean isFirstRun = true;

    @FXML
    public void pressedRUN(ActionEvent event) {
        try {
            int newGridSize = Integer.parseInt(tGridW.getText().trim());
            if(newGridSize > 200){
                checking.setText("⚠️Пожалейте компьютер⚠️");
            }
            else if (newGridSize <= 0) {
                checking.setText("Ширина сетки должна быть > 0 😡");
                return;
            }

            if (gridSize != newGridSize || !isGridInitialized) {
                gridSize = newGridSize;
                createGrid(false);
                isFirstRun = true;
            }
            isRunning = true;

            if (isFirstRun) {
                if (Titer.getText().isEmpty()) {
                    checking.setText("Сетка сгенерирована 🥳");
                    isRunning = false;
                } else {
                    iterations = Integer.parseInt(Titer.getText().trim());
                    if (iterations == 0 || iterations < 0) {
                        checking.setText("Итерации должны быть > 0❗️");
                        isRunning = false;
                        return;
                    }
                    performIterationsWithDelay(iterations);
                }
                isFirstRun = false;
            } else {
                if (Titer.getText().isEmpty()) {
                    iterations = 1;
                    performIterationsWithDelay(iterations);
                } else {
                    iterations = Integer.parseInt(Titer.getText().trim());
                    if (iterations == 0) {
                        checking.setText("Итерации должны быть > 0❗️");
                        isRunning = false;
                        return;
                    }

                    performIterationsWithDelay(iterations);
                }
            }
        } catch (NumberFormatException e) {
            checking.setText("Введите целые числа для шир. и итер.❗️");
        }
    }
    
    private void performIterationsWithDelay(int remainingIterations) {
        if (!isRunning) return;

        PauseTransition pause = new PauseTransition(Duration.seconds(0.5));
        pause.setOnFinished(e -> {
            applyRules();

            if (remainingIterations > 1 && isRunning) {
                performIterationsWithDelay(remainingIterations - 1);
            } else {
                isRunning = false;
                checking.setText("Завершено!");
            }
        });
        pause.play();
    }

    private void applyRules() {
        boolean[][] newStates = new boolean[gridSize][gridSize];
        for (int x = 0; x < gridSize; x++) {
            for (int y = 0; y < gridSize; y++) {
                String neighborsState = getNeighborsState(x, y);
                newStates[x][y] = truthTable.getOrDefault(neighborsState, false);
            }
        }
        for (int i = 0; i < gridSize; i++) {
            for (int j = 0; j < gridSize; j++) {
                cells[i][j].setFill(newStates[i][j] ? Color.DARKBLUE : Color.LIGHTGRAY);
            }
        }
    }
    //красиво
    private String getNeighborsState(int x, int y) {
        int center = cells[x][y].getFill() == Color.DARKBLUE ? 1 : 0;
        int up = (y > 0) ? (cells[x][y - 1].getFill() == Color.DARKBLUE ? 1 : 0) : 1;
        int down = (y < gridSize - 1) ? (cells[x][y + 1].getFill() == Color.DARKBLUE ? 1 : 0) : 1;
        int left = (x > 0) ? (cells[x - 1][y].getFill() == Color.DARKBLUE ? 1 : 0) : 1;
        int right = (x < gridSize - 1) ? (cells[x + 1][y].getFill() == Color.DARKBLUE ? 1 : 0) : 1;
        return "" + center + up + down + left + right;
    }
//не красиво (
//    private String getNeighborsState(int x, int y) {
//        int left = (x > 0) ? (cells[x - 1][y].getFill() == Color.DARKBLUE ? 1 : 0) : 0;
//        int right = (x < gridSize - 1) ? (cells[x + 1][y].getFill() == Color.DARKBLUE ? 1 : 0) : 0;
//        int up = (y > 0) ? (cells[x][y - 1].getFill() == Color.DARKBLUE ? 1 : 0) : 0;
//        int down = (y < gridSize - 1) ? (cells[x][y + 1].getFill() == Color.DARKBLUE ? 1 : 0) : 0;
//        int center = cells[x][y].getFill() == Color.DARKBLUE ? 1 : 0;
//
//        return "" + left + right + up + down + center;
//    }

    @FXML
    public void pressedClear(ActionEvent event) {
        isRunning = false;
        if (cells != null) {
            for (int i = 0; i < gridSize; i++) {
                for (int j = 0; j < gridSize; j++) {
                    cells[i][j].setFill(Color.LIGHTGRAY);
                }
            }
            checking.setText("Сетка очищена");
        }
    }


    @FXML
    public void pressedGenerate(ActionEvent actionEvent) {
        try {
            gridSize = Integer.parseInt(tGridW.getText().trim());
            createGrid(true);
            checking.setText("Результат: ");
            System.out.println("Случайная ы");
        } catch (NumberFormatException e) {
            checking.setText("Введите ширину сетки!");
            System.out.println("Ошибка ввода: ширина должна быть целочислена!");
        }
    }

    @FXML
    public void exit(ActionEvent actionEvent) {
        Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
        alert.setTitle("Стойте✋🏻");
        alert.setHeaderText("⚠️Вы нажали на выйти!⚠️");
        alert.setContentText("Вы уверены что хотите выйти?🤨");

        if (alert.showAndWait().get() == ButtonType.OK) {
            System.out.println("Удачи!");
            Stage stage = (Stage) gridPane.getScene().getWindow();
            stage.close();
        }
    }
}