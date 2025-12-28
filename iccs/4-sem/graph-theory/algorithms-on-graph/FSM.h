#pragma once
#include <iostream>
#include <string>

using namespace std;

enum class State { Start, Running, Paused, Stopped, Exit};

class FiniteStateMachine {

public:
    // Текущее состояние автомата
    State currentState = State::Start;
    void processInput(char input) {
        switch (currentState) {
            case State::Start:
                if (input == 's') {
                    std::cout << "Запуск" << std::endl;
                    currentState = State::Running;
                } else if (input == 'p') {
                    std::cout << "Пауза" << std::endl;
                    currentState = State::Paused;
                } else if (input == 'r') {
                    std::cout << "Заново" << std::endl;
                    currentState = State::Start;
                }
                break;
            case State::Running:
                if (input == 'p') {
                    std::cout << "Пауза" << std::endl;
                    currentState = State::Paused;
                } else if (input == 's') {
                    std::cout << "Остоновка" << std::endl;
                    currentState = State::Stopped;
                }
                break;
            case State::Paused:
                if (input == 'p') {
                    std::cout << "Уже на паузе" << std::endl;
                } else if (input == 'r') {
                    std::cout << "Продолжкнно" << std::endl;
                    currentState = State::Running;
                }
                break;
            case State::Stopped:
                if (input == 's') {
                    std::cout << "Запуск" << std::endl;
                    currentState = State::Running;
                } else if (input == 'p') {
                    std::cout << "Пауза" << std::endl;
                    currentState = State::Paused;
                }
            case State::Exit:
                if(input == 'e'){
                    cout << "Выход" << endl;
                    currentState = State::Exit;
                    break;
                }
                break;
        }
    }
};


