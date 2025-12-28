module Main where
import Lib

main :: IO ()
main = do
    putStrLn "Выберите действие:"
    putStrLn ""
    putStrLn "1. Проверить ссылку"
    putStrLn ""
    putStrLn "2. Сгенерировать валидную ссылку"
    putStrLn ""
    putStrLn "3. Посмотреть шаблон ссылки"
    putStrLn ""
    putStrLn "4. Выход"
    putStrLn ""
    putStrLn "Введите номер и нажмите Enter:"
    choice <- getLine
    
    case choice of
        "1" -> do
            putStrLn ""
            putStrLn "Введите ссылку для проверки:"
            putStrLn ""
            input <- getLine
            checkYandexDiskLink input
            main
        "2" -> do
            putStrLn ""
            link <- generateValidLink
            putStrLn $ "Сгенерированная ссылка: " ++ link
            putStrLn ""
            main
        "3" -> do
            putStrLn ""
            putStrLn $ "Шаблон /d/: " ++ "https://disk.yandex.ru/d/..." 
            putStrLn $ "Шаблон /clients/: " ++ "https://yadi.sk/d/..." 
            putStrLn $ "Шаблон /clients/disk/: " ++ "https://disk.yandex.ru/clients/disk/..." 
            putStrLn ""
            main
        "4" -> putStrLn "Пока!"
        _ -> do
            putStrLn ""
            putStrLn ""
            putStrLn "Неверный выбор, попробуйте снова"
            putStrLn ""
            putStrLn ""
            main