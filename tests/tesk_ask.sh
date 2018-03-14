#!/bin/sh
# Тестовый скрипт для тестирования тестового задания
# Пароль с количеством символов меньше 8 и логин с количеством символов меньше 2 считается не валидным

# Кейсы для проверки
# 1. Регистрация пользователя -> /user/registration
echo     "\nУспешная регистрация нового пользователя"
echo     "user anton: "
curl -H "Content-Type: application/json" -X POST -d '{"login":"anton","password":"12345678"}' \
      http://localhost:8080/user/registration
sleep 2
echo     "\nuser anton1: "
curl -H "Content-Type: application/json" -X POST -d '{"login":"anton1","password":"12345678"}' \
      http://localhost:8080/user/registration
sleep 2

echo     "\nПользователь уже зарегистрирован"
echo     "\nuser anton1: "
curl -H "Content-Type: application/json" -X POST -d '{"login":"anton1","password":"12345678"}' \
      http://localhost:8080/user/registration
sleep 2

echo     "\nНевалидный запрос"
echo     "\nrequest: "
curl -H "Content-Type: application/json" -X POST -d '{"login":"anton1"}' \
      http://localhost:8080/user/registration
sleep 2
echo     "\nrequest: "
curl -H "Content-Type: application/json" -X POST -d '{"login":"anton1","password":"123"}' \
      http://localhost:8080/user/registration
sleep 2

# 2. Авторизация пользователя -> /user/auth
echo     "\nУспешная авторизация пользователя, ответом возвращается токен"
echo     "\nuser anton1: "
curl -H "Content-Type: application/json" -X GET -d '{"login":"anton1","password":"12345678"}' http://localhost:8080/user/auth
sleep 2

echo     "\nПользователь не найден"
echo     "\nuser vadim: "
curl -H "Content-Type: application/json" -X GET -d '{"login":"vadim","password":"12345678"}' http://localhost:8080/user/auth
sleep 2

echo     "\nНеверный пароль"
echo     "\nuser anton: "
curl -H "Content-Type: application/json" -X GET -d '{"login":"anton","password":"12345678910"}' http://localhost:8080/user/auth
sleep 2

echo     "\nНевалидный запрос"
echo     "\nuser anton: "
curl -H "Content-Type: application/json" -X GET -d '{"login":"anton"}' http://localhost:8080/user/auth
sleep 2

# 3. Смена пароля пользователя -> /user/{login}
echo     "\nУспешная смена пароля пользователя"
echo     "\nuser anton1: "
curl --cookie "LoginId=anton1" -H "Content-Type: application/json" -X PUT -d '{"old_password":"12345678","new_password":"123456789"}' \
     http://localhost:8080/user/anton1
sleep 2

echo     "\nПопытка отправки неавторизованного запроса"
echo     "\nuser anton1: "
curl -H "Content-Type: application/json" -X PUT -d '{"old_password":"12345678","new_password":"123456789"}' \
     http://localhost:8080/user/anton1
sleep 2

echo     "\nПопытка смены чужого пароля"
echo     "\nuser anton1: "
curl --cookie "LoginId=anton1" -H "Content-Type: application/json" -X PUT -d '{"old_password":"12345678","new_password":"123456789"}' \
     http://localhost:8080/user/maxim
sleep 2

echo     "\nНеправильный old_password"
echo     "\nuser anton1: "
curl --cookie "LoginId=anton1" -H "Content-Type: application/json" -X PUT -d '{"old_password":"1234567810","new_password":"123456789"}' \
     http://localhost:8080/user/anton1
sleep 2

echo     "\nНевалидный запрос"
echo     "\nuser anton1: "
curl --cookie "LoginId=anton1" -H "Content-Type: application/json" -X PUT -d '{"old_password":"12345678"}' \
     http://localhost:8080/user/anton1
sleep 2

# 4. Получение списка пользователей -> /user/
echo     "\nУспешное получение списка зарегистрированных пользователей"
echo     "\nList: "
curl --cookie "LoginId=anton1" -X GET http://localhost:8080/user/
sleep 2

echo     "\nПопытка отправки неавторизованного запроса"
echo     "\nnoauth request"
curl -X GET http://localhost:8080/user/
sleep 2

echo "\nend script, work done\n"