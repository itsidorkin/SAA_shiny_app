# Веб-приложение для ML (SAA)

## Как пользоваться
 
### Шаг 1 Подготовка
 
Перед запуском необходимо установить пакеты используемые в приложении: 
 
     install.packages(c("ggplot2", "ggalt", "ggforce", "DT", "plotly", "GGally", "shiny", "devtools"))
     devtools::install_github('nikrodis/SAA', force = T)
 
### Шаг 2 Запуск

     shiny::runGitHub("SAA_shiny_app", "nikrodis")
     
## Наборы данных
     
 Ознакомиться c приложением можно на [этих](https://github.com/nikrodis/SAA_shiny_app/tree/master/DataSets) наборах данных.
 
### 1 вариант
 
 Скачать и в приложении выбрать file
 
 ![](https://puu.sh/FV9JP/0512923c87.jpg)
 
### 2 вариант
 
 Скопировать ссылку (например [эту](https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/HistData/Galton.csv)) и вставить в url
 
 ![](https://puu.sh/FV9B8/cdde2c90b8.jpg)
 
 **Имейте в виду, пока в url находится текст, загрузка из файла - не производится.**
 
## Внимание
 
 Следите за разделителем элементов (sep) и разделителем десятичного знака (dec). Приложение будет сыпать ошибками если что-то выбрано неправильно
 
 **Алгоритмы могут работать медленно** (проблема моих кривых рук), особенно на больших объемах данных (особенно DBSCAN). Поэтому учтите это, когда меняете параметры или данные. Иногда на странице с графиками может выводиться ошибка, когда ее на самом деле уже нет. Это связанно со временем работы алгоритмов. Пока алгоритм не завершит работу, окно вывода не обновится, а значит ошибка будет продолжать висеть. 
