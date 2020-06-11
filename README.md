Веб-приложение для SAA  
 
 ## Как пользоваться
 
 ### Запуск
 
     install.packages("shiny")
     shiny::runGitHub("SAA_shiny_app", "nikrodis")
     
 ### Наборы данных
     
 Оознакомиться c приложением можно на [этих](https://github.com/nikrodis/SAA_shiny_app/tree/master/DataSets) наборах данных.
 
 #### 1 вариант
 
 Скачать и в приложении выбрать file
 
 ![](https://puu.sh/FV9JP/0512923c87.jpg)
 
 #### 2 вариант
 
 Скопировать ссылку (например [эту](https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/HistData/Galton.csv)) и вставить в url
 
 ![](https://puu.sh/FV9B8/cdde2c90b8.jpg)
 
 ## Внимание
 
 Следите за разделителем элементов (sep) и разделителем десятичного знака (dec). Приложение будет писать ошибки если что-то выбранно неправильно
