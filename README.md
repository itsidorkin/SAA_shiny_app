### k-means?

Суть метода в разделении множества на кластеры таким образом, чтобы объекты внутри кластеров были более схожи, чем вне его. Мерой сходства является расстояние между объектами. Обычно для нахождения расстояний используют Евклидову метрику. Чем меньше расстояние, тем более схожи объекты.

### DBSCAN?

Если K-means опирается на расстояния между объектами, то DBSCAN на плотность между ними. Идея метода в том, чтобы плотность точек внутри кластера была заметно выше, чем снаружи. Для определения плотности используются два параметра: eps - радиус соседства и minpts - предельное число объектов (соседей) внутри радиуса eps. Если вокруг точки А в радиусе eps находятся не менее minpts объектов, то образуется кластер. Если точка А не имеет объектов в радиусе eps, то такая точка - шум.

## Как пользоваться?

### Шаг 0

Убедитесь, что у вас установлены язык [R](https://cran.r-project.org/bin/windows/base) и [Rstudio](https://rstudio.com/products/rstudio/download/#download)
 
### Шаг 1 Подготовка 
 
Перед запуском необходимо установить пакеты используемые в приложении: 
 
     install.packages(c("ggplot2", "ggalt", "ggforce", "DT", "plotly", "shiny", "devtools", "shinycssloaders", "shinydashboard"))
     devtools::install_github('itsidorkin/SAA', force = T)
 
### Шаг 2 Запуск

     shiny::runGitHub("SAA_shiny_app", "itsidorkin")
     
## Наборы данных
     
 Ознакомиться c приложением можно на [этих](https://github.com/itsidorkin/SAA_shiny_app/tree/master/DataSets) наборах данных.
 
### 1 вариант
 
 Скачать и в приложении выбрать file
 
 <img src=readmeSrc/1.jpg width=250>
 
### 2 вариант
 
 Скопировать ссылку (например [эту](https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/HistData/Galton.csv)) и вставить в url
 
 <img src=readmeSrc/2.jpg width=250>
 
 **Имейте в виду, пока в url находится текст, загрузка из файла не осуществляется.**
 
## Внимание
 
 **Следите за разделителем элементов (sep) и разделителем десятичного знака (dec).** Иногда на странице с графиками могут выводиться ошибки. Проверте еще раз, правильно ли вы указали sep и dec.
 
 **Приложение может работать медленно**. Особенно на больших объемах данных (особенно DBSCAN). Поэтому учтите это, когда меняете параметры или данные. 
 
 **Не используйте данные, в которых больше 1000 строк.** Это может занять больше времени, чем обычно.
 
## Пример использования
 
 <img src=readmeSrc/3.gif>

## Некоторые результаты:
| kmean                                                                              | dbscan                                                                              |
|------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|
| <img src=readmeSrc/KM1.jpeg width=450>                                             | <img src=readmeSrc/DBS1.jpeg width=450>                                             |
| <img src=readmeSrc/KM2.jpeg width=450>                                             | <img src=readmeSrc/DBS2.jpeg width=450>                                             |
| <img src=readmeSrc/KM3.jpeg width=450><br/><img src=readmeSrc/KM3-2.gif width=450> | <img src=readmeSrc/DBS3.jpeg width=450>                                             |
| <img src=readmeSrc/KM4.gif width=450>                                              | <img src=readmeSrc/DBS4.gif width=450><br/><img src=readmeSrc/DBS5.gif width=450>   |
| <img src=readmeSrc/KM6.jpg width=450>                                              | <img src=readmeSrc/DBS6.png width=450><br/><img src=readmeSrc/DBS6-2.gif width=450> |
