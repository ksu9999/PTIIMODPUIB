# Практическая работа №5
Солдатенкова К.А.

## Цель Работы

1\. Изучить возможности СУБД Clickhouse для обработки и анализ больших
данных

2\. Получить навыки применения Clickhouse совместно с языком
программирования R

3\. Получить навыки анализа метаинфомации о сетевом трафике

4\. Получить навыки применения облачных технологий хранения, подготовки
и анализа данных: Managed Service for ClickHouse, Rstudio Server.

## Ход работы

## Импорт данных

``` r
host <- Sys.getenv("HOST")
user <- Sys.getenv("USER")
pass <- Sys.getenv("PASS")
```

``` r
library(arrow, warn.conflicts = FALSE)
```

    Warning: пакет 'arrow' был собран под R версии 4.3.3

``` r
library(tidyverse, warn.conflicts = FALSE)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::duration() masks arrow::duration()
    ✖ dplyr::filter()       masks stats::filter()
    ✖ dplyr::lag()          masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ClickHouseHTTP, warn.conflicts = FALSE)
```

    Warning: пакет 'ClickHouseHTTP' был собран под R версии 4.3.3

``` r
library(DBI)
```

``` r
connection <- dbConnect(
   ClickHouseHTTP::ClickHouseHTTP(), 
   host="rc1d-sbdcf9jd6eaonra9.mdb.yandexcloud.net",
                      port=8443,
                      user="student24dwh",
                      password = "DiKhuiRIVVKdRt9XON",
                      db = "TMdata",
   https=TRUE, ssl_verifypeer=FALSE)
data<- dbGetQuery(connection, "SELECT * FROM data")
```

### Задание 1. Найдите утечку данных из Вашей сети

Важнейшие документы с результатами нашей исследовательской деятельности
в области создания вакцин скачиваются в виде больших заархивированных
дампов. Один из хостов в нашей сети используется для пересылки этой
информации – он пересылает гораздо больше информации на внешние ресурсы
в Интернете, чем остальные компьютеры нашей сети. Определите его
IP-адрес

``` r
ip_a1 <- dbGetQuery(connection, "
  SELECT src, sum(bytes) as sum
  FROM data
  WHERE (src LIKE '12.%' OR src LIKE '13.%' OR src LIKE '14.%')
      AND (dst not LIKE '12.%' and dst not LIKE '13.%' and dst not LIKE '14.%')
    GROUP BY src
    ORDER BY sum DESC
    LIMIT 1
")
ip_a1
```

               src         sum
    1 13.37.84.125 10625497574

### Задание 2. Найдите утечку данных 2

Другой атакующий установил автоматическую задачу в системном
планировщике cron для экспорта содержимого внутренней wiki системы. Эта
система генерирует большое количество трафика в нерабочие часы, больше
чем остальные хосты. Определите IP этой системы. Известно, что ее IP
адрес отличается от нарушителя из предыдущей задачи

Рабочие часы мы определили в ходе прошлых практических работ:  
Учитывая нагрузку на трафик, рабочее время: 16:00-23:00

``` r
ip_a2 <- data %>%
      select(timestamp, src, dst, bytes) %>%
      mutate(trafic = (str_detect(src, "^((12|13|14)\\.)") & !str_detect(dst, "^((12|13|14)\\.)")),time = hour(as_datetime(timestamp/1000))) %>%
      filter(trafic == TRUE, time >= 0 & time <= 24) %>% group_by(time) %>%
      summarise(trafictime = n()) %>% arrange(desc(trafictime))
ip_a2 |> collect()
```

    # A tibble: 24 × 2
        time trafictime
       <int>      <int>
     1    16    4490576
     2    22    4489703
     3    18    4489386
     4    23    4488093
     5    19    4487345
     6    21    4487109
     7    17    4483578
     8    20    4482712
     9    13     169617
    10     7     169241
    # ℹ 14 more rows

``` r
ip_a2_2 <- data %>% mutate(time = hour(as_datetime(timestamp/1000))) %>% 
  filter(!str_detect(src, "^13.37.84.125")) %>% 
  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>%
  filter(!grepl('^1[2-4].*', dst))  %>%
  filter(time >= 0 & time <= 15) %>% 
  group_by(src) %>% summarise("sum" = sum(bytes)) %>%
  arrange(desc(sum)) %>% select(src,sum) %>% slice(1)
ip_a2_2 |> collect()
```

    # A tibble: 1 × 2
      src               sum
      <chr>           <int>
    1 12.55.77.96 289566918

### Задание 3. Найдите утечку данных 3

Еще один нарушитель собирает содержимое электронной почты и отправляет в
Интернет используя порт, который обычно используется для другого типа
трафика. Атакующий пересылает большое количество информации используя
этот порт,которое нехарактерно для других хостов, использующих этот
номер порта. Определите IP этой системы. Известно, что ее IP адрес
отличается от нарушителей из предыдущих задач

``` r
ip_a3 <- data %>% filter(!str_detect(src, "^13.37.84.125")) %>% 
  filter(!str_detect(src, "^12.55.77.96")) %>% 
  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>%
  filter(!str_detect(dst, "^12.") | !str_detect(dst, "^13.") | !str_detect(dst, "^14."))  %>% select(src, bytes, port) 


ip_a3_1 <-ip_a3 %>%  group_by(port) %>% summarise("mean"=mean(bytes), "max"=max(bytes), "sum" = sum(bytes)) %>% 
  mutate("Raz"= max-mean)  %>% filter(Raz!=0, Raz>170000)

ip_a3_1 |> collect()
```

    # A tibble: 1 × 5
       port   mean    max         sum     Raz
      <int>  <dbl>  <int>       <dbl>   <dbl>
    1    37 33348. 209402 48192673159 176054.

``` r
ip_a3_2 <- ip_a3  %>% filter(port==37) %>% group_by(src) %>% 
  summarise("mean"=mean(bytes)) %>% filter(mean>37543) %>% select(src)
ip_a3_2 |> collect()
```

    # A tibble: 1 × 1
      src        
      <chr>      
    1 13.46.35.35

### Задание 4. Обнаружение канала управления

Зачастую в корпоротивных сетях находятся ранее зараженные системы,
компрометация которых осталась незамеченной. Такие системы генерируют
небольшое количество трафика для связи с панелью управления бот-сети, но
с одинаковыми параметрами – в данном случае с одинаковым номером порта.

Какой номер порта использует бот-панель для управления ботами?

``` r
n_port4 <- dbGetQuery(connection, "
  SELECT port, AVG(bytes) - min(bytes) as sume
  FROM data
  WHERE (select (AVG(bytes) - min(bytes)) from data) IS NOT NULL
    GROUP BY port
    ORDER BY sume
    LIMIT 8,1
  
")
n_port4
```

      port      sume
    1  124 0.3271471

### Задание 5. Обнаружение P2P трафика

Иногда компрометация сети проявляется в нехарактерном трафике между
хостами в локальной сети, который свидетельствует о горизонтальном
перемещении (lateral movement).

В нашей сети замечена система, которая ретранслирует по локальной сети
полученные от панели управления бот-сети команды, создав таким образом
внутреннюю пиринговую сеть.

Какой уникальный порт используется этой бот сетью для внутреннего
общения между собой?

``` r
n_port5 <- dbGetQuery(connection, "
  SELECT port, max(bytes) - min(bytes) as sume
  FROM data
  WHERE (src LIKE '12.%' OR src LIKE '13.%' OR src LIKE '14.%')
      AND (dst  LIKE '12.%' or dst  LIKE '13.%' or dst  LIKE '14.%')
    GROUP BY port
    ORDER BY sume DESC
    LIMIT 1
")
n_port5
```

      port   sume
    1  115 202766

### Задание 6. Чемпион малвари

Нашу сеть только что внесли в списки спам-ферм. Один из хостов сети
получает множество команд от панели C&C, ретранслируя их внутри сети. В
обычных условиях причин для такого активного взаимодействия внутри сети
у данного хоста нет.

Определите IP такого хоста.

``` r
ip_a6 <- dbGetQuery(connection, "
  SELECT src, COUNT(src) as sume
  FROM data
  WHERE (src LIKE '12.%' OR src LIKE '13.%' OR src LIKE '14.%')
      AND (dst  LIKE '12.%' or dst  LIKE '13.%' or dst  LIKE '14.%')
    GROUP BY src
    ORDER BY sume DESC
    LIMIT 1
")
ip_a6
```

              src  sume
    1 13.42.70.40 65109

### Задание 7. Скрытая бот-сеть

В нашем трафике есть еще одна бот-сеть, которая использует очень большой
интервал подключения к панели управления. Хосты этой продвинутой
бот-сети не входят в уже обнаруженную нами бот-сеть.

Какой порт используется продвинутой бот-сетью для коммуникации?

``` r
n_port7 <- dbGetQuery(connection, "
  SELECT port, AVG(timestamp) as time
  FROM data
   WHERE (src LIKE '12.%' OR src LIKE '13.%' OR src LIKE '14.%')
      AND (dst  LIKE '12.%' or dst  LIKE '13.%' or dst  LIKE '14.%')
  GROUP BY port
  ORDER BY time DESC
  LIMIT 1
")
n_port7
```

      port         time
    1   83 1.578562e+12

### Задание 8. Внутренний сканнер

Одна из наших машин сканирует внутреннюю сеть.

Что это за система?

``` r
ip_a8 <- dbGetQuery(connection, "
  SELECT src, AVG(timestamp) as time, count(DISTINCT dst) as coun
  FROM data
  WHERE (src LIKE '12.%' OR src LIKE '13.%' OR src LIKE '14.%')
      AND (dst  LIKE '12.%' or dst  LIKE '13.%' or dst  LIKE '14.%')
    GROUP BY src
    ORDER BY time 
    LIMIT 1
")
ip_a8
```

              src         time coun
    1 12.35.59.94 1.578513e+12  200

## Вывод

В ходе работы мы изучили возможности СУБД Clickhouse для обработки и
анализ больших данных, получили навыки применения Clickhouse совместно с
языком программирования R, получили навыки анализа метаинфомации о
сетевом трафике, а ткаже получили навыки применения облачных технологий
хранения, подготовки и анализа данных: Managed Service for ClickHouse,
Rstudio Server.
