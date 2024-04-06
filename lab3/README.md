# Практическая работа №3
Солдатенкова К.А.

## Цель работы

1.  Изучить возможности технологии Apache Arrow для обработки и анализ
    больших данных.

2.  Получить навыки применения Arrow совместно с языком программирования
    R.

3.  Получить навыки анализа метаинфомации о сетевом трафике.

4.  Получить навыки применения облачных технологий хранения, подготовки
    и анализа данных: Yandex Object Storage, Rstudio Server.

## Ход работы

``` r
library(arrow)
```

    Warning: пакет 'arrow' был собран под R версии 4.3.3


    Присоединяю пакет: 'arrow'

    Следующий объект скрыт от 'package:utils':

        timestamp

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.0     ✔ readr     2.1.4
    ✔ ggplot2   3.4.4     ✔ stringr   1.5.0
    ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::duration() masks arrow::duration()
    ✖ dplyr::filter()       masks stats::filter()
    ✖ dplyr::lag()          masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

### Доступ к датасету

``` r
dir.create("data", showWarnings = FALSE)
```

``` r
curl::multi_download("https://storage.yandexcloud.net/arrow-datasets/tm_data.pqt", "data/tm_data.pqt",resume = TRUE)
```

    # A tibble: 1 × 10
      success status_code resumefrom url    destfile error type  modified           
      <lgl>         <int>      <dbl> <chr>  <chr>    <chr> <chr> <dttm>             
    1 TRUE            200          0 https… "C:\\Us… <NA>  appl… 2024-02-19 15:25:05
    # ℹ 2 more variables: time <dbl>, headers <list>

### Чтение датасета

``` r
data <- read_parquet("data/tm_data.pqt", as_data_frame = FALSE)
```

``` r
glimpse(data)
```

    Table
    105,747,730 rows x 5 columns
    $ timestamp <double> 1.578326e+12, 1.578326e+12, 1.578326e+12, 1.578326e+12, 1.5…
    $ src       <string> "13.43.52.51", "16.79.101.100", "18.43.118.103", "15.71.108…
    $ dst       <string> "18.70.112.62", "12.48.65.39", "14.51.30.86", "14.50.119.33…
    $ port       <int32> 40, 92, 27, 57, 115, 92, 65, 123, 79, 72, 123, 123, 22, 118…
    $ bytes      <int32> 57354, 11895, 898, 7496, 20979, 8620, 46033, 1500, 979, 103…
    Call `print()` for full schema details

### Задание 1: Надите утечку данных из Вашей сети

Важнейшие документы с результатами нашей исследовательской деятельности
в области создания вакцин скачиваются в виде больших заархивированных
дампов. Один из хостов в нашей сети используется для пересылки этой
информации – он пересылает гораздо больше информации на внешние ресурсы
в Интернете, чем остальные компьютеры нашей сети. Определите его
IP-адрес.

``` r
out <- data %>% select(src, dst, bytes) %>% filter(!str_detect(dst, '1[2-4].*')) %>% select(src, bytes) %>% group_by(src) %>% summarize(sum_bytes = sum(bytes)) %>% filter(sum_bytes == max(sum_bytes))
```

    Warning: Expression sum_bytes == max(sum_bytes) not supported in Arrow; pulling
    data into R

``` r
out |> collect()
```

    # A tibble: 1 × 2
      src           sum_bytes
      <chr>           <int64>
    1 13.37.84.125 5765792351

### Задание 2: Надите утечку данных 2

Другой атакующий установил автоматическую задачу в системном
планировщике cron для экспорта содержимого внутренней wiki системы. Эта
система генерирует большое количество трафика в нерабочие часы, больше
чем остальные хосты. Определите IP этой системы. Известно, что ее IP
адрес отличается от нарушителя из предыдущей задачи.

``` r
data_filter <- data %>% select(timestamp, src, dst, bytes) %>% mutate(trafic = (str_detect(src, '1[2-4].*') & !str_detect(dst, '1[2-4].*')),time = hour(as_datetime(timestamp/1000))) %>% filter(trafic == TRUE, time >= 0 & time <= 24) %>% group_by(time) %>% summarise(trafictime = n()) %>% arrange(desc(trafictime))
```

``` r
data_filter |> collect()
```

    # A tibble: 24 × 2
        time trafictime
       <int>      <int>
     1    18    3305646
     2    23    3305086
     3    16    3304767
     4    22    3304743
     5    19    3303518
     6    21    3303328
     7    17    3301627
     8    20    3300709
     9    13     124857
    10     0     124851
    # ℹ 14 more rows

Определяем рабочие часы (учитывая нагрузку на трафик, рабочее время:
16:00-24:00)

``` r
data_last <- data %>% mutate(time = hour(as_datetime(timestamp/1000))) %>% filter(!str_detect(src, "^13.37.84.125")) %>% filter(str_detect(src, '1[2-4].*'))  %>% filter(!str_detect(dst, '1[2-4].*'))  %>% filter(time >= 1 & time <= 15) %>% group_by(src) %>% summarise("sum" = sum(bytes)) %>% select(src,sum)
```

``` r
data_last <- data_last %>% arrange(desc(sum)) %>% head(1)
data_last |> collect()
```

    # A tibble: 1 × 2
      src               sum
      <chr>           <int>
    1 12.55.77.96 191826796

### Задание 3: Надите утечку данных 3

Еще один нарушитель собирает содержимое электронной почты и отправляет в
Интернет используя порт, который обычно используется для другого типа
трафика. Атакующий пересылает большое количество информации используя
этот порт, которое нехарактерно для других хостов, использующих этот
номер порта. Определите IP этой системы. Известно, что ее IP адрес
отличается от нарушителей из предыдущих задач.

``` r
ip1 <- data %>% filter(!str_detect(src, "^13.37.84.125")) %>% filter(!str_detect(src, "^12.55.77.96")) %>% filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>% filter(!str_detect(dst, "^12.") | !str_detect(dst, "^13.") | !str_detect(dst, "^14."))  %>% select(src, bytes, port)
```

``` r
ip2 <-ip1 %>%  group_by(port) %>% summarise("mean"=mean(bytes), "max"=max(bytes), "sum" = sum(bytes)) %>%  mutate("raz"= max-mean)  %>% filter(raz!=0, raz>170000)
ip2 |> collect()
```

    # A tibble: 1 × 5
       port   mean    max         sum     raz
      <int>  <dbl>  <int>     <int64>   <dbl>
    1    37 33348. 209402 48192673159 176054.

``` r
ip3 <- ip1  %>% filter(port==37) %>% group_by(src) %>% summarise("mean"=mean(bytes)) %>% filter(mean>37543) %>% select(src)
ip3 |> collect()
```

    # A tibble: 1 × 1
      src        
      <chr>      
    1 13.46.35.35

## Вывод

В ходе работы мы ознакомились с применением облачных технологий
хранения, подготовки и анализа данных, а также проанализировали
метаинформацию о сетевом трафике.
