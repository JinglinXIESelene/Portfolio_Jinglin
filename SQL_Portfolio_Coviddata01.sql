-- data source: https://ourworldindata.org/covid-deaths
-- basic queries to get an initial understanding of the CovidDeaths data coving six continent and most of countries
SELECT top 100 * 
from PorfolioProject.dbo.CovidDeath

-- we got 6 continents and 248 countries here
SELECT COUNT(distinct continent) as continent_ct, COUNT(distinct location) as nation_ct
from PorfolioProject.dbo.CovidDeath

-- check the hightest daily_new_cases rate and hightest daily_new_deaths rate for each country
WITH DailyCasesAndDeaths
AS 
(
    SELECT continent, location, max(population) as population,
    max(new_cases) as daily_new_cases,
    max(new_deaths) as daily_new_deaths,
    (max(new_cases)*1.0/max(population))*100  as daily_new_cases_percentage, 
    (max(new_deaths)*1.0/max(population))*100  as daily_new_deaths_percentage
from PorfolioProject.dbo.CovidDeath
WHERE continent is not null
group by continent, location
--order by continent, daily_new_cases_percentage DESC
)

-- check the country with highest daily_new_cases_percentage for each continent: here's the six countries: North Macedonia,Papua New Guinea
--,Haiti,Venezuela,Niger,North Korea
SELECT continent, location, daily_new_cases_percentage
FROM(
SELECT *,
    ROW_NUMBER() OVER(partition by continent order by daily_new_cases_percentage) as rank
from DailyCasesAndDeaths
where daily_new_cases_percentage is not null) as a
where rank = 1


-- calculate the percentage change of new_cases for each month compared to the previous month for every country
-- aggregate by month
WITH laged_monthly_new_cases as 
(SELECT *,
    LAG(monthly_new_cases,1) OVER(partition by continent, location order by month) as prev_monthly_new_cases
from (
SELECT continent, location, CONCAT(YEAR(date), '-', month(date)) as 'month', sum(new_cases) as monthly_new_cases
from PorfolioProject.dbo.CovidDeath
WHERE continent is not null
group by continent, location, CONCAT(YEAR(date),'-', month(date))
) as b
--order by continent, location, month
),

monthly_percent_change as (SELECT *, 
    (CASE WHEN prev_monthly_new_cases = 0 THEN 0
    ELSE cast((monthly_new_cases*1.0/prev_monthly_new_cases)*100 as DECIMAL(18,2)) END) as change_in_percentage
from laged_monthly_new_cases
--order by continent, location, month
)


-- now we find the Find the consecutive months with the worst outbreaks in each country, 
-- consider the period is 'worst' if the growth rate of new cases per month exceeds 100%
-- first save the above results as views for later usage
CREATE VIEW Covid_monthly_newcases_lag
AS
    SELECT *,
    LAG(monthly_new_cases,1) OVER(partition by continent, location order by month) as prev_monthly_new_cases
from (
SELECT continent, location, CONCAT(YEAR(date), '-', month(date)) as 'month', sum(new_cases) as monthly_new_cases
from PorfolioProject.dbo.CovidDeath
WHERE continent is not null
group by continent, location, CONCAT(YEAR(date),'-', month(date))
) as b


CREATE VIEW Covid_monthly_newcases_change_precentage
AS 
    SELECT *, 
    (CASE WHEN prev_monthly_new_cases = 0 THEN 0
    ELSE cast((monthly_new_cases*1.0/prev_monthly_new_cases)*100 as DECIMAL(18,2)) END) as change_in_percentage
from Covid_monthly_newcases_lag

---------
WITH number_consecutive_months as (SELECT continent, location, replace(month,'-','') as numeric_month, epidemic_situation,
    ROW_NUMBER() OVER(PARTITION by continent, location order by month) as rank
from(
SELECT *, (CASE WHEN change_in_percentage > 100 then 'worst' ELSE 'not so bad' END) as epidemic_situation
from Covid_monthly_newcases_change_precentage) as c 
where epidemic_situation = 'worst'
)

SELECT continent, location, min(numeric_month) as worst_situation_start, max(numeric_month) as worst_situation_end, count(*) as count_month_in_total
from number_consecutive_months
group by continent, location, numeric_month-rank
HAVING(count(*)>1) -- exclude the Individual month
order by continent, location, worst_situation_start

-- then we calculate the proportion of deaths to total population by month, year
-- consider the deaths rate is high if yearly death rate per million exceeds 0.25,only for year 2020
SELECT continent, location as country_with_high_deaths_rate,
    (CASE WHEN deaths_yearly_proportion_per_million > 0.25 then 'yes'
    ELSE 'no' END) as high_deaths_rate
FROM(
    SELECT continent, location, left(month,4) as per_year, sum(monthly_new_deaths_per_million) as deaths_yearly_proportion_per_million
    from (
        SELECT continent, location, CONCAT(YEAR(date), '-', month(date)) as 'month', 100*sum(new_deaths)*1.0/1000000 as monthly_new_deaths_per_million
        from PorfolioProject.dbo.CovidDeath
        WHERE continent is not null and YEAR(date) = '2020'
        group by continent, location, CONCAT(YEAR(date),'-', month(date))
        ) as d
    group by continent, location, left(month,4)
    --order by continent, location, left(month,4)
    ) as e
order by deaths_yearly_proportion_per_million DESC

-- compare the number of icu patients to the number of hospital admissions
-- number of icu patients is counted everyday, number of hospital admissions is weekly data
-- we can see that the proportion is continue to go down year by year
CREATE VIEW avg_icu_patients_proportion 
AS (
SELECT continent, location, year(date) as year,
    avg(100*icu_patients*1.0/weekly_hosp_admissions) as avg_icu_patients_proportion
from (
SELECT continent, location, date, icu_patients, weekly_hosp_admissions,
    DATEPART(day, date) as icu_count_start_date, ROW_NUMBER() OVER(order by date) as rank 
from PorfolioProject.dbo.CovidDeath
where location like '%states' and icu_patients is not NULL) as f 
where (rank%7) = 0
group by continent, location, year(date)
)

-- calculate the proportion of number of death to number of icu patients
SET DATEFIRST 7;
WITH deaths_proportion AS(
    SELECT continent, location,date, 100*weekly_deaths_count*1.0/weekly_icu_count as deaths_proportion
FROM(
    SELECT  continent, location, max(date) as date, sum(new_deaths) AS weekly_deaths_count, sum(icu_patients) as weekly_icu_count, DATEPART(week, date) as week_index
    from PorfolioProject.dbo.CovidDeath
    where location like '%states' and icu_patients is not NULL and left(date,4) = '2020'
    group by continent, location, DATEPART(week, date)
    UNION 
    SELECT  continent, location, max(date) as date, sum(new_deaths) AS weekly_deaths_count, sum(icu_patients) as weekly_icu_count, DATEPART(week, date) as week_index
    from PorfolioProject.dbo.CovidDeath
    where location like '%states' and icu_patients is not NULL and left(date,4) = '2021'
    group by continent, location, DATEPART(week, date)
    UNION
    SELECT  continent, location, max(date) as date, sum(new_deaths) AS weekly_deaths_count, sum(icu_patients) as weekly_icu_count, DATEPART(week, date) as week_index
    from PorfolioProject.dbo.CovidDeath
    where location like '%states' and icu_patients is not NULL and left(date,4) = '2022'
    group by continent, location, DATEPART(week, date)
    UNION 
    SELECT  continent, location, max(date) as date, sum(new_deaths) AS weekly_deaths_count, sum(icu_patients) as weekly_icu_count, DATEPART(week, date) as week_index
    from PorfolioProject.dbo.CovidDeath
    where location like '%states' and icu_patients is not NULL and left(date,4) = '2023'
    group by continent, location, DATEPART(week, date)
) as g
),

avg_yearly_deaths_proportion AS(
    SELECT continent, location, year(date) as year, avg(deaths_proportion) as avg_yearly_deaths_proportion 
    from deaths_proportion
    group by continent, location, year(date)
)

-- compare the avg_yearly_deaths/icu_proportion with the avg_icu/hospital admission_patients_proportion
-- save the results as view
CREATE VIEW avg_yearly_deaths_and_icu_patients_proportion AS
SELECT avg_yearly_deaths_proportion.continent, avg_yearly_deaths_proportion.location, avg_yearly_deaths_proportion.year, 
    avg_yearly_deaths_proportion.avg_yearly_deaths_proportion, avg_icu_patients_proportion.avg_icu_patients_proportion
from avg_yearly_deaths_proportion
JOIN avg_icu_patients_proportion ON avg_yearly_deaths_proportion.year = avg_icu_patients_proportion.year



