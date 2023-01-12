-- data is collected from TMBD, covering 5000 movies and relative infomations
--（infomation abt the movie content and information about the film crew)
-- frist we explore the movie data
SELECT top 100 *
FROM PorfolioProject..tmdb_movies_info
ORDER BY id

SELECT COUNT(distinct id)
FROM PorfolioProject..tmdb_movies_info

-- calculate the proportion of old movies and new movies(released before or after year 2000)
SELECT ct_new_movies*1.0/(SELECT COUNT(DISTINCT id) FROM PorfolioProject..tmdb_movies_info) as proportion_of_new_movies,
    ct_old_movies*1.0/(SELECT COUNT(DISTINCT id) FROM PorfolioProject..tmdb_movies_info) as proportion_of_old_movies
FROM (
    SELECT
        SUM(CASE WHEN release_date >= '2000-01-01' THEN 1 END) AS ct_new_movies,
        SUM(CASE WHEN release_date <= '2000-01-01' THEN 1 END) AS ct_old_movies
    FROM PorfolioProject..tmdb_movies_info) as a
 
-- separate element from colunms 'genres' that is json
-- calculate number of movies and proportion for each genre, there will be some overlap buz one movie can belong to several genres
-- we can see Drama has the most number of movies, and TV movies is the least popular
SELECT genre, count(*) as ct_movie
FROM(
    SELECT title, PorfolioProject..tmdb_movies_info.id, name as genre
    FROM
    PorfolioProject..tmdb_movies_info
    CROSS APPLY
    OPENJSON(PorfolioProject..tmdb_movies_info.genres)
        WITH
        (
            id   INT           N'$.id',
            name NVARCHAR(MAX) N'$.name'
        )
            AS genre
) as a
GROUP BY genre
ORDER BY count(*) DESC
-- save it as view for later usage
CREATE VIEW movies_with_separated_genres
AS 
SELECT budget, name, PorfolioProject..tmdb_movies_info.id, keywords,original_language, original_title,overview,
    popularity,production_companies,production_countries, release_date,revenue,spoken_languages,title,vote_average,vote_count
    FROM
    PorfolioProject..tmdb_movies_info
    CROSS APPLY
    OPENJSON(PorfolioProject..tmdb_movies_info.genres)
        WITH
        (
            id   INT           N'$.id',
            name NVARCHAR(MAX) N'$.name'
        )
            AS genre

-- dive deep into the top first three movies(Drama, Comedy, Thriller) and bottom ranked movies(mystery,animation,history), 
-- and calculate and compare the relative infomation(avg_ratings, production country, investment), here we ingnore the duplicates because
-- we want to see the complete situation in each genre, and to find out is there a relationship between quantity and reputation
WITH avg_rating 
AS (SELECT name As genre, avg(popularity) as avg_popularity, round(avg(vote_average),2) as avg_vote_average, count(distinct original_language) as lan_ct
FROM movies_with_separated_genres
WHERE name IN ('Drama','Comedy','Thriller','Mystery','Animation','History')
GROUP BY name
--ORDER BY avg(popularity) DESC
),

-- check the median rating
madian_rating AS (SELECT name as genre, AVG(vote_average) as median_rating
FROM(
    SELECT *, ROW_NUMBER() OVER(PARTITION BY name ORDER BY vote_average,title) as r1, ROW_NUMBER() OVER(PARTITION BY name ORDER BY vote_average DESC, title DESC) as r2
    FROM movies_with_separated_genres
    WHERE name IN ('Drama','Comedy','Thriller','Mystery','Animation','History')
    ) as b
WHERE r1 = r2 OR ABS(r1-r2) = 1
GROUP BY name
)
-- compare
-- history, animation,drama have relatively high reputation, mystery,comedy,thriller have relatively low reputation even if in large quantities 
SELECT m.genre, m.median_rating, a.avg_vote_average
FROM madian_rating m JOIN avg_rating a ON m.genre = a.genre

-- dive deep into the history, animation,drama genres and see what are the possible reasons for creating a high reputaion movies
-- the budget? jointly production? big film company?
SELECT name as genre, avg(CAST(budget as float)) as avg_budget, avg(revenue) as avg_rev, round(avg(revenue)/avg(CAST(budget as float)),2) as ROI
FROM movies_with_separated_genres
WHERE name IN ('Drama','Comedy','Thriller','Mystery','Animation','History')
GROUP BY name
-- oh we can get animation has the most budget and almost the 2~3 times of that of other genres and the ROI is also very impressive


-- count the number of co-founder countries for each film
-- also need to separate the elements
WITH count_cofounder_countries AS
    (SELECT title, count(*) as ct_jointly_production_countries
    FROM(
        SELECT title,id,name as countries
        FROM
        PorfolioProject..tmdb_movies_info
        CROSS APPLY
        OPENJSON(PorfolioProject..tmdb_movies_info.production_countries)
            WITH
            (
                iso_3166_1   NVARCHAR(MAX) N'$.iso_3166_1',
                name NVARCHAR(MAX) N'$.name'
            )
                AS prod_countries
    ) as a
    GROUP BY title
    HAVING count(*) > 1
    )

SELECT genre, avg(ct_jointly_production_countries)
FROM (
    SELECT m.title, name as genre, id, keywords, original_language, overview, popularity,production_companies,release_date,revenue,
        spoken_languages,vote_average, c.ct_jointly_production_countries
    FROM movies_with_separated_genres m 
    LEFT JOIN count_cofounder_countries c 
    ON m.title = c.title
    WHERE name IN ('Drama','Comedy','Thriller','Mystery','Animation','History') and c.ct_jointly_production_countries IS NOT NULL
    ) as c
GROUP BY genre
-- there's not obvious differences between each genre in terms of the number os jointly_production_countries
-- let's check the production company, we want to see which companies are highly productive, and what genres they mainly produce,
-- and explore that in these movies with high reputation how many of them were produced by these prolific companies
-- separate each production companies for each film
SELECT title,genre, count(*) as ct_product_companies
FROM (
        SELECT movies_with_separated_genres.title, movies_with_separated_genres.name as genre, prod_companies.name
        FROM movies_with_separated_genres
            CROSS APPLY
        OPENJSON(movies_with_separated_genres.production_companies)
            WITH
            (
                name   NVARCHAR(MAX) N'$.name',
                id INT N'$.id'
            )
                AS prod_companies
        ) AS D
GROUP BY title, genre

-- top 100 most productive film companies
CREATE VIEW movie_from_top100_companies
AS 
WITH top100_companies AS
(
SELECT top 100 company_name, COUNT(distinct title) as movie_num_by_company
FROM (
    SELECT movies_with_separated_genres.title, movies_with_separated_genres.name as genre, prod_companies.name as company_name
        FROM movies_with_separated_genres
            CROSS APPLY
        OPENJSON(movies_with_separated_genres.production_companies)
            WITH
            (
                name   NVARCHAR(MAX) N'$.name',
                id INT N'$.id'
            )
                AS prod_companies
    ) AS e 
GROUP BY company_name
ORDER BY COUNT(distinct title) DESC
)

-- cover only the movies produced by top 100 film companies
    SELECT t.company_name,t.movie_num_by_company,f.genre,f.title
    FROM top100_companies t
    JOIN (
            SELECT movies_with_separated_genres.title, movies_with_separated_genres.name as genre, prod_companies.name as company_name
            FROM movies_with_separated_genres
                CROSS APPLY
            OPENJSON(movies_with_separated_genres.production_companies)
                WITH
                (
                    name   NVARCHAR(MAX) N'$.name',
                    id INT N'$.id'
                )
                    AS prod_companies
    ) AS f 
    ON t.company_name = f.company_name

-- how many movies from other companies are excluded in the whole set
-- nearly 65% of movies are produced by top 100 companies
SELECT (SELECT COUNT(distinct title) FROM movies_with_separated_genres)-COUNT(distinct title)
FROM movie_from_top100_companies
-- see how much they contribute to movies with high reputation（rating above 7）
WITH top100_index 
AS
(SELECT (CASE WHEN top100_title IS NOT NULL THEN 'yes'
    WHEN top100_title IS NULL THEN 'no' END) AS is_it_produced_by_top100_film_companies, *
FROM (
    SELECT total.*,h.title as top100_title
    FROM PorfolioProject..tmdb_movies_info as total
    LEFT JOIN (SELECT distinct title FROM movie_from_top100_companies top100) as h
    ON total.title  = h.title
    WHERE vote_average > 7 
    ) as g
)
SELECT 100*SUM(CASE WHEN is_it_produced_by_top100_film_companies = 'yes' THEN 1 END)*1.0/count(is_it_produced_by_top100_film_companies) as percentage
FROM top100_index
-- more than 67% of movies are produced by top100 movie makers
-- to wrap up, in addition to budget and grene, a big film production company is also a guarantee of good reputation
-- while it nearly has noting to do with the jointly production countries

