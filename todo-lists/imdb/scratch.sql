-- which directors appear most often
SELECT director, COUNT(*)
FROM Movies
GROUP BY director
ORDER BY COUNT(*) DESC
LIMIT 10;

-- top-rated, most recent movies
SELECT *
FROM (
  SELECT *
  FROM Movies
  ORDER BY rating DESC
  LIMIT 20
)
ORDER BY YEAR DESC;

-- top-rated, most recent movies (ignore foreign)
SELECT *
FROM (
  SELECT *
  FROM Movies
  WHERE requiresSubtitles = 0
  ORDER BY rating DESC
  LIMIT 20
)
ORDER BY YEAR DESC;

-- most recent movies
SELECT *
FROM Movies
ORDER BY YEAR DESC
LIMIT 15;

-- most recent movies (ignore foreign)
SELECT *
FROM Movies
WHERE requiresSubtitles = 0
ORDER BY YEAR DESC
LIMIT 10;

-- only cartoons
SELECT *
FROM Movies
WHERE isCartoon = true;

-- only cartoons (ignore foreign)
SELECT *
FROM Movies
WHERE isCartoon = true AND requiresSubtitles = false;

-- show the movies from the directors that show up on the list more than once.
SELECT *
FROM Movies
WHERE director in (
  SELECT director
  FROM (
    SELECT director, COUNT(*) as num
    FROM Movies
    GROUP BY director
    HAVING num > 1
    ORDER BY num DESC
  )
)
ORDER BY director, rating DESC, year DESC;
